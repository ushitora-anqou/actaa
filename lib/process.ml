module Stop_reason = struct
  type t = ..
  type t += Normal | Exn of string

  let to_string = function
    | Normal -> "normal"
    | Exn s -> Printf.sprintf "exception: %s" s
    | _ -> "unknown reason"
end

class type t0 = object end

class type ['msg] t1 = object
  method send : 'msg -> unit
end

type monitor = [ `EXIT of t0 * Stop_reason.t ] t1

class ['msg] t2 =
  object
    val m : 'msg Mailbox.t = Mailbox.create ()
    val mtx : Eio.Mutex.t = Eio.Mutex.create ()
    val mutable links : monitor list = []
    method send : 'msg -> unit = Mailbox.send m
    method private receive : 'msg = Mailbox.receive m

    method link l =
      Eio.Mutex.use_rw ~protect:true mtx @@ fun () -> links <- l :: links
  end

exception Stop of Stop_reason.t

type _ Effect.t += Trace : string Effect.t

let setup f =
  Effect.(
    Deep.(
      try_with f ()
        {
          effc =
            (fun (type a) (e : a t) ->
              match e with
              | Trace ->
                  Some
                    (fun (k : (a, _) continuation) ->
                      let bt =
                        Printexc.raw_backtrace_to_string (get_callstack k 100)
                      in
                      continue k bt)
              | _ -> None);
        }))

class virtual ['init_arg, 'msg] t =
  object (self)
    inherit ['msg] t2

    method virtual on_spawn
        : Eio_unix.Stdenv.base ->
          sw:Eio__core.Switch.t ->
          'init_arg ->
          Stop_reason.t

    method spawn ?(raise_exn = false) env ~sw args : unit =
      Eio.Fiber.fork ~sw @@ fun () ->
      let reason =
        try self#on_spawn env ~sw args
        with e ->
          Stop_reason.Exn
            (Printf.sprintf "exception: %s\n%s" (Printexc.to_string e)
               (Printexc.get_backtrace ()))
      in
      Eio.Mutex.use_rw ~protect:true mtx @@ fun () ->
      links |> List.iter (fun l -> l#send (`EXIT ((self :> t0), reason)));
      if reason <> Stop_reason.Normal then (
        (* FIXME: more reliable and fast way? *)
        let trace = Effect.perform Trace in
        Logs.info (fun m ->
            m "process stopped abnormally: %s: spawned with trace\n%s"
              (Stop_reason.to_string reason)
              trace);
        if raise_exn then failwith (Stop_reason.to_string reason))
  end

let send s msg = s#send msg
let spawn ?raise_exn env ~sw args s : unit = s#spawn ?raise_exn env ~sw args
let link s l = s#link l
