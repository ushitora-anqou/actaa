type msg = [ `EXIT of Process.t0 * Process.Stop_reason.t ]

type ('init_arg, 'msg) config = {
  process_factory : unit -> ('init_arg, 'msg) Process.t;
  process_arg : 'init_arg;
  max_restarts : int;
  max_seconds : float;
}

let list_take_at_most n =
  let rec aux n acc = function
    | [] -> (List.rev acc, [])
    | rest when n = 0 -> (List.rev acc, rest)
    | x :: xs -> aux (n - 1) (x :: acc) xs
  in
  aux n []

class virtual ['init_arg, 'child_init_arg, 'child_msg] t =
  object (self)
    inherit ['init_arg, msg] Process.t
    method virtual init : 'init_arg -> ('child_init_arg, 'child_msg) config

    method on_spawn env ~sw arg =
      let c = self#init arg in

      let rec loop restarts =
        let p = c.process_factory () in
        Process.link p (self :> Process.monitor);
        Process.spawn env ~sw c.process_arg p;
        match self#receive with
        | `EXIT (_, Process.Stop_reason.Normal) -> ()
        | `EXIT (_, reason) ->
            Logs.warn (fun m ->
                m "A supervised process died: %s"
                  (match reason with
                  | Process.Stop_reason.Exn s -> s
                  | _ -> "unknown reason"));
            (* Check if we have reached the maximum number of restarts *)
            let restarts, _ =
              list_take_at_most c.max_restarts (Unix.gettimeofday () :: restarts)
            in
            let duration = List.hd restarts -. List.hd (List.rev restarts) in
            if List.length restarts = c.max_restarts && duration < c.max_seconds
            then ()
            else (
              Logs.warn (fun m -> m "Restarting a gen server");
              loop restarts)
      in
      loop [];
      Process.Stop_reason.Normal
  end
