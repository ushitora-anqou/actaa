module Make (S : sig
  type key
  type process = private < link : Process.monitor -> unit ; .. >

  val compare : key -> key -> int
end) =
struct
  module ProcessSet = Set.Make (struct
    type t = Process.t0

    let compare = compare
  end)

  module M = Map.Make (struct
    type t = S.key

    let compare = S.compare
  end)

  type init_arg = unit
  type call_msg = [ `Register of S.key * S.process | `Lookup of S.key ]

  type call_reply =
    [ `Register of bool (* is correctly registered *)
    | `Lookup of S.process option ]

  type cast_msg = [ `Stop ]
  type info_msg = [ `EXIT of Process.t0 * Process.Stop_reason.t ]
  type msg = [ (call_msg, call_reply, cast_msg) Gen_server.basic_msg | info_msg ]

  type state = {
    m : S.process M.t;
    died : ProcessSet.t; (* FIXME: inefficient *)
  }

  class t =
    object (self)
      inherit [init_arg, msg, state] Gen_server.behaviour

      method private init _env ~sw:_ _ =
        { m = M.empty; died = ProcessSet.empty }

      method! private handle_cast _env ~sw:_ state =
        function `Stop -> `Stop (Process.Stop_reason.Normal, state)

      method! private handle_call _env ~sw:_ state =
        function
        | `Register (k, v) -> (
            let do_register state =
              Process.link v (self :> Process.monitor);
              `Reply (`Register true, { state with m = M.add k v state.m })
            in
            match M.find_opt k state.m with
            | None -> do_register state
            | Some v' when ProcessSet.mem (v' :> Process.t0) state.died ->
                let state =
                  {
                    state with
                    died = ProcessSet.remove (v' :> Process.t0) state.died;
                  }
                in
                do_register state
            | Some _ -> `Reply (`Register false, state))
        | `Lookup k -> (
            match M.find_opt k state.m with
            | None -> `Reply (`Lookup None, state)
            | Some v when ProcessSet.mem (v :> Process.t0) state.died ->
                let state =
                  {
                    m = M.remove k state.m;
                    died = ProcessSet.remove (v :> Process.t0) state.died;
                  }
                in
                `Reply (`Lookup None, state)
            | Some v -> `Reply (`Lookup (Some v), state))

      method! private handle_info _env ~sw:_ state =
        function
        | #Gen_server.basic_msg -> assert false
        | `EXIT (p, _) ->
            `NoReply { state with died = ProcessSet.add p state.died }
    end

  let _ = (new t :> _ Gen_server.t)

  let register (s : t) k v =
    match Gen_server.call s (`Register (k, v)) with
    | `Register b -> b
    | _ -> assert false

  let lookup (s : t) k =
    match Gen_server.call s (`Lookup k) with
    | `Lookup v -> v
    | _ -> assert false

  let stop (s : t) = Gen_server.cast s `Stop
  let make () = new t
  let start env ~sw (s : t) = Gen_server.start env ~sw () s
end
