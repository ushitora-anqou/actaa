module ProcessSet = Set.Make (struct
  type t = Process.t0

  let compare = compare
end)

type 'a key = 'a Process.t2 Hmap.key

type register_param =
  | P : 'a Process.t2 Hmap.key * 'a Process.t2 -> register_param

type send_param = P : 'a Process.t2 Hmap.key * 'a -> send_param
type init_arg = unit
type call_msg = [ `Register of register_param | `Send of send_param ]

type call_reply =
  [ `Register of bool (* correctly registered *)
  | `Send of bool (* correctly sent *) ]

type cast_msg = [ `Stop ]
type info_msg = [ `EXIT of Process.t0 * Process.Stop_reason.t ]
type msg = [ (call_msg, call_reply, cast_msg) Gen_server.basic_msg | info_msg ]
type state = { m : Hmap.t; died : ProcessSet.t (* FIXME: inefficient *) }

class t =
  object (self)
    inherit [init_arg, msg, state] Gen_server.behaviour

    method private init _env ~sw:_ _ =
      { m = Hmap.empty; died = ProcessSet.empty }

    method! private handle_cast _env ~sw:_ state =
      function `Stop -> `Stop (Process.Stop_reason.Normal, state)

    method! private handle_call _env ~sw:_ state =
      function
      | `Register (P (k, v)) -> (
          let do_register () =
            Process.link v (self :> Process.monitor);
            `Reply (`Register true, { state with m = Hmap.add k v state.m })
          in
          match Hmap.find k state.m with
          | None -> do_register ()
          | Some v' when ProcessSet.mem (v' :> Process.t0) state.died ->
              do_register ()
          | Some _ -> `Reply (`Register false, state))
      | `Send (P (k, msg)) -> (
          match Hmap.find k state.m with
          | None -> `Reply (`Send false, state)
          | Some v when ProcessSet.mem (v :> Process.t0) state.died ->
              `Reply (`Send false, state)
          | Some v ->
              Process.send v msg;
              `Reply (`Send true, state))

    method! private handle_info _env ~sw:_ state =
      function
      | #Gen_server.basic_msg -> assert false
      | `EXIT (p, _) ->
          `NoReply { state with died = ProcessSet.add p state.died }
  end

let _ = (new t :> _ Gen_server.t)
let create_key () = Hmap.Key.create ()

let register (s : t) k v =
  match Gen_server.call s (`Register (P (k, v))) with
  | `Register b -> b
  | _ -> assert false

let send (s : t) k msg =
  match Gen_server.call s (`Send (P (k, msg))) with
  | `Send b -> b
  | _ -> assert false

let stop (s : t) = Gen_server.cast s `Stop
