type ('a, 'state) call_result =
  [ `Reply of 'a * 'state | `Stop of Process.Stop_reason.t * 'a * 'state ]

type 'state cast_result =
  [ `NoReply of 'state | `Stop of Process.Stop_reason.t * 'state ]

type 'state info_result =
  [ `NoReply of 'state | `Stop of Process.Stop_reason.t * 'state ]

type ('call_msg, 'call_reply, 'cast_msg) basic_msg =
  [ `Cast of 'cast_msg
  | `Call of 'call_msg * 'call_reply Eio.Stream.t
  | `Stop of Process.Stop_reason.t * unit Eio.Stream.t ]

class virtual ['init_arg, 'msg, 'state] behaviour =
  object (self)
    inherit ['init_arg, 'msg] Process.t

    method private virtual init
        : Eio_unix.Stdenv.base -> sw:Eio.Switch.t -> 'init_arg -> 'state

    method private handle_call (_ : Eio_unix.Stdenv.base) ~sw:(_ : Eio.Switch.t)
        (_ : 'state) (_ : 'call_msg) : ('call_reply, 'state) call_result =
      failwith "not implemented"

    method private handle_cast (_ : Eio_unix.Stdenv.base) ~sw:(_ : Eio.Switch.t)
        (_ : 'state) (_ : 'cast_msg) : 'state cast_result =
      failwith "not implemented"

    method private handle_info (_ : Eio_unix.Stdenv.base) ~sw:(_ : Eio.Switch.t)
        (_ : 'state) (_ : 'info_msg) : 'state info_result =
      failwith "not implemented"

    method private terminate (_ : Eio_unix.Stdenv.base) ~sw:(_ : Eio.Switch.t)
        (_ : 'state) (_ : Process.Stop_reason.t) =
      ()

    method private run (env : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t)
        (args : 'init_arg) : Process.Stop_reason.t =
      let state = self#init env ~sw args in
      let rec loop state =
        match self#receive with
        | `Call (msg, reply_stream) -> (
            match self#handle_call env ~sw state msg with
            | `Reply (reply, state) ->
                Eio.Stream.add reply_stream reply;
                loop state
            | `Stop (reason, reply, state) ->
                Eio.Stream.add reply_stream reply;
                (reason, state))
        | `Cast msg -> (
            match self#handle_cast env ~sw state msg with
            | `NoReply state -> loop state
            | `Stop (reason, state) -> (reason, state))
        | `Stop (reason, reply_stream) ->
            Eio.Stream.add reply_stream ();
            (reason, state)
        | msg -> (
            match self#handle_info env ~sw state msg with
            | `NoReply state -> loop state
            | `Stop (reason, state) -> (reason, state))
      in
      let reason, state = loop state in
      self#terminate env ~sw state reason;
      reason

    method on_spawn env ~sw (args : 'init_arg) = self#run env ~sw args
  end

let start = Process.spawn

let stop s =
  let stream = Eio.Stream.create 0 in
  s#send (`Stop (Process.Stop_reason.Normal, stream));
  Eio.Stream.take stream

let call s msg =
  let stream = Eio.Stream.create 0 in
  s#send (`Call (msg, stream));
  Eio.Stream.take stream

let cast s msg = s#send (`Cast msg)

class type ['call_msg, 'call_reply, 'cast_msg] t = object
  method send :
    [ `Call of 'call_msg * 'call_reply Eio.Stream.t | `Cast of 'cast_msg ] ->
    unit
end

class type ['cast_msg] t_cast = object
  method send : [ `Cast of 'cast_msg ] -> unit
end
