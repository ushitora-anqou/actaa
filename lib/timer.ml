type bot = |
type 'id receiver = [ `Timeout of 'id ] Process.t1
type 'id init_arg = { id : 'id; seconds : float; target : 'id receiver }

class ['id] t =
  object
    inherit ['id init_arg, bot] Process.t

    method on_spawn env ~sw:_ a =
      Eio.Time.sleep (Eio.Stdenv.clock env) a.seconds;
      Process.send a.target (`Timeout a.id);
      Process.Stop_reason.Normal
  end

let spawn env ~sw ~id ~seconds ~target =
  let t = new t in
  t |> Process.spawn env ~sw { id; seconds; target };
  t
