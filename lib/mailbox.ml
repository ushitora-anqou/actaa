type 'a t = {
  q : 'a Queue.t;
  c : Eio.Condition.t;
  m : Eio.Mutex.t;
  closed : bool Atomic.t;
}

let create () =
  {
    q = Queue.create ();
    c = Eio.Condition.create ();
    m = Eio.Mutex.create ();
    closed = Atomic.make false;
  }

let close t = Atomic.set t.closed true

let is_empty t =
  if Atomic.get t.closed then true
  else Eio.Mutex.use_ro t.m (fun () -> Queue.is_empty t.q)

let send t v =
  if Atomic.get t.closed then (
    Logs.warn (fun m -> m "Mailbox.send: already closed");
    ())
  else (
    Eio.Mutex.use_rw ~protect:false t.m (fun () -> Queue.add v t.q);
    Eio.Condition.broadcast t.c)

let receive t =
  Eio.Mutex.use_rw ~protect:false t.m (fun () ->
      while Queue.is_empty t.q do
        Eio.Condition.await t.c t.m
      done;
      Queue.take t.q)

let receive_nowait t =
  Eio.Mutex.use_rw ~protect:false t.m (fun () -> Queue.take_opt t.q)
