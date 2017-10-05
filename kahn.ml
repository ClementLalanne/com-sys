module type S = sig
  type 'a process
  type 'a in_port
  type 'a out_port

  val new_channel: unit -> 'a in_port * 'a out_port
  val put: 'a -> 'a out_port -> unit process
  val get: 'a in_port -> 'a process

  val doco: unit process list -> unit process

  val return: 'a -> 'a process
  val bind: 'a process -> ('a -> 'b process) -> 'b process

  val run: 'a process -> 'a
end

module Lib (K : S) = struct

  let ( >>= ) x f = K.bind x f

  let delay f x =
    K.bind (K.return ()) (fun () -> K.return (f x))

  let par_map f l =
    let rec build_workers l (ports, workers) =
      match l with
      | [] -> (ports, workers)
      | x :: l ->
          let qi, qo = K.new_channel () in
          build_workers
            l
            (qi :: ports,
             ((delay f x) >>= (fun v -> K.put v qo)) :: workers)
    in
    let ports, workers = build_workers l ([], []) in
    let rec collect l acc qo =
      match l with
      | [] -> K.put acc qo
      | qi :: l -> (K.get qi) >>= (fun v -> collect l (v :: acc) qo)
    in
    let qi, qo = K.new_channel () in
    K.run
      ((K.doco ((collect ports [] qo) :: workers)) >>= (fun _ -> K.get qi))

end

module SequencialKahn: S = struct
	type 'a process = ('a -> unit) -> unit
	type 'a channel = 'a Queue.t
	type 'a in_port = 'a channel
	type 'a out_port = 'a channel

	let toDo = Queue.create ()

	let new_channel () =
		let q = Queue.create () in
		q, q

  let put_aux x c continuation =
    Queue.push x c;
    Queue.push (fun () -> continuation ()) toDo

	let put x c = put_aux x c

  let rec get_aux c continuation =
		try
	   let v = Queue.pop c in (Queue.push (fun () -> continuation v) toDo)
	  with Queue.Empty ->
			Queue.push (fun () -> get_aux c continuation) toDo

  let get c = get_aux c

	let doco_aux l continuation=
		 let processCounter = ref (List.length l) in
     List.iter
      (fun p ->
        Queue.push (fun () ->
          p (fun () -> processCounter := !processCounter - 1)) toDo) l;
     let rec isFinished () = match !processCounter with
       | 0 -> continuation ()
       |_ -> Queue.push isFinished toDo
     in isFinished ()

  let doco l = doco_aux l

	let return_aux v continuation = continuation v

  let return v = return_aux v

	let bind_aux e f continuation = e (fun x -> f x continuation)

  let bind e f = bind_aux e f

	let run e =
    let res = ref None in
		e (fun x -> res := Some x);
		while not (Queue.is_empty toDo) do
			let x = Queue.pop toDo in
			x ()
		done;
    match !res with
      | Some x -> x
      | None -> failwith "This case never happens"

end

module UnixKahn: S = struct
  type 'a process = (unit -> 'a)
  type 'a in_port = in_channel
  type 'a out_port = out_channel


  let new_channel () = let a,b= Unix.pipe () in
    (Unix.in_channel_of_descr a, Unix.out_channel_of_descr b)

  let put v c () =
    Marshal.to_channel c v [];
    flush c

  let run e = e ()

  let rec get c () =
    Marshal.from_channel c

 let doco l () =
    let ths = List.map (fun f -> Thread.create f ()) l in
    List.iter (fun th -> Thread.join th) ths

    let return v = (fun () -> v)

    let bind e e' () =
      let v = e () in
      e' v ()


end

module SocketKahn: S = struct
  type 'a process = (unit -> 'a)
  type 'a in_port = in_channel
  type 'a out_port = out_channel

  let sockets =
    ref [   "shes" ;
            "got" ;
            "a" ;
            "smile" ;
            "that" ;
            "it" ;
            "seems" ;
            "to" ;
            "me" ;
            "remind" ;
            "our" ;
            "childhood" ;
            "memories" ]

  let new_channel () =
    let a = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let b = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let port = ref "" in
    (try
      port := (List.hd !sockets) ;
      sockets := (List.tl !sockets) ;
    with
      _ -> failwith "too many sockets used");
    Unix.bind a (Unix.ADDR_UNIX  !port) ;
    Unix.listen a 1 ;
    Unix.connect b (Unix.ADDR_UNIX !port) ;
    let c,d = Unix.accept a in
    (Unix.in_channel_of_descr c, Unix.out_channel_of_descr b)

  let put v c () =
    Marshal.to_channel c v [];
    flush c

  let run e = e ()

  let rec get c () =
    Marshal.from_channel c

  let doco l () =
    let ths = List.map (fun f -> Thread.create f ()) l in
    List.iter (fun th -> Thread.join th) ths

    let return v = (fun () -> v)

    let bind e e' () =
      let v = e () in
      e' v ()
end
