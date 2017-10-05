let main () = match Sys.argv.(1) with
  |"PictureInPicture" -> (match Sys.argv.(2) with
    |"Sequencial" ->
      let module E = Picture_in_picture.PictureInPicture(Kahn.SequencialKahn) in
      ignore (E.K.run E.main2 )
    |"Unix" ->
      let module E = Picture_in_picture.PictureInPicture(Kahn.UnixKahn) in
      ignore (E.K.run E.main2 )
    |"Socket" ->
      let module E = Picture_in_picture.PictureInPicture(Kahn.SocketKahn) in
      ignore (E.K.run E.main2 )
    | _ -> failwith "Entree invalide")
  |"Entiers" -> (match Sys.argv.(2) with
    |"Sequencial" ->
      let module E = Entiers.Example(Kahn.SequencialKahn) in
      ignore (E.K.run E.main )
    |"Unix" ->
      let module E = Entiers.Example(Kahn.UnixKahn) in
      ignore (E.K.run E.main )
    |"Socket" ->
      let module E = Entiers.Example(Kahn.SocketKahn) in
      ignore (E.K.run E.main )
    | _ -> failwith "Entree invalide")
  |_ -> failwith "Entree invalide"

let () = main ()
