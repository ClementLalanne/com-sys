module PictureInPicture (K : Kahn.S) = struct
  module K = K
  module Lib = Kahn.Lib(K)
  open Lib

  let chanel1 (qo : 'a K.out_port) : unit K.process =
    let rec loop l1 l2 = match l1 with
      [] -> loop (List.rev l2) []
      |t::q -> (K.put (Images.read_image t) qo) >>= (fun () -> loop q (t::l2))
    in
    loop ["Vid1/Lena.512.ppm"; "Vid2/Mire_HSV.512.ppm"] []

  let chanel2 (qo : 'a K.out_port) : unit K.process =
    let rec loop l1 l2 = match l1 with
      [] -> loop (List.rev l2) []
      |t::q -> (K.put (Images.read_image t) qo) >>= (fun () -> loop q (t::l2))
    in
    loop ["Vid2/Mire_HSV.512.ppm"; "Vid1/Lena.512.ppm"] []

  let output (q1 : 'a K.in_port) (q2 : 'a K.in_port) : unit K.process =
    let rec loop () =
      (K.get q1) >>=
        (fun im1 ->
          (K.get q2) >>=
            (fun im2 ->
              Graphics.draw_image
                (Images.image_to_image
                    (Images.supperpose im1 (Images.resize im2 100 100))) 0 0;
              loop ()))
    in
    Graphics.open_graph " 600x600";
    loop ()

  let main2 : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q1_in, q1_out) -> (delay K.new_channel ()) >>= (fun (q2_in, q2_out) ->
              K.doco [ chanel1 q1_out ; chanel2 q2_out ; output q1_in q2_in]))

end
