(*PPM Images*)

let read_image nom =
  let ic = open_in nom in
  ignore (input_line ic) ;
  let dims = input_line ic in
  ignore (input_line ic) ;
  let l = String.split_on_char ' ' dims in
  let w = List.hd l in
  let h = (List.hd (List.tl l)) in
  let ip = Array.make_matrix (int_of_string h) (int_of_string w) (0,0,0) in
  for i = 0 to (int_of_string h) - 1 do
    for j = 0 to (int_of_string w) -1 do
      let r = input_byte ic in
      let g = input_byte ic in
      let b = input_byte ic in
      ip.(i).(j) <- (r,g,b)
    done;
  done;
  close_in ic;
  ip

let resize image width height =
  let hp = Array.length image in
  let wp = Array.length image.(0) in
  let ip = Array.make_matrix height width (0,0,0,0) in
  for i = 0 to height - 1 do
    for j = 0 to width -1 do
      let x = int_of_float ((float_of_int i ) /.
                            (float_of_int height) *. (float_of_int hp)) in
      let y = int_of_float ((float_of_int j ) /.
                            (float_of_int width) *. (float_of_int wp)) in
      let (a,b,c,d) = ip.(i).(j) in
      let (e,f,g) = image.(x).(y)in
      ip.(i).(j) <- (a+e,b+f,c+g,d+1)
    done;
  done;
  let ipp = Array.make_matrix height width (0,0,0) in
  for i = 0 to height - 1 do
    for j = 0 to width -1 do
      let (a,b,c,d) = ip.(i).(j) in
      ipp.(i).(j) <- (a/d mod 255, b/d mod 255, c/d mod 255)
    done;
  done;
  ipp

let supperpose background foreground =
  let h = Array.length background in
  let w = Array.length background.(0) in
  let hp = Array.length foreground in
  let wp = Array.length foreground.(0) in
  let ip = Array.make_matrix h w (0,0,0) in
  for i = 0 to h - 1 do
    for j = 0 to w -1 do
      ip.(i).(j) <- background.(i).(j)
    done;
  done;
  for i = 0 to (min h hp) - 1 do
    for j = 0 to (min w wp) - 1 do
      ip.(i).(j) <- foreground.(i).(j)
    done;
  done;
  ip

let image_to_image image =
  let h = Array.length image in
  let w = Array.length image.(0) in
  let im = Array.make_matrix h w Graphics.transp in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      let (r,g,b) = image.(i).(j) in
      im.(i).(j) <- Graphics.rgb r g b
    done;
  done;
  Graphics.make_image im
