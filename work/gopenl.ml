open Printf

module Color = struct
  type t = int (* RGB*)
  let make r g b = r*256*256 + g*256 + b (* no A yet *)
  let red   = make 255 0 0
  let green = make 0 255 0
  let white = make 255 255 255
  let blue  = make 0 0 255
  let yel   = make 255 255 0
end

module Vector2D = struct
  type t = int * int
  let make x y = (x,y)
  let (+) (x0,y0) (x1,y1) = (x0+x1, y0+y1)
  let (-) (x0,y0) (x1,y1) = (x0-x1, y0-y1)
  let ( * ) (x0,y0) c = (x0*c, y0*c)
  let mul_float (x,y) f = (float_of_int x *. f |> int_of_float, float_of_int y *. f |> int_of_float)
  let x = fst
  let y = snd
end

module Image = struct
  type t = { width: int
           ; height: int
           ; pix: int array
           ; mutable mirror_y: bool
           }
  let create width height =
    { width; height; pix = Array.make (width*height) 0; mirror_y=false }

  let set ~img:{width; height; pix; mirror_y} x y ~(color:Color.t) =
    let y = if mirror_y then height-y else y in
    try pix.(x+y*width) <- color
    with Invalid_argument s -> failwith @@ sprintf "Can't set pixel (%d,%d): %s" x y  s

  let set_mirror_y : t -> unit = fun img -> img.mirror_y <- true

end

let rec line ~color ~img x0 y0 x1 y1 =
  let rec helper steep x0 y0 x1 y1 =
    (*printf "Drawing %b %d %d %d %d\n%!" steep x0 y0 x1 y1;*)
    let dx = x1-x0 in
    let dy = y1-y0 in

    let derror2 = 2 * abs dy in
    let error2  = ref 0 in
    let y = ref y0 in

    for x=x0 to x1 do
      if steep
      then Image.set ~img ~color !y  x
      else Image.set ~img ~color  x !y;

      error2 := !error2 + derror2;
      if !error2 > dx then ( if y1>y0 then incr y else decr y; error2 := !error2 - 2*dx );
    done;
    ()
  in

  let (steep, x0, y0, x1, y1) =
    if abs (x0 - x1) < abs (y0 - y1)
    then (true, y0, x0, y1, x1)
    else (false, x0, y0, x1, y1)
  in
  let (steep, x0, y0, x1, y1) =
    if x0>x1 then (steep, x1, y1, x0, y0) else (steep, x0, y0, x1, y1)
  in
  helper steep x0 y0 x1 y1
