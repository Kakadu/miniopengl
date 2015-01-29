open Printf

module Color = struct
  type t = int (* RGB*)
  let make r g b = r*256*256 + g*256 + b (* no A yet *)
end

module Image = struct
  type t = { width: int
           ; height: int
           ; pix: int array
           }
  let create width height =
    { width; height; pix = Array.make (width*height) 0 }

  let set ~img:{width; height; pix} x y ~(color:Color.t) =
    try pix.(x+y*width) <- color
    with Invalid_argument s -> failwith @@ sprintf "Can't set pixel (%d,%d): %s" x y  s


end
