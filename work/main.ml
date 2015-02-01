open Printf

let swap: 'a . 'a ref -> 'a ref -> unit = fun r1 r2 ->
  let x = !r1 in
  r1 := !r2;
  r2 := x

let rec _dline0 () = if false then _dline0 ()
let rec _dline1 () = if false then _dline1 ()
let rec _dline2 () = if false then _dline2 ()
let rec _dline3 () = if false then _dline3 ()
let rec _dline4 () = if false then _dline4 ()

open Gopenl

type image_data =
  { vertexes: (float*float*float) array
  ; triangles: (int*int*int) list
  }

let get_vertex ~n (data: image_data) =
  let len = Array.length data.vertexes in
  if n >= len then failwith (sprintf "Can't get vertex %d in array len=%d" n len);
  data.vertexes.(n)

let read_image_data filename =
  let ch = open_in filename in
  let rec loop verts tris =
    try
      let str = input_line ch in
      if str = "" then loop verts tris
      else if Str.first_chars str 2 = "vt" then loop verts tris
      else if Str.first_chars str 2 = "vn" then loop verts tris
      else if Str.first_chars str 1 = "#"  then loop verts tris
      else if Str.first_chars str 1 = "g"  then loop verts tris
      else if Str.first_chars str 1 = "s"  then loop verts tris
      else if Str.first_chars str 2 = "f " then (
        let tri = Scanf.sscanf str "f %d/%d/%d %d/%d/%d %d/%d/%d" (fun n1 _ _ n2 _ _ n3 _ _ -> (n1,n2,n3) ) in
        loop verts (tri::tris)
      )
      else if Str.first_chars str 2 = "v " then (
        let v = Scanf.sscanf str "v %f %f %f" (fun x y z -> (x,y,z) ) in
        loop (v::verts) tris
      ) else failwith "Reading file is not fully implemented"
    with End_of_file -> (verts, tris)
  in
  let (verts, triangles) = loop [] [] in
  close_in ch;
  { vertexes = Array.of_list @@ List.rev verts; triangles }

type config =
  { mutable line: color:Color.t -> img:Image.t -> int -> int -> int -> int -> unit
  ; mutable doread: bool
  }

let config = { line = Gopenl.line; doread=false }

let main =
  let red   = Color.make 255 0 0 in
  let green = Color.make 0 255 0 in
  let blue  = Color.make 0 0 255 in
  let yel   = Color.make 255 255 0 in
  let _ = [red; green; blue; yel] in
  let args =
    [ ("-read", Arg.Unit (fun () -> config.doread <- true), "really read")
    ]
  in
  Arg.parse args (fun s -> failwith @@ sprintf "No anonymous args '%s'\n%!" s) "usage";

  let width = 800 in
  let height = 800 in
  let img   = Image.create width height in

  if config.doread then begin
      let data = read_image_data "data/african_head.obj" in
      printf "File is read: %d %d\n%!" (Array.length data.vertexes) (List.length data.triangles);
      let f (n1,n2,n3) =
        (* float argument is in range [ -1 .. +1 ] which should be project onto [0..(width-1)] *)
        (* Also we mirror y *)
        let p1 = get_vertex ~n:(n1-1) data in
        let p2 = get_vertex ~n:(n2-1) data in
        let p3 = get_vertex ~n:(n3-1) data in
        let f = fun ( (x0f,y0f,_), (x1f,y1f,_) ) ->
          let x0 = (x0f +. 1.) *. (float_of_int (width-1) ) /. 2.0  |> int_of_float in
          let y0 = (y0f +. 1.) *. (float_of_int (height-1)) /. 2.0  |> int_of_float |> (fun y -> height-1-y) in
          let x1 = (x1f +. 1.) *. (float_of_int (width-1) ) /. 2.0  |> int_of_float in
          let y1 = (y1f +. 1.) *. (float_of_int (height-1)) /. 2.0  |> int_of_float |> (fun y -> height-1-y) in
          Gopenl.line ~img ~color:green x0 y0 x1 y1
        in
        List.iter f [ p1,p2; p2,p3; p3,p1 ]
      in
      List.iter f data.triangles
  end;
  Tga.write_tga ~filename:"out.tga" ~img
