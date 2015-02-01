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
open Wavefront

type config =
  { mutable line: color:Color.t -> img:Image.t -> int -> int -> int -> int -> unit
  ; mutable doread: bool
  }

let config = { line = Gopenl.line; doread=false }
(*
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
 *)

let triangle ~img ~color p0 p1 p2 =
  let sort_2dots ((_,y0)as p0) ((_,y1)as p1) =  if y0 < y1 then (p0,p1) else (p1,p0) in
  let sort_3dots p0 p1 p2 =
    let (p0,p1) = sort_2dots p0 p1 in
    let (p1,p2) = sort_2dots p1 p2 in
    let (p0,p1) = sort_2dots p0 p1 in
    assert ( (snd p0 < snd p1) && (snd p1 < snd p2) );
    (p0,p1,p2)
  in
  let ( (x0,y0) as p0), ((x1,y1) as p1),  ((x2,y2) as p2) = sort_3dots p0 p1 p2 in
  let dx = x1-x0 in
  let dy = y1-y0 in

  let total_height = y2-y0 in
  for y=y0 to y1 do
    let segment_height = y1-y0+1 in
    let alpha = (float_of_int (y-y0)) /. (float_of_int total_height) in
    let beta  = (float_of_int (y-y0)) /. (float_of_int segment_height) in
    (* http://habrahabr.ru/post/248159/ *)
    ()
  done;
  line ~color:Color.green ~img x0 y0 x1 y1;
  line ~color:Color.green ~img x1 y1 x2 y2;
  line ~color:Color.red   ~img x2 y2 x0 y0;
  ()

let main =
  let width  = 200 in
  let height = 200 in
  let img   = Image.create width height in
  Image.set_mirror_y img;
  triangle ~img ~color:Color.red   ( 10, 70) ( 50,160) ( 70, 80);
  triangle ~img ~color:Color.white (180, 50) (150,  1) ( 70,180);
  triangle ~img ~color:Color.green (180,150) (120,160) (130,180);
  Tga.write_tga ~filename:"out.tga" ~img
