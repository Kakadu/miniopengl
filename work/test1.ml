open Printf
let app_name = "Test 1"

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

let rec line0 ~color ~img x0 y0 x1 y1 =
  _dline0 ();

  let rec helper steep x0 y0 x1 y1 =
    for x= x0 to x1 do
      let t = float_of_int (x - x0) /. float_of_int (x1 - x0) in
      let y = float_of_int y0 *. (1. -. t) +. (float_of_int y1  *. t) in
      if steep
      then Image.set ~img ~color (int_of_float y) x
      else Image.set ~img ~color x (int_of_float y)
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
  helper steep x0 y0 x1 y1;
  _dline1 ()

let rec line4 ~color ~img x0 y0 x1 y1 =
  _dline0 ();

  let rec helper steep x0 y0 x1 y1 =
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
  helper steep x0 y0 x1 y1;
  _dline1 ()

let rec line ~color ~img x0 y0 x1 y1 =
  _dline0 ();
  let x0 = ref x0 in
  let x1 = ref x1 in
  let y0 = ref y0 in
  let y1 = ref y1 in
  let steep = ref false in

  if abs (!x0 - !x1) < abs(!y0 - !y1) then begin
      swap x0 y0;
      swap x1 y1;
      steep := true;
  end;
  if x0>x1 then begin
      swap x0 x1;
      swap y0 y1;
  end;

  for x= !x0 to !x1 do
    let t = float_of_int (x - !x0) /. float_of_int (!x1 - !x0) in
    let y = float_of_int !y0 *. (1. -. t) +. (float_of_int !y1  *. t) in
    if !steep
    then Image.set ~img ~color (int_of_float y) x
    else Image.set ~img ~color x (int_of_float y)
  done;
  _dline1 ()

let rec line2 ~color ~img x0 y0 x1 y1 =
  _dline2 ();
  let dx = x1-x0 in
  let dy = y1-y0 in

  let x0 = ref x0 in
  let x1 = ref x1 in
  let y0 = ref y0 in
  let y1 = ref y1 in
  let steep = ref false in

  if abs (!x0 - !x1) < abs(!y0 - !y1) then begin
      swap x0 y0;
      swap x1 y1;
      steep := true;
  end;
  if x0>x1 then begin
      swap x0 x1;
      swap y0 y1;
  end;

  let derror = (float_of_int @@ abs dy) /. (float_of_int @@ abs dx) in
  let error  = ref 0. in
  let y = ref !y0 in

  for x= !x0 to !x1 do
    if !steep
    then Image.set ~img ~color !y  x
    else Image.set ~img ~color  x !y;

    error := !error +. derror;
    if !error > 0.5 then ( (if y1>y0 then incr else decr) y; error := !error -. 1. );
  done;
  _dline3 ()

let rec line3 ~color ~img x0 y0 x1 y1 =
  _dline2 ();
  let dx = x1-x0 in
  let dy = y1-y0 in

  let x0 = ref x0 in
  let x1 = ref x1 in
  let y0 = ref y0 in
  let y1 = ref y1 in
  let steep = ref false in

  if abs (!x0 - !x1) < abs(!y0 - !y1) then begin
      swap x0 y0;
      swap x1 y1;
      steep := true;
  end;
  if x0>x1 then begin
      swap x0 x1;
      swap y0 y1;
  end;

  let derror2 = 2 * abs dy in
  let error2  = ref 0 in
  let y = ref !y0 in

  for x= !x0 to !x1 do
    if !steep
    then Image.set ~img ~color !y  x
    else Image.set ~img ~color  x !y;

    error2 := !error2 + derror2;
    if !error2 > dx then ( (if y1>y0 then incr else decr) y; error2 := !error2 - 2*dx );
  done;
  _dline3 ()

type config =
  { mutable line: color:Color.t -> img:Image.t -> int -> int -> int -> int -> unit
  }

let config = { line = line }

let main =
  print_endline app_name;
  let img   = Image.create 100 100 in
  let red   = Color.make 255 0 0 in
  let green = Color.make 0 255 0 in
  let blue  = Color.make 0 0 255 in
  let yel   = Color.make 255 255 0 in
  let _ = [red; green; blue; yel] in
  let args =
    [ ("-mode0", Arg.Unit (fun () -> config.line <- line0), "line0")
    ; ("-mode1", Arg.Unit (fun () -> config.line <- line ), "line1")
    ; ("-mode2", Arg.Unit (fun () -> config.line <- line2), "line2")
    ; ("-mode3", Arg.Unit (fun () -> config.line <- line3), "line3")
    ; ("-mode4", Arg.Unit (fun () -> config.line <- line4), "line4")
    ]
  in
  Arg.parse args (fun s -> failwith @@ sprintf "No anonymous args '%s'\n%!" s) "usage";
(*
  Image.line ~img ~color:red 50 50 3  3;
  Image.line ~img ~color:red 50 50 96 3;
  Image.line ~img ~color:red 50 50 3  96;
  Image.line ~img ~color:red 50 50 96 96;
  *)
  (*
  Image.line ~img ~color:green 0 0 49 99;
   *)
(*
  Image.line ~img ~color:green 25  0 50 99;
  Image.line ~img ~color:red   50 0 75 99; *)
(*
  Image.line ~img ~color:blue   0 99 30 60;
  Image.line ~img ~color:yel   99  0 70 99;
  Image.line ~img ~color:red   99 77 13 13;
 *)
(*
  Image.line ~img ~color:red   49  0 49 99;
  Image.line ~img ~color:blue   0  0 99  0;
  Image.line ~img ~color:yel    0 10 99 40; *)
(*
  Image.line ~img ~color:green 50 50 3  25;
  Image.line ~img ~color:green 50 50 25 96;
  Image.line ~img ~color:green 50 50 96 25;
 *)

  for n =1 to 1000000 do
    config.line ~img ~color:blue   12 20 80 40;
    config.line ~img ~color:blue   20 13 40 80;
    config.line ~img ~color:blue   80 40 13 20;
  done;
  Tga.write_tga ~filename:"out.tga" ~img
