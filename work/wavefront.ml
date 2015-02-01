open Printf

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
