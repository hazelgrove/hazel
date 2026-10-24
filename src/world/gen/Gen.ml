(* Writes the world tree's slides (WorldSlides.ml) from its source
   folder: usage gen <folder> *)
open WorldFolder

let quote text =
  let rec delim n =
    let d = "hz" ^ String.make n 'x' in
    let closes = "|" ^ d ^ "}" in
    let rec has i =
      i + String.length closes <= String.length text
      && (String.sub text i (String.length closes) = closes || has (i + 1))
    in
    if has 0 then delim (n + 1) else d
  in
  let d = delim 0 in
  "{" ^ d ^ "|" ^ text ^ "|" ^ d ^ "}"

let () =
  let dir = Sys.argv.(1) in
  print_string
    "(* Generated from hazel-programs/world by src/world/gen: do not edit *)\n\n\
     type kind = Program | Prose\n\n\
     type slide = { path : string list; text : string; kind : kind; exp : bool \
     }\n\n\
     let slides : slide list = [\n";
  let rec walk above (es : entry list) =
    List.iter
      (fun e ->
        let path = above @ [ e.name ] in
        match e.kind with
        | Dir kids -> walk path kids
        | (Hz | Md) when above = [] -> () (* the library: baked, not a slide *)
        | Hz | Md ->
            Printf.printf "  { path = [%s]; text = %s; kind = %s; exp = %b };\n"
              (String.concat "; " (List.map (Printf.sprintf "%S") path))
              (quote (read_file e.path))
              (if e.kind = Md then "Prose" else "Program")
              e.exp)
      es
  in
  walk [] (read dir);
  print_string "]\n"
