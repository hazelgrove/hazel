(* The world tree's source folder (hazel-programs/world): a folder is a
   module, a .hz file a slide (or, at the top level, one of the standard
   library's modules), a .md file a slide of prose. A folder's [order]
   file lists its entries, one a line:

     <file or folder name, no extension>  [<display name>]  [exp]

   separated by two or more spaces or a tab; `#` starts a comment line.
   [exp]: the file is an expression program, not module members. Entries
   it doesn't list follow, by name, named by their file. *)

type kind = Hz | Md | Dir of entry list

and entry = {
  stem : string;
  name : string;
  exp : bool;
  path : string;  (** on disk *)
  kind : kind;
}

let read_file path = In_channel.read_all path

let fields line =
  let line =
    String.map ~f:(fun c -> if Char.equal c '\t' then '\x00' else c) line
  in
  let rec split acc cur i =
    if i >= String.length line then List.rev (String.strip cur :: acc)
    else if Char.equal line.[i] '\x00' then
      split (String.strip cur :: acc) "" (i + 1)
    else if
      i + 1 < String.length line
      && Char.equal line.[i] ' '
      && Char.equal line.[i + 1] ' '
    then (
      let j = ref i in
      while !j < String.length line && Char.equal line.[!j] ' ' do
        incr j
      done;
      split (String.strip cur :: acc) "" !j)
    else split acc (cur ^ String.make 1 line.[i]) (i + 1)
  in
  List.filter ~f:(fun f -> not (String.is_empty f)) (split [] "" 0)

let order_of dir =
  let file = Filename.concat dir "order" in
  if not (Stdlib.Sys.file_exists file) then []
  else
    read_file file |> String.split ~on:'\n' |> List.map ~f:String.strip
    |> List.filter ~f:(fun l ->
           (not (String.is_empty l)) && not (Char.equal l.[0] '#'))
    |> List.map ~f:(fun l ->
        match fields l with
        | [ stem ] -> (stem, stem, false)
        | [ stem; name ] -> (stem, name, String.equal name "exp")
        | stem :: name :: flags ->
            (stem, name, List.mem flags "exp" ~equal:String.equal)
        | [] -> failwith "unreachable")

let ignored = [ "order"; "README.md" ]

let rec read dir : entry list =
  let on_disk =
    Stdlib.Sys.readdir dir |> Array.to_list
    |> List.filter ~f:(fun f -> not (List.mem ignored f ~equal:String.equal))
    |> List.filter_map ~f:(fun f ->
        let path = Filename.concat dir f in
        if Stdlib.Sys.is_directory path then Some (f, path, `Dir)
        else if Filename.check_suffix f ".hz" then
          Some (Filename.chop_suffix f ".hz", path, `Hz)
        else if Filename.check_suffix f ".md" then
          Some (Filename.chop_suffix f ".md", path, `Md)
        else None)
  in
  let entry (stem, name, exp) =
    match List.find on_disk ~f:(fun (s, _, _) -> String.equal s stem) with
    | None -> failwith (Printf.sprintf "%s/order: no %s" dir stem)
    | Some (_, path, k) ->
        let kind =
          match k with `Dir -> Dir (read path) | `Hz -> Hz | `Md -> Md
        in
        { stem; name; exp; path; kind }
  in
  let listed = order_of dir in
  let rest =
    on_disk
    |> List.filter ~f:(fun (s, _, _) ->
        not (List.exists listed ~f:(fun (s', _, _) -> String.equal s s')))
    |> List.map ~f:(fun (s, _, _) -> (s, s, false))
    |> List.sort ~compare:Poly.compare
  in
  List.map ~f:entry (listed @ rest)

(* the top level's .hz files: the standard library, in order. Reads only
   the top level (a build may have copied nothing below it) *)
let library dir =
  let file stem = Filename.concat dir (stem ^ ".hz") in
  let listed =
    order_of dir
    |> List.map ~f:(fun (stem, _, _) -> stem)
    |> List.filter ~f:(fun stem -> Stdlib.Sys.file_exists (file stem))
  in
  let rest =
    Stdlib.Sys.readdir dir |> Array.to_list
    |> List.filter ~f:(fun f -> Filename.check_suffix f ".hz")
    |> List.map ~f:(fun f -> Filename.chop_suffix f ".hz")
    |> List.filter ~f:(fun stem -> not (List.mem listed stem ~equal:String.equal))
    |> List.sort ~compare:String.compare
  in
  List.map ~f:file (listed @ rest)
