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

let read_file path = In_channel.with_open_bin path In_channel.input_all

let fields line =
  let line = String.map (fun c -> if c = '\t' then '\x00' else c) line in
  let rec split acc cur i =
    if i >= String.length line then List.rev (String.trim cur :: acc)
    else if line.[i] = '\x00' then split (String.trim cur :: acc) "" (i + 1)
    else if i + 1 < String.length line && line.[i] = ' ' && line.[i + 1] = ' '
    then (
      let j = ref i in
      while !j < String.length line && line.[!j] = ' ' do
        incr j
      done;
      split (String.trim cur :: acc) "" !j)
    else split acc (cur ^ String.make 1 line.[i]) (i + 1)
  in
  List.filter (fun f -> f <> "") (split [] "" 0)

let order_of dir =
  let file = Filename.concat dir "order" in
  if not (Sys.file_exists file) then []
  else
    read_file file |> String.split_on_char '\n' |> List.map String.trim
    |> List.filter (fun l -> l <> "" && l.[0] <> '#')
    |> List.map (fun l ->
        match fields l with
        | [ stem ] -> (stem, stem, false)
        | [ stem; name ] -> (stem, name, name = "exp")
        | stem :: name :: flags -> (stem, name, List.mem "exp" flags)
        | [] -> failwith "unreachable")

let ignored = [ "order"; "README.md" ]

let rec read dir : entry list =
  let on_disk =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> not (List.mem f ignored))
    |> List.filter_map (fun f ->
        let path = Filename.concat dir f in
        if Sys.is_directory path then Some (f, path, `Dir)
        else if Filename.check_suffix f ".hz" then
          Some (Filename.chop_suffix f ".hz", path, `Hz)
        else if Filename.check_suffix f ".md" then
          Some (Filename.chop_suffix f ".md", path, `Md)
        else None)
  in
  let entry (stem, name, exp) =
    match List.find_opt (fun (s, _, _) -> s = stem) on_disk with
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
    |> List.filter (fun (s, _, _) ->
        not (List.exists (fun (s', _, _) -> s = s') listed))
    |> List.map (fun (s, _, _) -> (s, s, false))
    |> List.sort compare
  in
  List.map entry (listed @ rest)

(* the top level's .hz files: the standard library, in order. Reads only
   the top level (a build may have copied nothing below it) *)
let library dir =
  let file stem = Filename.concat dir (stem ^ ".hz") in
  let listed =
    order_of dir
    |> List.map (fun (stem, _, _) -> stem)
    |> List.filter (fun stem -> Sys.file_exists (file stem))
  in
  let rest =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".hz")
    |> List.map (fun f -> Filename.chop_suffix f ".hz")
    |> List.filter (fun stem -> not (List.mem stem listed))
    |> List.sort compare
  in
  List.map file (listed @ rest)
