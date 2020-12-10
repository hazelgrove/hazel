let read_all : in_channel -> string =
 fun channel ->
  let contents = really_input_string channel (in_channel_length channel) in
  close_in channel ; contents

let read_file : string -> string = fun path -> read_all (open_in path)

let path : string list -> string =
 fun parts ->
  let standard =
    parts
    |> List2.concat_map (String.split_on_char '/')
    |> List.filter (fun s -> not (String.equal s ""))
    |> String.concat "/"
  in
  let combined = parts |> String.concat "" |> String.trim in
  if String.length combined > 0 && combined.[0] = '/' then "/" ^ standard
  else standard

let read_path : string list -> string =
 fun parts -> parts |> path |> read_file

let visible_files : string -> string list =
 fun dir ->
  dir |> Sys.readdir |> Array.to_list
  |> List.filter (fun file ->
         (not (Sys.is_directory (path [dir; file])))
         && String.length file > 0
         && file.[0] <> '.')
  |> List.sort String.compare
