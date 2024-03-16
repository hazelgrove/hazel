open Js_of_ocaml;
open Haz3lcore;
open Util;

/* File handling */

let string_of_file = (~encoding: string="utf8", path: string): string => {
  Js.Unsafe.(
    Array.map(inject, [|Js.string(path), Js.string(encoding)|])
    |> fun_call(get(js_expr("require('fs')"), "readFileSync"))
    |> Js.to_string
  );
};

let hash_of_string = (str: string): string =>
  str |> Digest.string |> Digest.to_hex;

let tempDir = "tmp"; // Name of the temporary directory

/* Ensure the temporary directory exists */
let ensureTempDir = (): unit =>
  if (!Sys.file_exists(tempDir)) {
    Sys.mkdir(
      tempDir,
      0o755 // Uses Unix permissions, adjust as necessary
    );
  };

/* Construct a path within the temporary directory for a given filename */
let tempFilePath = (filename: string): string => {
  ensureTempDir();
  tempDir ++ "/" ++ filename;
};

let is_file = (path: string): bool => {
  Js.Unsafe.(
    [|inject(Js.string(tempFilePath(path)))|]
    |> fun_call(get(js_expr("require('fs')"), "existsSync"))
    |> Js.to_bool
  );
};

let serialize_a = (a: 'a, filename: string): unit => {
  let tempFilename = tempFilePath(filename);
  let channel = open_out_bin(tempFilename);
  Marshal.to_channel(channel, a, []);
  close_out(channel);
};

let deserialize_a = (filename: string): 'a => {
  let tempFilename = tempFilePath(filename);
  let channel = open_in_bin(tempFilename);
  let a: 'a = Marshal.from_channel(channel);
  close_in(channel);
  a;
};

let serialize_ctx: (Ctx.t, string) => unit = serialize_a;
let serialize_zipper: (Zipper.t, string) => unit = serialize_a;

let deserialize_ctx: string => Ctx.t = deserialize_a;
let deserialize_zipper: string => Zipper.t = deserialize_a;

let serialized_filename_z = program_str =>
  hash_of_string(program_str) ++ ".zipper.serialized";

let process_zipper = (~db=ignore, z_str: string): Zipper.t => {
  let serialized_filename = serialized_filename_z(z_str);
  db("LSP: Process zipper: Recieved string:");
  db(z_str);
  if (is_file(serialized_filename)) {
    db("LSP: Process Zipper: Found serialized zipper, deserializing");
    deserialize_zipper(serialized_filename);
  } else {
    db("LSP: Process Zipper: No serialized zipper, processing string");
    let z =
      OptUtil.get_or_fail(
        "LSP: EXN: Couldn't parse string",
        Printer.zipper_of_string(z_str),
      );
    serialize_zipper(z, serialized_filename);
    z;
  };
};

let get_zipper = (~db, program, new_token) => {
  switch (new_token) {
  | None =>
    db("LS: Recieved string" /*++ program*/);
    let z = process_zipper(~db, program);
    db("LS: String parsed successfully to zipper");
    z;
  | Some(new_token) =>
    db("LS: New token mode: " ++ program ++ new_token);
    let base_z = process_zipper(~db, program);
    let new_z =
      OptUtil.get_or_fail(
        "LS: EXN: New token mode: Couldn't paste into zipper",
        Printer.paste_into_zip(base_z, new_token),
      );
    serialize_zipper(new_z, serialized_filename_z(program ++ new_token));
    new_z;
  };
};

/* Save text to a file given a path and content */
let save_text_to_file = (~path: string, ~content: string): unit => {
  Js.Unsafe.(
    [|inject(Js.string(path)), inject(Js.string(content))|]
    |> fun_call(get(js_expr("require('fs')"), "writeFileSync"))
  );
};

/* Load text from a file given a path */
let load_text_from_file = (~path: string): string => {
  string_of_file(~encoding="utf8", path);
};

let mk_dir = (dirPath: string): unit =>
  if (!Sys.file_exists(dirPath)) {
    Sys.mkdir(
      dirPath,
      0o755 // Uses Unix permissions, adjust as necessary
    );
  };

let getCurrentGitCommit = (): string => {
  let tempFile = "git_commit.tmp";
  let command = "git rev-parse HEAD > " ++ tempFile;
  ignore(Sys.command(command)); /* Executes the command and ignores its exit status */
  let channel = open_in(tempFile); /* Open the temporary file for reading */
  let commitHash = input_line(channel); /* Read the first line, which contains the commit hash */
  close_in(channel);
  Sys.remove(tempFile); /* Clean up the temporary file */
  commitHash;
};

let getCurrentUnixTimestamp = (): string => {
  let date = JsUtil.date_now();
  date##valueOf /. 1000. |> int_of_float |> string_of_int;
};

let getCurrentISOTimestamp = (): string =>
  JsUtil.date_now()##toISOString |> Js.to_string;

// let append_key_value_to_file =
//     (~key: string, ~value: string, ~path: string): unit => {
//   let line = "\"" ++ key ++ "\":\"" ++ value ++ "\"\n";
//   let flag = Js.Unsafe.(get(js_expr("require('fs')"), "a")); // Open file in append mode
//   Js.Unsafe.(
//     [|inject(Js.string(path)), inject(Js.string(line)), inject(flag)|]
//     |> fun_call(get(js_expr("require('fs')"), "writeFileSync"))
//   );
// };

let append_key_value_to_file = (~path: string, key, value): unit => {
  let line = "\"" ++ key ++ "\":\"" ++ value ++ "\"\n";
  let channel =
    open_out_gen([Open_wronly, Open_append, Open_creat], 0o666, path);
  output_string(channel, line);
  close_out(channel);
};

let read_key_value_pairs_from_file = (path: string): list((string, string)) =>
  if (!Sys.file_exists(path)) {
    []; // Return an empty list if the file doesn't exist
  } else {
    let content = load_text_from_file(~path);
    let lines = Str.split(Str.regexp("\n"), content);
    List.fold_left(
      (acc, line) =>
        if (String.trim(line) !== "") {
          switch (Str.split(Str.regexp(":"), line)) {
          | [key, value] =>
            let key = Str.global_replace(Str.regexp("\""), "", key);
            let value = Str.global_replace(Str.regexp("\""), "", value);
            [(key, value), ...acc];
          | _ => acc
          };
        } else {
          acc;
        },
      [],
      lines,
    );
  };

// let update_key_value_in_file =
//     (~path: string, ~key: string, update_func: string => string): unit => {
//   let pairs = read_key_value_pairs_from_file(path);
//   let filtered_pairs = List.filter(((k, _)) => k == key, pairs);

//   switch (List.length(filtered_pairs)) {
//   | 0 => failwith("LSFiles: update_key_value_in_file: Key not found")
//   | 1 =>
//     let updated_pairs =
//       List.map(
//         ((k, v)) => k == key ? (k, update_func(v)) : (k, v),
//         pairs,
//       );
//     let lines =
//       List.map(
//         ((k, v)) => "\"" ++ k ++ "\":\"" ++ v ++ "\"\n",
//         updated_pairs,
//       );
//     let channel = open_out(path); // Open file for writing, which truncates the file
//     List.iter(line => output_string(channel, line), lines);
//     close_out(channel);
//   | _ =>
//     failwith("LSFiles: update_key_value_in_file: Key exists more than once")
//   };
// };

let update_key_value_in_file =
    (~path: string, ~key: string, update_func: option(string) => string)
    : unit => {
  let pairs = read_key_value_pairs_from_file(path);
  let (filtered_pairs, existing_value_opt) =
    List.fold_left(
      (acc, (k, v)) => {
        let (filtered, value_opt) = acc;
        if (k == key) {
          (
            filtered,
            Some(v) // Do not add this pair to filtered, capture the value
          );
        } else {
          (
            [(k, v)] @ filtered,
            value_opt // Add other pairs to filtered
          );
        };
      },
      ([], None),
      pairs,
    );

  let updated_pairs =
    switch (existing_value_opt) {
    | Some(existing_value) =>
      filtered_pairs @ [(key, update_func(Some(existing_value)))] // Append updated pair
    | None =>
      let updated_value = update_func(None);
      if (updated_value != "") {
        filtered_pairs @ [(key, updated_value)]; // Append new key-value pair
      } else {
        filtered_pairs; // If the update function returns an empty string, don't add a new pair
      };
    };

  let lines =
    List.map(
      ((k, v)) => "\"" ++ k ++ "\":\"" ++ v ++ "\"\n",
      updated_pairs,
    );
  let channel = open_out(path); // Open the file for writing, truncating it
  List.iter(line => output_string(channel, line), lines);
  close_out(channel);
};

let get_value_by_key_from_file =
    (~path: string, ~key: string): option(string) => {
  let pairs = read_key_value_pairs_from_file(path);
  let filtered_pairs = List.filter(((k, _v)) => k == key, pairs);
  switch (filtered_pairs) {
  | [] => None
  | [(_, value)] => Some(value)
  | _ => failwith("Key exists more than once or not found")
  };
};
