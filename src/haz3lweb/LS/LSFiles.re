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

let get_info_map = (~init_ctx, z: Zipper.t) =>
  z
  |> MakeTerm.from_zip_for_sem
  |> fst
  |> Interface.Statics.mk_map_ctx(CoreSettings.on, init_ctx);

let pp_inner = (~db=ignore, ~init_ctx, str: string): Ctx.t => {
  let sym = 666;
  let str = str ++ "\n" ++ string_of_int(sym);
  let ctx_if_sym = (_, info: Info.t, acc) =>
    switch (info) {
    | InfoExp({ctx, term: {term: Int(n), _}, _}) when n == sym => Some(ctx)
    | _ => acc
    };
  let get_ctx_thing = (map: Statics.Map.t): option(Ctx.t) =>
    Id.Map.fold(ctx_if_sym, map, None);
  db("LS: Prelude: Recieved string: " ++ str);
  let z =
    OptUtil.get_or_fail(
      "LS: Prelude: EXN: Couldn't parse string",
      Printer.zipper_of_string(str),
    );
  db("LS: Prelude: String parsed successfully");
  let info_map = get_info_map(~init_ctx, z);
  switch (get_ctx_thing(info_map)) {
  | Some(ctx) => ctx
  | None =>
    failwith(
      "LS: Prelude: EXN: Couldn't find sym to extract ctx:"
      ++ string_of_int(sym),
    )
  };
};

let process_prelude = (~db=ignore, ~init_ctx, str: string): Ctx.t => {
  let hashed_str = hash_of_string(str);
  let serialized_filename = hashed_str ++ ".ctx.serialized";
  if (is_file(serialized_filename)) {
    db("LS: Prelude: Found serialized file, deserializing");
    deserialize_ctx(serialized_filename);
  } else {
    db("LS: Prelude: No serialized file, processing string");
    let ctx = pp_inner(~db, ~init_ctx, str);
    serialize_ctx(ctx, serialized_filename);
    ctx;
  };
};

let process_zipper = (~db=ignore, z_str: string): Zipper.t => {
  let serialized_filename = serialized_filename_z(z_str);
  db("LS: Process zipper: Recieved string:");
  db(z_str);
  if (is_file(serialized_filename)) {
    db("LS: Process Zipper: Found serialized zipper, deserializing");
    deserialize_zipper(serialized_filename);
  } else {
    db("LS: Process Zipper: No serialized zipper, processing string");
    let z =
      OptUtil.get_or_fail(
        "LS: EXN: Couldn't parse string",
        Printer.zipper_of_string(z_str),
      );
    serialize_zipper(z, serialized_filename);
    z;
  };
};

let get_zipper = (~db, program, new_token) => {
  switch (new_token) {
  | None =>
    db("LS: Recieved string: " ++ program);
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
