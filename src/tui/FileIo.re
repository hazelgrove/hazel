open Haz3lcore;

/* File load/save. Plain OCaml channels work under js_of_ocaml + node
   (the runtime maps them to fs), which is how src/CLI does its I/O. */

let read_file = (path: string): string => {
  let ic = open_in_bin(path);
  let n = in_channel_length(ic);
  let s = really_input_string(ic, n);
  close_in(ic);
  s;
};

let write_file = (path: string, contents: string): unit => {
  let oc = open_out_bin(path);
  output_string(oc, contents);
  close_out(oc);
};

/* Parse a .haz file into a zipper. Implicit holes round-trip via the
   `¿` marker (see TextRoundtrip). */
let load = (path: string): option(Zipper.t) =>
  if (Sys.file_exists(path)) {
    let text = read_file(path);
    String.trim(text) == ""
      ? Some(Zipper.init()) : TextRoundtrip.of_text(~root=Exp, text);
  } else {
    Some
      (Zipper.init()); /* new file */
  };

/* Serialize a zipper the way TextRoundtrip.to_text does, so saved files
   re-load with identical structure (covered by Test_TextRoundtrip). */
let zipper_to_text = (z: Zipper.t): string => {
  let segment = Zipper.unselect_and_zip(~erase_buffer=true, z);
  Printer.of_segment(
    ~holes=TextRoundtrip.default_implicit_hole,
    ~concave_holes=TextRoundtrip.default_implicit_hole,
    ~indent="",
    ~refractors=z.refractors.manuals,
    segment,
  );
};

let save = (path: string, z: Zipper.t): unit =>
  write_file(path, zipper_to_text(z) ++ "\n");
