/* Parser.to_zipper_segmented, which MarkerParse.of_text now uses, against
   Parser.to_zipper, the char-by-char parser it replaces: the same zipper,
   printed losslessly with every hole as a marker, projectors included. The
   segmented parser is linear where to_zipper is quadratic (4 KB of prose
   took 15 s). Checked on every program in hazel-programs when it was
   written (128 programs and sections, all the same); these are the ones
   with projectors, which an earlier version dropped, and a plain one. On
   text that is not code the two can differ in where holes sit: see
   fresh_prose. */
open Alcotest;

let read = file => {
  let path =
    List.find_opt(Sys.file_exists, [file, "../../../" ++ file])
    |> Option.value(~default=file);
  let ic = open_in_bin(path);
  let text = really_input_string(ic, in_channel_length(ic));
  close_in(ic);
  Util.StringUtil.strip_final_newline(text);
};

let hole = Haz3lcore.MarkerParse.default_implicit_hole;
let printed = (z: option(Haz3lcore.Zipper.t)) =>
  Option.map(
    z =>
      Haz3lcore.MarkerParse.to_text(
        Haz3lcore.MarkerParse.strip_implicit_holes(~implicit_hole=hole, z),
      ),
    z,
  );

let same_as_to_zipper = text => {
  let by_char = printed(Haz3lcore.Parser.to_zipper(~root=Exp, text));
  let segmented =
    printed(Haz3lcore.Parser.to_zipper_segmented(~root=Exp, text));
  check(bool, "parsed", true, Option.is_some(segmented));
  check(option(string), "the same zipper as to_zipper", by_char, segmented);
};

let file_case = file =>
  test_case(Filename.basename(file), `Slow, () =>
    same_as_to_zipper(read(file))
  );

let prose_text = "Implement the `clean` function. It takes a gradebook (a list of labeled
tuples where every value is a String) and should return a new table with
two changes: convert columns to proper types, and add an overall grade.
Weighting: 1/3 quizzes, 1/3 midterm, 1/3 final. There are 2 quizzes, each
scored out of 10 points; the Midterm and Final are each out of 100. First
convert the quizzes to a percentage, then compute the grade out of 100.
Feel free to define any helper functions you may find useful. The task
reference to the right provides functions we think may be helpful.";

/* Text that is not code, where holes are inferred between juxtaposed
   forms: the words come back in order, but where one segment ends and the
   next begins, the final regrout can put a hole on the other side of a
   space, or add one (81 holes against to_zipper's 79 here). Code does not
   hit this: its segments end after complete forms that need no hole
   between them. to_zipper is not stable on such text either -- reparsing
   its own output of this prose grows it from 732 to 1044 characters. */
let fresh_prose = () => {
  let strip = s =>
    Str.global_replace(Str.regexp_string(hole), "", s)
    |> Str.global_replace(Str.regexp("[ \n]+"), " ");
  let by_char = printed(Haz3lcore.Parser.to_zipper(~root=Exp, prose_text));
  let segmented =
    printed(Haz3lcore.Parser.to_zipper_segmented(~root=Exp, prose_text));
  check(
    option(string),
    "the same text but for where holes sit",
    Option.map(strip, by_char),
    Option.map(strip, segmented),
  );
};

let tests = (
  "Parser.Segmented",
  [
    test_case("fresh prose", `Quick, fresh_prose),
    file_case("hazel-programs/docs/reference/probes.hz"),
    file_case("hazel-programs/docs/reference/cards.hz"),
    file_case("hazel-programs/docs/reference/tables.hz"),
    file_case("hazel-programs/docs/reference/tuples.hz"),
    file_case("hazel-programs/docs/b2t2/example-programs-dot-product.hz"),
  ],
);
