/* Memoized `Parser.to_segment`, shared by the tests that parse the slide
 * corpus. MenhirCorpus and DocSlides.ReparseBackuptext ask different questions
 * of the same programs, but both need the typing parser's segment and it is the
 * dominant cost in each; without this they parse everything twice.
 *
 * Callers pass whatever text they have and normalization happens here, because
 * a raw .hz file and the slide's backup_text differ by indentation and a
 * trailing newline even when they are the same program. Normalizing the way the
 * load path does is what makes the two agree on a key.
 *
 * Keyed on (root, normalized text), so this is a memo on a pure function: a
 * caller cannot observe a segment parsed from anything but its own input. */

let normalize = (s: string): string =>
  s |> Util.StringUtil.trim_leading |> Util.StringUtil.strip_final_newline;

let cache:
  Hashtbl.t((Haz3lcore.Sort.t, string), option(Haz3lcore.Segment.t)) =
  Hashtbl.create(64);

let to_segment =
    (~root: Haz3lcore.Sort.t, text: string): option(Haz3lcore.Segment.t) => {
  let text = normalize(text);
  let key = (root, text);
  switch (Hashtbl.find_opt(cache, key)) {
  | Some(seg) => seg
  | None =>
    let seg = Haz3lcore.Parser.to_segment(text, ~root);
    Hashtbl.replace(cache, key, seg);
    seg;
  };
};

/* The recovering parser is near-quadratic in a long `let ... in` chain,
   which is what a Livelit Playground slide is: 452 s for full-body.hz
   (122 KB) in MenhirCorpus alone, on a laptop. Programs longer than this
   are parsed by it only in the weekly Extended run (QCHECK_LONG=1, a
   350-minute budget); per PR, their cases are skipped, and say so. They
   still load (FastParse), type-check and evaluate in the quick tests:
   Unproject, IncrSlides. A shipped slide's text is its file with the
   indentation trimmed, about 15% shorter: Hair is 34 KB as a file and
   29 KB as a slide, so the bound is below both. */
let long_text = 25_000;

let in_long_run = Sys.getenv_opt("QCHECK_LONG") != None;

let skip_if_long = (text: string): unit =>
  if (String.length(text) > long_text && !in_long_run) {
    Alcotest.skip();
  };
