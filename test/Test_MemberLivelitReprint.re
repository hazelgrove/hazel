/* ^ticket(472): a livelit's commit re-prints its use from the term
   (ProjectorPerform.term_to_segment), and the slide is saved as text. A
   module's livelit came back spaced, `Drawing. ^slider(...)`, which does
   not read back as one: on #2635's Scenes slide every use whose slider
   had been moved said "Malformed Label: ^slider" after a reload. */
open Alcotest;
open Haz3lcore;
open Language;

let read = (file: string): string => {
  let path = "../../../hazel-programs/docs/livelits/playground/" ++ file;
  let ic = open_in_bin(path);
  let src = really_input_string(ic, in_channel_length(ic));
  close_in(ic);
  src;
};

/* a use, as a livelit's commit re-prints it */
let reprint = (src: string): string =>
  switch (Parser.to_term(src, ~root=Exp)) {
  | None => fail("could not parse " ++ src)
  | Some(e) =>
    Printer.of_segment(
      ~holes="?",
      ProjectorPerform.term_to_segment(
        ~original_syntax=[],
        ~preserve_splices=false,
        Exp(e),
      ),
    )
  };

let errors = (text: string): list(string) =>
  switch (
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root=Exp,
      text,
    )
  ) {
  | None => fail("the slide does not parse")
  | Some(seg) =>
    let (map, _) =
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        MakeTerm.go(seg).term,
      );
    ErrorPrint.all(map);
  };

let find = (sub: string, s: string): option(int) => {
  let n = String.length(sub);
  let rec go = i =>
    i + n > String.length(s)
      ? None : String.sub(s, i, n) == sub ? Some(i) : go(i + 1);
  go(0);
};

let swap = (a, b, s) =>
  switch (find(a, s)) {
  | Some(i) =>
    String.sub(s, 0, i)
    ++ b
    ++ String.sub(
         s,
         i + String.length(a),
         String.length(s) - i - String.length(a),
       )
  | None => fail("no " ++ a ++ " in the slide")
  };

let uses = [
  "Drawing.^slider(\"figure size\", 10, 200)(100)",
  "Drawing.^slider(\"across\", -300, 300)(0)",
  "Drawing.^choice@<Bool>([(\"no legs\", false), (\"chicken legs\", true)])(false)",
];

let tests = (
  "MemberLivelitReprint",
  [
    test_case("a module's livelit re-prints unspaced", `Quick, () =>
      List.iter(
        use => {
          let out = reprint(use);
          let head = List.hd(String.split_on_char('(', use));
          let head = List.hd(String.split_on_char('@', head));
          check(
            bool,
            "`" ++ head ++ "` in `" ++ out ++ "`",
            true,
            Option.is_some(find(head, out)),
          );
        },
        uses,
      )
    ),
    test_case(
      "the Scenes slide with its uses re-printed still has no errors",
      `Quick,
      () => {
        let shipped = read("scenes.hz");
        check(list(string), "the shipped slide", [], errors(shipped));
        let reprinted =
          List.fold_left((s, u) => swap(u, reprint(u), s), shipped, uses);
        check(
          list(string),
          "with three uses re-printed",
          [],
          errors(reprinted),
        );
      },
    ),
  ],
);
