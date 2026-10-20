/* ^ticket(472), a probe, not for merging: the Scenes slide's static errors
   by each way a slide's text becomes a zipper. Always fails; the failure
   is the report. */
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

let errors = (z: Zipper.t): list(string) =>
  ErrorPrint.all(
    fst(
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        MakeTerm.from_zip_for_sem(z, ~root=Exp).term,
      ),
    ),
  );

let line = (name, z: option(Zipper.t)) =>
  switch (z) {
  | None => name ++ ": no zipper"
  | Some(z) =>
    let es = errors(z);
    Printf.sprintf(
      "%s: %d errors%s",
      name,
      List.length(es),
      String.concat(
        "",
        List.map(
          e => "\n      " ++ String.sub(e, 0, min(160, String.length(e))),
          List.filteri((i, _) => i < 6, es),
        ),
      ),
    );
  };

let tests = (
  "Diag472",
  [
    test_case(
      "the Scenes slide by each load path",
      `Quick,
      () => {
        let text = read("scenes.hz");
        let small = "module D = { let ^c = typfun A -> fun cases : [(String, A)] -> { type Model = A; type Action = A; type Expansion = A; let init = Pure(1); let update = fun _m : Model -> fun a : Action -> Pure(a); let view = fun m : Model -> Pure(Html.text(\"x\")); let expand = Functional(fun m : Model -> m); let shape = Inline(5) } } in D.^c@<Int>([(\"one\", 1)])";
        let report =
          String.concat(
            "\n",
            [
              line(
                "scenes, load path (FastParse first)",
                PersistentZipper.parse_text(~source="probe", ~root=Exp, text),
              ),
              line(
                "small, load path",
                PersistentZipper.parse_text(
                  ~source="probe",
                  ~root=Exp,
                  small,
                ),
              ),
              line(
                "small, slow path",
                MarkerParse.of_text(~root=Exp, small),
              ),
              line(
                "small, Parser.to_zipper",
                Parser.to_zipper(~root=Exp, small),
              ),
            ],
          );
        check(string, "the report (a probe: it always fails)", "", report);
      },
    ),
  ],
);
