// The paper's worked examples parse without holes and synthesise their stated types.
open Web;
open Alcotest;
open Language;

let parse = (src: string): Exp.t =>
  switch (Haz3lcore.Parser.to_term(src, ~root=Exp)) {
  | Some(e) => e
  | None => Alcotest.failf("example did not parse")
  };

let bad_nodes = (e: Exp.t): list(string) => {
  let acc = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          switch (Exp.term_of(e)) {
          | Invalid(s) => acc := acc^ @ ["Invalid(" ++ s ++ ")"]
          | MultiHole(_) => acc := acc^ @ ["MultiHole"]
          | EmptyHole => acc := acc^ @ ["EmptyHole"]
          | _ => ()
          };
          continue(e);
        },
      ~f_typ=
        (continue, t) => {
          switch (Typ.term_of(t)) {
          | Unknown(Hole(_)) => acc := acc^ @ ["typ hole"]
          | _ => ()
          };
          continue(t);
        },
      e,
    );
  acc^;
};

let root_type = (e: Exp.t): Typ.t => {
  let (m, _) = Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), e);
  switch (Statics.Map.lookup_exp(Exp.rep_id(e), m)) {
  | Some({ty, _}) => ty
  | None => Alcotest.failf("example root missing from the info map")
  };
};

let example_case = ((name, src): (string, string)) =>
  test_case(
    name,
    `Quick,
    () => {
      let e = parse(src);
      check(list(string), name ++ ": no holes or invalid nodes", [], bad_nodes(e));
      check(bool, name ++ ": root type is not a gap", false, Typ.is_gap(root_type(e)));
    },
  );

let slide_case = ((name, slide): (string, CellEditor.Model.persistent)) =>
  test_case(
    name ++ " slide",
    `Quick,
    () => {
      let text = slide.editor.zipper.backup_text;
      check(bool, name ++ ": slide is not empty", true, String.trim(text) != "");
      let e = parse(text);
      check(
        list(string),
        name ++ ": slide has no holes or invalid nodes",
        [],
        bad_nodes(e),
      );
    },
  );

let tests = (
  "SlicingExamples",
  List.map(example_case, SlicingExamples.all)
  @ List.map(
    slide_case,
    List.filteri(
      (i, _) => i < List.length(SlicingExamples.all),
      snd(Init.startup.scratch),
    ),
  ),
);
