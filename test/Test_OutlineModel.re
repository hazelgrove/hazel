open Alcotest;
open Haz3lcore;
module OT = Web.OutlineTree;

let term_of = (~root, seg) =>
  MakeTerm.from_zip_for_sem(Zipper.unzip(seg), ~root).term;

let kind_s = (k: OT.kind) =>
  switch (k) {
  | KModule => "module"
  | KFn => "fn"
  | KConst => "const"
  | KType => "type"
  | KTest => "test"
  | KTests => "tests"
  | KStmt => "stmt"
  | KUse => "use"
  | KTrail => "trail"
  };

let id_s = (o: option(Id.t)) =>
  switch (o) {
  | Some(id) => String.sub(Id.to_string(id), 0, 6)
  | None => "-"
  };

let rec show = (~indent="", ns: list(OT.node)): list(string) =>
  List.concat_map(
    (n: OT.node) =>
      [
        Printf.sprintf(
          "%s%s %s id=%s test=%s binder=%s",
          indent,
          kind_s(n.o_kind),
          n.o_label,
          id_s(n.o_id),
          id_s(n.o_test),
          id_s(Option.map(fst, n.o_binder)),
        ),
      ]
      @ show(~indent=indent ++ "  ", n.o_children),
    ns,
  );

/* the term walk's rows and the outline model's, line by line */
let parity = (~root, name, seg): option(string) => {
  let old = show(OT.of_term(term_of(~root, seg)));
  let fresh = show(OT.of_outline(Outline.build(~root, seg)));
  old == fresh
    ? None
    : Some(
        name
        ++ "\n--- term walk\n"
        ++ String.concat("\n", old)
        ++ "\n--- outline\n"
        ++ String.concat("\n", fresh),
      );
};

let tests = (
  "OutlineModel",
  [
    test_case(
      "rows match the term walk",
      `Quick,
      () => {
        let progs =
          List.map(
            ((n, src)) => (n, Sort.Exp, CorpusUtil.seg_of(src)),
            CorpusUtil.edge_programs,
          )
          /* as typed: unfinished forms and grout */
          @ List.filter_map(
              ((n, src)) =>
                Option.map(
                  seg => ("typed " ++ n, Sort.Exp, seg),
                  CorpusUtil.typed_seg(src),
                ),
              CorpusUtil.edge_programs @ CorpusUtil.incomplete_programs,
            )
          @ List.filter_map(
              ((path, src)) =>
                Option.map(
                  seg => (path, Sort.Exp, seg),
                  CorpusUtil.parse(src),
                ),
              CorpusUtil.slide_corpus(),
            )
          @ [
            ("mega-1k", Sort.Exp, CorpusUtil.mega_seg("mega-1k.hz")),
            (
              "mega-mod-1k",
              Sort.Mod,
              CorpusUtil.mega_seg(~root=Mod, "mega-mod-1k.hz"),
            ),
          ];
        let diffs =
          List.filter_map(
            ((name, root, seg)) => parity(~root, name, seg),
            progs,
          );
        List.iter(print_endline, diffs);
        check(
          int,
          Printf.sprintf("programs differing, of %d", List.length(progs)),
          0,
          List.length(diffs),
        );
      },
    ),
  ],
);
