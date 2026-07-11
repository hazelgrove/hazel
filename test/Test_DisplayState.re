open Alcotest;
open Haz3lcore;
open Language;

/* THE display-state harness: per keystroke, render what the user
   actually sees — visible text, caret ¦, inline ghost in ⟪⟫ (from
   the REAL Editor.calculate pipeline: TyDi buffer first, chip ghost
   fallback), plus the chip stream on a second line. For eyeballing
   trajectories and, later, asserting stability properties. */

let string_testable = testable(Fmt.string, String.equal);

let print_flat = (seg: Segment.t): string =>
  Printer.of_segment(~holes="?", ~concave_holes="~", seg);

let display_state = (z: Zipper.t): string => {
  let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Sort.Exp);
  let (info_map, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let obs = TypeObligations.derive(info_map);
  let eseg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let assist =
    TypeObligations.as_insertions(
      ~seg=eseg,
      ~existing=CanonicalCompletion.for_editor(eseg).insertions,
      obs,
    );
  /* replicate Editor.calculate: TyDi first, chip ghost fallback */
  let z =
    switch (TyDi.set_buffer(~ci=Indicated.ci_for_completion(z, info_map), z)) {
    | Some(z) => z
    | None => z
    };
  let z =
    if (!Selection.is_buffer(z.selection)) {
      switch (CanonicalCompletion.chip_among(z, assist)) {
      | Some(ins) =>
        switch (TypeObligations.ghost_pieces(z, ins)) {
        | Some(pieces) =>
          Zipper.set_buffer(z, ~content=pieces, ~mode=Unparsed)
        | None => z
        }
      | None => z
      };
    } else {
      z;
    };
  let ghost =
    switch (z.selection.mode) {
    | Buffer(_) when z.selection.content != [] =>
      "⟪" ++ print_flat(z.selection.content) ++ "⟫"
    | _ => ""
    };
  let (l, r) = z.relatives.siblings;
  /* ancestors: rebuild the outer context text crudely by zipping a
     buffer-erased copy and splitting at the caret via sibling text */
  let left_text = print_flat(List.rev(l) |> List.rev);
  let right_text = print_flat(r);
  let anc = z.relatives.ancestors == [] ? "" : "…"; /* target-0 states are all top-level; flag otherwise */
  let chips =
    assist
    |> List.map((i: CanonicalCompletion.insertion) =>
         i.delimiters
         |> List.map((d: CanonicalCompletion.delimiter_info) => d.text)
         |> String.concat("+")
       )
    |> String.concat(" | ");
  anc
  ++ left_text
  ++ "¦"
  ++ ghost
  ++ right_text
  ++ "   CHIPS["
  ++ chips
  ++ "]";
};

let trajectory = (text: string): string => {
  let acts = Test_Editing.mk(text ++ "¦");
  let rec steps = (k, z, acc) =>
    if (k > List.length(acts)) {
      List.rev(acc);
    } else {
      let z' =
        Test_Editing.perform(
          Zipper.init(),
          List.filteri((i, _) => i < k, acts),
        );
      ignore(z);
      steps(k + 1, z', [display_state(z'), ...acc]);
    };
  steps(1, Zipper.init(), []) |> String.concat("\n");
};

let tests = [
  (
    "DisplayState: target-0",
    [
      /* SNAPSHOT of current behavior incl. KNOWN JANK (each fix
         shows as a diff here): (1) `(¦⟪)⟫?` — ghost closer BEFORE
         the visible hole = displayed shape conflict; (2) after a
         typed comma `a,¦⟪, ?)⟫?` — comma-adjacent-comma misorder,
         material actually lands past the visible hole; target =
         region-replacement ghost `⟪ ?, ?)⟫` covering the hole (W1).
         Good states: a¦ / b¦ / c¦ read exactly right. */
      test_case("string_replace left-to-right snapshot", `Quick, () =>
        check(
          string_testable,
          "t0",
          {|s¦   CHIPS[]
st¦⟪ring_capitalize⟫   CHIPS[]
str¦⟪ing_capitalize⟫   CHIPS[]
stri¦⟪ng_capitalize⟫   CHIPS[]
strin¦⟪g_capitalize⟫   CHIPS[]
string¦⟪_capitalize⟫   CHIPS[]
string_¦⟪capitalize⟫   CHIPS[]
string_r¦⟪eplace⟫   CHIPS[]
string_re¦⟪place⟫   CHIPS[]
string_rep¦⟪lace⟫   CHIPS[]
string_repl¦⟪ace⟫   CHIPS[]
string_repla¦⟪ce⟫   CHIPS[]
string_replac¦⟪e⟫   CHIPS[]
string_replace¦   CHIPS[]
string_replace(¦⟪)⟫?   CHIPS[)]
string_replace(a¦⟪, ?, ?)⟫   CHIPS[,+,+)]
string_replace(a,¦⟪, ?)⟫?   CHIPS[,+)]
string_replace(a, ¦⟪, ?)⟫?   CHIPS[,+)]
string_replace(a, b¦⟪, ?)⟫   CHIPS[,+)]
string_replace(a, b,¦⟪)⟫?   CHIPS[)]
string_replace(a, b, ¦⟪)⟫?   CHIPS[)]
string_replace(a, b, c¦⟪)⟫   CHIPS[)]
string_replace(a, b, c)¦   CHIPS[]|},
          trajectory("string_replace(a, b, c)"),
        )
      ),
    ],
  ),
];
