open Alcotest;
open Haz3lcore;

/* Editor.Update rebuilds CachedSyntax only when its inputs may have
   changed: a caret move with no completion buffer reuses it outright. */

let settings = Language.CoreSettings.on;

let statics_of = (z: Zipper.t): CachedStatics.t =>
  CachedStatics.init(
    ~settings,
    ~is_dynamic_term=false,
    ~stitch=Fun.id,
    ~root=Exp,
    z,
  );

/* An editor as the app drives it: update then calculate per action,
   statics recomputed only on edits. */
type state = {
  ed: Editor.t,
  statics: CachedStatics.t,
};

let calculate = (~is_edited, statics, ed: Editor.t): Editor.t =>
  Editor.Update.calculate(
    ~settings,
    ~autoprobe_mode=Off,
    ~is_edited,
    statics,
    Language.Dynamics.Map.empty,
    ed,
  );

let init = (~is_edited=false, code: string): state => {
  let z = Test_Editing.perform(Zipper.init(), Test_Editing.mk(code));
  let statics = statics_of(z);
  {
    ed: calculate(~is_edited, statics, Editor.Model.mk(z, ~root=Exp)),
    statics,
  };
};

let act = (a: Action.t, {ed, statics}: state): state => {
  let ed =
    switch (
      Editor.Update.update(
        ~settings,
        a,
        statics,
        Language.Dynamics.Map.empty,
        ed,
      )
    ) {
    | Ok(ed) => ed
    | Error(e) => fail("action failed: " ++ Action.Failure.show(e))
    };
  let is_edited = Action.is_edit(a);
  let statics = is_edited ? statics_of(ed.state.zipper) : statics;
  {
    ed: calculate(~is_edited, statics, ed),
    statics,
  };
};

let print = (seg: Segment.t): string => Printer.of_segment(~holes="?", seg);

let caret_moves = () => {
  let s0 = init("let x = 1 in\nx + 2¦");
  let s1 = act(Move(Local(Left, ByChar)), s0);
  let s2 = act(Move(Vertical(Up, ByChar)), s1);
  check(
    bool,
    "caret moved",
    false,
    Test_Editing.printer(s2.ed.state.zipper)
    == Test_Editing.printer(s0.ed.state.zipper),
  );
  check(
    bool,
    "horizontal move keeps syntax",
    true,
    s1.ed.syntax === s0.ed.syntax,
  );
  check(
    bool,
    "vertical move keeps syntax",
    true,
    s2.ed.syntax === s0.ed.syntax,
  );
};

let selection = () => {
  let s0 = init("let x = 1 in\nx + 2¦");
  let s1 = act(Select(Resize(Local(Left, ByChar))), s0);
  check(
    bool,
    "measured kept",
    true,
    s1.ed.syntax.measured === s0.ed.syntax.measured,
  );
  check(
    bool,
    "selection ids",
    true,
    s1.ed.syntax.selection_ids != []
    && s1.ed.syntax.selection_ids
    == Selection.selection_ids(s1.ed.state.zipper.selection),
  );
};

let buffer_clear = () => {
  /* the post-edit calculate sets the TyDi buffer `ngth` */
  let s0 =
    init(
      ~is_edited=true,
      "let m : (x=Int, length=Int) = (x=1, length=5) in m.le¦",
    );
  check(
    bool,
    "buffer set",
    true,
    Selection.is_buffer(s0.ed.state.zipper.selection),
  );
  let s1 = act(Move(Local(Left, ByChar)), s0);
  check(
    bool,
    "buffer cleared",
    false,
    Selection.is_buffer(s1.ed.state.zipper.selection),
  );
  check(
    string,
    "segment rebuilt without the buffer",
    print(Zipper.unselect_and_zip(s1.ed.state.zipper)),
    print(s1.ed.syntax.segment),
  );
};

/* A syntax projector's model lives in the segment, but SetModel isn't an
   edit. */
let projector_model = () => {
  let s0 =
    act(Project(SetIndicated(Specific(Fold))), init("let x = 1¦ in x"));
  let id =
    switch (s0.ed.syntax.projector_list) {
    | [id] => id
    | _ => fail("expected one projector")
    };
  let model =
    FoldProj.sexp_of_t({
      ...FoldProj.default,
      text: "abc",
    })
    |> Sexplib.Sexp.to_string;
  let s1 = act(Project(SetModel(0, Fold, model)), s0);
  check(
    string,
    "cached model",
    model,
    Id.Map.find(id, s1.ed.syntax.projectors).model,
  );
  check(
    int,
    "shape width",
    3,
    ProjectorCore.Shape.Map.lookup(id, s1.ed.syntax.shape_map).horizontal,
  );
};

/* Inputs outside the segment, which calculate checks itself */
let shape_inputs = () => {
  let z = Test_Editing.perform(Zipper.init(), Test_Editing.mk("1 + 2¦"));
  let dyn_map = Language.Dynamics.Map.empty;
  let syntax = CachedSyntax.mk(~info_map=Id.Map.empty, ~dyn_map, z);
  let calc = z => CachedSyntax.calculate(z, Id.Map.empty, dyn_map, syntax);
  let with_focus = f =>
    Zipper.update_refractors(z, r =>
      {
        ...r,
        sample_focus: f(r.sample_focus),
      }
    );
  check(bool, "unchanged", true, calc(z) === syntax);
  /* ProbeFocus rebuilds the focus record on every Editor.calculate */
  check(
    bool,
    "equal focus",
    true,
    calc(
      with_focus(f =>
        {
          ...f,
          seq: f.seq,
        }
      ),
    )
    === syntax,
  );
  let moved =
    with_focus(f =>
      {
        ...f,
        seq: f.seq + 1,
      }
    );
  check(
    bool,
    "new focus",
    true,
    Language.Sample.Focus.equal(
      calc(moved).shape_sample_focus,
      moved.refractors.sample_focus,
    ),
  );
  ProbeProj.Settings.version := ProbeProj.Settings.version^ + 1;
  let z_sel =
    Test_Editing.perform(z, [Select(Resize(Local(Left, ByChar)))]);
  let refreshed = calc(z_sel);
  check(bool, "probe settings", true, refreshed.measured !== syntax.measured);
  check(
    bool,
    "refresh keeps the selection current",
    true,
    refreshed.selection_ids != []
    && refreshed.selection_ids == Selection.selection_ids(z_sel.selection),
  );
};

let tests = (
  "SyntaxCache",
  [
    test_case("caret moves keep the syntax", `Quick, caret_moves),
    test_case("selection keeps the measured", `Quick, selection),
    test_case("clearing a buffer rebuilds", `Quick, buffer_clear),
    test_case("syntax projector model rebuilds", `Quick, projector_model),
    test_case("calculate checks non-segment inputs", `Quick, shape_inputs),
  ],
);
