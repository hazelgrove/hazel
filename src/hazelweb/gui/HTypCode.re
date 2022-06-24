open Virtual_dom.Vdom;
open Pretty;

let take_step = step =>
  List.filter_map(
    fun
    | [step', ...steps] when step == step' => Some(steps)
    | _ => None,
  );

let view_of_layout =
    (diff_steps: list(CursorPath.steps), l: HTypLayout.t): list(Node.t) => {
  let col: ref(int) = ref(0);
  let rec go = (indent: int, dpaths, l: HTypLayout.t) =>
    switch (l) {
    | Linebreak =>
      col := indent;
      [
        Node.br([]),
        Node.span(
          [],
          [Node.text(StringUtil.replicat(indent, Unicode.nbsp))],
        ),
      ];
    | Text(s) =>
      col := col^ + Unicode.length(s);
      [Node.text(s)];
    | Cat(l1, l2) =>
      let vs1 = go(indent, dpaths, l1);
      let vs2 = go(indent, dpaths, l2);
      vs1 @ vs2;
    | Align(l) => go(col^, dpaths, l)
    | Annot(annot, l) =>
      switch (annot) {
      | Term =>
        let vs = go(indent, dpaths, l);
        List.exists((==)([]), dpaths)
          ? [Node.span([Attr.classes(["Diff"])], vs)] : vs;
      | Step(step) =>
        let dpaths' = take_step(step, dpaths);
        go(indent, dpaths', l);
      | Delim =>
        let vs = go(indent, dpaths, l);
        [Node.span([Attr.classes(["Delim"])], vs)];
      | HoleLabel =>
        let vs = go(indent, dpaths, l);
        [Node.span([Attr.classes(["HoleLabel"])], vs)];
      | TyVarHole =>
        let vs = go(indent, dpaths, l);
        [Node.span([Attr.classes(["InTyVarHole"])], vs)];
      }
    };
  go(0, diff_steps, l);
};

let view =
    (~width=30, ~pos=0, ~diff_steps: list(CursorPath.steps)=[], ty: HTyp.t)
    : Node.t =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("width", () => Sexplib.Std.sexp_of_int(width)),
      ("pos", () => Sexplib.Std.sexp_of_int(pos)),
      (
        "diff_steps",
        () => Sexplib.Std.sexp_of_list(CursorPath.sexp_of_steps, diff_steps),
      ),
      ("ty", () => HTyp.sexp_of_t(ty)),
    ],
    ~result_sexp=_ => List([]),
    () => {
      let l =
        ty
        |> HTypDoc.mk(~enforce_inline=false)
        |> LayoutOfDoc.layout_of_doc(~width, ~pos);
      switch (l) {
      | None => failwith("unimplemented: view_of_htyp on layout failure")
      | Some(l) =>
        Node.div(
          [Attr.classes(["code", "HTypCode"])],
          view_of_layout(diff_steps, l),
        )
      };
    },
  );
