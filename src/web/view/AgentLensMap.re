open Haz3lcore;
open Util;

/* View as agent, Stacked: the map the request sent, as a read-only
   editor. Its pieces are the map's (CompositionView.map_of: folded
   bodies are ⋯ inside tiles that keep their ids), so the request
   program's own statics and run draw on it by id; nothing is analyzed
   here, as ⋯ would read as errors. The map's notes that the editor
   doesn't draw sit at their lines' ends. */

module CV = CompositionView;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  editor: CellEditor.Model.t,
  notes: list(CV.note),
  /* the program's last expression: its value's note goes there once the
     program has run */
  tail: option(Id.t),
};

/* the notes the editor draws itself (test marks; probe values, which
   are not map notes), or that aren't wanted here (use counts); the
   value comes from this program's run */
let note_settings = (view: CV.settings): CV.settings => {
  ...view,
  uses: false,
  test_status: false,
  value: false,
};

let ids_of = (seg: Segment.t): Id.Map.t(unit) =>
  List.fold_left(
    (m, id) => Id.Map.add(id, (), m),
    Id.Map.empty,
    Segment.ids(seg),
  );

let build =
    (
      ~view: CV.settings,
      ~root: Sort.t,
      ~statics: CachedStatics.t,
      ~opened: list(Id.t),
      z: Zipper.t,
    )
    : t => {
  let o = Outline.of_program(~root, Zipper.unselect_and_zip(z));
  let ctx = error_ids =>
    CV.mk_ctx(
      ~settings=note_settings(view),
      ~facts={
        ...CV.no_facts,
        info_map: statics.info_map,
        error_ids,
      },
      o,
      opened,
    );
  /* an error on a term the map shows is drawn there; those in folded
     bodies have nothing to draw on, so their entries' notes keep them */
  let shown = ids_of(fst(CV.map_of(ctx([]), o)));
  let (seg, notes) =
    CV.map_of(
      ctx(List.filter(id => !Id.Map.mem(id, shown), statics.error_ids)),
      o,
    );
  {
    editor:
      Divided.editor_of(
        ~root,
        ~probes=z.refractors.manuals,
        ~anchor=None,
        seg,
      ),
    notes,
    tail:
      List.find_map(
        (e: Outline.entry) =>
          e.kind == Tail ? CV.last_content(Outline.core(e.lv, e.k)) : None,
        o.entries,
      ),
  };
};

let segment = (m: t): Segment.t =>
  Zipper.unselect_and_zip(m.editor.editor.editor.state.zipper);

/* the request program's statics and run, on the map: its errors and
   warnings only those on the map's terms, so ⋯ shows none */
let calculate =
    (~settings: Language.CoreSettings.t, program: Program.t, m: t): t => {
  let st = Program.statics(program);
  let cws = m.editor.editor;
  let statics =
    cws.statics.info_map === st.info_map
    && cws.statics.elaborated === st.elaborated
      ? cws.statics
      : {
        let shown = ids_of(segment(m));
        let on_map = List.filter(id => Id.Map.mem(id, shown));
        {
          ...st,
          error_ids: on_map(st.error_ids),
          warning_ids: on_map(st.warning_ids),
        };
      };
  let dynamics = EvalResult.Model.dynamics(Program.result(program));
  let editor =
    Editor.Update.calculate(
      ~settings,
      ~autoprobe_mode=Off,
      ~is_edited=false,
      statics,
      dynamics,
      cws.editor,
    );
  {
    ...m,
    editor: {
      ...m.editor,
      editor: {
        ...cws,
        editor,
        statics,
        dynamics,
      },
    },
  };
};

/* read-only: the caret moves and selects; nothing else lands */
let allowed = (a: CellEditor.Update.t): bool =>
  switch (a) {
  | MainEditor(Perform(Move(_) | Select(_) | Unselect(_) | Copy)) => true
  | _ => false
  };

let update =
    (~settings: Settings.t, a: CellEditor.Update.t, m: t)
    : option(Updated.t(t)) =>
  allowed(a)
    ? Some(
        Updated.(
          let* editor = CellEditor.Update.update(~settings, a, m.editor);
          {
            ...m,
            editor,
          }
        ),
      )
    : None;

/* its notes, the program's value at its tail */
let notes = (~value: option(Language.Exp.t), m: t): list(CV.note) =>
  switch (value, m.tail) {
  | (Some(v), Some(tail)) =>
    let part = {js|⇒ |js} ++ CV.comment_safe(ProbeText.format_value(v));
    List.map(
      (n: CV.note) =>
        n.at == tail && n.last
          ? {
            ...n,
            parts: n.parts @ [part],
          }
          : n,
      m.notes,
    );
  | _ => m.notes
  };

/* the comment at each of [rows] rows' ends, or "" */
let comments =
    (
      ~rows: int,
      find: Id.t => option(Measured.measurement),
      notes: list(CV.note),
    )
    : array(string) =>
  CV.annotate(Array.make(rows, ""), find, notes) |> Array.map(String.trim);

/* the map as text, its notes at line ends: the map the request sent,
   less the notes left out here */
let text = (~value: option(Language.Exp.t)=?, m: t): string => {
  let seg = segment(m);
  let (lines, find) =
    CV.print_measured(
      ~refractors=m.editor.editor.editor.state.zipper.refractors.manuals,
      seg,
    );
  CV.annotate(lines, find, notes(~value, m))
  |> Array.to_list
  |> CV.strip_trailing_blank
  |> String.concat("\n");
};
