open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;

let jump_to = (~globals: Globals.t, id: Id.t, _) =>
  globals.inject_global(ActiveEditor(Move(Goal(TileId(id)))));

let basic = (~globals: Globals.t, id: Id.t) =>
  div(
    ~attrs=[
      Attr.create("style", "cursor: pointer;"),
      Attr.on_pointerdown(jump_to(~globals, id)),
    ],
    [text(Id.str3(id))],
  );

let exp_view = (~available, term: Language.Exp.t) =>
  Language.Abbreviate.abbreviate_exp(~available, term)
  |> fst
  |> Haz3lcore.ExpToSegment.exp_to_segment(
       ~settings=
         Haz3lcore.ExpToSegment.Settings.of_core(
           ~inline=false,
           Language.CoreSettings.off,
         ),
     );

let pat_view = (~available, term: Language.Pat.t) =>
  Language.Abbreviate.abbreviate_pat(~available, term)
  |> fst
  |> (x => Language.Grammar.Pat(x))
  |> Haz3lcore.ExpToSegment.any_to_segment(
       ~settings=
         Haz3lcore.ExpToSegment.Settings.of_core(
           ~inline=false,
           Language.CoreSettings.off,
         ),
     );

let term_view = (~globals: Globals.t, ~available=8, term: Language.Any.t) =>
  ProjectorView.simple_code(
    ~background=true,
    globals.font_metrics,
    Language.Sort.Exp,
    switch (term) {
    | Language.Grammar.Exp(x) => exp_view(~available, x)
    | Language.Grammar.Pat(x) => pat_view(~available, x)
    | _ => Example.mk_example("TODO")
    },
  );

let probe_view = (font_metrics, refractor_data, id: Id.t) => {
  let inject = _ => Ui_effect.Ignore;
  let projector_data =
    List.find_opt(
      (p: ProjectorView.Model.projector_data) => p.p.id == id,
      refractor_data,
    );
  switch (projector_data) {
  | Some(projector_data) =>
    let views = ProjectorView.mk_view(inject, font_metrics, projector_data);
    let offside_view = views.offside |> Option.to_list;
    div(~attrs=[Attr.class_("probe-view")], offside_view);
  | None => div([text("?")])
  };
};

let fancy =
    (
      ~refractor_data,
      ~info_map: Language.Statics.Map.t,
      ~globals: Globals.t,
      id: Id.t,
    ) => {
  let any =
    switch (Language.Statics.Map.lookup(id, info_map)) {
    | Some(InfoExp({term, _})) => Language.Grammar.Exp(term)
    | Some(InfoPat({term, _})) => Language.Grammar.Pat(term)
    | _ => Language.Grammar.Any()
    };
  div(
    ~attrs=[
      Attr.class_("probe-entry"),
      Attr.on_pointerdown(jump_to(~globals, id)),
    ],
    [
      term_view(~globals, ~available=8, any),
      probe_view(
        globals.font_metrics,
        refractor_data,
        Id.transform_variant(id),
      ),
    ],
  );
};

let sort_ids_by_measurement =
    (~measured: Haz3lcore.Measured.t, ids: list((Id.t, _))) =>
  ids
  |> List.sort(((id1, _p1), (id2, _p2)) =>
       compare(
         switch (Haz3lcore.Measured.find_by_id(id1, measured)) {
         | Some(m) => m.last.row
         | None => 0
         },
         switch (Haz3lcore.Measured.find_by_id(id2, measured)) {
         | Some(m) => m.last.row
         | None => 0
         },
       )
     );

let view =
    (
      ~globals: Globals.t,
      ~cursor as _: Cursor.cursor(Editors.Update.t),
      ~signal as _,
      ~editor: CodeEditable.Model.t,
    ) => {
  let refractor_data =
    ProjectorView.Model.mk(
      Id.Map.union(
        (_, _, b) => Some(b),
        editor.editor.state.zipper.refractors.manuals,
        editor.editor.state.zipper.refractors.ephemerals,
      ),
      editor.editor.syntax.measured,
      editor.editor.syntax.term_data,
      editor.editor.syntax.selection_ids,
      Haz3lcore.Indicated.piece(editor.editor.state.zipper),
      editor.statics.info_map,
      editor.dynamics,
      editor.editor.state.zipper.refractors.dyn_cursor,
      true,
    );
  let refractors = editor.editor.state.zipper.refractors;
  let measured = editor.editor.syntax.measured;
  let dyn_cursor = refractors.dyn_cursor;
  let fancyd =
    fancy(~refractor_data, ~info_map=editor.statics.info_map, ~globals);
  div(
    ~attrs=[Attr.id("probesys")],
    [
      div(
        ~attrs=[clss(["header"])],
        [div(~attrs=[clss(["main-title"])], [text("Probe System")])],
      ),
      div([
        text("Call Cursor: "),
        div(
          List.mapi(
            (i, id) =>
              div(
                i == dyn_cursor.index
                  ? [
                    div(
                      ~attrs=[Attr.create("style", "font-weight: bold;")],
                      [fancyd(id)],
                    ),
                  ]
                  : [fancyd(id)],
              ),
            dyn_cursor.stack,
          ),
        ),
        br(),
        text("Indicated Call: "),
        dyn_cursor.indicated_call
        |> Option.map(fancyd)
        |> Option.value(~default=div([text("None")]), _),
        br(),
        text("Pinned Call: "),
        dyn_cursor.pinned_stack
        |> Option.map(stack => div(List.map(id => fancyd(id), stack)))
        |> Option.value(~default=div([text("None")]), _),
      ]),
      div(
        [br(), text("Manual Probes:"), br()]
        @ List.map(
            ((id, _p)) => fancyd(id),
            refractors.manuals
            |> Id.Map.to_list
            |> sort_ids_by_measurement(~measured),
          ),
      ),
      div(
        [br(), text("REPL Probes:"), br()]
        @ List.map(id => fancyd(id), refractors.autos)
        @ [
          div(
            ~attrs=[clss(["ephemerals"])],
            List.map(
              ((id, _p)) => fancyd(id),
              refractors.ephemerals
              |> Id.Map.to_list
              |> sort_ids_by_measurement(~measured),
            ),
          ),
        ],
      ),
    ],
  );
};
