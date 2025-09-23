open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    |;

  let can_undo = _action => false; //TODO(andrew)
};

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = unit;

  let init = ();

  module Store =
    Store.F({
      [@deriving (show({with_path: false}), yojson, sexp)]
      type t = unit;
      let default = () => init;

      let key = Store.ProbeSystem;
    });
};

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

let fancy = (~info_map: Language.Statics.Map.t, ~globals: Globals.t, id: Id.t) =>
  switch (Language.Statics.Map.lookup(id, info_map)) {
  | Some(InfoExp({term, _})) =>
    div(
      ~attrs=[
        Attr.create("style", "cursor: pointer; position: relative;"),
        Attr.on_pointerdown(jump_to(~globals, id)),
      ],
      [term_view(~globals, ~available=8, Exp(term))],
    )
  | Some(InfoPat({term, _})) =>
    div(
      ~attrs=[
        Attr.create("style", "cursor: pointer; position: relative;"),
        Attr.on_pointerdown(jump_to(~globals, id)),
      ],
      [term_view(~globals, ~available=8, Pat(term))],
    )
  | _ => basic(~globals, id)
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
      ~inject as _: Update.t => Ui_effect.t(unit),
      ~model as _: Model.t,
      ~editor: CodeEditable.Model.t,
    ) => {
  let info_map = editor.statics.info_map;
  //let term_data = editor.editor.syntax.term_data;
  let refractors = editor.editor.state.zipper.refractors;
  let measured = editor.editor.syntax.measured;
  let manual_map = refractors.map;
  let pinned_term_ids = refractors.pinned_term_ids;
  let ephemerals = refractors.ephemerals;
  let dyn_cursor = refractors.dyn_cursor;
  let call_cursor = dyn_cursor.call_cursor;
  let indicated_call = dyn_cursor.indicated_call;
  let pinned_call = dyn_cursor.pinned_call;
  let fancyd = fancy(~info_map, ~globals);
  let basicd = basic(~globals);
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
                i == call_cursor.index
                  ? [
                    div(
                      ~attrs=[Attr.create("style", "font-weight: bold;")],
                      [fancyd(id)],
                    ),
                  ]
                  : [fancyd(id)],
              ),
            call_cursor.stack,
          ),
        ),
        br(),
        text("Indicated Call: "),
        indicated_call
        |> Option.map(fancyd)
        |> Option.value(~default=div([text("None")]), _),
        br(),
        text("Pinned Call: "),
        pinned_call
        |> Option.map(stack => div(List.map(id => fancyd(id), stack)))
        |> Option.value(~default=div([text("None")]), _),
      ]),
      div(
        [br(), text("Manual Probes:"), br()]
        @ List.map(
            ((id, _p)) => fancyd(id),
            manual_map |> Id.Map.to_list |> sort_ids_by_measurement(~measured),
          ),
      ),
      div(
        [br(), text("REPL Probes:"), br()]
        @ List.map(id => fancyd(id), pinned_term_ids)
        @ [
          div(
            ~attrs=[clss(["ephemerals"])],
            List.map(
              ((id, _p)) => fancyd(id),
              ephemerals
              |> Id.Map.to_list
              |> sort_ids_by_measurement(~measured),
            ),
          ),
        ],
      ),
    ],
  );
};
