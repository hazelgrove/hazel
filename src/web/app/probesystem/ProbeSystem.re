open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;

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
  |> ExpToSegment.exp_to_segment(
       ~settings=
         ExpToSegment.Settings.of_core(
           ~inline=true,
           Language.CoreSettings.off,
         ),
     );

let pat_view = (~available, term: Language.Pat.t) =>
  Language.Abbreviate.abbreviate_pat(~available, term)
  |> fst
  |> (x => Language.Grammar.Pat(x))
  |> ExpToSegment.any_to_segment(
       ~settings=
         ExpToSegment.Settings.of_core(
           ~inline=true,
           Language.CoreSettings.off,
         ),
     );

let segment_of =
    (
      ~default=Some([Example.exp("<In Builtin>")]),
      ~available=8,
      term: Language.Any.t,
    )
    : option(Segment.t) =>
  switch (term) {
  | Exp(x) => Some(exp_view(~available, x))
  | Pat(x) => Some(pat_view(~available, x))
  | _ => default
  };

let term_view =
    (~globals: Globals.t, ~default, ~available=8, term: Language.Any.t)
    : option(Node.t) => {
  open Util.OptUtil.Syntax;
  let+ segment = segment_of(~default, ~available, term);
  ProjectorView.flex_code(
    ~background=true,
    ~is_single_line=Some(),
    ~text_only=Option.None,
    ~font_metrics=globals.font_metrics,
    Language.Sort.Exp,
    segment,
  );
};

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
  | None => div([] /*text("Not Probed")*/)
  };
};

let fancy =
    (
      ~refractor_data,
      ~info_map: Language.Statics.Map.t,
      ~globals: Globals.t,
      ~default,
      id: Id.t,
    ) => {
  open Util.OptUtil.Syntax;
  let any =
    switch (Language.Statics.Map.lookup(id, info_map)) {
    | Some(InfoExp({term, _})) => Language.Grammar.Exp(term)
    | Some(InfoPat({term, _})) => Language.Grammar.Pat(term)
    | _ => Language.Grammar.Any()
    };
  let+ term_view = term_view(~globals, ~default, ~available=12, any);
  div(
    ~attrs=[
      Attr.class_("probe-entry"),
      Attr.on_pointerdown(jump_to(~globals, id)),
    ],
    [
      term_view,
      probe_view(
        globals.font_metrics,
        refractor_data,
        Id.transform_variant(id),
      ),
    ],
  );
};

let sort_ids_by_measurement = (~measured: Measured.t, ids: list((Id.t, _))) =>
  ids
  |> List.sort(((id1, _p1), (id2, _p2)) =>
       compare(
         switch (Measured.find_by_id(id1, measured)) {
         | Some(m) => m.last.row
         | None => 0
         },
         switch (Measured.find_by_id(id2, measured)) {
         | Some(m) => m.last.row
         | None => 0
         },
       )
     );

let div_cs = (cls, node) => div(~attrs=[Attr.classes(cls)], [node]);

let legend_closure_view =
    (
      ~indicated: bool,
      ~mode: ProbeProj.Settings.window,
      ~font_metrics: FontMetrics.t,
      ~ap_id: option(Id.t),
      ~indicated_call: option(Id.t),
      ~cursor_stack: list(Id.t),
      ~closure_stack: list(Id.t),
      ~caption: string,
    ) => {
  let closure: Language.Dynamics.Probe.Closure.t = {
    closure_id: 0,
    syntax_id: Id.invalid,
    value: Language.IdTagged.FreshGrammar.Exp.constructor(caption, None),
    env: Language.Dynamics.Probe.Env.empty,
    call_stack: closure_stack,
    time: 0.0,
  };
  let di: Language.Dynamics.Info.t = {
    closures: [closure],
    dyn_cursor: {
      stack: cursor_stack,
      index: List.length(cursor_stack) - 1,
      pinned_stack: None,
      indicated_call,
    },
  };
  ProbeProj.closure_view(
    ~ap_id,
    ~hide_env=true,
    ~settings={window: mode},
    di,
    ProjectorInfo.utility,
    (~text_only) =>
      ProjectorView.flex_code(
        ~font_metrics,
        ~background=false,
        ~is_single_line=Some(),
        ~text_only?,
      ),
    _ => Effect.Ignore,
    _ => Effect.Ignore,
    (0, closure),
  )
  |> div_cs(["closure-group"])
  |> div_cs(["closure-groups"])
  |> div_cs(["live-offside", ProbeProj.Settings.show_window(mode)])
  |> div_cs(["projector", "probe", indicated ? "indicated" : "not-indicated"]);
};

let legend_view = (~font_metrics: FontMetrics.t) => {
  let mode = ProbeProj.Settings.s^.window;
  let legend_closure_view = legend_closure_view(~mode, ~font_metrics);
  div(
    ~attrs=[clss(["legend", "panel"])],
    [
      div(~attrs=[clss(["title"])], [text("Sample Legend")]),
      legend_closure_view(
        ~indicated=false,
        ~ap_id=None,
        ~indicated_call=None,
        ~cursor_stack=[Id.invalid, Id.invalid],
        ~closure_stack=[Id.invalid],
        ~caption="Before",
      ),
      legend_closure_view(
        ~indicated=true,
        ~ap_id=None,
        ~indicated_call=None,
        ~cursor_stack=[Id.invalid],
        ~closure_stack=[Id.invalid],
        ~caption="At Cursor",
      ),
      legend_closure_view(
        ~indicated=false,
        ~ap_id=None,
        ~indicated_call=None,
        ~cursor_stack=[Id.invalid],
        ~closure_stack=[Id.invalid, Id.invalid],
        ~caption="After",
      ),
      legend_closure_view(
        ~indicated=false,
        ~indicated_call=None,
        ~ap_id=Some(Id.invalid),
        ~cursor_stack=[Id.invalid, Id.invalid],
        ~closure_stack=[Id.invalid],
        ~caption="Contains",
      ),
      legend_closure_view(
        ~indicated=false,
        ~ap_id=None,
        ~indicated_call=None,
        ~cursor_stack=[Id.mk()],
        ~closure_stack=[Id.invalid],
        ~caption="Off Cursor",
      ),
      legend_closure_view(
        ~indicated=false,
        ~indicated_call=Some(Id.invalid),
        ~ap_id=None,
        ~cursor_stack=[Id.invalid],
        ~closure_stack=[Id.invalid, Id.invalid],
        ~caption="Inside",
      ),
    ],
  );
};

let sketch_view = () =>
  div(
    ~attrs=[clss(["sketch"])],
    [Node.img(~attrs=[Attr.src("../../img/probe-lenses.webp")], ())],
  );

let call_cursor_view = (~dyn_cursor: Language.Dynamics.Cursor.t, ~fancyd) =>
  div(
    ~attrs=[clss(["panel", "call-cursor"])],
    [
      div(~attrs=[clss(["title"])], [text("Dynamic Cursor")]),
      switch (dyn_cursor.indicated_call) {
      | Some(id) when !List.mem(id, dyn_cursor.stack) =>
        div(
          ~attrs=[Attr.classes(["indicated-call", "not-in-stack"])],
          [fancyd(id)],
        )
      | _ => div([])
      },
      div(
        List.mapi(
          (i, id) =>
            div([
              div(
                ~attrs=[
                  Attr.classes([
                    i == dyn_cursor.index ? "is-index" : "not",
                    i > dyn_cursor.index ? "after-index" : "not",
                    List.mem(id, dyn_cursor.stack)
                    && Some(id) == dyn_cursor.indicated_call
                      ? "indicated-call" : "not",
                  ]),
                ],
                [fancyd(id)],
              ),
            ]),
          dyn_cursor.stack,
        ),
      ),
    ],
  );

type probe_type =
  | Manual(Base.projector)
  | Auto(list((Id.t, Base.projector)));

let prep_refractors =
    (~refractors: Zipper.Refractor.t, ~info_map, ~syntax: CachedSyntax.t) => {
  let manuals =
    refractors.manuals
    |> Id.Map.to_list
    |> List.map(((id, p)) => (id, Manual(p)));
  let autos =
    refractors.autos
    |> List.map(id => {
         let ids =
           Refractors.ids_from_term(
             ~term_data=syntax.term_data,
             ~terms=syntax.terms,
             ~measured=syntax.measured,
             ~info_map,
             id,
           );
         let pairs =
           List.filter_map(
             id =>
               switch (Id.Map.find_opt(id, refractors.ephemerals)) {
               | Some(p) => Some((id, p))
               | None => None
               },
             ids,
           );
         (id, Auto(pairs));
       });
  List.concat([manuals, autos])
  |> sort_ids_by_measurement(~measured=syntax.measured);
};

let probes_panel_view =
    (
      ~refractors: Zipper.Refractor.t,
      ~info_map: Language.Statics.Map.t,
      ~syntax: CachedSyntax.t,
      ~fancyd,
    ) =>
  div(
    ~attrs=[clss(["panel", "probes"])],
    [div(~attrs=[clss(["title"])], [text("Probes")])]
    @ List.filter_map(
        ((id, probe_type)) =>
          switch (probe_type) {
          | Manual(_) => fancyd(id)
          | Auto(pairs) =>
            Some(
              div(
                ~attrs=[clss(["auto"])],
                List.filter_map(((id, _p)) => fancyd(id), pairs),
              ),
            )
          },
        prep_refractors(~refractors, ~info_map, ~syntax),
      ),
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
      Indicated.piece(editor.editor.state.zipper),
      editor.statics.info_map,
      editor.dynamics,
      editor.editor.state.zipper.refractors.dyn_cursor,
      true,
    );
  let refractors = editor.editor.state.zipper.refractors;
  div(
    ~attrs=[Attr.id("probesys")],
    [
      div(
        ~attrs=[clss(["header"])],
        [div(~attrs=[clss(["main-title"])], [text("Live Probes")])],
      ),
      legend_view(~font_metrics=globals.font_metrics),
      sketch_view(),
      call_cursor_view(~dyn_cursor=refractors.dyn_cursor, ~fancyd=id =>
        fancy(
          ~refractor_data,
          ~info_map=editor.statics.info_map,
          ~default=Some([Example.exp("<In Builtin>")]),
          ~globals,
          id,
        )
        |> Option.value(~default=div([]))
      ),
      probes_panel_view(
        ~refractors,
        ~info_map=editor.statics.info_map,
        ~syntax=editor.editor.syntax,
        ~fancyd=id =>
        fancy(
          ~refractor_data,
          ~info_map=editor.statics.info_map,
          ~default=None,
          ~globals,
          id,
        )
      ),
    ],
  );
};
