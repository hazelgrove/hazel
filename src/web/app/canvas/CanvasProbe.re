open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;

/* A canvas sample well IS a probe: the focus strip renders each anchor id
   through ProbeProj's own cooked view — same DOM structure and CSS, same
   global probe settings (One/Many windowing, color scheme, drawer width),
   same interactions (sample click = real SampleFocus capture, double-click
   toggles window mode, drawer toggle gives the 2D pretty-printed view,
   shift-drag resizes) — instead of a parallel display implementation.

   The one divergence from in-editor probes: there is no refractor entry to
   hold the probe MODEL (drawer on/off, per-sample widths, renderer), so it
   lives in sidebar settings keyed by a name-stable slot key, dispatched
   through SetCanvasProbeModel (which also provides the redraw). */

let probe_kind: ProjectorCore.Kind.t = Probe;

/* Where a well's sample-focus / probe actions go: the WHOLE-PROGRAM
   (master) editor, whose dynamics the wells display. Routing them to
   the "active" editor sent them to the open definition cell in
   constellation mode — the master's sample focus never moved, so ← →
   did nothing and no sample read as selected. CanvasSidebar installs
   the master dispatcher each render. */
let master_perform: ref(option(Haz3lcore.Action.t => Effect.t(unit))) =
  ref(Option.none);

let stored_model = (~globals: Globals.t, key: string): option(string) =>
  List.assoc_opt(key, globals.settings.sidebar.canvas_probe_models);

let view =
    (
      ~globals: Globals.t,
      ~editor: CodeWithStatics.Model.t,
      ~key: string,
      /* card mode: the aligned sample alone (rich view / pretty value),
         for the canvas type cards */
      ~card: bool=false,
      id: Id.t,
    )
    : option(Node.t) => {
  let syntax = editor.editor.syntax;
  let statics = editor.statics.info_map;
  let dynamics = editor.dynamics;
  let sample_focus = editor.editor.state.zipper.refractors.sample_focus;
  /* anchor syntax: same recipe as RefractorView.mk_data */
  let syntax_piece =
    TermData.segment(id, syntax.term_data)
    |> Option.map(Segment.unparenthesize)
    |> Option.map(Segment.trim_secondary(Left))
    |> Option.map(Segment.trim_secondary(Right))
    |> Option.map(Segment.parenthesize);
  switch (syntax_piece) {
  | None => None
  | Some(syntax_piece) =>
    let entry =
      Refractors.mk_entry(
        /* wells auto-render the first applicable rich renderer (html,
           card, ...) — the plain display is the fallback, not the default */
        ~model=
          card
            ? Haz3lcore.ProbeProj.model_string_card(
                stored_model(~globals, key),
              )
            : Haz3lcore.ProbeProj.model_string_auto_rich(
                stored_model(~globals, key),
              ),
        probe_kind,
      );
    let p = Refractors.to_projector(syntax_piece, id, entry);
    let info =
      ProjectorInfo.mk_info(
        p,
        ~sample_focus,
        ~statics,
        ~dynamics,
        ~elaborated=None,
      );
    let sort = TermData.sort(id, syntax.term_data);
    let status =
      ProjectorView.Model.mk_status(
        p,
        ~editor_active=false,
        ~indicated=None,
        ~selection_ids=[],
        ~info,
        ~id,
        ~sort,
      );
    let (module P) = ProjectorInit.to_module(probe_kind);
    let local = a => {
      let new_model = P.update(p.model, info, a);
      globals.inject_global(
        Set(Sidebar(SetCanvasProbeModel(key, new_model))),
      );
    };
    let perform = (a: Haz3lcore.Action.t) =>
      switch (master_perform^) {
      | Option.Some(f) => f(a)
      | Option.None => globals.inject_global(ActiveEditor(a))
      };
    let parent = (a: ProjectorBase.external_action) =>
      switch (a) {
      | SampleFocus(sc) => perform(Project(SampleFocus(sc)))
      | Probe(pa) => perform(Probe(pa))
      | _ => Effect.Ignore
      };
    let view_seg =
        (~single_line=?, ~background=?, ~text_only=?, sort, segment) =>
      ProjectorView.flex_code(
        ~font_metrics=globals.font_metrics,
        ~single_line?,
        ~background?,
        ~text_only?,
        sort,
        segment,
      );
    let v: ProjectorBase.View.t =
      P.view({
        model: p.model,
        info,
        local,
        /* the canvas probe-model store is settings-side (not the zipper),
           so there is no undo entry to suppress — quiet == loud here */
        local_quiet: local,
        parent,
        view_seg,
        status,
        core_settings: globals.settings.core,
        col_width: globals.font_metrics.col_width,
        row_height: globals.font_metrics.row_height,
      });
    /* projector/probe/sort classes so the probe system's own CSS applies;
       canvas-probe scopes the static-flow layout overrides */
    Some(
      div(
        ~attrs=[
          clss(
            ["projector", "probe", "canvas-probe", Sort.show(sort)]
            @ (card ? ["canvas-card-probe"] : []),
          ),
        ],
        Option.to_list(v.offside) @ Option.to_list(v.below),
      ),
    );
  };
};

/* A site that IS a livelit invocation (an app instance) renders the
   real projector — interactive: an action commits the update redex to
   the master editor, the program re-evaluates, every other card
   follows. Same construction as the projector panel's cards. */
let is_app_site = (~editor: CodeWithStatics.Model.t, id: Id.t): bool => {
  let syntax = editor.editor.syntax;
  List.mem(id, syntax.projector_list)
  && (
    switch (Id.Map.find_opt(id, syntax.projectors)) {
    | Some(p) => p.kind == ProjectorCore.Kind.Livelit
    | None => false
    }
  );
};

let app_data_memo:
  ref(
    option(
      (
        CachedSyntax.t,
        Language.Statics.Map.t,
        Language.Dynamics.Map.t,
        Language.Sample.Focus.t,
        list(ProjectorView.Model.projector_data),
      ),
    ),
  ) =
  ref(Option.none);

let app_view =
    (~globals: Globals.t, ~editor: CodeWithStatics.Model.t, id: Id.t)
    : option(Node.t) => {
  let syntax = editor.editor.syntax;
  let zipper = editor.editor.state.zipper;
  let inject = (a: Haz3lcore.Action.t) =>
    switch (master_perform^) {
    | Option.Some(f) => f(a)
    | Option.None => globals.inject_global(ActiveEditor(a))
    };
  /* projector data for the whole editor, once per (syntax, statics,
     dynamics, focus): every app card on every render asks */
  let data =
    switch (app_data_memo^) {
    | Option.Some((sy, st, dy, sf, d))
        when
          sy === syntax
          && st === editor.statics.info_map
          && dy === editor.dynamics
          && sf == zipper.refractors.sample_focus => d
    | _ =>
      let d =
        ProjectorView.Model.mk(
          ~syntax,
          ~indicated=None,
          ~statics=editor.statics.info_map,
          ~dynamics=editor.dynamics,
          ~sample_focus=zipper.refractors.sample_focus,
          ~editor_active=false,
          ~elaborated=Some(editor.statics.elaborated),
        );
      app_data_memo :=
        Option.some((
          syntax,
          editor.statics.info_map,
          editor.dynamics,
          zipper.refractors.sample_focus,
          d,
        ));
      d;
    };
  switch (
    List.find_opt(
      (d: ProjectorView.Model.projector_data) => d.p.id == id,
      data,
    )
  ) {
  | Some(d) =>
    let views =
      ProjectorView.mk_view(
        inject,
        globals.font_metrics,
        ~core_settings=globals.settings.core,
        d,
        syntax.projector_list,
      );
    Some(
      div(
        ~attrs=[
          clss(
            ProjectorView.projector_clss(~view_error=views.error, d.status)
            @ ["canvas-app"],
          ),
        ],
        [views.inline],
      ),
    );
  | None => None
  };
};

/* ~app: the card belongs to a livelit's own node — the app itself. A
   TYPE's card shows values, even at the app's site (its stream holds
   the app's values too). */
let card_view = (~globals, ~editor, ~key, ~app: bool=false, id) =>
  app && is_app_site(~editor, id)
    ? app_view(~globals, ~editor, id)
    : view(~globals, ~editor, ~key, ~card=true, id);

/* ---- aggregate value strip (type-node wells) ----
   One chip per distinct value: the sample-display RENDERING (green chip,
   in-chip rich views) without the sample-stream machinery — aggregating
   samples from different probes into one navigable stream would break
   the indication/window invariants. Clicking a chip captures that
   value's real occurrence (jump-to-occurrence). */
let value_chip =
    (
      ~globals: Globals.t,
      ~editor: CodeWithStatics.Model.t,
      ~count: int,
      ~target_cols: int=44,
      sample: Language.Sample.t,
    )
    : option(Node.t) => {
  let syntax = editor.editor.syntax;
  let statics = editor.statics.info_map;
  let id = sample.syntax_id;
  let syntax_piece =
    TermData.segment(id, syntax.term_data)
    |> Option.map(Segment.unparenthesize)
    |> Option.map(Segment.trim_secondary(Left))
    |> Option.map(Segment.trim_secondary(Right))
    |> Option.map(Segment.parenthesize);
  syntax_piece
  |> Option.map(syntax_piece => {
       let entry = Refractors.mk_entry(probe_kind);
       let p = Refractors.to_projector(syntax_piece, id, entry);
       let info =
         ProjectorInfo.mk_info(
           p,
           ~sample_focus=Language.Sample.Focus.init,
           ~statics,
           ~dynamics=editor.dynamics,
           ~elaborated=None,
         );
       let view_seg = (_, segment) =>
         ProjectorView.flex_code(
           ~font_metrics=globals.font_metrics,
           ~single_line=true,
           Sort.Exp,
           segment,
         );
       let rich =
         Haz3lcore.ProbeProj.standalone_rich(
           ~info,
           ~sort=Sort.Exp,
           ~view_seg,
           sample.value,
         );
       let content =
         switch (rich) {
         | Some(n) => [div(~attrs=[clss(["value-rich"])], [n])]
         | None =>
           /* the probe pill's OWN rendering: structural abbreviation
              fit to a column target (never CSS truncation), through
              the same term_to_seg the probe uses */
           let rec fit = (budgets: list(int)): Segment.t =>
             switch (budgets) {
             | [] =>
               ProbeUtil.abbreviated_seg_of(
                 ProjectorInfo.utility,
                 8,
                 sample.value,
               )
               |> fst
             | [b, ...rest] =>
               let (seg, len) =
                 ProbeUtil.abbreviated_seg_of(
                   ProjectorInfo.utility,
                   b,
                   sample.value,
                 );
               len <= target_cols ? seg : fit(rest);
             };
           let seg = fit([200, 120, 80, 56, 40, 28, 18]);
           [
             ProjectorView.flex_code(
               ~font_metrics=globals.font_metrics,
               ~single_line=true,
               Sort.Exp,
               seg,
             ),
           ];
         };
       let value_title =
         switch (rich) {
         | Some(_) => ""
         | None =>
           let t =
             ProbeUtil.abbreviated_seg_of(
               ProjectorInfo.utility,
               220,
               sample.value,
             )
             |> fst
             |> ProjectorInfo.utility.seg_to_string;
           t ++ "\n";
         };
       /* the strip shows the MASTER editor's dynamics: the capture goes
          there too (the active editor is the open definition cell) */
       let jump =
         (
           switch (master_perform^) {
           | Option.Some(f) => f
           | Option.None => (a => globals.inject_global(ActiveEditor(a)))
           }
         )(
           Project(
             SampleFocus(
               Capture(Language.Sample.capture_of_sample(sample), None),
             ),
           ),
         );
       /* this occurrence IS the dynamic focus: outlined like a focused
          sample */
       let anchored =
         switch (editor.editor.state.zipper.refractors.sample_focus.anchor) {
         | Some(a) =>
           a.probe_id == sample.syntax_id
           && (
             switch (a.opened) {
             | Some(o) => o == sample.step_start
             | None => true
             }
           )
         | None => false
         };
       div(
         ~attrs=[
           /* the probe pill's own DOM hierarchy, so proj-probe.css
              (backing, ink, typography) applies natively instead of
              being imitated */
           clss(
             ["live-offside", "Single", "agg-value"]
             @ (anchored ? ["agg-anchored"] : []),
           ),
           Attr.title(
             value_title ++ "click: jump the dynamic focus to this occurrence",
           ),
           Attr.on_pointerdown(_ => jump),
         ],
         [ProbePill.standalone(content)]
         @ (
           count > 1
             ? [
               span(
                 ~attrs=[clss(["type-value-count"])],
                 [text(Printf.sprintf({js|×%d|js}, count))],
               ),
             ]
             : []
         ),
       );
     });
};
