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

let stored_model = (~globals: Globals.t, key: string): option(string) =>
  List.assoc_opt(key, globals.settings.sidebar.canvas_probe_models);

let view =
    (
      ~globals: Globals.t,
      ~editor: CodeWithStatics.Model.t,
      ~key: string,
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
      Refractors.mk_entry(~model=?stored_model(~globals, key), probe_kind);
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
    let parent = (a: ProjectorBase.external_action) =>
      switch (a) {
      | SampleFocus(sc) =>
        globals.inject_global(ActiveEditor(Project(SampleFocus(sc))))
      | Probe(pa) => globals.inject_global(ActiveEditor(Probe(pa)))
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
        parent,
        view_seg,
        status,
        core_settings: globals.settings.core,
      });
    /* projector/probe/sort classes so the probe system's own CSS applies;
       canvas-probe scopes the static-flow layout overrides */
    Some(
      div(
        ~attrs=[
          clss(["projector", "probe", "canvas-probe", Sort.show(sort)]),
        ],
        Option.to_list(v.offside) @ Option.to_list(v.below),
      ),
    );
  };
};
