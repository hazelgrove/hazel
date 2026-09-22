open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;

/* Sidebar panel for projectors docked via ProjectorCore.Placement.Sidebar.
 * One card per docked projector, in syntax order; the code site keeps a
 * chip (ProjectorView.chip). Undocking is the same action that docked it
 * (Action.Project(TogglePlacement) on the indicated projector). */

/* Global jump: resolves which editor holds the id, selects that cell, and
 * focuses it (the caret is hidden unless .code-editor has DOM focus). */
let jump_to = (~globals: Globals.t, id: Id.t, _) =>
  globals.inject_global(JumpToTile(id));

let empty_view = (): Node.t =>
  div(
    ~attrs=[clss(["projector-panel-empty"])],
    [
      text("No projectors docked."),
      text(
        "Put the caret on a projector and press Alt+S to dock it here, "
        ++ "or dock an open rich probe with the \u{21e5} button.",
      ),
    ],
  );

/* `undock` is the action that puts this card's content back where it came
   from: for a docked projector that is TogglePlacement on the projector
   itself; for a probe's rich probe it is the probe's own placement toggle,
   since the probe never left the code. */
let card =
    (
      ~globals: Globals.t,
      p: Base.projector,
      view: Node.t,
      ~undock: unit => Ui_effect.t(unit),
    )
    : Node.t =>
  div(
    ~attrs=[clss(["projector-card"])],
    [
      div(
        ~attrs=[
          clss(["projector-card-header"]),
          Attr.title("Jump to source"),
          Attr.on_click(jump_to(~globals, p.id)),
        ],
        [
          div_c(
            "projector-card-syntax",
            [
              ProjectorView.chip_syntax(~font_metrics=globals.font_metrics, p),
            ],
          ),
          Node.button(
            ~attrs=[
              clss(["projector-card-undock"]),
              Attr.title("Move back inline"),
              Attr.on_click(_ =>
                Effect.Many([jump_to(~globals, p.id, ()), undock()])
              ),
            ],
            [text({|⇤|})],
          ),
        ],
      ),
      div_c("projector-card-body", [view]),
    ],
  );

let view = (~globals: Globals.t, ~editor: CodeWithStatics.Model.t): Node.t => {
  let zipper = editor.editor.state.zipper;
  let inject = (a: Action.t) => globals.inject_global(ActiveEditor(a));
  /* Same inputs CodeEditable passes to ProjectorView.Model.mk, sourced from
   * the sidebar's active code editor. editor_active is true because the
   * panel always tracks the editor Page reports as current. */
  let projector_data =
    ProjectorView.Model.mk(
      ~syntax=editor.editor.syntax,
      ~indicated=Indicated.for_decoration(zipper),
      ~statics=editor.statics.info_map,
      ~dynamics=editor.dynamics,
      ~sample_focus=zipper.refractors.sample_focus,
      ~editor_active=true,
      ~elaborated=Some(editor.statics.elaborated),
    );
  let docked_projectors =
    ProjectorView.sidebar_views(
      inject,
      globals.font_metrics,
      ~core_settings=globals.settings.core,
      projector_data,
      editor.editor.syntax.projector_list,
    )
    |> List.map(((p, view)) =>
         card(~globals, p, view, ~undock=() =>
           inject(Action.Project(TogglePlacement))
         )
       );
  let cards = docked_projectors;
  div(
    ~attrs=[Attr.id("projector-panel")],
    [
      div(~attrs=[clss(["header"])], [text("Projectors")]),
      div(
        ~attrs=[clss(["projector-panel-body"])],
        cards == [] ? [empty_view()] : cards,
      ),
    ],
  );
};
