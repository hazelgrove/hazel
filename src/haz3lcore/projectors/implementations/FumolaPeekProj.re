open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;

/* The widget for a reference into a Fumola runtime.
 *
 * Unlike every other projector, this one is never chosen by a user: it is put
 * there by translation, when a Fumola program returns a pointer. So it is not
 * in ProjectorKind.livelit_projectors and does not appear in the projector
 * menu.
 *
 * It renders entirely from its own model -- the reference and the value, as
 * text -- and consults neither statics nor the syntax it wraps. That is what
 * lets it work where it actually appears: in a result, which is rendered from
 * a bare segment with no info map behind it. */

/* The model lives in Language.FumolaPeekModel, shared with the translation
   that builds these: the value carrying a projector is built in language,
   which cannot look up here, so sharing the type keeps the two sides from
   drifting in how the model is spelled. */
type t = Language.FumolaPeekModel.t;

let default: t = Language.FumolaPeekModel.empty;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = Language.FumolaPeekModel.t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  /* Widened here when experimental-lang-integration merged: ProjectorBase's
     init now also takes the syntax and may return an override. A FumolaPeek
     is placed by translation rather than by an author, so it needs neither. */
  let init = (_, _) =>
    Some((default, None: option(ProjectorBase.init_override)));

  let focusable = Focusable.non;
  let dynamics = false;
  let elaborate_syntax = false;

  /* Wide enough for both halves, the arrow between them, and a little over.

     The three is the arrow and its spaces. The four beyond it is an
     allowance, and it is not arbitrary: the widget paints in the code font
     at the code size (see proj-livelit.css, which had to be told to), and
     that font's advance width is a shade wider than the editor's column --
     10.4px against 10.0 at the default size. Over a hundred characters that
     is three columns, so a box sized to its exact character count comes out
     wider than the slot reserved for it and sits proud of the text beside
     it.

     Erring the other way leaves a moat, which is what this looked like
     before the font was fixed: the widget was inheriting the proportional
     UI font, so a hundred and ten columns of slot held eighty of drawing. */
  /* Third argument added by the merge: ProjectorBase now passes
     [splice_size] so a projector can size itself around its splices. A
     FumolaPeek has none, so it sizes itself from its own text as before. */
  let placeholder = (m: model, _, _) => {
    let first =
      Unicode.Width.columns_of_string(m.reads)
      + Unicode.Width.columns_of_string(m.shown)
      + 7;
    /* The node's line sits under the first, so the widget is two rows tall
       and as wide as the wider of them. Block rather than Tab: the line
       belongs to this widget and should arrive with it, not be deferred to
       wherever the line happens to break. */
    m.info == ""
      ? ProjectorCore.Shape.inline(first)
      : {
        horizontal: max(first, Unicode.Width.columns_of_string(m.info) + 4),
        vertical: Block(1),
      };
  };

  /* Added when experimental-lang-integration merged into a branch whose
     ProjectorBase requires these. A FumolaPeek is a rendering of a value a
     Fumola program returned: it has no splices of its own, so no rows to
     report and no splice-local menu items to contribute. */
  let splice_rows = (_, _, _) => Id.Map.empty;

  let context_actions = (_, _, ~splice as _) => [];

  let update = (m, _, _) => m;
  let error = (_, _): option(ProjectorBase.error) => None;

  /* With no reference to show -- an opaque Fumola value rather than a peek
     -- there is nothing to put left of the equals, so the value stands
     alone.

     The node's line goes below rather than beside, because it is a step
     further in: the first line is what the cell holds, and a cell holding a
     thunk holds its code. The second is what the node remembers the force
     answered, which is what a reader looking at a thunk actually wants. */
  let view = ({model, _}: View.args(model, action)) =>
    ProjectorBase.View.mk(
      div(
        ~attrs=[
          Attr.classes(
            ["fumola-peek"] @ (model.info == "" ? [] : ["fumola-peek-deep"]),
          ),
        ],
        (
          model.reads == ""
            ? []
            : [
              span(
                ~attrs=[Attr.classes(["fumola-peek-reads"])],
                [text(model.reads)],
              ),
              span(
                ~attrs=[Attr.classes(["fumola-peek-arrow"])],
                [text("=")],
              ),
            ]
        )
        @ [
          span(
            ~attrs=[Attr.classes(["fumola-peek-value"])],
            [text(model.shown)],
          ),
        ]
        @ (
          model.info == ""
            ? []
            : [
              div(
                ~attrs=[Attr.classes(["fumola-peek-info"])],
                [text(model.info)],
              ),
            ]
        ),
      ),
    );
};
