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

  let init = _ => Some(default);

  let focusable = Focusable.non;
  let dynamics = false;
  let elaborate_syntax = false;

  /* Wide enough for both halves and the arrow between them. */
  let placeholder = (m: model, _) =>
    ProjectorCore.Shape.inline(
      Unicode.Width.columns_of_string(m.reads)
      + Unicode.Width.columns_of_string(m.shown)
      + 3,
    );

  let update = (m, _, _) => m;
  let error = (_, _): option(ProjectorBase.error) => None;

  /* With no reference to show -- an opaque Fumola value rather than a peek
     -- there is nothing to put left of the equals, so the value stands
     alone. */
  let view = ({model, _}: View.args(model, action)) =>
    ProjectorBase.View.mk(
      div(
        ~attrs=[Attr.classes(["fumola-peek"])],
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
        ],
      ),
    );
};
