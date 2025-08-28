open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

[@deriving (show({with_path: false}), sexp, yojson)]
type exo_model = {
  exo_kind: ProjectorCore.Kind.exo_kind,
  width: int,
  height: int,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type exo_action =
  | Resize(int, int);

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = exo_model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = exo_action;

  let int_of = (any: Language.Any.t): option(Bigint.t) =>
    switch (any) {
    | Exp({term: Atom(Int(i)), _}) => Some(i)
    | _ => None
    };

  let init = (any: Language.Any.t) =>
    switch (int_of(any)) {
    | Some(_) =>
      Some({
        exo_kind: ProjectorCore.Kind.Slider,
        width: 400,
        height: 80,
      })
    | None => None
    };

  let get_value = (info: info): string =>
    switch (
      info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(int_of)
    ) {
    | Some(i) => Bigint.to_string(i)
    | None => "0"
    };

  let get_range = (_info: info): (int, int, int) =>
    /* TODO: Extract from term type/context, for now default to 0-100 */
    (0, 100, 1);

  let focusable = Focusable.non;
  let dynamics = false;

  let placeholder = (model, _info) =>
    ProjectorCore.Shape.{
      horizontal: model.width / 10, /* Convert pixels to char units approximation */
      vertical: Block(model.height / 10) // TODO: update with actual height
    };

  let update = (model, _info, action) =>
    switch (action) {
    | Resize(w, h) => {
        ...model,
        width: w,
        height: h,
      }
    };

  let view =
      (
        model,
        info,
        ~local,
        ~parent: external_action => Ui_effect.t(unit),
        ~parent_global: external_action => unit,
        ~view_seg as _,
      ) => {
    /* Register this projector with the external bridge */
    ExternalProjectorBridge.register_projector(info.id, parent_global, info);

    let current_value = get_value(info);
    let (min_val, max_val, step_val) = get_range(info);

    /* Build iframe URL with parameters */
    let iframe_url =
      Printf.sprintf(
        "http://localhost:5173/?min=%d&max=%d&step=%d&initial=%s&id=%s&parentOrigin=%s",
        min_val,
        max_val,
        step_val,
        current_value,
        Id.to_string(info.id),
        "http://localhost:8000" /* Hazel dev server origin */
      );

    View.mk(
      Node.create(
        "iframe",
        ~attrs=[
          Attr.create("src", iframe_url),
          Attr.create("sandbox", "allow-scripts allow-same-origin"),
          Attr.create("allow", ""),
          Attr.create(
            "style",
            Printf.sprintf(
              "width: %dpx; height: %dpx; border: 1px solid #ddd; border-radius: 4px;",
              model.width,
              model.height,
            ),
          ),
          Attr.id(Id.cls(info.id) ++ "-exo-iframe"),
          /* Store projector info for the message bridge */
          Attr.create("data-projector-id", Id.cls(info.id)),
          Attr.create(
            "data-exo-type",
            ProjectorCore.Kind.exo_name(model.exo_kind),
          ),
        ],
        [],
      ),
    );
  };
};
