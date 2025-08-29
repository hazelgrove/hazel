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
        height: 160,
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

  let focusable = Focusable.non;
  let dynamics = false;

  let placeholder = (model: exo_model, _info: info): ProjectorCore.Shape.t => {
    //TODO(andrew): route font metrics here
    let char_width = 10.4375;
    let char_height = 25.125;
    let round_up_to_multiple = (value: int, multiple: float): int => {
      int_of_float(
        ceil(float_of_int(value) /. multiple) *. multiple /. multiple,
      );
    };
    {
      horizontal: round_up_to_multiple(model.width, char_width) + 1,
      vertical: Block(round_up_to_multiple(model.height, char_height) - 1),
    };
  };

  let update = (model, _info, action) =>
    switch (action) {
    | Resize(w, h) => {
        ...model,
        width: w,
        height: h,
      }
    };

  let iframe_url = (~min_val, ~max_val, ~step_val, ~current_value, ~id) =>
    Printf.sprintf(
      "http://localhost:5173/?min=%d&max=%d&step=%d&initial=%s&id=%s&parentOrigin=%s",
      min_val,
      max_val,
      step_val,
      current_value,
      Id.to_string(id),
      "http://localhost:8000" /* Hazel dev server origin */
    );

  let iframe_view = (id: Id.t, model: exo_model, current_value: string) => {
    let iframe_url =
      iframe_url(~min_val=0, ~max_val=100, ~step_val=1, ~current_value, ~id);
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
        Attr.id(Id.cls(id) ++ "-exo-iframe"),
        Attr.create("data-projector-id", Id.cls(id)),
        Attr.create(
          "data-exo-type",
          ProjectorCore.Kind.exo_name(model.exo_kind),
        ),
      ],
      [],
    );
  };

  let view =
      (
        model,
        info,
        ~local as _,
        ~parent: external_action => Ui_effect.t(unit),
        ~view_seg as _,
      ) => {
    ExternalProjectorBridge.register_projector(parent, info);
    View.mk(iframe_view(info.id, model, get_value(info)));
  };
};
