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
        exo_kind: ExoSlider,
        width: 400,
        height: 160,
      })
    | None => None
    };

  let focusable = Focusable.non;
  let dynamics = false;

  let placeholder = (model: exo_model, _): ProjectorCore.Shape.t => {
    let px_to_grid = (value: int, multiple: float): int =>
      int_of_float(ceil(float_of_int(value) /. multiple));
    let m = Util.font_metrics^;
    {
      horizontal: px_to_grid(model.width, m.col_width) + 1,
      vertical: Block(px_to_grid(model.height, m.row_height) - 1),
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

  let iframe_view = (id: Id.t, url: string, model: exo_model) => {
    Node.create(
      "iframe",
      ~attrs=[
        Attr.create("src", url),
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
        Attr.id(ExternalProjectorBridge.iframe_id(id)),
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
        model: model,
        info: info,
        ~local as _,
        ~parent: external_action => Ui_effect.t(unit),
        ~view_seg as _,
      ) => {
    let exo = ExoAdapters.exo_info(model.exo_kind, info.id);
    ExternalProjectorBridge.register(
      exo.codec,
      exo.target_origin,
      parent,
      info,
    );
    View.mk(iframe_view(info.id, exo.url, model));
  };
};
