open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

module M = (ExoP: Exo.Info) : Projector => {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = Exo.model;

  let model_of_sexp = (sexp: Sexplib.Sexp.t): model =>
    switch (model_of_sexp(sexp)) {
    | exception _ => {
        width: 100,
        height: 100,
      }
    | m => m
    };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = Exo.action;

  let init = (any: Language.Any.t) => ExoP.init(any);

  let focusable = Focusable.non;
  let dynamics = false;

  let placeholder = (model: model, _): ProjectorCore.Shape.t => {
    let px_to_grid = (value: int, multiple: float): int =>
      int_of_float(ceil(float_of_int(value) /. multiple));
    let m = Util.font_metrics^;
    {
      horizontal: px_to_grid(model.width, m.col_width) + 1,
      vertical: Block(px_to_grid(model.height, m.row_height) - 1),
    };
  };

  let update = (_model: model, _info, action: action): model =>
    switch (action) {
    | Resize(w, h) => {
        //...model,
        width: w,
        height: h,
      }
    };

  let iframe_view = (id: Id.t, url: string, model: model) => {
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
        // Attr.create(
        //   "data-exo-type",
        //   ProjectorCore.Kind.exo_name(ExoP.exo_kind),
        // ),
      ],
      [],
    );
  };

  let view =
      (
        model: model,
        info: info,
        ~local: action => Ui_effect.t(unit),
        ~parent: external_action => Ui_effect.t(unit),
        ~view_seg as _,
      ) => {
    let entry = Exo.mk_entry(parent, local, info, (module ExoP));
    ExternalProjectorBridge.register(entry);
    View.mk(iframe_view(info.id, entry.url, model));
  };
};
