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

/* Slider-specific adapter for ExternalProjectorBridge */
module SliderAdapter = {
  let codec: ExternalProjectorBridge.codec = {
    syntax_to_string: (info: ProjectorBase.info) => (
      try(
        switch (info.utility.seg_to_term(info.syntax)) {
        | Some(Exp({term: Atom(Int(i)), _})) => Some(Bigint.to_string(i))
        | _ => None
        }
      ) {
      | _ => None
      }:
        option(string)
    ),
    json_to_segment: (info: ProjectorBase.info, value_str: string) => (
      try({
        let int_val = Bigint.of_string(value_str);
        info.utility.lift_syntax(
          fun
          | Exp(t) =>
            Exp({
              ...t,
              term: Atom(Int(int_val)),
            })
          | _ => failwith("not an int literal"),
          info.syntax,
        );
      }) {
      | _ => None
      }:
        option(Base.segment)
    ),
    codec_name: "int",
  };

  let target_origin = "http://localhost:5173";

  let build_url = (~min_val, ~max_val, ~step_val, ~id) =>
    Printf.sprintf(
      "%s/?min=%d&max=%d&step=%d&id=%s&parentOrigin=%s",
      target_origin,
      min_val,
      max_val,
      step_val,
      Id.to_string(id),
      "http://localhost:8000" /* Hazel dev server origin */
    );
};

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

  let focusable = Focusable.non;
  let dynamics = false;

  let placeholder = (model: exo_model, _info: info): ProjectorCore.Shape.t => {
    //TODO(andrew): route font metrics here
    let char_width = Util.font_metrics^.col_width;
    let char_height = Util.font_metrics^.row_height;
    print_endline(
      "char_width: "
      ++ string_of_float(char_width)
      ++ " char_height: "
      ++ string_of_float(char_height),
    );
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

  let iframe_view = (id: Id.t, model: exo_model) => {
    let url =
      switch (model.exo_kind) {
      | ProjectorCore.Kind.Slider =>
        SliderAdapter.build_url(~min_val=0, ~max_val=100, ~step_val=1, ~id)
      };
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
    let (codec, target_origin) =
      switch (model.exo_kind) {
      | ProjectorCore.Kind.Slider => (
          SliderAdapter.codec,
          SliderAdapter.target_origin,
        )
      };
    ExternalProjectorBridge.register_projector(
      codec,
      target_origin,
      parent,
      info,
    );
    View.mk(iframe_view(info.id, model));
  };
};
