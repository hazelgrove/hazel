open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  [@default "I am statics"]
  text: string,
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;
  let init = {text: "I am statics"};
  let can_project = _ => true;
  let can_focus = false;
  let placeholder = (m, _) => Block({row: 100, col: 100});
  let update = (m, _) => m;
  let view = (m: model, ~info, ~local as _, ~parent) =>
    switch (info.ci) {
    | Some(ci) =>
      div(
        ~attrs=[Attr.on_double_click(_ => parent(Remove))],
        [text(sexp_of_info(info) |> Sexplib.Sexp.to_string)],
      )
    | None => failwith("StaticsProj: No info")
    };
  let focus = _ => ();
};
