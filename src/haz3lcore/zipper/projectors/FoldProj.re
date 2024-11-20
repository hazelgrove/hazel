open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  [@default "⋱"]
  text: string,
};

/* Proof of concept value exposure. This isn't getting set right
   after actions, only initially */
let get_first_val = (rs: option(list(TestMap.instance_report))) => {
  switch (rs) {
  | Some(rs) =>
    List.map(((d: DHExp.t, _)) => d.term |> TermBase.Exp.show_term, rs)
    |> String.concat(", ")
  | _ => "Nein"
  };
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;
  let init = {text: "⋱"};
  let can_project = _ => true;
  let can_focus = false;
  let placeholder = (m, _) =>
    Inline(m.text == "⋱" ? 2 : m.text |> String.length);
  let update = (m, _) => m;
  let view = (m: model, ~info, ~local as _, ~parent) =>
    div(
      ~attrs=[
        Attr.on_double_click(_ => parent(Remove)),
        Attr.title(get_first_val(info.dyn)),
      ],
      [text(m.text)],
    );
  let focus = _ => ();
};
