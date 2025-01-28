open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;
open OptUtil.Syntax;

type offside_info = {
  goal: Exp.t,
  assumptions: list(Exp.t),
};

let get_offside_info = (info: info): option(offside_info) => {
  let* dynamics = info.dynamics;
  let* first = ListUtil.hd_opt(dynamics);
  Some({goal: first.value, assumptions: first.assumptions});
};

let goal_view = (goal: Exp.t, ~utility, ~view_seg) =>
  div(
    ~attrs=[Attr.classes(["type-cell"])],
    [TermBase.Exp(goal) |> utility.term_to_seg |> view_seg(Sort.Exp)],
  );

let offside_view = (offside_info: offside_info, ~utility, ~view_seg) =>
  div(
    ~attrs=[Attr.classes(["offside"])],
    [text("prove: ")]
    @ [goal_view(offside_info.goal, ~utility, ~view_seg)]
    @ [text("assuming: ")]
    @ List.map(
        assumption => goal_view(assumption, ~utility, ~view_seg),
        offside_info.assumptions,
      ),
  );

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = ();

  let dynamics = true;

  let can_focus = false;

  let focus = _ => ();

  let can_project = (_, any: Term.Any.t) =>
    switch (any) {
    | Exp(_) => true
    | _ => false
    };

  let placeholder = (_, info: info) =>
    ProjectorCore.inline(2 + String.length(ProbeProj.syntax_str(info)));

  let update = (_, _, _) => ();

  let view = (_model, info, ~local as _, ~parent as _, ~view_seg as _): Node.t => {
    div(
      ~attrs=[Attr.classes(["main"])],
      [info |> ProbeProj.syntax_str |> text],
    );
  };

  let offside_view =
    Option.Some(
      (_, info, ~local as _, ~parent as _, ~view_seg) =>
        {
          let+ offside_info = get_offside_info(info);
          offside_view(offside_info, ~utility=info.utility, ~view_seg);
        }
        |> Option.value(~default=Node.div([])),
    );

  let overlay_view = Option.None;

  let underlay_view = Option.None;
};
