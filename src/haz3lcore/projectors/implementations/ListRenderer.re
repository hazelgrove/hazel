open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* ListRenderer - Visualize a list value as a numbered, vertical list with a
 * small toolbar of list-wide actions (currently: Reverse). Serves as the
 * second reference renderer alongside TableRenderer. */

[@deriving (show({with_path: false}), sexp, yojson)]
type v = list(Exp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type m = {selected: option(int)};
[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | Select(option(int));

[@deriving (show({with_path: false}), sexp, yojson)]
type model = m;
[@deriving (show({with_path: false}), sexp, yojson)]
type action = a;
[@deriving (show({with_path: false}), sexp, yojson)]
type value = v;

let parse = (_sort: Sort.t, exp: Exp.t): option(value) =>
  switch (exp.term) {
  | ListLit(es) => Some(es)
  | _ => None
  };

let init = (_: value): model => {selected: None};

/* One row for the header, one per list item. */
let placeholder = (value: value, _: m): ProjectorCore.Shape.t =>
  ProjectorCore.Shape.{
    vertical: Block(1 + List.length(value)),
    horizontal: 0,
  };

let update = (_model: model, action: action): model =>
  switch (action) {
  | Select(s) => {selected: s}
  };

/* Build a `xs |> reverse` segment from the projected syntax. */
let reverse_segment = (info: info): option(Base.segment) =>
  info.utility.lift_syntax(
    ~inline=false,
    fun
    | Exp(exp) =>
      IdTagged.FreshGrammar.(Exp(Exp.ap(Reverse, Exp.var("reverse"), exp)))
    | other => other,
    info.syntax,
  );

let item_view =
    (
      ~utility: utility,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~selected: bool,
      ~on_click,
      idx: int,
      exp: Exp.t,
    )
    : Node.t => {
  let (seg, _) = ProbeUtil.seg_of_exp(utility, exp);
  Node.div(
    ~attrs=[
      Attr.classes(["list-item"] @ (selected ? ["selected"] : [])),
      Attr.on_click(on_click),
    ],
    [
      Node.span(
        ~attrs=[Attr.classes(["list-index"])],
        [Node.text(string_of_int(idx))],
      ),
      Node.div(
        ~attrs=[Attr.classes(["list-value"])],
        [view_seg(Sort.Exp, seg)],
      ),
    ],
  );
};

let toolbar =
    (
      ~is_readonly: bool,
      ~info: info,
      ~parent: external_action => Ui_effect.t(unit),
    )
    : Node.t => {
  let reverse_button =
    Node.button(
      ~attrs=[
        Attr.classes(["list-action"]),
        Attr.title("Wrap with reverse"),
        Attr.on_click(_ =>
          switch (reverse_segment(info)) {
          | Some(seg) => parent(SetSyntax(seg))
          | None => Effect.Ignore
          }
        ),
      ],
      [Node.text("Reverse")],
    );
  Node.div(
    ~attrs=[Attr.classes(["list-toolbar"])],
    is_readonly ? [] : [reverse_button],
  );
};

let render =
    (
      ~info: info,
      ~exp as _: Exp.t,
      ~value: value,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~model: model,
      ~local: action => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      ~sort: Sort.t,
      _: unit,
    )
    : Node.t => {
  let is_readonly = sort != Sort.Exp;
  let items =
    List.mapi(
      (i, e) =>
        item_view(
          ~utility=info.utility,
          ~view_seg,
          ~selected=model.selected == Some(i),
          ~on_click=
            _ => local(Select(model.selected == Some(i) ? None : Some(i))),
          i,
          e,
        ),
      value,
    );
  let count =
    Node.span(
      ~attrs=[Attr.classes(["list-count"])],
      [
        Node.text(
          string_of_int(List.length(value))
          ++ (List.length(value) == 1 ? " item" : " items"),
        ),
      ],
    );
  Node.div(
    ~attrs=[Attr.classes(["list-renderer"])],
    [
      Node.div(
        ~attrs=[Attr.classes(["list-header"])],
        [count, toolbar(~is_readonly, ~info, ~parent)],
      ),
      Node.div(~attrs=[Attr.classes(["list-items"])], items),
    ],
  );
};

let icon_size = 20.;

let list_icon =
  Node.create_svg(
    "svg",
    ~attrs=
      Attr.[
        create("viewBox", "0 0 8 8"),
        create("width", Printf.sprintf("%fpx", icon_size)),
        create("height", Printf.sprintf("%fpx", icon_size)),
        create("preserveAspectRatio", "none"),
      ],
    [
      Node.create_svg(
        "circle",
        ~attrs=
          Attr.[
            create("cx", "1.5"),
            create("cy", "2"),
            create("r", "0.5"),
          ],
        [],
      ),
      Node.create_svg(
        "circle",
        ~attrs=
          Attr.[
            create("cx", "1.5"),
            create("cy", "4"),
            create("r", "0.5"),
          ],
        [],
      ),
      Node.create_svg(
        "circle",
        ~attrs=
          Attr.[
            create("cx", "1.5"),
            create("cy", "6"),
            create("r", "0.5"),
          ],
        [],
      ),
      Node.create_svg(
        "rect",
        ~attrs=
          Attr.[
            create("x", "3"),
            create("y", "1.7"),
            create("width", "4"),
            create("height", "0.6"),
          ],
        [],
      ),
      Node.create_svg(
        "rect",
        ~attrs=
          Attr.[
            create("x", "3"),
            create("y", "3.7"),
            create("width", "4"),
            create("height", "0.6"),
          ],
        [],
      ),
      Node.create_svg(
        "rect",
        ~attrs=
          Attr.[
            create("x", "3"),
            create("y", "5.7"),
            create("width", "4"),
            create("height", "0.6"),
          ],
        [],
      ),
    ],
  );

let badge =
  Node.span(
    ~attrs=[
      Attr.classes(["list-badge"]),
      Attr.title("Click to view as list"),
    ],
    [list_icon],
  );
