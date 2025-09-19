open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

let max_column_length = 12;

let table_from_exp = (exp: Exp.t) => {
  switch (exp.term) {
  | ListLit(es) =>
    let data: list(option((list(string), list(TermBase.exp_t)))) =
      List.map(
        e => {
          switch (Unboxing.unbox(LabeledTupleEntries, e)) {
          // TODO Stop doing this with unboxing and deconstruct it here with the parens
          | IndetMatch => None
          | DoesNotMatch => None
          | Matches(entries: list((option(string), TermBase.exp_t))) =>
            let f: option(list((string, TermBase.exp_t))) =
              OptUtil.sequence(
                List.map(
                  ((label, value)) =>
                    switch (label) {
                    | Some(l) => Some((l, value))
                    | None => None
                    },
                  entries,
                ),
              );

            let g: option((list(string), list(TermBase.exp_t))) =
              f |> Option.map(List.split);

            g;
          }
        },
        es,
      );

    let data: option(list((list(string), list(TermBase.exp_t)))) =
      OptUtil.sequence(data);
    switch (data) {
    | Some(data: list((list(string), list(TermBase.exp_t)))) =>
      let (headers: list(list(string)), rows: list(list(TermBase.exp_t))) =
        List.split(data);

      // If all the headers aren't the same return None
      switch (headers) {
      | [] => None
      | [h, ..._] when List.for_all(x => x == h, headers) =>
        let headers = h;
        Some((headers, rows));

      | _ => None
      };
    | _ => None
    };
  | _ => None
  };
};

let table_of =
    (any: Any.t): option((list(LabeledTuple.label), list(list(Exp.t)))) =>
  switch (any) {
  | Exp(exp) => table_from_exp(exp)
  | _ => None
  };

let get = (info: info): (list(LabeledTuple.label), list(list(Exp.t))) =>
  switch (info.syntax |> info.utility.seg_to_term) {
  | Some(s) =>
    switch (table_of(s)) {
    | Some(s) => s
    | None => failwith("TextArea: get: Not a table")
    }
  | None => failwith("TextArea: get: Not a table")
  };

let key_handler = (id, ~parent, evt) => {
  open Effect;
  let key = Key.mk(KeyDown, evt);

  switch (key.key) {
  | D("ArrowRight" | "ArrowDown")
      when WebUtil.TextArea.is_last_pos(Id.cls(id)) =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Many([parent(Escape(Right)), Stop_propagation]);
  | D("ArrowLeft" | "ArrowUp")
      when WebUtil.TextArea.is_first_pos(Id.cls(id)) =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Many([parent(Escape(Left)), Stop_propagation]);
  /* Defer to parent editor undo for now */
  | D("z" | "Z" | "y" | "Y") when Key.ctrl_held(evt) || Key.meta_held(evt) =>
    Many([Prevent_default])
  | D("z" | "Z")
      when Key.shift_held(evt) && (Key.ctrl_held(evt) || Key.meta_held(evt)) =>
    Many([Prevent_default])
  | D("\"") =>
    /* Hide quotes from both the textarea and parent editor */
    Many([Prevent_default, Stop_propagation])
  | _ => Stop_propagation
  };
};

let len_seg = (utility: utility, seg: Segment.t): int =>
  seg |> utility.seg_to_string |> String.length;

let seg_of_exp = (utility: utility, exp: Exp.t): (Segment.t, int) => {
  let seg = utility.term_to_seg(Exp(exp));
  (seg, len_seg(utility, seg));
};

let abbreviated_seg_of =
    (utility: utility, available: int, exp: Exp.t): (Segment.t, int) => {
  let (abbr_exp, _length) =
    exp |> DHExp.strip_ascriptions |> Abbreviate.abbreviate_exp(~available);
  seg_of_exp(utility, abbr_exp);
};
let length_cls = (length: int): string =>
  if (length > 10) {
    "extra";
  } else if (length > 9) {
    "s6";
  } else if (length > 8) {
    "s5";
  } else if (length > 7) {
    "s4";
  } else if (length > 6) {
    "s3";
  } else if (length > 5) {
    "s2";
  } else if (length > 4) {
    "s1";
  } else {
    "s0";
  };
let value_view = (_info: info, utility: utility, view_seg, exp) => {
  let (seg, length) = abbreviated_seg_of(utility, max_column_length, exp);

  Node.div(
    ~attrs=[
      //Attr.title(DynCursor.Debug.str(info, closure)),
      Attr.classes([
        "value",
        length_cls(length),
        // @ DynCursor.clss(info, closure)
        // @ (Option.is_some(cur_ap(info)) ? ["ap"] : [])
        // @ (!is_value(closure.value) ? ["indet"] : []),
      ]),
      // Attr.on_double_click(_ => local(ToggleShowAllVals(index))),
      // Attr.on_pointerdown(val_pointerdown),
      // Attr.on_pointerup(val_pointerup),
      // Attr.on_mousemove(val_mousemove),
    ],
    [view_seg(Sort.Exp, seg)],
  );
};

let table =
    (
      info: info,
      ~parent as _: external_action => Ui_effect.t(unit),
      (headers, rows): (list(LabeledTuple.label), list(list(Exp.t))),
      ~view_seg: (Sort.t, Segment.t) => Node.t,
    ) => {
  Node.table(
    ~attrs=[Attr.classes(["table"])],
    [
      Node.thead([
        Node.tr(List.map(h => Node.th([Node.text(h)]), headers)),
      ]),
      Node.tbody(
        List.map(
          row =>
            Node.tr(
              List.map(
                e => Node.td([value_view(info, info.utility, view_seg, e)]),
                row,
              ),
            ),
          rows,
        ),
      ),
    ],
  );
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = option(int);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | Next
    | Previous;

  let init = (any: Term.Any.t) => Some(None);

  let focusable =
    Focusable.{
      pointer: None,
      keyboard: None,
    };
  let dynamics = true;
  let placeholder = (_, info) => {
    ProjectorCore.Shape.{
      vertical: Block(11), // +1 for header row
      /* +2 for left and right padding */
      horizontal: 50 // +2 for left and right padding
    };
  };
  let update = (model, info, action) => {
    let dynamics = info.dynamics |> Option.value(~default=[]);
    let length = List.length(dynamics);
    if (length == 0) {
      model;
    } else {
      let current = Option.value(model, ~default=0);
      switch (action) {
      | Next => Some((current + 1) mod length)
      | Previous => Some((current + length - 1) mod length)
      };
    };
  };

  let view = (model, info, ~local, ~parent, ~view_seg: View.seg) => {
    let dynamics: list(Dynamics.Probe.Closure.t) =
      info.dynamics |> Option.value(~default=[]);

    let v =
      if (List.length(dynamics) == 0) {
        Node.div([Node.text("Loading dynamics...")]);
      } else {
        let length = List.length(dynamics);
        let observed = Option.value(model, ~default=0) mod length;
        let closure = List.nth(dynamics, observed);
        let table_node =
          switch (table_from_exp(closure.value)) {
          | Some((hd, tl)) => table(info, ~view_seg, ~parent, (hd, tl))
          | _ => Node.div([Node.text("No table data")])
          };

        let buttons =
          if (length <= 1) {
            Node.div([]);
          } else {
            Node.div(
              ~attrs=[Attr.classes(["closure-buttons"])],
              [
                Node.button(
                  ~attrs=[
                    Attr.on_click(_ => local(Previous)),
                    Attr.classes(["btn", "btn-secondary"]),
                  ],
                  [Node.text("Previous")],
                ),
                Node.button(
                  ~attrs=[
                    Attr.on_click(_ => local(Next)),
                    Attr.classes(["btn", "btn-secondary"]),
                  ],
                  [Node.text("Next")],
                ),
              ],
            );
          };

        Node.div([buttons, table_node]);
      };

    View.mk(v);
  };
};
