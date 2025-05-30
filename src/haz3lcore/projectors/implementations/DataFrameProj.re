open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

let string_of = (any: Any.t): option(string) =>
  switch (any) {
  | Exp({term: Atom(String(s)), _}) =>
    Some(StringUtil.unescape_linebreaks(s))
  | _ => None
  };

let dataframe_of =
    (any: Any.t): option((list(LabeledTuple.label), list(list(Exp.t)))) =>
  switch (any) {
  | Exp({term: ListLit(es), _}) =>
    print_endline("Processing ListLit...");
    let data: list(option((list(string), list(TermBase.exp_t)))) =
      List.map(
        e => {
          print_endline("Mapping over list elements..." ++ Exp.show(e));
          switch (Unboxing.unbox(LabeledTupleEntries, e)) {
          // TODO Stop doing this with unboxing and deconstruct it here with the parens
          | IndetMatch =>
            print_endline("Unboxing result: IndetMatch");
            None;
          | DoesNotMatch =>
            print_endline("Unboxing result: DoesNotMatch");
            None;
          | Matches(entries: list((option(string), TermBase.exp_t))) =>
            print_endline("Unboxing result: Matches");
            let f: option(list((string, TermBase.exp_t))) =
              OptUtil.sequence(
                List.map(
                  ((label, value)) =>
                    switch (label) {
                    | Some(l) =>
                      print_endline("Found label: " ++ l);
                      Some((l, value));
                    | None =>
                      print_endline("Label is None");
                      None;
                    },
                  entries,
                ),
              );

            let g: option((list(string), list(TermBase.exp_t))) =
              f |> Option.map(List.split);

            g;
          };
        },
        es,
      );

    print_endline("Finished mapping over list elements.");
    let data: option(list((list(string), list(TermBase.exp_t)))) =
      OptUtil.sequence(data);
    switch (data) {
    | Some(data: list((list(string), list(TermBase.exp_t)))) =>
      print_endline("Data successfully sequenced.");
      let (headers: list(list(string)), rows: list(list(TermBase.exp_t))) =
        List.split(data);

      print_endline("Split headers and rows.");
      // If all the headers aren't the same return None
      switch (headers) {
      | [] =>
        print_endline("Headers are empty.");
        None;
      | [h, ..._] when List.for_all(x => x == h, headers) =>
        print_endline("Headers are consistent.");
        let headers = h;
        Some((headers, rows));

      | _ =>
        print_endline("Headers are inconsistent.");
        None;
      };
    | None =>
      print_endline("Data sequencing failed.");
      None;
    };
  | _ =>
    print_endline("Input is not a ListLit.");
    None;
  };

let get = (info: info): (list(LabeledTuple.label), list(list(Exp.t))) =>
  switch (info.syntax |> info.utility.seg_to_term) {
  | Some(s) =>
    switch (dataframe_of(s)) {
    | Some(s) => s
    | None => failwith("TextArea: get: Not string literal")
    }
  | None => failwith("TextArea: get: Not string literal")
  };

let key_handler = (id, ~parent, evt) => {
  open Effect;
  let key = Key.mk(KeyDown, evt);

  switch (key.key) {
  | D("ArrowRight" | "ArrowDown") when Web.TextArea.is_last_pos(Id.cls(id)) =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Many([parent(Escape(Right)), Stop_propagation]);
  | D("ArrowLeft" | "ArrowUp") when Web.TextArea.is_first_pos(Id.cls(id)) =>
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

let rec of_segment = (~holes: option(string), seg: Segment.t): string =>
  seg |> List.map(of_piece(~holes)) |> String.concat("")
and of_piece = (~holes, p: Piece.t): string =>
  switch (p) {
  | Tile(t) => of_tile(~holes, t)
  | Grout({shape: Concave, _}) => " "
  | Grout({shape: Convex, _}) when holes != None => Option.get(holes)
  | Grout({shape: Convex, _}) => " "
  | Secondary(w) =>
    Secondary.is_linebreak(w) ? "\n" : Secondary.get_string(w.content)
  | Projector(p) => of_segment(~holes, Piece.unparenthesize(p.syntax))
  }
and of_tile = (~holes, t: Tile.t): string =>
  Aba.mk(t.shards, t.children)
  |> Aba.join(of_delim(t), of_segment(~holes))
  |> String.concat("")
and of_delim = (t: Piece.tile, i: int): string => List.nth(t.label, i);
let len_seg = (seg: Segment.t): int =>
  seg |> of_segment(~holes=Some("?")) |> String.length;

let seg_of_exp = (utility: utility, exp: Exp.t): (Segment.t, int) => {
  let seg = utility.term_to_seg(Exp(exp));
  (seg, len_seg(seg));
};

let abbreviated_seg_of =
    (utility: utility, available: int, exp: Exp.t): (Segment.t, int) => {
  let (abbr_exp, _length) =
    exp |> DHExp.strip_casts |> Abbreviate.abbreviate_exp(~available);
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
let value_view = (info: info, utility: utility, view_seg, exp) => {
  let (seg, length) = abbreviated_seg_of(utility, 30, exp);

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
      info,
      ~parent: external_action => Ui_effect.t(unit),
      (headers, rows): (list(LabeledTuple.label), list(list(Exp.t))),
      ~view_seg: (Sort.t, Segment.t) => Node.t,
    ) =>
  Node.table(
    ~attrs=[Attr.classes(["dataframe"])],
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

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (any: Term.Any.t) =>
    switch (dataframe_of(any)) {
    | Some(_) => Some()
    | None => None
    };

  let focus_keyboard = (id: Id.t, d: Direction.t) => {
    JsUtil.get_elem_by_id(Id.cls(id))##focus;
    switch (d) {
    | Left => Web.TextArea.set_caret_to_start(Web.TextArea.get(Id.cls(id)))
    | Right => Web.TextArea.set_caret_to_end(Web.TextArea.get(Id.cls(id)))
    };
  };

  let focus_pointer = (id: Id.t) => {
    JsUtil.get_elem_by_id(Id.cls(id))##focus;
  };

  let focusable =
    Focusable.{
      pointer: Some(focus_pointer),
      keyboard: Some(focus_keyboard),
    };
  let dynamics = false;
  let placeholder = (_, info) => {
    let data: (list(string), list(list(TermBase.exp_t))) = info |> get;
    let num_rows = List.length(data |> snd);
    let num_cols = List.length(data |> fst);
    ProjectorCore.Shape.{
      vertical: Block(num_rows * 2 + 1), // +1 for header row
      /* +2 for left and right padding */
      horizontal: 2 + num_cols * 7,
    };
  };
  let update = (model, _, _) => model;

  let view = (_, info, ~local as _, ~parent, ~view_seg: View.seg) =>
    View.mk(
      Node.div(
        ~attrs=[Attr.classes(["wrapper"])],
        [
          Node.div(
            ~attrs=[Attr.classes(["cols", "code"])],
            [table(info, ~view_seg, ~parent, info |> get)],
          ),
        ],
      ),
    );
};
