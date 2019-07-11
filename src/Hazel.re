module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
open Incr_dom;
open ViewUtil;

// https://github.com/janestreet/incr_dom/blob/6aa4aca2cfc82a17bbcc0424ff6b0ae3d6d8d540/example/text_input/README.md
// https://github.com/janestreet/incr_dom/blob/master/src/app_intf.ml

module Model = Model;
module Action = Update.Action;
module State = {
  type setting_caret = ref(bool);
  type t = setting_caret;
};

[@warning "-27"]
let on_startup = (~schedule_action, _) => {
  let _ =
    JSUtil.listen_to_t(
      Dom.Event.make("selectionchange"), Dom_html.document, _ =>
      schedule_action(Update.Action.SelectionChange)
    );
  Dom_html.window##.onfocus :=
    Dom_html.handler(_ => {
      schedule_action(Update.Action.FocusWindow);
      Js._true;
    });
  schedule_action(Update.Action.FocusCell);
  Async_kernel.Deferred.return(ref(false));
};

[@warning "-27"]
let create =
    (
      model: Incr.t(Model.t),
      ~old_model: Incr.t(Model.t),
      ~inject: Update.Action.t => Vdom.Event.t,
    ) => {
  open Incr.Let_syntax;
  let%map model = model;
  Component.create(
    ~apply_action=Update.apply_action(model),
    ~on_display=
      (setting_caret: State.t, ~schedule_action: Update.Action.t => unit) => {
        let path = model |> Model.path;
        if (model.is_cell_focused) {
          if (!Code.is_caret_consistent_with_path(path)) {
            switch (Code.caret_position_of_path(path)) {
            | None => assert(false)
            | Some((node, offset)) =>
              setting_caret := true;
              JSUtil.set_caret(node, offset);
            };
          } else {
            setting_caret := false;
            let cursor_elem = JSUtil.force_get_elem_by_cls("cursor");
            // cursor_elem is either SBox or SSeq
            if (cursor_elem |> Code.elem_is_SBox) {
              Cell.place_box_node_indicator_over_snode_elem(
                ~child_indices=
                  model.cursor_info |> CursorInfo.child_indices_of_current_node,
                cursor_elem,
              );
              Cell.place_box_term_indicator(cursor_elem);
              // draw node staging guides
              let (steps, _) = model |> Model.path;
              switch (model.cursor_info.position) {
              | OnText(_)
              | OnDelim(_, _) => ()
              | Staging(delim_index) =>
                let delim_elem = Code.get_sdelim_elem((steps, delim_index));

                // always draw currently occupied delimiter shift targets
                delim_elem |> Cell.draw_current_horizontal_shift_target;
                delim_elem
                |> Code.parent_sline_elem_of_sdelim_elem
                |> Cell.draw_current_vertical_shift_target;

                // There is always a parent of the current cursor node.
                // Even if the current cursor node forms the entire
                // visible expression, there is the block containing it.
                let (parent_steps, current_child_index) =
                  steps |> split_last |> Opt.get(() => assert(false));
                let parent_elem = Code.force_get_snode_elem(parent_steps);

                switch (
                  model.cursor_info
                  |> CursorInfo.delim_neighborhood
                  |> Opt.get(() => assert(false))
                ) {
                | LetDefInBody(Block(def_lines, _) as def, body) =>
                  // cursor_elem is a let line, parent_elem is a block
                  if (cursor_elem |> Code.elem_is_multi_line) {
                    // only draw guides for defining expression if
                    // multi-line i.e. on separate lines from `in`
                    let def_elem =
                      JSUtil.force_get_elem_by_id(node_id(steps @ [2]));
                    def_elem
                    |> sline_elems_of_snode_elem
                    |> filteri((i, _) => i < List.length(def_lines))
                    |> List.iter(Cell.draw_vertical_shift_target);
                  };
                  parent_elem
                  |> sline_elems_of_snode_elem
                  |> filteri((i, _) => i > current_child_index)
                  |> List.iter(Cell.draw_vertical_shift_target);
                | BetweenChildren(_, _) =>
                  // Only lines can be transferred between two complete terms.
                  // We don't currently have any delimiters in this position
                  // such that it can move, so we don't bother drawing the
                  // neighborhood shift targets, but you could imagine being able
                  // to shift the `else` between the two branches of an if-else.
                  ()
                | LeftBorderInSeq(surround, child_seq) =>
                  // draw shift targets on terms in surrounding prefix
                  current_child_index
                  |> range
                  |> List.iter(i =>
                       Code.force_get_snode_elem(parent_steps @ [i])
                       |> Cell.draw_horizontal_shift_target(~side=Before)
                     );
                  // draw shift targets on terms in prefix of child_seq
                  switch (child_seq) {
                  | (None, _) => ()
                  | (Some(prefix), tm) =>
                    prefix
                    |> OperatorSeq.prefix_length
                    |> range
                    |> List.iter(i =>
                         Code.force_get_snode_elem(steps @ [i])
                         |> Code.draw_horizontal_shift_target(~side=Before)
                       )
                  };
                | RightBorderInSeq(child_seq, surround) =>
                  // draw shift targets on terms in surrounding suffix
                  surround
                  |> OperatorSeq.surround_suffix_length
                  |> range
                  |> List.iter(i =>
                       Code.force_get_snode_elem(
                         steps
                         @ [(surround |> OperatorSeq.prefix_length) + 1 + i],
                       )
                       |> Cell.draw_horizontal_shift_target(~side=Before)
                     );
                  // draw shift targets on terms in suffix of child_seq
                  switch (child_seq) {
                  | (_, None) => ()
                  | (_, Some(prefix)) =>
                    prefix
                    |> OperatorSeq.prefix_length
                    |> range
                    |> List.iter(i =>
                         Code.force_get_snode_elem(steps @ [i])
                         |> Code.draw_horizontal_shift_target(~side=Before)
                       )
                  };
                | LeftBorderInBlock(
                    _,
                    Expression(B(Block([], OpSeq(_, child_seq)))),
                  ) =>
                  // draw shift targets on preceding lines in surrounding block
                  parent_elem
                  |> sline_elems_of_snode_elem
                  |> filteri((i, _) => i < current_child_index)
                  |> List.iter(Cell.draw_vertical_shift_target);
                  // draw shift targets on terms in prefix of child_seq
                  child_seq
                  |> OperatorSeq.seq_length
                  |> range
                  |> filter(i => i > 0)
                  |> List.iter(i =>
                       Code.force_get_snode_elem(steps @ [i])
                       |> Code.draw_horizontal_shift_target(~side=Before)
                     );
                | RightBorderInBlock(
                    Expression(B(Block([], OpSeq(_, child_seq)))),
                    lines_after,
                  ) =>
                  // draw shift targets on following lines in surrounding block
                  parent_elem
                  |> sline_elems_of_snode_elem
                  |> filteri((i, _) => i > current_child_index)
                  |> List.iter(Cell.draw_vertical_shift_target);
                  // draw shift targets on terms in suffix of child_seq
                  let n = child_seq |> OperatorSeq.seq_length;
                  n
                  |> range
                  |> filter(i => i < n - 1)
                  |> List.iter(i =>
                       Code.force_get_snode_elem(steps @ [i])
                       |> Code.draw_horizontal_shift_target(~side=After)
                     );
                | LeftBorderInBlock(lines_before, child) =>
                  // draw shift targets on preceding lines in surround block
                  parent_elem
                  |> sline_elems_of_snode_elem
                  |> filteri((i, _) => i < current_child_index)
                  |> List.iter(Cell.draw_vertical_shift_target)

                | RightBorderInBlock(child, lines_after) => ()
                };
              };
            } else {
              switch (model.cursor_info.position) {
              | Staging(_) => assert(false)
              | OnText(_) => assert(false)
              | OnDelim(k, _) =>
                let (steps, _) = model |> Model.path;
                let op_elem = JSUtil.force_get_elem_by_id(op_id(steps, k));
                op_elem |> Cell.place_op_node_indicator_over_op_elem;
                switch (op_elem |> JSUtil.get_attr("op-range")) {
                | None => assert(false)
                | Some(ssexp) =>
                  let (a, b) =
                    Code.seq_range_of_sexp(Sexplib.Sexp.of_string(ssexp));
                  if (cursor_elem |> Code.elem_is_multi_line) {
                    Cell.place_multi_line_seq_term_indicator(
                      steps,
                      (a, b),
                      cursor_elem,
                    );
                  } else {
                    let tm_a =
                      JSUtil.force_get_elem_by_id(node_id(steps @ [a]));
                    let tm_b =
                      JSUtil.force_get_elem_by_id(node_id(steps @ [b]));
                    Cell.place_box_term_indicator_over_single_line_seq(
                      tm_a,
                      tm_b,
                    );
                  };
                };
              };
            };
          };
        };
      },
    model,
    Page.view(~inject, model),
  );
};
