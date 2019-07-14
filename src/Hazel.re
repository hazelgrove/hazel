module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
open Incr_dom;
open GeneralUtil;
open ViewUtil;
open SemanticsCommon;

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
          let (steps, cursor) = path;
          let cursor_elem = JSUtil.force_get_elem_by_cls("cursor");
          switch (cursor) {
          | OnText(_)
          | OnDelim(_, _) =>
            if (!Code.is_caret_consistent_with_path(path)) {
              switch (Code.caret_position_of_path(path)) {
              | None => assert(false)
              | Some((node, offset)) =>
                setting_caret := true;
                JSUtil.set_caret(node, offset);
              };
            } else {
              setting_caret := false;
              // cursor_elem is either SBox or SSeq
              if (cursor_elem |> Code.elem_is_SBox) {
                cursor_elem
                |> Cell.draw_box_node_indicator(
                     ~cursor,
                     ~child_indices=
                       model.cursor_info
                       |> CursorInfo.child_indices_of_current_node,
                   );
                cursor_elem |> Cell.draw_box_term_indicator;
              } else {
                // cursor_elem is SSeq
                switch (model.cursor_info.position) {
                | Staging(_) => assert(false)
                | OnText(_) => assert(false)
                | OnDelim(k, _) =>
                  let (steps, _) = model |> Model.path;
                  let op_elem = JSUtil.force_get_elem_by_id(op_id(steps, k));
                  op_elem |> Cell.draw_op_node_indicator;
                  switch (op_elem |> JSUtil.get_attr("op-range")) {
                  | None => assert(false)
                  | Some(ssexp) =>
                    let (a, b) =
                      Code.seq_range_of_sexp(Sexplib.Sexp.of_string(ssexp));
                    if (cursor_elem |> Code.elem_is_multi_line) {
                      Cell.draw_multi_line_seq_term_indicator(
                        steps,
                        (a, b),
                        cursor_elem,
                      );
                    } else {
                      let tm_a =
                        JSUtil.force_get_elem_by_id(node_id(steps @ [a]));
                      let tm_b =
                        JSUtil.force_get_elem_by_id(node_id(steps @ [b]));
                      Cell.draw_box_term_indicator_over_single_line_seq(
                        tm_a,
                        tm_b,
                      );
                    };
                  };
                };
              };
            }
          | Staging(delim_index) =>
            JSUtil.unset_caret();

            // only SBox elems can be in staging mode
            cursor_elem
            |> Cell.draw_box_node_indicator(
                 ~cursor,
                 ~child_indices=
                   model.cursor_info
                   |> CursorInfo.child_indices_of_current_node,
               );

            // There is always a parent of the current cursor node.
            // Even if the current cursor node forms the entire
            // visible expression, there is the block containing it.
            let (parent_steps, current_child_index) =
              steps |> split_last |> Opt.get(() => assert(false));
            let parent_elem = Code.force_get_snode_elem(parent_steps);

            Cell.start_drawing_shift_targets();

            let sdelim_elem =
              Code.force_get_sdelim_elem((steps, delim_index));
            sdelim_elem
            |> Cell.draw_current_shifting_delim_indicator(
                 ~cursor_info=model.cursor_info,
               );

            // draw shift targets in subject
            switch (delim_index, model.cursor_info.node) {
            | (
                0,
                Typ(List(OpSeq(_, _)) | Parenthesized(OpSeq(_, _))) |
                Pat(Inj(_, _, OpSeq(_, _)) | Parenthesized(OpSeq(_, _))) |
                Exp(
                  Inj(_, _, Block([], OpSeq(_, _))) |
                  Parenthesized(Block([], OpSeq(_, _))),
                ),
              ) =>
              let shift_target_relative_paths =
                switch (model.cursor_info.node) {
                | Typ(List(OpSeq(_, seq)) | Parenthesized(OpSeq(_, seq))) =>
                  seq
                  |> OperatorSeq.seq_length
                  |> range(~lo=1)
                  |> List.map(i => [0, i])
                | Pat(
                    Inj(_, _, OpSeq(_, seq)) | Parenthesized(OpSeq(_, seq)),
                  ) =>
                  seq
                  |> OperatorSeq.seq_length
                  |> range(~lo=1)
                  |> List.map(i => [0, i])
                | Exp(
                    Inj(_, _, Block([], OpSeq(_, seq))) |
                    Parenthesized(Block([], OpSeq(_, seq))),
                  ) =>
                  seq
                  |> OperatorSeq.seq_length
                  |> range(~lo=1)
                  |> List.map(i => [0, 0, i])
                | _ => assert(false)
                };
              shift_target_relative_paths
              |> List.iter(rel_path => {
                   let (_, last) =
                     rel_path |> split_last |> Opt.get(() => assert(false));
                   Code.force_get_snode_elem(steps @ rel_path)
                   |> Cell.draw_horizontal_shift_target_in_subject(
                        ~index=last,
                        ~side=Before,
                      );
                 });
            | (
                _one,
                Typ(List(OpSeq(_, _)) | Parenthesized(OpSeq(_, _))) |
                Pat(Inj(_, _, OpSeq(_, _)) | Parenthesized(OpSeq(_, _))) |
                Exp(
                  Inj(_, _, Block([], OpSeq(_, _))) |
                  Parenthesized(Block([], OpSeq(_, _))),
                ),
              ) =>
              let shift_target_relative_paths =
                switch (model.cursor_info.node) {
                | Typ(List(OpSeq(_, seq)) | Parenthesized(OpSeq(_, seq))) =>
                  (seq |> OperatorSeq.seq_length)
                  - 1
                  |> range
                  |> List.map(i => [0, i])
                | Pat(
                    Inj(_, _, OpSeq(_, seq)) | Parenthesized(OpSeq(_, seq)),
                  ) =>
                  (seq |> OperatorSeq.seq_length)
                  - 1
                  |> range
                  |> List.map(i => [0, i])
                | Exp(
                    Inj(_, _, Block([], OpSeq(_, seq))) |
                    Parenthesized(Block([], OpSeq(_, seq))),
                  ) =>
                  (seq |> OperatorSeq.seq_length)
                  - 1
                  |> range
                  |> List.map(i => [0, 0, i])
                | _ => assert(false)
                };
              shift_target_relative_paths
              |> List.iter(rel_path => {
                   let (_, last) =
                     rel_path |> split_last |> Opt.get(() => assert(false));
                   Code.force_get_snode_elem(steps @ rel_path)
                   |> Cell.draw_horizontal_shift_target_in_subject(
                        ~index=last,
                        ~side=After,
                      );
                 });
            | (_, _) => ()
            };

            // draw shift targets in seq frame
            switch (
              delim_index,
              model.cursor_info.node,
              model.cursor_info.frame,
            ) {
            | (_, _, TypFrame(None))
            | (_, _, PatFrame(None))
            | (_, _, ExpFrame(_, None, _)) => ()
            | (0, Typ(List(_) | Parenthesized(_)), TypFrame(Some(_)))
            | (0, Pat(Inj(_, _, _) | Parenthesized(_)), PatFrame(Some(_)))
            | (
                0,
                Exp(
                  Inj(_, _, Block([], _)) | Parenthesized(Block([], _)),
                ),
                ExpFrame(_, Some(_), _),
              ) =>
              let shift_target_indices =
                switch (model.cursor_info.frame) {
                | TypFrame(Some(surround)) =>
                  surround
                  |> OperatorSeq.prefix_tms_of_surround
                  |> List.length
                  |> range
                | PatFrame(Some(surround)) =>
                  surround
                  |> OperatorSeq.prefix_tms_of_surround
                  |> List.length
                  |> range
                | ExpFrame(_, Some(surround), _) =>
                  surround
                  |> OperatorSeq.prefix_tms_of_surround
                  |> List.length
                  |> range
                | _ => assert(false)
                };
              shift_target_indices
              |> List.iter(i =>
                   Code.force_get_snode_elem(parent_steps @ [i])
                   |> Cell.draw_horizontal_shift_target_in_frame(
                        ~index=i,
                        ~side=Before,
                      )
                 );
            | (1, Typ(List(_) | Parenthesized(_)), TypFrame(Some(_)))
            | (1, Pat(Inj(_, _, _) | Parenthesized(_)), PatFrame(Some(_)))
            | (
                1,
                Exp(
                  Inj(_, _, Block([], _)) | Parenthesized(Block([], _)),
                ),
                ExpFrame(_, Some(_), _),
              ) =>
              let shift_target_indices =
                switch (model.cursor_info.frame) {
                | TypFrame(Some(surround)) =>
                  surround
                  |> OperatorSeq.suffix_tms_of_surround
                  |> List.mapi((i, _) => current_child_index + 1 + i)
                | PatFrame(Some(surround)) =>
                  surround
                  |> OperatorSeq.suffix_tms_of_surround
                  |> List.mapi((i, _) => current_child_index + 1 + i)
                | ExpFrame(_, Some(surround), _) =>
                  surround
                  |> OperatorSeq.suffix_tms_of_surround
                  |> List.mapi((i, _) => current_child_index + 1 + i)
                | _ => assert(false)
                };
              shift_target_indices
              |> List.iter(i =>
                   Code.force_get_snode_elem(parent_steps @ [i])
                   |> Cell.draw_horizontal_shift_target_in_frame(
                        ~index=i,
                        ~side=After,
                      )
                 );
            | (_, _, _) => ()
            };

            // draw shift targets in block frame
            switch (model.cursor_info.node, model.cursor_info.frame) {
            | (_, TypFrame(_))
            | (_, PatFrame(_)) => ()
            | (Line(LetLine(_, _, _)), ExpFrame(prefix, _, suffix)) =>
              Cell.elem()
              |> Code.sline_elems_of_snode_elem(parent_elem)
              |> List.iteri((i, sline) =>
                   if (delim_index == 3 && i > (prefix |> List.length)) {
                     sline
                     |> Cell.draw_vertical_shift_target_in_frame(
                          ~index=i,
                          ~side=After,
                        );
                   }
                 )
            | (
                Exp(Inj(_, _, Block([], _)) | Parenthesized(Block([], _))),
                ExpFrame(prefix, surround, suffix),
              ) =>
              let (block_elem, block_steps) =
                switch (surround, parent_steps |> split_last) {
                | (None, _)
                | (_, None) => (parent_elem, parent_steps)
                | (Some(_), Some((grandparent_steps, _))) => (
                    parent_elem |> JSUtil.force_get_parent_elem,
                    grandparent_steps,
                  )
                };
              let side =
                switch (delim_index) {
                | 0 => Before
                | _one => After
                };
              Cell.elem()
              |> Code.sline_elems_of_snode_elem(block_elem)
              |> List.iteri((i, sline) =>
                   if (delim_index == 0 && i < (prefix |> List.length)) {
                     sline
                     |> Cell.draw_vertical_shift_target_in_frame(
                          ~index=i,
                          ~side,
                        );
                   } else if (delim_index == 1 && i > (prefix |> List.length)) {
                     sline
                     |> Cell.draw_vertical_shift_target_in_frame(
                          ~index=i,
                          ~side,
                        );
                   }
                 );
            | (_, _) => ()
            };

            Cell.draw_horizontal_shift_rail(sdelim_elem);
            Cell.draw_vertical_shift_rail();
          };
        };
      },
    model,
    Page.view(~inject, model),
  );
};
