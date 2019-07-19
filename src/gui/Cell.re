module Vdom = Virtual_dom.Vdom;
module Dom_html = Js_of_ocaml.Dom_html;
module Js = Js_of_ocaml.Js;
module Sexp = Sexplib.Sexp;
module KeyCombo = JSUtil.KeyCombo;
open GeneralUtil;
open ViewUtil;
open SemanticsCommon;

let string_insert = (s1, offset, s2) => {
  let prefix = String.sub(s1, 0, offset);
  let length = String.length(s1);
  let suffix = String.sub(s1, offset, length - offset);
  prefix ++ s2 ++ suffix;
};

let string_backspace = (s, offset, ctrlKey) => {
  let prefix = ctrlKey ? "" : String.sub(s, 0, offset - 1);
  let length = String.length(s);
  let suffix = String.sub(s, offset, length - offset);
  let offset' = ctrlKey ? 0 : offset - 1;
  (prefix ++ suffix, offset');
};

let string_delete = (s, offset, ctrlKey) => {
  let prefix = String.sub(s, 0, offset);
  let length = String.length(s);
  let suffix = ctrlKey ? "" : String.sub(s, offset + 1, length - offset - 1);
  (prefix ++ suffix, offset);
};

let kc_actions: Hashtbl.t(KeyCombo.t, Action.t) =
  Hashtbl.of_seq(
    [
      (KeyCombo.Backspace, Action.Backspace),
      (KeyCombo.Delete, Action.Delete),
      (KeyCombo.ShiftTab, Action.MoveToPrevHole),
      (KeyCombo.Tab, Action.MoveToNextHole),
      (KeyCombo.Key_N, Action.Construct(SNum)),
      (KeyCombo.Key_B, Action.Construct(SBool)),
      (KeyCombo.GT, Action.Construct(SOp(SArrow))),
      (KeyCombo.VBar, Action.Construct(SOp(SVBar))),
      (KeyCombo.Key_L, Action.Construct(SList)),
      (KeyCombo.LeftParen, Action.Construct(SParenthesized)),
      (KeyCombo.Colon, Action.Construct(SAsc)),
      (KeyCombo.Equals, Action.Construct(SLet)),
      (KeyCombo.Enter, Action.Construct(SLine)),
      (KeyCombo.Backslash, Action.Construct(SLam)),
      (KeyCombo.Plus, Action.Construct(SOp(SPlus))),
      (KeyCombo.Asterisk, Action.Construct(SOp(STimes))),
      (KeyCombo.LT, Action.Construct(SOp(SLessThan))),
      (KeyCombo.Space, Action.Construct(SOp(SSpace))),
      (KeyCombo.Comma, Action.Construct(SOp(SComma))),
      (KeyCombo.LeftBracket, Action.Construct(SListNil)),
      (KeyCombo.Semicolon, Action.Construct(SOp(SCons))),
      (KeyCombo.Alt_L, Action.Construct(SInj(L))),
      (KeyCombo.Alt_R, Action.Construct(SInj(R))),
      (KeyCombo.Alt_C, Action.Construct(SCase)),
    ]
    |> List.to_seq,
  );

let multi_line_seq_indicators = (is_active, n) =>
  Vdom.[
    Node.div(
      [
        Attr.id(op_node_indicator_id),
        Attr.classes([
          "node-indicator",
          "normal",
          is_active ? "active" : "inactive",
        ]),
      ],
      [],
    ),
    ...range(n)
       |> List.map(i =>
            Node.div(
              [
                Attr.id(seq_tm_indicator_id(i)),
                Attr.classes(
                  ["term-indicator", is_active ? "active" : "inactive"]
                  @ (i == 0 ? ["term-indicator-first"] : [])
                  @ (i == n - 1 ? ["term-indicator-last"] : []),
                ),
              ],
              [],
            )
          ),
  ];

let single_line_seq_indicators = is_active => {
  Vdom.[
    Node.div(
      [
        Attr.id(op_node_indicator_id),
        Attr.classes([
          "node-indicator",
          "normal",
          is_active ? "active" : "inactive",
        ]),
      ],
      [],
    ),
    Node.div(
      [
        Attr.id(box_tm_indicator_id),
        Attr.classes([
          "term-indicator",
          "term-indicator-first",
          "term-indicator-last",
          is_active ? "active" : "inactive",
        ]),
      ],
      [],
    ),
  ];
};

let horizontal_shift_targets_in_subject = (model: Model.t) => {
  switch (model.cursor_info.position, model.cursor_info.node) {
  | (OnText(_) | OnDelim(_, _), _) => []
  | (Staging(k), Typ(List(OpSeq(_, _)) | Parenthesized(OpSeq(_, _))))
  | (
      Staging(k),
      Pat(Inj(_, _, OpSeq(_, _)) | Parenthesized(OpSeq(_, _))),
    )
  | (
      Staging(k),
      Exp(
        Inj(_, _, Block([], OpSeq(_, _))) |
        Parenthesized(Block([], OpSeq(_, _))),
      ),
    ) =>
    let steps_of_subject_seq =
      switch (model.cursor_info.node) {
      | Line(_)
      | Rule(_) => assert(false)
      | Typ(_)
      | Pat(_) => (model |> Model.steps) @ [0]
      | Exp(_) => (model |> Model.steps) @ [0, 0]
      };
    let subject_indices =
      switch (model.cursor_info.node) {
      | Typ(List(OpSeq(_, seq)) | Parenthesized(OpSeq(_, seq))) =>
        k == 0
          ? range(~lo=1, seq |> OperatorSeq.seq_length)
          : range((seq |> OperatorSeq.seq_length) - 1)
      | Pat(Inj(_, _, OpSeq(_, seq)) | Parenthesized(OpSeq(_, seq))) =>
        k == 0
          ? range(~lo=1, seq |> OperatorSeq.seq_length)
          : range((seq |> OperatorSeq.seq_length) - 1)
      | Exp(
          Inj(_, _, Block([], OpSeq(_, seq))) |
          Parenthesized(Block([], OpSeq(_, seq))),
        ) =>
        k == 0
          ? range(~lo=1, seq |> OperatorSeq.seq_length)
          : range((seq |> OperatorSeq.seq_length) - 1)
      | _ => assert(false)
      };
    subject_indices
    |> List.map(i => steps_of_subject_seq @ [i])
    |> List.map(target_steps =>
         Vdom.(
           Node.div(
             [
               Attr.classes(["horizontal-shift-target-in-subject"]),
               Attr.create(
                 "target-steps",
                 Sexp.to_string(Path.sexp_of_steps(target_steps)),
               ),
               Attr.create(
                 "target-side",
                 Sexp.to_string(sexp_of_side(k == 0 ? Before : After)),
               ),
             ],
             [],
           )
         )
       );
  | (_, _) => []
  };
};

let rec subject_block_shifted_to_prefix_chunk_sizes =
        (~u_gen: MetaVarGen.t, ~chunk_sizes=[], prefix, subject_block)
        : list(int) =>
  switch (subject_block |> UHExp.shift_line_to_prefix(~u_gen, prefix)) {
  | None => chunk_sizes |> List.rev
  | Some((new_prefix, new_subject_block, u_gen)) =>
    let num_shifted_lines =
      switch (subject_block, new_subject_block) {
      | (Block(_, EmptyHole(_)), Block(_, EmptyHole(_))) =>
        UHExp.num_lines_in_block(subject_block)
        - UHExp.num_lines_in_block(new_subject_block)
      | (Block(_, _), Block(_, EmptyHole(_))) =>
        UHExp.num_lines_in_block(subject_block)
        - UHExp.num_lines_in_block(new_subject_block)
        + 1
      | (Block(_, _), Block(_, _)) =>
        UHExp.num_lines_in_block(subject_block)
        - UHExp.num_lines_in_block(new_subject_block)
      };
    subject_block_shifted_to_prefix_chunk_sizes(
      ~u_gen,
      ~chunk_sizes=[num_shifted_lines, ...chunk_sizes],
      new_prefix,
      new_subject_block,
    );
  };

let rec subject_block_shifted_to_suffix_block_chunk_sizes =
        (~u_gen, ~chunk_sizes=[], subject_block, suffix_block): list(int) =>
  switch (
    subject_block |> UHExp.shift_line_to_suffix_block(~u_gen, suffix_block)
  ) {
  | None => chunk_sizes |> List.rev
  | Some((_, None, _)) => assert(false)
  | Some((new_subject_block, Some(new_suffix_block), u_gen)) =>
    let num_shifted_lines =
      switch (subject_block, new_subject_block) {
      | (Block(_, EmptyHole(_)), Block(_, EmptyHole(_))) =>
        UHExp.num_lines_in_block(subject_block)
        - UHExp.num_lines_in_block(new_subject_block)
      | (Block(_, _), Block(_, EmptyHole(_))) =>
        UHExp.num_lines_in_block(subject_block)
        - UHExp.num_lines_in_block(new_subject_block)
        + 1
      | (Block(_, _), Block(_, _)) =>
        UHExp.num_lines_in_block(subject_block)
        - UHExp.num_lines_in_block(new_subject_block)
      };
    subject_block_shifted_to_suffix_block_chunk_sizes(
      ~chunk_sizes=[num_shifted_lines, ...chunk_sizes],
      ~u_gen,
      new_subject_block,
      Some(new_suffix_block),
    );
  };

let vertical_shift_targets_in_subject = (model: Model.t) => {
  switch (
    model.cursor_info.position,
    model.cursor_info.node,
    model.cursor_info.frame,
  ) {
  | (OnText(_) | OnDelim(_, _), _, _)
  | (_, Typ(_) | Pat(_), _)
  | (_, _, TypFrame(_) | PatFrame(_)) => []
  | (
      Staging(0),
      Exp(
        Case(_, subject_block, _, _) | Parenthesized(subject_block) |
        Inj(_, _, subject_block),
      ),
      ExpFrame(prefix, None, _),
    ) =>
    let steps_of_subject_block = (model |> Model.steps) @ [0];
    let subject_block_shift_target_indices =
      subject_block_shifted_to_prefix_chunk_sizes(
        ~u_gen=model |> Model.u_gen,
        prefix,
        subject_block,
      )
      |> List.fold_left(
           (shift_target_indices, chunk_size) =>
             switch (shift_target_indices) {
             | [] => [chunk_size]
             | [last_index, ..._] => [
                 last_index + chunk_size,
                 ...shift_target_indices,
               ]
             },
           [],
         );
    let num_subject_lines = subject_block |> UHExp.num_lines_in_block;
    subject_block_shift_target_indices
    |> List.map(i =>
         i >= num_subject_lines
           ? (steps_of_subject_block @ [num_subject_lines - 1], After)
           : (steps_of_subject_block @ [i], Before)
       )
    |> List.map(((target_steps, target_side)) =>
         Vdom.(
           Node.div(
             [
               Attr.classes(["vertical-shift-target-in-subject"]),
               Attr.create(
                 "target-steps",
                 Sexp.to_string(Path.sexp_of_steps(target_steps)),
               ),
               Attr.create(
                 "target-side",
                 Sexp.to_string(sexp_of_side(target_side)),
               ),
             ],
             [],
           )
         )
       );
  | (
      Staging(1),
      Exp(Parenthesized(block) | Inj(_, _, block)),
      ExpFrame(_, None, Some(suffix_block)),
    ) =>
    let steps_of_subject_block = (model |> Model.steps) @ [0];
    let index_of_current_subject_line =
      (block |> UHExp.num_lines_in_block) - 1;
    let subject_block_shift_target_indices =
      subject_block_shifted_to_suffix_block_chunk_sizes(
        ~u_gen=model |> Model.u_gen,
        block,
        Some(suffix_block),
      )
      |> List.fold_left(
           (shift_target_indices, chunk_size) =>
             switch (shift_target_indices) {
             | [] => [index_of_current_subject_line - chunk_size]
             | [last_index, ..._] => [
                 last_index - chunk_size,
                 ...shift_target_indices,
               ]
             },
           [],
         );
    subject_block_shift_target_indices
    |> List.map(i =>
         i < 0
           ? (steps_of_subject_block @ [0], Before)
           : (steps_of_subject_block @ [i], After)
       )
    |> List.map(((target_steps, target_side)) =>
         Vdom.(
           Node.div(
             [
               Attr.classes(["vertical-shift-target-in-subject"]),
               Attr.create(
                 "target-steps",
                 Sexp.to_string(Path.sexp_of_steps(target_steps)),
               ),
               Attr.create(
                 "target-side",
                 Sexp.to_string(sexp_of_side(target_side)),
               ),
             ],
             [],
           )
         )
       );
  | (_, _, _) => []
  };
};

let horizontal_shift_targets_in_frame = (model: Model.t) => {
  switch (
    model.cursor_info.position,
    model.cursor_info.node,
    model.cursor_info.frame,
  ) {
  | (OnText(_) | OnDelim(_, _), _, _) => []
  | (_, _, TypFrame(None))
  | (_, _, PatFrame(None))
  | (_, _, ExpFrame(_, None, _)) => []
  | (Staging(k), Typ(List(_) | Parenthesized(_)), TypFrame(Some(_)))
  | (Staging(k), Pat(Inj(_, _, _) | Parenthesized(_)), PatFrame(Some(_)))
  | (
      Staging(k),
      Exp(Inj(_, _, _) | Parenthesized(_)),
      ExpFrame(_, Some(_), _),
    ) =>
    let (steps_of_parent_seq, _) =
      model |> Model.steps |> split_last |> Opt.get(_ => assert(false));
    let surround_indices =
      switch (model.cursor_info.frame) {
      | TypFrame(Some(surround)) =>
        let (prefix_tms, suffix_tms) =
          surround |> OperatorSeq.tms_of_surround;
        let prefix_len = prefix_tms |> List.length;
        k == 0
          ? range(prefix_len)
          : suffix_tms |> List.mapi((i, _) => prefix_len + 1 + i);
      | PatFrame(Some(surround)) =>
        let (prefix_tms, suffix_tms) =
          surround |> OperatorSeq.tms_of_surround;
        let prefix_len = prefix_tms |> List.length;
        k == 0
          ? range(prefix_len)
          : suffix_tms |> List.mapi((i, _) => prefix_len + 1 + i);
      | ExpFrame(_, Some(surround), _) =>
        let (prefix_tms, suffix_tms) =
          surround |> OperatorSeq.tms_of_surround;
        let prefix_len = prefix_tms |> List.length;
        k == 0
          ? range(prefix_len)
          : suffix_tms |> List.mapi((i, _) => prefix_len + 1 + i);
      | _ => []
      };
    surround_indices
    |> List.map(i => steps_of_parent_seq @ [i])
    |> List.map(target_steps =>
         Vdom.(
           Node.div(
             [
               Attr.classes(["horizontal-shift-target-in-frame"]),
               Attr.create(
                 "target-steps",
                 Sexp.to_string(Path.sexp_of_steps(target_steps)),
               ),
               Attr.create(
                 "target-side",
                 Sexp.to_string(sexp_of_side(k == 0 ? Before : After)),
               ),
             ],
             [],
           )
         )
       );
  | (_, _, _) => []
  };
};

let rec prefix_shifted_chunk_sizes =
        (~u_gen, ~chunk_sizes: list(int)=[], prefix, block): list(int) =>
  switch (prefix, block |> UHExp.shift_line_from_prefix(~u_gen, prefix)) {
  | ([], _)
  | (_, None) => chunk_sizes |> List.rev
  | (_, Some((new_prefix, new_block, u_gen))) =>
    let num_shifted_lines = List.length(prefix) - List.length(new_prefix);
    prefix_shifted_chunk_sizes(
      ~u_gen,
      ~chunk_sizes=[num_shifted_lines, ...chunk_sizes],
      new_prefix,
      new_block,
    );
  };

// Takes u_gen in order to run UHExp.shift_line_from_suffix_block
// but doesn't return it since we don't want any hole numbering
// state changes caused by creating shift targets.
let rec suffix_block_shifted_chunk_sizes =
        (
          ~chunk_sizes: list(int)=[],
          ~is_node_terminal,
          ~u_gen,
          block,
          suffix_block: option(UHExp.block),
        )
        : list(int) =>
  switch (suffix_block) {
  | None => []
  | Some(suffix_block) =>
    switch (
      block
      |> UHExp.shift_line_from_suffix_block(
           ~is_node_terminal,
           ~u_gen,
           Some(suffix_block),
         )
    ) {
    | None => chunk_sizes |> List.rev
    | Some((_, None, _)) =>
      let num_shifted_lines = suffix_block |> UHExp.num_lines_in_block;
      [num_shifted_lines, ...chunk_sizes] |> List.rev;
    | Some((new_block, Some(new_suffix_block), u_gen)) =>
      let num_shifted_lines =
        switch (suffix_block, new_suffix_block) {
        | (Block(_, EmptyHole(_)), Block(_, EmptyHole(_))) =>
          UHExp.num_lines_in_block(suffix_block)
          - UHExp.num_lines_in_block(new_suffix_block)
        | (Block(_, _), Block(_, EmptyHole(_))) =>
          UHExp.num_lines_in_block(suffix_block)
          - UHExp.num_lines_in_block(new_suffix_block)
          + 1
        | (Block(_, _), Block(_, _)) =>
          UHExp.num_lines_in_block(suffix_block)
          - UHExp.num_lines_in_block(new_suffix_block)
        };
      suffix_block_shifted_chunk_sizes(
        ~chunk_sizes=[num_shifted_lines, ...chunk_sizes],
        ~is_node_terminal,
        ~u_gen,
        new_block,
        Some(new_suffix_block),
      );
    }
  };

let vertical_shift_targets_in_frame = (model: Model.t) => {
  switch (
    model.cursor_info.position,
    model.cursor_info.node,
    model.cursor_info.frame,
  ) {
  | (OnText(_) | OnDelim(_, _), _, _)
  | (_, Typ(_) | Pat(_) | Rule(_), _)
  | (_, _, TypFrame(_) | PatFrame(_)) => []
  | (Staging(0), Exp(Parenthesized(block)), ExpFrame(prefix, surround, _)) =>
    let (steps_of_parent_block, index_of_current_line) =
      switch (surround) {
      | None =>
        model |> Model.steps |> split_last |> Opt.get(_ => assert(false))
      | Some(_) =>
        let (steps_of_parent_seq, _) =
          model |> Model.steps |> split_last |> Opt.get(_ => assert(false));
        steps_of_parent_seq |> split_last |> Opt.get(_ => assert(false));
      };
    let parent_block_shift_target_indices =
      prefix_shifted_chunk_sizes(~u_gen=model |> Model.u_gen, prefix, block)
      |> List.fold_left(
           (shift_target_indices, chunk_size) =>
             switch (shift_target_indices) {
             | [] => [index_of_current_line - chunk_size]
             | [last_index, ..._] => [
                 last_index - chunk_size,
                 ...shift_target_indices,
               ]
             },
           [],
         );
    parent_block_shift_target_indices
    |> List.map(i => steps_of_parent_block @ [max(i, 0)])
    |> List.map(target_steps =>
         Vdom.(
           Node.div(
             [
               Attr.classes(["vertical-shift-target-in-frame"]),
               Attr.create(
                 "target-steps",
                 Sexp.to_string(Path.sexp_of_steps(target_steps)),
               ),
               Attr.create(
                 "target-side",
                 Sexp.to_string(sexp_of_side(Before)),
               ),
             ],
             [],
           )
         )
       );
  | (
      Staging(1),
      Exp(Parenthesized(block) | Inj(_, _, block)),
      ExpFrame(_, surround, Some(suffix_block)),
    )
  | (
      Staging(3),
      Line(LetLine(_, _, block)),
      ExpFrame(_, surround, Some(suffix_block)),
    ) =>
    let (steps_of_parent_block, index_of_current_line) =
      switch (surround) {
      | None =>
        model |> Model.steps |> split_last |> Opt.get(_ => assert(false))
      | Some(_) =>
        let (steps_of_parent_seq, _) =
          model |> Model.steps |> split_last |> Opt.get(_ => assert(false));
        steps_of_parent_seq |> split_last |> Opt.get(_ => assert(false));
      };
    let parent_block_shift_target_indices =
      suffix_block_shifted_chunk_sizes(
        ~u_gen=model |> Model.u_gen,
        ~is_node_terminal=
          switch (model.cursor_info.node) {
          | Exp(_) => true
          | _line => false
          },
        block,
        Some(suffix_block),
      )
      |> List.fold_left(
           (shift_target_indices, chunk_size) =>
             switch (shift_target_indices) {
             | [] => [index_of_current_line + chunk_size]
             | [last_index, ..._] => [
                 last_index + chunk_size,
                 ...shift_target_indices,
               ]
             },
           [],
         );
    let last_line_index_of_parent_block =
      index_of_current_line + UHExp.num_lines_in_block(suffix_block);
    parent_block_shift_target_indices
    |> List.map(i =>
         steps_of_parent_block @ [min(i, last_line_index_of_parent_block)]
       )
    |> List.map(target_steps =>
         Vdom.(
           Node.div(
             [
               Attr.classes(["vertical-shift-target-in-frame"]),
               Attr.create(
                 "target-steps",
                 Sexp.to_string(Path.sexp_of_steps(target_steps)),
               ),
               Attr.create(
                 "target-side",
                 Sexp.to_string(sexp_of_side(After)),
               ),
             ],
             [],
           )
         )
       );
  | (Staging(_), Line(_) | Exp(_), ExpFrame(_, _, _)) => []
  };
};
let child_indicators = (model: Model.t) => {
  let child_indices =
    model.cursor_info |> CursorInfo.child_indices_of_current_node;
  let is_active = i => child_indices |> List.exists(j => j == i);
  model
  |> Model.zblock
  |> ZExp.erase_block
  |> UHExp.max_degree_block
  |> range
  |> List.map(i =>
       Vdom.(
         Node.div(
           [
             Attr.id(child_indicator_id(i)),
             Attr.classes([
               "child-indicator",
               switch (model.cursor_info.position) {
               | Staging(_) =>
                 model.cursor_info
                 |> CursorInfo.preserved_child_term_of_node
                 |> Opt.map_default(~default="staging", ((child_index, _)) =>
                      child_index == i ? "normal" : "staging"
                    )
               | OnText(_)
               | OnDelim(_, _) => "normal"
               },
               model.is_cell_focused && is_active(i) ? "active" : "inactive",
             ]),
           ],
           [],
         )
       )
     );
};

let indicators = (model: Model.t) => {
  switch (model.cursor_info.node) {
  | Exp(OpSeq(_, seq) as e) =>
    Code.is_multi_line_exp(e)
      ? multi_line_seq_indicators(
          model.is_cell_focused,
          OperatorSeq.seq_length(seq),
        )
      : single_line_seq_indicators(model.is_cell_focused)
  | Pat(OpSeq(_, seq) as p) =>
    Code.is_multi_line_pat(p)
      ? multi_line_seq_indicators(
          model.is_cell_focused,
          OperatorSeq.seq_length(seq),
        )
      : single_line_seq_indicators(model.is_cell_focused)
  | Typ(OpSeq(_, seq) as ty) =>
    Code.is_multi_line_typ(ty)
      ? multi_line_seq_indicators(
          model.is_cell_focused,
          OperatorSeq.seq_length(seq),
        )
      : single_line_seq_indicators(model.is_cell_focused)
  | _ =>
    Vdom.[
      Node.div(
        [
          Attr.id(box_node_indicator_id),
          Attr.classes([
            "node-indicator",
            model.is_cell_focused && model.cursor_info.node != Line(EmptyLine)
              ? "active" : "inactive",
            switch (model.cursor_info.position) {
            | OnText(_)
            | OnDelim(_, _) => "normal"
            | Staging(_) => "staging"
            },
          ]),
        ],
        [],
      ),
      Node.div(
        [
          Attr.id(box_tm_indicator_id),
          Attr.classes([
            "term-indicator",
            "term-indicator-first",
            "term-indicator-last",
            model.is_cell_focused
            && !(model.cursor_info |> CursorInfo.is_staging)
              ? "active" : "inactive",
          ]),
        ],
        [],
      ),
      Node.div(
        [
          Attr.id(current_horizontal_shift_target_id),
          Attr.classes([
            "current-horizontal-shift-target",
            switch (model.cursor_info.position) {
            | Staging(_) => "active"
            | _ => "inactive"
            },
          ]),
        ],
        [],
      ),
      Node.div(
        [
          Attr.id(current_vertical_shift_target_id),
          Attr.classes([
            "current-vertical-shift-target",
            switch (model.cursor_info.position) {
            | Staging(_) => "active"
            | _ => "inactive"
            },
          ]),
        ],
        [],
      ),
      Node.div(
        [
          Attr.id(current_shifting_delim_indicator_id),
          Attr.classes([
            "current-shifting-delim-indicator",
            switch (model.cursor_info.position) {
            | Staging(_) => "active"
            | _ => "inactive"
            },
          ]),
        ],
        [],
      ),
      Node.div(
        [
          Attr.id(horizontal_shift_rail_id),
          Attr.classes([
            "horizontal-shift-rail",
            switch (model.cursor_info.position) {
            | Staging(_) => "active"
            | _ => "inactive"
            },
          ]),
        ],
        [],
      ),
      Node.div(
        [
          Attr.id(vertical_shift_rail_id),
          Attr.classes([
            "vertical-shift-rail",
            switch (model.cursor_info.position) {
            | Staging(_) => "active"
            | _ => "inactive"
            },
          ]),
        ],
        [],
      ),
    ]
    @ (
      model.cursor_info |> CursorInfo.is_concluding_let_line
        ? [
          Vdom.(
            Node.div(
              [
                Attr.id(empty_hole_conclusion_mask_id),
                Attr.classes(["empty-hole-conclusion-mask"]),
              ],
              [],
            )
          ),
        ]
        : []
    )
    @ child_indicators(model)
    @ horizontal_shift_targets_in_subject(model)
    @ vertical_shift_targets_in_subject(model)
    @ horizontal_shift_targets_in_frame(model)
    @ vertical_shift_targets_in_frame(model)
  };
};

let font_size = 20.0;
let line_height = 1.5;
let indicator_padding = font_size *. (line_height -. 1.0) /. 2.0 -. 1.5;

let cell_padding = 10.0;
let cell_border = 2.0;
let shift_target_thickness = indicator_padding;

let get_relative_bounding_rect = elem => {
  let cell_rect =
    JSUtil.force_get_elem_by_id(cell_id) |> JSUtil.get_bounding_rect;
  elem
  |> JSUtil.get_bounding_rect(
       ~top_origin=cell_rect.top,
       ~left_origin=cell_rect.left,
     );
};

let indent_of_snode_elem = elem =>
  switch (elem |> JSUtil.get_attr("indent_level")) {
  | None => 0.0
  | Some(ssexp) =>
    switch (ssexp |> Sexplib.Sexp.of_string |> Code.indent_level_of_sexp) {
    | NotIndentable => 0.0
    | Indented(m) => float_of_int(m)
    | OpPrefix(m, n) => float_of_int(m + n)
    }
  };

let draw_box_node_indicator = (~child_indices, elem) => {
  let rect = elem |> get_relative_bounding_rect;
  let indent = elem |> indent_of_snode_elem;
  JSUtil.force_get_elem_by_id(box_node_indicator_id)
  |> JSUtil.place_over_rect(
       ~indent,
       {
         top: rect.top -. indicator_padding,
         right: rect.right +. indicator_padding,
         bottom:
           elem |> Code.elem_is_on_last_line
             ? rect.bottom +. indicator_padding
             : rect.bottom -. indicator_padding,
         left: rect.left -. indicator_padding,
       },
     );
  switch (elem |> Code.child_elems_of_snode_elem) {
  | None => assert(false)
  | Some(child_elems) =>
    zip(child_indices, child_elems)
    |> List.iter(((i, child_elem)) => {
         let child_rect = child_elem |> get_relative_bounding_rect;
         if (elem
             |> Code.elem_is_multi_line
             && child_elem
             |> Code.snode_elem_occupies_full_sline) {
           let indent = child_elem |> Code.elem_is_multi_line ? indent : 0.0;
           JSUtil.force_get_elem_by_id(child_indicator_id(i))
           |> JSUtil.place_over_rect(
                ~indent,
                {...child_rect, right: rect.right},
              );
         } else {
           JSUtil.force_get_elem_by_id(child_indicator_id(i))
           |> JSUtil.place_over_rect(child_rect);
         };
       })
  };
};

let draw_box_term_indicator = (~cursor_info, cursor_elem) => {
  let steps =
    cursor_elem
    |> JSUtil.get_attr("term")
    |> Opt.get(() => assert(false))
    |> Sexplib.Sexp.of_string
    |> Path.steps_of_sexp;
  let term_elem = Code.force_get_snode_elem(steps);
  let term_rect = term_elem |> get_relative_bounding_rect;
  let indent = term_elem |> indent_of_snode_elem;
  if (term_elem |> Code.snode_elem_is_Block) {
    let all_sline_elems =
      JSUtil.force_get_elem_by_id(cell_id)
      |> Code.sline_elems_of_snode_elem(term_elem);
    let first_sline_index =
      cursor_elem
      |> JSUtil.force_get_parent_elem
      |> Code.line_no_of_sline_elem
      |> Opt.get(() => assert(false));
    let sub_block_rect =
      all_sline_elems
      |> filteri((i, _) => i >= first_sline_index)
      |> List.map(get_relative_bounding_rect)
      |> JSUtil.get_covering_rect;
    JSUtil.force_get_elem_by_id(box_tm_indicator_id)
    |> JSUtil.place_over_rect(
         ~indent,
         {
           top: sub_block_rect.top -. indicator_padding,
           right: sub_block_rect.right +. indicator_padding,
           bottom:
             term_elem |> Code.elem_is_on_last_line
               ? sub_block_rect.bottom +. indicator_padding
               : sub_block_rect.bottom -. indicator_padding,
           left: sub_block_rect.left -. indicator_padding,
         },
       );
  } else {
    JSUtil.force_get_elem_by_id(box_tm_indicator_id)
    |> JSUtil.place_over_rect(
         ~indent,
         {
           top: term_rect.top -. indicator_padding,
           right: term_rect.right +. indicator_padding,
           bottom:
             term_elem |> Code.elem_is_on_last_line
               ? term_rect.bottom +. indicator_padding
               : term_rect.bottom -. indicator_padding,
           left: term_rect.left -. indicator_padding,
         },
       );
  };
  if (cursor_info |> CursorInfo.is_concluding_let_line) {
    // TODO encapsulate these details in Code.re
    let empty_hole_conclusion_elem =
      cursor_elem
      |> JSUtil.force_get_parent_elem
      |> JSUtil.force_get_next_sibling_elem
      |> JSUtil.force_get_next_sibling_elem;
    let empty_hole_rect =
      empty_hole_conclusion_elem |> get_relative_bounding_rect;
    JSUtil.force_get_elem_by_id(empty_hole_conclusion_mask_id)
    |> JSUtil.place_over_rect(
         ~indent=-0.8,
         {
           left: empty_hole_rect.left,
           right: term_rect.right +. indicator_padding,
           bottom:
             term_elem |> Code.elem_is_on_last_line
               ? term_rect.bottom +. indicator_padding
               : term_rect.bottom -. indicator_padding,
           top: empty_hole_rect.top,
         },
       );
  };
};

let draw_box_term_indicator_over_single_line_seq = (operand1, operand2) => {
  let rect1 = operand1 |> get_relative_bounding_rect;
  let rect2 = operand2 |> get_relative_bounding_rect;
  JSUtil.force_get_elem_by_id(box_tm_indicator_id)
  |> JSUtil.place_over_rect({
       top: rect1.top -. indicator_padding,
       left: rect1.left -. indicator_padding,
       bottom:
         operand1 |> Code.elem_is_on_last_line
           ? rect2.bottom +. indicator_padding
           : rect2.bottom -. indicator_padding,
       right: rect2.right +. indicator_padding,
     });
};

let draw_op_node_indicator = op_elem => {
  let rect = op_elem |> get_relative_bounding_rect;
  JSUtil.force_get_elem_by_id(op_node_indicator_id)
  |> JSUtil.place_over_rect({
       top: rect.top -. indicator_padding,
       left: rect.left -. indicator_padding,
       bottom:
         op_elem |> Code.elem_is_on_last_line
           ? rect.bottom +. indicator_padding
           : rect.bottom -. indicator_padding,
       right: rect.right +. indicator_padding,
     });
};

let draw_multi_line_seq_term_indicator = (steps, (a, b), opseq_elem) => {
  let a_elem = steps @ [a] |> node_id |> JSUtil.force_get_elem_by_id;
  let a_rect = a_elem |> get_relative_bounding_rect;
  JSUtil.force_get_elem_by_id(seq_tm_indicator_id(a))
  |> JSUtil.place_over_rect({
       top: a_rect.top -. indicator_padding,
       left: a_rect.left -. indicator_padding,
       bottom:
         a_elem |> Code.elem_is_on_last_line
           ? a_rect.bottom +. indicator_padding
           : a_rect.bottom -. indicator_padding,
       right: a_rect.right +. indicator_padding,
     });
  let sline_elems =
    JSUtil.force_get_elem_by_id(cell_id)
    |> Code.sline_elems_of_snode_elem(opseq_elem);
  range(~lo=a + 1, b + 1)
  |> List.map(List.nth(sline_elems))
  |> List.iteri((i, sline_elem) => {
       let rect = sline_elem |> get_relative_bounding_rect;
       JSUtil.force_get_elem_by_id(seq_tm_indicator_id(a + 1 + i))
       |> JSUtil.place_over_rect({
            top: rect.top -. indicator_padding,
            left: rect.left -. indicator_padding,
            bottom:
              sline_elem |> Code.elem_is_on_last_line
                ? rect.bottom +. indicator_padding
                : rect.bottom -. indicator_padding,
            right: rect.right +. indicator_padding,
          });
     });
};

let steps_and_side_of_shift_target_elem = shift_target_elem => (
  shift_target_elem
  |> JSUtil.force_get_attr("target-steps")
  |> Sexp.of_string
  |> Path.steps_of_sexp,
  shift_target_elem
  |> JSUtil.force_get_attr("target-side")
  |> Sexp.of_string
  |> side_of_sexp,
);

let horizontal_rail_left = ref(Float.max_float);
let horizontal_rail_right = ref(Float.min_float);
let vertical_rail_top = ref(Float.max_float);
let vertical_rail_bottom = ref(Float.min_float);

let place_horizontal_shift_target = (rect, shift_target_elem) => {
  shift_target_elem |> JSUtil.place_over_rect(rect);
  horizontal_rail_left := min(horizontal_rail_left^, rect.left);
  horizontal_rail_right := max(horizontal_rail_right^, rect.right);
};

let place_vertical_shift_target = (rect, shift_target_elem) => {
  shift_target_elem |> JSUtil.place_over_rect(rect);
  vertical_rail_top := min(vertical_rail_top^, rect.top);
  vertical_rail_bottom := max(vertical_rail_bottom^, rect.bottom);
};

let draw_current_shifting_delim_indicator = (~cursor_info, sdelim_elem) => {
  let rect = sdelim_elem |> get_relative_bounding_rect;
  JSUtil.force_get_elem_by_id(current_shifting_delim_indicator_id)
  |> JSUtil.place_over_rect(rect);
  JSUtil.force_get_elem_by_id(current_horizontal_shift_target_id)
  |> place_horizontal_shift_target({
       left:
         cursor_info |> CursorInfo.staging_left_border
           ? rect.left -. indicator_padding : rect.left,
       right:
         cursor_info |> CursorInfo.staging_right_border
           ? rect.right +. indicator_padding : rect.right,
       top: rect.bottom,
       bottom:
         sdelim_elem |> Code.elem_is_on_last_line
           ? rect.bottom +. indicator_padding
           : rect.bottom -. indicator_padding,
     });
  let cell_border_left = 0.0 -. cell_padding;
  let cell_border_right = 0.0 -. cell_padding -. cell_border;
  let midpoint = (cell_border_left +. cell_border_right) /. 2.0;
  JSUtil.force_get_elem_by_id(current_vertical_shift_target_id)
  |> place_vertical_shift_target({
       top: rect.top,
       bottom: rect.bottom,
       right: midpoint +. shift_target_thickness /. 2.0,
       left: midpoint -. shift_target_thickness /. 2.0,
     });
};

let draw_horizontal_shift_rail = sdelim_elem => {
  let rect = sdelim_elem |> get_relative_bounding_rect;
  let delim_bottom = rect.bottom;
  let indicator_bottom =
    sdelim_elem |> Code.elem_is_on_last_line
      ? rect.bottom +. indicator_padding : rect.bottom -. indicator_padding;
  let midpoint = (delim_bottom +. indicator_bottom) /. 2.0;
  JSUtil.force_get_elem_by_id(horizontal_shift_rail_id)
  |> JSUtil.place_over_rect({
       top: midpoint -. 1.0,
       bottom: midpoint +. 1.0,
       left: horizontal_rail_left^,
       right: horizontal_rail_right^,
     });
};

let draw_vertical_shift_rail = () => {
  let cell_border_left = 0.0 -. cell_padding -. cell_border;
  let cell_border_right = 0.0 -. cell_padding;
  let midpoint = (cell_border_left +. cell_border_right) /. 2.0;
  JSUtil.force_get_elem_by_id(vertical_shift_rail_id)
  |> JSUtil.place_over_rect({
       right: midpoint +. 1.0,
       left: midpoint -. 1.0,
       top: vertical_rail_top^,
       bottom: vertical_rail_bottom^,
     });
};

let draw_shift_targets = (~cursor_info, sdelim_elem) => {
  // used to track location of drawn shift targets
  // so far so that we know where to draw the rails
  horizontal_rail_left := Float.max_float;
  horizontal_rail_right := Float.min_float;
  vertical_rail_top := Float.max_float;
  vertical_rail_bottom := Float.min_float;

  draw_current_shifting_delim_indicator(~cursor_info, sdelim_elem);

  JSUtil.get_elems_with_cls("horizontal-shift-target-in-subject")
  @ JSUtil.get_elems_with_cls("horizontal-shift-target-in-frame")
  |> List.iter(target_elem => {
       let (target_steps, target_side) =
         steps_and_side_of_shift_target_elem(target_elem);
       {
         switch (
           Dom_html.document##getElementById(
             Js.string(node_id(target_steps)),
           )
           |> Js.Opt.to_option
         ) {
         | None => JSUtil.log("horizontal")
         | _ => ()
         };
       };
       let snode_elem = Code.force_get_snode_elem(target_steps);
       let rect = snode_elem |> get_relative_bounding_rect;
       let xpos =
         switch (target_side) {
         | Before => rect.left
         | After => rect.right
         };
       target_elem
       |> place_horizontal_shift_target({
            left: xpos -. shift_target_thickness /. 2.0,
            right: xpos +. shift_target_thickness /. 2.0,
            top: rect.bottom,
            bottom:
              snode_elem |> Code.elem_is_on_last_line
                ? rect.bottom +. indicator_padding
                : rect.bottom -. indicator_padding,
          });
     });
  JSUtil.get_elems_with_cls("vertical-shift-target-in-subject")
  @ JSUtil.get_elems_with_cls("vertical-shift-target-in-frame")
  |> List.iter(target_elem => {
       let (target_steps, target_side) =
         steps_and_side_of_shift_target_elem(target_elem);
       {
         switch (
           Dom_html.document##getElementById(
             Js.string(node_id(target_steps)),
           )
           |> Js.Opt.to_option
         ) {
         | None => JSUtil.log("vertical")
         | _ => ()
         };
       };
       let snode_elem = Code.force_get_snode_elem(target_steps);
       let rect = snode_elem |> get_relative_bounding_rect;
       let ypos =
         switch (target_side) {
         | Before => rect.top
         | After => rect.bottom
         };
       target_elem
       |> place_vertical_shift_target({
            left:
              0.0
              -. cell_padding
              -. cell_border
              /. 2.0
              -. shift_target_thickness
              /. 2.0,
            right:
              0.0
              -. cell_padding
              -. cell_border
              /. 2.0
              +. shift_target_thickness
              /. 2.0,
            top: ypos -. shift_target_thickness /. 2.0,
            bottom: ypos +. shift_target_thickness /. 2.0,
          });
     });

  draw_horizontal_shift_rail(sdelim_elem);
  draw_vertical_shift_rail();
};

let draw_horizontal_shift_target_in_subject = (~side, ~index, snode_elem) => {
  let rect = snode_elem |> get_relative_bounding_rect;
  let xpos =
    switch (side) {
    | Before => rect.left
    | After => rect.right
    };
  JSUtil.force_get_elem_by_id(horizontal_shift_target_in_subject_id(index))
  |> place_horizontal_shift_target({
       left: xpos -. shift_target_thickness /. 2.0,
       right: xpos +. shift_target_thickness /. 2.0,
       top: rect.bottom,
       bottom:
         snode_elem |> Code.elem_is_on_last_line
           ? rect.bottom +. indicator_padding
           : rect.bottom -. indicator_padding,
     });
};

let draw_horizontal_shift_target_in_frame = (~side, ~index, snode_elem) => {
  let rect = snode_elem |> get_relative_bounding_rect;
  let xpos =
    switch (side) {
    | Before => rect.left
    | After => rect.right
    };
  JSUtil.force_get_elem_by_id(horizontal_shift_target_in_frame_id(index))
  |> place_horizontal_shift_target({
       left: xpos -. shift_target_thickness /. 2.0,
       right: xpos +. shift_target_thickness /. 2.0,
       top: rect.bottom,
       bottom:
         snode_elem |> Code.elem_is_on_last_line
           ? rect.bottom +. indicator_padding
           : rect.bottom -. indicator_padding,
     });
};

let draw_vertical_shift_target_in_subject = (~side, ~index, sline_elem) => {
  let rect = sline_elem |> get_relative_bounding_rect;
  let ypos =
    switch (side) {
    | Before => rect.top
    | After => rect.bottom
    };
  JSUtil.force_get_elem_by_id(vertical_shift_target_in_subject_id(index))
  |> place_vertical_shift_target({
       left:
         0.0
         -. cell_padding
         -. cell_border
         /. 2.0
         -. shift_target_thickness
         /. 2.0,
       right:
         0.0
         -. cell_padding
         -. cell_border
         /. 2.0
         +. shift_target_thickness
         /. 2.0,
       top: ypos -. shift_target_thickness /. 2.0,
       bottom: ypos +. shift_target_thickness /. 2.0,
     });
};

let draw_vertical_shift_target_in_frame = (~side, ~index, sline_elem) => {
  let rect = sline_elem |> get_relative_bounding_rect;
  let ypos =
    switch (side) {
    | Before => rect.top
    | After => rect.bottom
    };
  JSUtil.force_get_elem_by_id(vertical_shift_target_in_frame_id(index))
  |> place_vertical_shift_target({
       left:
         0.0
         -. cell_padding
         -. cell_border
         /. 2.0
         -. shift_target_thickness
         /. 2.0,
       right:
         0.0
         -. cell_padding
         -. cell_border
         /. 2.0
         +. shift_target_thickness
         /. 2.0,
       top: ypos -. shift_target_thickness /. 2.0,
       bottom: ypos +. shift_target_thickness /. 2.0,
     });
};

let view =
    (~inject: Update.Action.t => Vdom.Event.t, model: Model.t): Vdom.Node.t => {
  Vdom.(
    Node.div(
      [
        Attr.id("pp_view"),
        Attr.classes(["ModelExp"]),
        Attr.create(
          "style",
          "font-size: "
          ++ (font_size |> JSUtil.px)
          ++ "; line-height: "
          ++ string_of_float(line_height)
          ++ "; padding: "
          ++ (cell_padding |> JSUtil.px)
          ++ "; border: "
          ++ (cell_border |> JSUtil.px)
          ++ " solid #CCC;",
        ),
      ],
      [
        Node.div(
          [
            Attr.id(cell_id),
            Attr.create("contenteditable", "true"),
            Attr.on("drop", _ => Event.Prevent_default),
            Attr.on_focus(_ => inject(FocusCell)),
            Attr.on_blur(_ => inject(BlurCell)),
            Attr.on_keypress(evt =>
              switch (
                model.cursor_info.position,
                JSUtil.is_movement_key(evt),
              ) {
              | (Staging(_), _) => Event.Prevent_default
              | (OnText(_) | OnDelim(_, _), true) => Event.Many([])
              | (OnText(_) | OnDelim(_, _), false) => Event.Prevent_default
              }
            ),
            Attr.on_keydown(evt => {
              let prevent_stop_inject = a =>
                Event.Many([
                  Event.Prevent_default,
                  Event.Stop_propagation,
                  inject(a),
                ]);
              let ci = model.cursor_info;
              switch (
                ci.position,
                JSUtil.is_movement_key(evt),
                JSUtil.is_single_key(evt),
                KeyCombo.of_evt(evt),
              ) {
              | (Staging(_), true, _, _) =>
                switch (evt |> JSUtil.get_key) {
                | "ArrowLeft" =>
                  prevent_stop_inject(Update.Action.EditAction(ShiftLeft))
                | "ArrowRight" =>
                  prevent_stop_inject(Update.Action.EditAction(ShiftRight))
                | "ArrowUp" =>
                  prevent_stop_inject(Update.Action.EditAction(ShiftUp))
                | "ArrowDown" =>
                  prevent_stop_inject(Update.Action.EditAction(ShiftDown))
                | _ => Event.Ignore
                }
              | (OnText(_) | OnDelim(_, _), true, _, _) => Event.Many([])
              | (_, _, None, None) => Event.Ignore
              | (_, _, Some(single_key), opt_kc) =>
                switch (ci.node, opt_kc) {
                | (Typ(_), Some((Key_B | Key_L | Key_N) as kc)) =>
                  prevent_stop_inject(
                    Update.Action.EditAction(Hashtbl.find(kc_actions, kc)),
                  )
                | (
                    Line(EmptyLine | ExpLine(EmptyHole(_))) |
                    Exp(EmptyHole(_)) |
                    Pat(EmptyHole(_)),
                    _,
                  ) =>
                  let shape =
                    switch (single_key) {
                    | Number(n) => Action.SNumLit(n, OnText(num_digits(n)))
                    | Letter(x) => Action.SVar(x, OnText(Var.length(x)))
                    | Underscore => Action.SWild
                    };
                  prevent_stop_inject(
                    Update.Action.EditAction(Construct(shape)),
                  );
                | (
                    Exp(NumLit(_, _) | BoolLit(_, _) | Var(_, _, _)) |
                    Pat(NumLit(_, _) | BoolLit(_, _) | Var(_, _, _)),
                    _,
                  ) =>
                  let nodeValue = JSUtil.force_get_anchor_node_value();
                  let anchorOffset = JSUtil.get_anchor_offset();
                  let key_string = JSUtil.single_key_string(single_key);
                  let newNodeValue =
                    string_insert(nodeValue, anchorOffset, key_string);
                  switch (int_of_string_opt(newNodeValue), single_key) {
                  | (Some(_), Underscore) =>
                    // OCaml accepts and ignores underscores
                    // when parsing ints from strings, we don't
                    Event.Ignore
                  | (Some(new_n), _) =>
                    // defensive check in case OCaml is
                    // doing any other weird things
                    num_digits(new_n) != String.length(newNodeValue)
                      ? Event.Ignore
                      : prevent_stop_inject(
                          Update.Action.EditAction(
                            Action.Construct(
                              Action.SNumLit(
                                new_n,
                                OnText(anchorOffset + 1),
                              ),
                            ),
                          ),
                        )
                  | (None, _) =>
                    Var.is_valid(newNodeValue)
                      ? prevent_stop_inject(
                          Update.Action.EditAction(
                            Action.Construct(
                              Action.SVar(
                                newNodeValue,
                                OnText(anchorOffset + 1),
                              ),
                            ),
                          ),
                        )
                      : prevent_stop_inject(
                          Update.Action.InvalidVar(newNodeValue),
                        )
                  };
                | (Line(_) | Exp(_) | Rule(_) | Pat(_) | Typ(_), _) => Event.Ignore
                }
              | (_, _, _, Some((Backspace | Delete) as kc)) =>
                let (string_edit, update, cursor_escaped) =
                  switch (kc) {
                  | Backspace => (
                      string_backspace,
                      Update.Action.EditAction(Backspace),
                      ci |> CursorInfo.is_before_node,
                    )
                  | _ => (
                      string_delete,
                      Update.Action.EditAction(Delete),
                      ci |> CursorInfo.is_after_node,
                    )
                  };
                switch (
                  kc,
                  model.user_newlines
                  |> Path.StepsMap.mem(model |> Model.steps),
                  cursor_escaped,
                  ci.position,
                ) {
                | (Backspace, true, _, _) =>
                  prevent_stop_inject(
                    Update.Action.RemoveUserNewline(model |> Model.steps),
                  )
                | (_, true, _, _) => prevent_stop_inject(update)
                | (_, false, true, _)
                | (_, false, _, OnDelim(_, _) | Staging(_)) =>
                  prevent_stop_inject(update)
                | (_, false, false, OnText(_)) =>
                  let nodeValue = JSUtil.force_get_anchor_node_value();
                  let anchorOffset = JSUtil.get_anchor_offset();
                  let ctrlKey = Js.to_bool(evt##.ctrlKey);
                  let (nodeValue', anchorOffset') =
                    string_edit(nodeValue, anchorOffset, ctrlKey);
                  switch (
                    String.equal(nodeValue', ""),
                    int_of_string_opt(nodeValue'),
                  ) {
                  | (true, _) => prevent_stop_inject(update)
                  | (false, Some(new_n)) =>
                    prevent_stop_inject(
                      Update.Action.EditAction(
                        Construct(SNumLit(new_n, OnText(anchorOffset'))),
                      ),
                    )
                  | (false, None) =>
                    Var.is_valid(nodeValue')
                      ? prevent_stop_inject(
                          Update.Action.EditAction(
                            Construct(
                              SVar(nodeValue', OnText(anchorOffset')),
                            ),
                          ),
                        )
                      : prevent_stop_inject(
                          Update.Action.InvalidVar(nodeValue'),
                        )
                  };
                };
              | (OnText(_) | OnDelim(_, _), _, _, Some(Enter)) =>
                switch (
                  model.user_newlines
                  |> Path.StepsMap.mem(model |> Model.steps),
                  model |> Model.zblock |> ZExp.is_after_case_rule,
                  model |> Model.zblock |> ZExp.is_on_user_newlineable_hole,
                ) {
                | (false, false, true) =>
                  prevent_stop_inject(
                    Update.Action.AddUserNewline(model |> Model.steps),
                  )
                | (_, _, _) =>
                  prevent_stop_inject(
                    Update.Action.EditAction(
                      Hashtbl.find(kc_actions, Enter),
                    ),
                  )
                }
              | (_, _, _, Some(kc)) =>
                prevent_stop_inject(
                  Update.Action.EditAction(Hashtbl.find(kc_actions, kc)),
                )
              };
            }),
          ],
          [
            model.is_cell_focused
              ? Code.view_of_zblock(
                  ~inject,
                  ~user_newlines=model.user_newlines,
                  model |> Model.zblock,
                )
              : Code.view_of_block(
                  ~inject,
                  ~user_newlines=model.user_newlines,
                  model |> Model.block,
                ),
            ...indicators(model),
          ],
        ),
      ],
    )
  );
};

let elem = () => JSUtil.force_get_elem_by_id(cell_id);
