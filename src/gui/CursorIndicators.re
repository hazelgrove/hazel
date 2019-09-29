open GeneralUtil;
open SemanticsCommon;
open ViewUtil;
module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
module Sexp = Sexplib.Sexp;

[@deriving sexp]
type vertical_alignment =
  | Above
  | Below
  | MidAbove
  | MidBelow;

let get_relative_bounding_rect = elem => {
  let cell_rect =
    JSUtil.force_get_elem_by_id(cell_id) |> JSUtil.get_bounding_rect;
  elem
  |> JSUtil.get_bounding_rect(
       ~top_origin=cell_rect.top,
       ~left_origin=cell_rect.left,
     );
};

type holes_steps = (
  // err holes around tms
  list(Path.steps),
  // err holes around skels rooted at a non-space op
  // + range of each skel
  list((op_path, Skel.range, bool)),
  // err holes around skels rooted at a space op
  // (where steps is to first operand of the space-sep seq)
  // + range of each skel
  list((Path.steps, Skel.range, bool)),
);

let cons_holes = (i, (holes, skel_holes, ap_holes)) => (
  holes |> List.map(steps => [i, ...steps]),
  skel_holes
  |> List.map((((steps, op_index), subseq_len, is_multi_line)) =>
       (([i, ...steps], op_index), subseq_len, is_multi_line)
     ),
  ap_holes
  |> List.map(((steps, subseq_len, is_multi_line)) =>
       ([i, ...steps], subseq_len, is_multi_line)
     ),
);

let concat_holes =
    ((holes1, skel_holes1, ap_holes1), (holes2, skel_holes2, ap_holes2)) => (
  holes1 @ holes2,
  skel_holes1 @ skel_holes2,
  ap_holes1 @ ap_holes2,
);

let err_holes: ErrStatus.t => holes_steps =
  fun
  | NotInHole => ([], [], [])
  | InHole(_, _) => ([[]], [], []);

let rec collect_holes_pat: UHPat.t => holes_steps =
  fun
  | EmptyHole(_) => ([], [], [])
  | Wild(err)
  | Var(err, _, _)
  | NumLit(err, _)
  | BoolLit(err, _)
  | ListNil(err) => err_holes(err)
  | Parenthesized(p) => cons_holes(0, collect_holes_pat(p))
  | OpSeq(skel, seq) => collect_holes_pat_skel(skel, seq)
  | Inj(_, _, _) => ([], [], [])
and collect_holes_pat_skel = (skel, seq) =>
  switch (skel) {
  | Placeholder(n) =>
    seq
    |> Seq.nth_operand(n)
    |> Opt.get(() => assert(false))
    |> collect_holes_pat
    |> cons_holes(n)
  | BinOp(err, Space, skel1, skel2) =>
    let (a, _) = skel1 |> Skel.range;
    let (_, b) = skel2 |> Skel.range;
    concat_holes(
      (
        [],
        [],
        switch (err) {
        | NotInHole => []
        | InHole(_, _) => [([], (a, b), false)]
        },
      ),
      concat_holes(
        collect_holes_pat_skel(skel1, seq),
        collect_holes_pat_skel(skel2, seq),
      ),
    );
  | BinOp(err, _, skel1, skel2) =>
    let (a, _) = skel1 |> Skel.range;
    let (k, b) = skel2 |> Skel.range;
    concat_holes(
      (
        [],
        switch (err) {
        | NotInHole => []
        | InHole(_, _) => [(([], k), (a, b), false)]
        },
        [],
      ),
      concat_holes(
        collect_holes_pat_skel(skel1, seq),
        collect_holes_pat_skel(skel2, seq),
      ),
    );
  };

let rec collect_holes: UHExp.block => holes_steps =
  fun
  | Block(lines, e) =>
    concat_holes(
      lines
      |> List.mapi((i, line) => cons_holes(i, collect_holes_line(line)))
      |> List.fold_left(
           (holes, holes_line) => concat_holes(holes, holes_line),
           ([], [], []),
         ),
      cons_holes(List.length(lines), collect_holes_exp(e)),
    )
and collect_holes_line: UHExp.line => holes_steps =
  fun
  | EmptyLine => ([], [], [])
  | ExpLine(e) => collect_holes_exp(e)
  | LetLine(p, _, def) =>
    concat_holes(
      cons_holes(0, collect_holes_pat(p)),
      cons_holes(2, collect_holes(def)),
    )
and collect_holes_exp: UHExp.t => holes_steps =
  fun
  | EmptyHole(_) => ([], [], [])
  | Var(err, _, _)
  | NumLit(err, _)
  | BoolLit(err, _)
  | ListNil(err) => err_holes(err)
  | Lam(err, p, _, def) =>
    concat_holes(
      err_holes(err),
      concat_holes(
        cons_holes(0, collect_holes_pat(p)),
        cons_holes(2, collect_holes(def)),
      ),
    )
  | Inj(_, _, _) => ([], [], [])
  | Case(err, scrut, rules, _) =>
    concat_holes(
      err_holes(err),
      concat_holes(
        cons_holes(0, collect_holes(scrut)),
        rules
        |> List.mapi((i, rule) =>
             cons_holes(i + 1, collect_holes_rule(rule))
           )
        |> List.fold_left(
             (holes, rule_holes) => concat_holes(holes, rule_holes),
             ([], [], []),
           ),
      ),
    )
  | ApPalette(_, _, _, _) => ([], [], [])
  | Parenthesized(body) => cons_holes(0, collect_holes(body))
  | OpSeq(skel, seq) =>
    collect_holes_skel(
      ~is_multi_line=
        seq |> Seq.operands |> List.exists(UHExp.is_multi_line_exp),
      skel,
      seq,
    )
and collect_holes_rule: UHExp.rule => holes_steps =
  fun
  | Rule(p, clause) =>
    concat_holes(
      cons_holes(0, collect_holes_pat(p)),
      cons_holes(1, collect_holes(clause)),
    )
and collect_holes_skel = (~is_multi_line, skel, seq) =>
  switch (skel) {
  | Placeholder(n) =>
    seq
    |> Seq.nth_operand(n)
    |> Opt.get(() => assert(false))
    |> collect_holes_exp
    |> cons_holes(n)
  | BinOp(err, Space, skel1, skel2) =>
    let (a, _) = skel1 |> Skel.range;
    let (_, b) = skel2 |> Skel.range;
    concat_holes(
      (
        [],
        [],
        switch (err) {
        | NotInHole => []
        | InHole(_, _) => [([], (a, b), is_multi_line)]
        },
      ),
      concat_holes(
        collect_holes_skel(~is_multi_line, skel1, seq),
        collect_holes_skel(~is_multi_line, skel2, seq),
      ),
    );
  | BinOp(err, _, skel1, skel2) =>
    let (a, _) = skel1 |> Skel.range;
    let (k, b) = skel2 |> Skel.range;
    concat_holes(
      (
        [],
        switch (err) {
        | NotInHole => []
        | InHole(_, _) => [(([], k), (a, b), is_multi_line)]
        },
        [],
      ),
      concat_holes(
        collect_holes_skel(~is_multi_line, skel1, seq),
        collect_holes_skel(~is_multi_line, skel2, seq),
      ),
    );
  };

let rec subject_block_shifted_to_prefix_chunk_sizes =
        (~u_gen=MetaVarGen.init, ~chunk_sizes=[], prefix, subject_block)
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
        (~u_gen=MetaVarGen.init, ~chunk_sizes=[], subject_block, suffix_block)
        : list(int) =>
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

let rec prefix_shifted_chunk_sizes =
        (~u_gen=MetaVarGen.init, ~chunk_sizes: list(int)=[], prefix, block)
        : list(int) =>
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
          ~u_gen=MetaVarGen.init,
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

let op_node_indicator = (~is_cell_focused, ~ci: CursorInfo.t) =>
  Vdom.(
    Node.div(
      [
        Attr.id(op_node_indicator_id),
        Attr.classes([
          "node-indicator",
          "normal",
          is_cell_focused ? "active" : "inactive",
        ]),
        Attr.create(
          "steps",
          ci.node_steps |> Path.sexp_of_steps |> Sexp.to_string,
        ),
        Attr.create(
          "op-index",
          switch (ci.position) {
          | Staging(_)
          | OnText(_) => assert(false)
          | OnDelim(k, _) => string_of_int(k)
          },
        ),
        Attr.create(
          "subskel-range",
          ci.subskel_range
          |> Opt.get(_ => assert(false))
          |> Skel.sexp_of_range
          |> Sexp.to_string,
        ),
      ],
      [],
    )
  );

let multi_line_seq_indicators = (~is_cell_focused, ~ci: CursorInfo.t) => {
  let (a, b) = ci.subskel_range |> Opt.get(_ => assert(false));
  Vdom.[
    op_node_indicator(~is_cell_focused, ~ci),
    ...range(~lo=a, b + 1)
       |> List.map(i =>
            Node.div(
              [
                Attr.id(multi_line_seq_tm_indicator_id(i)),
                Attr.classes(
                  ["term-indicator", is_cell_focused ? "active" : "inactive"]
                  @ (i == a ? ["term-indicator-first"] : [])
                  @ (i == b ? ["term-indicator-last"] : []),
                ),
              ],
              [],
            )
          ),
  ];
};

let single_line_seq_indicators = (~is_cell_focused, ~ci: CursorInfo.t) => {
  Vdom.[
    op_node_indicator(~is_cell_focused, ~ci),
    Node.div(
      [
        Attr.id(single_line_seq_tm_indicator_id),
        Attr.classes([
          "term-indicator",
          "term-indicator-first",
          "term-indicator-last",
          is_cell_focused ? "active" : "inactive",
        ]),
      ],
      [],
    ),
  ];
};

let child_indicators = (~is_cell_focused: bool, ~ci: CursorInfo.t) => {
  let parent_steps = ci.node_steps;
  let child_indices = ci |> CursorInfo.child_indices_of_current_node;
  child_indices
  |> List.map(i =>
       Vdom.(
         Node.div(
           [
             Attr.id(child_indicator_id(i)),
             Attr.classes([
               "child-indicator",
               switch (ci.position) {
               | Staging(_) =>
                 ci
                 |> CursorInfo.preserved_child_term_of_node
                 |> Opt.map_default(~default="staging", ((child_index, _)) =>
                      child_index == i ? "normal" : "staging"
                    )
               | OnText(_)
               | OnDelim(_, _) => "normal"
               },
               is_cell_focused ? "active" : "inactive",
             ]),
             Attr.create(
               "steps",
               Sexp.to_string(Path.sexp_of_steps(parent_steps @ [i])),
             ),
           ],
           [],
         )
       )
     );
};

let horizontal_shift_targets_in_subject = (~ci: CursorInfo.t) => {
  switch (ci.position, ci.node) {
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
      switch (ci.node) {
      | Line(_)
      | Rule(_) => assert(false)
      | Typ(_)
      | Pat(_) => ci.node_steps @ [0]
      | Exp(_) => ci.node_steps @ [0, 0]
      };
    let subject_indices =
      switch (ci.node) {
      | Typ(List(OpSeq(_, seq)) | Parenthesized(OpSeq(_, seq))) =>
        k == 0
          ? range(~lo=1, seq |> Seq.length) : range((seq |> Seq.length) - 1)
      | Pat(Inj(_, _, OpSeq(_, seq)) | Parenthesized(OpSeq(_, seq))) =>
        k == 0
          ? range(~lo=1, seq |> Seq.length) : range((seq |> Seq.length) - 1)
      | Exp(
          Inj(_, _, Block([], OpSeq(_, seq))) |
          Parenthesized(Block([], OpSeq(_, seq))),
        ) =>
        k == 0
          ? range(~lo=1, seq |> Seq.length) : range((seq |> Seq.length) - 1)
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

let vertical_shift_targets_in_subject = (~ci: CursorInfo.t) => {
  switch (ci.position, ci.node, ci.frame) {
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
    let steps_of_subject_block = ci.node_steps @ [0];
    let subject_block_shift_target_indices =
      subject_block_shifted_to_prefix_chunk_sizes(prefix, subject_block)
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
           ? (steps_of_subject_block @ [num_subject_lines - 1], MidBelow)
           : (steps_of_subject_block @ [i], Above)
       )
    |> List.map(((target_steps, target_alignment)) =>
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
                 Sexp.to_string(
                   sexp_of_vertical_alignment(target_alignment),
                 ),
               ),
             ],
             [],
           )
         )
       );
  | (
      Staging(1),
      Exp(Parenthesized(block) | Inj(_, _, block)),
      ExpFrame(_, None, suffix_block),
    )
  | (
      Staging(3),
      Line(LetLine(_, _, block)),
      ExpFrame(_, None, suffix_block),
    ) =>
    let steps_of_subject_block =
      switch (ci.node) {
      | Line(_) => ci.node_steps @ [2]
      | _exp => ci.node_steps @ [0]
      };
    let index_of_current_subject_line =
      (block |> UHExp.num_lines_in_block) - 1;
    let subject_block_shift_target_indices =
      subject_block_shifted_to_suffix_block_chunk_sizes(block, suffix_block)
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
           ? (steps_of_subject_block @ [0], MidAbove)
           : (steps_of_subject_block @ [i], Below)
       )
    |> List.map(((target_steps, target_alignment)) =>
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
                 Sexp.to_string(
                   sexp_of_vertical_alignment(target_alignment),
                 ),
               ),
             ],
             [],
           )
         )
       );
  | (_, _, _) => []
  };
};

let horizontal_shift_targets_in_frame = (~ci: CursorInfo.t) => {
  switch (ci.position, ci.node, ci.frame) {
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
      ci.node_steps |> split_last |> Opt.get(_ => assert(false));
    let surround_indices =
      switch (ci.frame) {
      | TypFrame(Some(surround)) =>
        let (prefix_tms, suffix_tms) = surround |> Seq.operands_of_surround;
        let prefix_len = prefix_tms |> List.length;
        k == 0
          ? range(prefix_len)
          : suffix_tms |> List.mapi((i, _) => prefix_len + 1 + i);
      | PatFrame(Some(surround)) =>
        let (prefix_tms, suffix_tms) = surround |> Seq.operands_of_surround;
        let prefix_len = prefix_tms |> List.length;
        k == 0
          ? range(prefix_len)
          : suffix_tms |> List.mapi((i, _) => prefix_len + 1 + i);
      | ExpFrame(_, Some(surround), _) =>
        let (prefix_tms, suffix_tms) = surround |> Seq.operands_of_surround;
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

let vertical_shift_targets_in_frame = (~ci: CursorInfo.t) => {
  switch (ci.position, ci.node, ci.frame) {
  | (OnText(_) | OnDelim(_, _), _, _)
  | (_, Typ(_) | Pat(_) | Rule(_), _)
  | (_, _, TypFrame(_) | PatFrame(_)) => []
  | (Staging(0), Exp(Parenthesized(block)), ExpFrame(prefix, surround, _)) =>
    let (steps_of_parent_block, index_of_current_line) =
      switch (surround) {
      | None => ci.node_steps |> split_last |> Opt.get(_ => assert(false))
      | Some(_) =>
        let (steps_of_parent_seq, _) =
          ci.node_steps |> split_last |> Opt.get(_ => assert(false));
        steps_of_parent_seq |> split_last |> Opt.get(_ => assert(false));
      };
    let parent_block_shift_target_indices =
      prefix_shifted_chunk_sizes(prefix, block)
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
                 Sexp.to_string(sexp_of_vertical_alignment(Above)),
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
      | None => ci.node_steps |> split_last |> Opt.get(_ => assert(false))
      | Some(_) =>
        let (steps_of_parent_seq, _) =
          ci.node_steps |> split_last |> Opt.get(_ => assert(false));
        steps_of_parent_seq |> split_last |> Opt.get(_ => assert(false));
      };
    let parent_block_shift_target_indices =
      suffix_block_shifted_chunk_sizes(
        ~is_node_terminal=
          switch (ci.node) {
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
                 Sexp.to_string(sexp_of_vertical_alignment(Below)),
               ),
             ],
             [],
           )
         )
       );
  | (Staging(_), Line(_) | Exp(_), ExpFrame(_, _, _)) => []
  };
};

let hole_indicators = ((holes, skel_holes, ap_holes)) => {
  let hole_indicators =
    holes
    |> List.map(steps =>
         Vdom.(
           Node.div(
             [
               Attr.classes(["hole-indicator"]),
               Attr.create(
                 "steps",
                 Sexplib.Sexp.to_string(Path.sexp_of_steps(steps)),
               ),
             ],
             [],
           )
         )
       );
  let skel_holes =
    skel_holes
    |> List.map((((steps, _) as op_path, (a, b), is_multi_line)) =>
         is_multi_line
           ? range(~lo=a, b + 1)
             |> List.map(i =>
                  Vdom.(
                    Node.div(
                      [
                        Attr.id(multi_line_skel_hole_id(steps, (a, b), i)),
                        Attr.classes(
                          switch (i == a, i == b) {
                          | (true, _) => [
                              "skel-hole-indicator",
                              "skel-hole-indicator-first",
                            ]
                          | (_, true) => [
                              "skel-hole-indicator",
                              "skel-hole-indicator-last",
                            ]
                          | (_, _) => ["skel-hole-indicator"]
                          },
                        ),
                        Attr.create(
                          "op-path",
                          Sexplib.Sexp.to_string(sexp_of_op_path(op_path)),
                        ),
                      ],
                      [],
                    )
                  )
                )
           : [
             Vdom.(
               Node.div(
                 [
                   Attr.id(single_line_skel_hole_id(steps, (a, b))),
                   Attr.classes([
                     "skel-hole-indicator",
                     "skel-hole-indicator-first",
                     "skel-hole-indicator-last",
                   ]),
                   Attr.create(
                     "op-path",
                     Sexplib.Sexp.to_string(sexp_of_op_path(op_path)),
                   ),
                 ],
                 [],
               )
             ),
           ]
       )
    |> List.concat;
  let ap_holes =
    ap_holes
    |> List.map(((steps, (a, b), is_multi_line)) =>
         is_multi_line
           ? range(~lo=a, b + 1)
             |> List.map(i =>
                  Vdom.(
                    Node.div(
                      [
                        Attr.id(multi_line_ap_hole_id(steps, (a, b), i)),
                        Attr.classes(
                          switch (i == a, i == b) {
                          | (true, _) => [
                              "ap-hole-indicator",
                              "ap-hole-indicator-first",
                            ]
                          | (_, true) => [
                              "ap-hole-indicator",
                              "ap-hole-indicator-last",
                            ]
                          | (_, _) => ["ap-hole-indicator"]
                          },
                        ),
                        Attr.create(
                          "steps",
                          Sexplib.Sexp.to_string(Path.sexp_of_steps(steps)),
                        ),
                      ],
                      [],
                    )
                  )
                )
           : [
             Vdom.(
               Node.div(
                 [
                   Attr.id(single_line_ap_hole_id(steps, (a, b))),
                   Attr.classes([
                     "ap-hole-indicator",
                     "ap-hole-indicator-first",
                     "ap-hole-indicator-last",
                   ]),
                   Attr.create(
                     "steps",
                     Sexplib.Sexp.to_string(Path.sexp_of_steps(steps)),
                   ),
                 ],
                 [],
               )
             ),
           ]
       )
    |> List.concat;
  hole_indicators @ skel_holes @ ap_holes;
};

let skel_hole_selector = op_path =>
  ".skel-hole-indicator[op-path=\'"
  ++ Sexplib.Sexp.to_string(sexp_of_op_path(op_path))
  ++ "\']";
let skel_hole_indicator_elems = (op_path): list(Js.t(Dom_html.element)) =>
  Dom_html.document##querySelectorAll(
    Js.string(skel_hole_selector(op_path)),
  )
  |> Dom.list_of_nodeList;

let ap_hole_selector = steps =>
  ".ap-hole-indicator[steps=\'"
  ++ Sexplib.Sexp.to_string(Path.sexp_of_steps(steps))
  ++ "\']";
let ap_hole_indicator_elems = steps =>
  Dom_html.document##querySelectorAll(Js.string(ap_hole_selector(steps)))
  |> Dom.list_of_nodeList;

let draw_hole_indicators = (model: Model.t) => {
  let (_, skel_holes, ap_holes) =
    model |> Model.zblock |> ZExp.erase_block |> collect_holes;
  JSUtil.get_elems_with_cls("hole-indicator")
  |> List.iter(indicator_elem => {
       let tm_elem =
         indicator_elem
         |> JSUtil.force_get_attr("steps")
         |> Sexplib.Sexp.of_string
         |> Path.steps_of_sexp
         |> Code.force_get_snode_elem;

       indicator_elem
       |> JSUtil.place_over_rect(tm_elem |> get_relative_bounding_rect);
     });
  skel_holes
  |> List.iter(((op_path, (a, b), _)) => {
       let (steps, _) = op_path;
       let seq_elem = Code.force_get_snode_elem(steps);
       if (seq_elem |> Code.elem_is_multi_line) {
         let rect_a =
           Code.force_get_snode_elem(steps @ [a])
           |> get_relative_bounding_rect;
         JSUtil.force_get_elem_by_id(
           multi_line_skel_hole_id(steps, (a, b), a),
         )
         |> JSUtil.place_over_rect(rect_a);
         let seq_sline_elems =
           JSUtil.force_get_elem_by_id(cell_id)
           |> Code.sline_elems_of_snode_elem(seq_elem);
         range(~lo=a + 1, b + 1)
         |> List.map(i =>
              (
                List.nth(seq_sline_elems, i),
                JSUtil.force_get_elem_by_id(
                  multi_line_ap_hole_id(steps, (a, b), i),
                ),
              )
            )
         |> List.iter(((sline_elem, indicator_elem)) => {
              let rect = sline_elem |> get_relative_bounding_rect;
              indicator_elem |> JSUtil.place_over_rect(rect);
            });
       } else {
         let rect_a =
           Code.force_get_snode_elem(steps @ [a])
           |> get_relative_bounding_rect;
         let rect_b =
           Code.force_get_snode_elem(steps @ [b])
           |> get_relative_bounding_rect;

         JSUtil.force_get_elem_by_id(
           single_line_skel_hole_id(steps, (a, b)),
         )
         |> JSUtil.place_over_rect({
              left: rect_a.left,
              top: rect_a.top,
              right: rect_b.right,
              bottom: rect_b.bottom,
            });
       };
     });
  ap_holes
  |> List.iter(((steps, (a, b), _)) => {
       let seq_elem = Code.force_get_snode_elem(steps);
       if (seq_elem |> Code.elem_is_multi_line) {
         let rect_a =
           Code.force_get_snode_elem(steps @ [a])
           |> get_relative_bounding_rect;
         JSUtil.force_get_elem_by_id(
           multi_line_ap_hole_id(steps, (a, b), a),
         )
         |> JSUtil.place_over_rect(rect_a);
         let seq_sline_elems =
           JSUtil.force_get_elem_by_id(cell_id)
           |> Code.sline_elems_of_snode_elem(seq_elem);
         range(~lo=a + 1, b + 1)
         |> List.map(i =>
              (
                List.nth(seq_sline_elems, i),
                JSUtil.force_get_elem_by_id(
                  multi_line_ap_hole_id(steps, (a, b), i),
                ),
              )
            )
         |> List.iter(((sline_elem, indicator_elem)) => {
              let rect = sline_elem |> get_relative_bounding_rect;
              indicator_elem |> JSUtil.place_over_rect(rect);
            });
       } else {
         let rect_a =
           Code.force_get_snode_elem(steps @ [a])
           |> get_relative_bounding_rect;
         let rect_b =
           Code.force_get_snode_elem(steps @ [b])
           |> get_relative_bounding_rect;

         JSUtil.force_get_elem_by_id(single_line_ap_hole_id(steps, (a, b)))
         |> JSUtil.place_over_rect({
              left: rect_a.left,
              top: rect_a.top,
              right: rect_b.right,
              bottom: rect_b.bottom,
            });
       };
     });
};

let view = (~is_cell_focused: bool, ~holes_steps, ~ci: CursorInfo.t) => {
  (
    switch (ci.node) {
    | Exp(OpSeq(_, _) as e) =>
      Code.is_multi_line_exp(e)
        ? multi_line_seq_indicators(~is_cell_focused, ~ci)
        : single_line_seq_indicators(~is_cell_focused, ~ci)
    | Pat(OpSeq(_, _) as p) =>
      Code.is_multi_line_pat(p)
        ? multi_line_seq_indicators(~is_cell_focused, ~ci)
        : single_line_seq_indicators(~is_cell_focused, ~ci)
    | Typ(OpSeq(_, _) as ty) =>
      Code.is_multi_line_typ(ty)
        ? multi_line_seq_indicators(~is_cell_focused, ~ci)
        : single_line_seq_indicators(~is_cell_focused, ~ci)
    | _ =>
      Vdom.[
        Node.div(
          [
            Attr.id(box_node_indicator_id),
            Attr.classes([
              "node-indicator",
              is_cell_focused && ci.node != Line(EmptyLine)
                ? "active" : "inactive",
              switch (ci.position) {
              | OnText(_)
              | OnDelim(_, _) => "normal"
              | Staging(_) => "staging"
              },
            ]),
            Attr.create(
              "steps",
              Sexp.to_string(Path.sexp_of_steps(ci.node_steps)),
            ),
            Attr.create(
              "child-indices",
              Sexp.to_string(
                ci
                |> CursorInfo.child_indices_of_current_node
                |> CursorInfo.sexp_of_child_indices,
              ),
            ),
            Attr.create(
              "is-concluding-let-line",
              string_of_bool(ci |> CursorInfo.is_concluding_let_line),
            ),
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
              is_cell_focused && !(ci |> CursorInfo.is_staging)
                ? "active" : "inactive",
            ]),
            Attr.create(
              "steps",
              Sexp.to_string(Path.sexp_of_steps(ci.term_steps)),
            ),
          ],
          [],
        ),
        Node.div(
          [
            Attr.id(current_horizontal_shift_target_id),
            Attr.classes([
              "current-horizontal-shift-target",
              switch (ci.position) {
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
              switch (ci.position) {
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
              switch (ci.position) {
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
              switch (ci.position) {
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
              switch (ci.position) {
              | Staging(_) => "active"
              | _ => "inactive"
              },
            ]),
          ],
          [],
        ),
      ]
      @ (
        ci |> CursorInfo.is_concluding_let_line
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
      @ child_indicators(~is_cell_focused, ~ci)
      @ horizontal_shift_targets_in_subject(~ci)
      @ vertical_shift_targets_in_subject(~ci)
      @ horizontal_shift_targets_in_frame(~ci)
      @ vertical_shift_targets_in_frame(~ci)
    }
  )
  @ hole_indicators(holes_steps);
};

let draw_box_node_indicator = () => {
  let indicator_elem = JSUtil.force_get_elem_by_id(box_node_indicator_id);
  let steps =
    indicator_elem
    |> JSUtil.force_get_attr("steps")
    |> Sexp.of_string
    |> Path.steps_of_sexp;
  let child_indices =
    indicator_elem
    |> JSUtil.force_get_attr("child-indices")
    |> Sexp.of_string
    |> CursorInfo.child_indices_of_sexp;
  let cursor_elem = Code.force_get_snode_elem(steps);
  let indent =
    switch (Code.indent_level_of_snode_elem(cursor_elem)) {
    | Indented(_) => 0.0
    | ToBeIndented(indentation) => float_of_int(indentation)
    };

  // shade current node
  let rect = cursor_elem |> get_relative_bounding_rect;
  indicator_elem
  |> JSUtil.place_over_rect(
       ~indent,
       {
         top: rect.top -. indicator_padding,
         right: rect.right +. indicator_padding,
         bottom:
           cursor_elem |> Code.elem_is_on_last_line
             ? rect.bottom +. indicator_padding
             : rect.bottom -. indicator_padding,
         left: rect.left -. indicator_padding,
       },
     );

  // unshade child nodes
  child_indices
  |> List.iter(i => {
       let child_indicator_elem =
         JSUtil.force_get_elem_by_id(child_indicator_id(i));
       let child_elem = Code.force_get_snode_elem(steps @ [i]);
       let child_rect = child_elem |> get_relative_bounding_rect;
       if (cursor_elem
           |> Code.elem_is_multi_line
           && child_elem
           |> Code.snode_elem_occupies_full_sline) {
         // if child node occupies full sline, then we need
         // to trim the right border to reveal the shading border
         // TODO revisit rect margin logic so this kind of thing isn't necessary
         let indent = child_elem |> Code.elem_is_multi_line ? indent : 0.0;
         child_indicator_elem
         |> JSUtil.place_over_rect(
              ~indent,
              {...child_rect, right: rect.right},
            );
       } else {
         child_indicator_elem |> JSUtil.place_over_rect(child_rect);
       };
     });
};

let draw_box_term_indicator = () => {
  let indicator_elem = JSUtil.force_get_elem_by_id(box_tm_indicator_id);
  let term_steps =
    indicator_elem
    |> JSUtil.force_get_attr("steps")
    |> Sexp.of_string
    |> Path.steps_of_sexp;
  let term_elem = Code.force_get_snode_elem(term_steps);
  let term_rect = term_elem |> get_relative_bounding_rect;
  let indent =
    switch (Code.indent_level_of_snode_elem(term_elem)) {
    | Indented(_) => 0.0
    | ToBeIndented(m) => float_of_int(m)
    };
  let node_indicator_elem =
    JSUtil.force_get_elem_by_id(box_node_indicator_id);
  let cursor_elem =
    node_indicator_elem
    |> JSUtil.force_get_attr("steps")
    |> Sexp.of_string
    |> Path.steps_of_sexp
    |> Code.force_get_snode_elem;
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

    indicator_elem
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
    indicator_elem
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
  let is_concluding_let_line =
    node_indicator_elem
    |> JSUtil.force_get_attr("is-concluding-let-line")
    |> bool_of_string;
  if (is_concluding_let_line) {
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

let draw_single_line_seq_indicators = () => {
  // shade op
  let op_node_indicator_elem =
    JSUtil.force_get_elem_by_id(op_node_indicator_id);
  let seq_steps =
    op_node_indicator_elem
    |> JSUtil.force_get_attr("steps")
    |> Sexp.of_string
    |> Path.steps_of_sexp;
  let op_index =
    op_node_indicator_elem
    |> JSUtil.force_get_attr("op-index")
    |> int_of_string;
  let op_elem = JSUtil.force_get_elem_by_id(op_id(seq_steps, op_index));
  draw_op_node_indicator(op_elem);

  // draw term indicator over skel rooted at op
  let term_indicator_elem =
    JSUtil.force_get_elem_by_id(single_line_seq_tm_indicator_id);
  let (a, b) =
    op_node_indicator_elem
    |> JSUtil.force_get_attr("subskel-range")
    |> Sexp.of_string
    |> Skel.range_of_sexp;
  let tm_a_elem = Code.force_get_snode_elem(seq_steps @ [a]);
  let tm_b_elem = Code.force_get_snode_elem(seq_steps @ [b]);
  let rect_a = tm_a_elem |> get_relative_bounding_rect;
  let rect_b = tm_b_elem |> get_relative_bounding_rect;
  term_indicator_elem
  |> JSUtil.place_over_rect({
       top: rect_a.top -. indicator_padding,
       left: rect_a.left -. indicator_padding,
       bottom:
         tm_a_elem |> Code.elem_is_on_last_line
           ? rect_b.bottom +. indicator_padding
           : rect_b.bottom -. indicator_padding,
       right: rect_b.right +. indicator_padding,
     });
};

let draw_multi_line_seq_indicators = () => {
  // shade op
  let op_node_indicator_elem =
    JSUtil.force_get_elem_by_id(op_node_indicator_id);
  let seq_steps =
    op_node_indicator_elem
    |> JSUtil.force_get_attr("steps")
    |> Sexp.of_string
    |> Path.steps_of_sexp;
  let op_index =
    op_node_indicator_elem
    |> JSUtil.force_get_attr("op-index")
    |> int_of_string;
  let op_elem = JSUtil.force_get_elem_by_id(op_id(seq_steps, op_index));
  draw_op_node_indicator(op_elem);

  // draw term indicators over first line of subskel rooted at op
  let (a, b) =
    op_node_indicator_elem
    |> JSUtil.force_get_attr("subskel-range")
    |> Sexp.of_string
    |> Skel.range_of_sexp;
  let a_elem = seq_steps @ [a] |> node_id |> JSUtil.force_get_elem_by_id;
  let a_indent =
    switch (Code.indent_level_of_snode_elem(a_elem)) {
    | Indented(_) => 0.0
    | ToBeIndented(m) => float_of_int(m)
    };
  let a_rect = a_elem |> get_relative_bounding_rect;
  JSUtil.force_get_elem_by_id(multi_line_seq_tm_indicator_id(a))
  |> JSUtil.place_over_rect(
       ~indent=a_indent,
       {
         top: a_rect.top -. indicator_padding,
         left: a_rect.left -. indicator_padding,
         bottom:
           a_elem |> Code.elem_is_on_last_line
             ? a_rect.bottom +. indicator_padding
             : a_rect.bottom -. indicator_padding,
         right: a_rect.right +. indicator_padding,
       },
     );

  // draw term indicators over remaining
  // lines of subskell rooted at op
  let sline_elems =
    JSUtil.force_get_elem_by_id(cell_id)
    |> Code.sline_elems_of_snode_elem(Code.force_get_snode_elem(seq_steps));
  range(~lo=a + 1, b + 1)
  |> List.map(List.nth(sline_elems))
  |> List.iteri((i, sline_elem) => {
       let rect = sline_elem |> get_relative_bounding_rect;
       JSUtil.force_get_elem_by_id(multi_line_seq_tm_indicator_id(a + 1 + i))
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

let steps_and_side_of_horizontal_shift_target_elem = shift_target_elem => (
  shift_target_elem
  |> JSUtil.force_get_attr("target-steps")
  |> Sexp.of_string
  |> Path.steps_of_sexp,
  shift_target_elem
  |> JSUtil.force_get_attr("target-side")
  |> Sexp.of_string
  |> side_of_sexp,
);

let steps_and_alignment_of_vertical_shift_target_elem = shift_target_elem => (
  shift_target_elem
  |> JSUtil.force_get_attr("target-steps")
  |> Sexp.of_string
  |> Path.steps_of_sexp,
  shift_target_elem
  |> JSUtil.force_get_attr("target-side")
  |> Sexp.of_string
  |> vertical_alignment_of_sexp,
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
         steps_and_side_of_horizontal_shift_target_elem(target_elem);
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
       let (target_steps, target_alignment) =
         steps_and_alignment_of_vertical_shift_target_elem(target_elem);
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
         switch (target_alignment) {
         | Above => rect.top
         | Below => rect.bottom
         | MidAbove => rect.top -. 18.0 // TODO remove magic numbers
         | MidBelow => rect.bottom +. 18.0
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

let draw_SSeq_indicators = (~cursor_elem) => {
  cursor_elem |> Code.elem_is_multi_line
    ? draw_multi_line_seq_indicators() : draw_single_line_seq_indicators();
};

let draw_SBox_indicators = () => {
  draw_box_node_indicator();
  draw_box_term_indicator();
};

let draw = (~ci: CursorInfo.t) => {
  let cursor_elem = Code.force_get_snode_elem(ci.node_steps);
  switch (ci.position) {
  | OnText(_)
  | OnDelim(_, _) =>
    cursor_elem |> Code.elem_is_SBox
      ? draw_SBox_indicators() : draw_SSeq_indicators(~cursor_elem)
  | Staging(delim_index) =>
    draw_box_node_indicator();
    draw_shift_targets(
      ~cursor_info=ci,
      Code.force_get_sdelim_elem((ci.node_steps, delim_index)),
    );
  };
};
