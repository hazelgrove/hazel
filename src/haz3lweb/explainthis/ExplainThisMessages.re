/*let get_group_opt = (group_id, doc: t) =>
    List.find_opt(((id, _)) => id == group_id, doc.groups);
  let get_group = (group_id, doc: t) => {
    let form_group =
      switch (get_group_opt(group_id, doc)) {
      | Some((_, form_group)) => form_group
      | None =>
        raise(
          Invalid_argument(group_id ++ " is not present in the list of groups"),
        )
      };
    form_group;
  };

  let get_form_and_options = (group_id, doc: t) => {
    let form_group = get_group(group_id, doc);
    let selected_id =
      switch (List.nth_opt(form_group.options, form_group.current_selection)) {
      | Some((selected_id, _)) => selected_id
      | None =>
        raise(
          Invalid_argument(
            "The options and current_selection for "
            ++ group_id
            ++ " are misconfigured",
          ),
        )
      };
    let form =
      switch (List.find_opt(({id, _}) => id == selected_id, doc.forms)) {
      | Some(form) => form
      | None =>
        raise(
          Invalid_argument(
            selected_id ++ " is not present in the list of forms",
          ),
        )
      };
    (form, form_group.options);
  };

  let get_example_opt = (example_sub_id, examples) =>
    List.find_opt(({sub_id, _}) => sub_id == example_sub_id, examples);
  let get_example = (example_sub_id, examples) =>
    switch (get_example_opt(example_sub_id, examples)) {
    | Some(example) => example
    | None =>
      raise(
        Invalid_argument(
          example_sub_id ++ " is not present in the list of examples",
        ),
      )
    };

  let get_form_opt = (form_id, docs) =>
    List.find_opt(({id, _}) => id == form_id, docs);
  let get_form = (form_id, docs) =>
    switch (get_form_opt(form_id, docs)) {
    | Some(form) => form
    | None =>
      raise(
        Invalid_argument(form_id ++ " is not present in the list of forms"),
      )
    };*/
/*let rec update_form = (new_form, docs) => {
    switch (docs) {
    | [] => []
    | [x, ...xs] =>
      if (x.id == new_form.id) {
        [new_form, ...xs];
      } else {
        [x, ...update_form(new_form, xs)];
      }
    };
  };

  let rec update_example = (new_example, docs) => {
    switch (docs) {
    | [] => []
    | [x, ...xs] =>
      if (x.sub_id == new_example.sub_id) {
        [new_example, ...xs];
      } else {
        [x, ...update_example(new_example, xs)];
      }
    };
  };

  let rec update_group = (group_id, new_selection, groups) => {
    switch (groups) {
    | [] => []
    | [(id, options) as x, ...xs] =>
      if (id == group_id) {
        [
          (id, {options: options.options, current_selection: new_selection}),
          ...xs,
        ];
      } else {
        [x, ...update_group(group_id, new_selection, xs)];
      }
    };
  };*/
/*
     list_exp_group: ListExp.list_exp,
   cons_exp_group: ListExp.cons_exp,(
   FunctionExp.function_str_group,
   init_options([
     (FunctionExp.function_exp.id, [pat("p")]),
     (FunctionExp.function_strlit_exp.id, [pat("StringLit")]),
   ]),
 ),
 (
   FunctionExp.function_triv_group,
   init_options([
     (FunctionExp.function_exp.id, [pat("p")]),
     (FunctionExp.function_triv_exp.id, [pat("triv")]),
   ]),
 ),
 (
   FunctionExp.function_listnil_group,
   init_options([
     (FunctionExp.function_exp.id, [pat("p")]),
     (FunctionExp.function_listnil_exp.id, [pat("nil")]),
   ]),
 ),
 (
   FunctionExp.function_listlit_group,
   init_options([
     (FunctionExp.function_exp.id, [pat("p")]),
     (
       FunctionExp.function_listlit_exp.id,
       [mk_list_pat([[pat("p1"), comma_pat(), pat("...")]])],
     ),
   ]),
 ),
 (
   FunctionExp.function_cons_group,
   init_options([
     (FunctionExp.function_exp.id, [pat("p")]),
     (
       FunctionExp.function_cons_exp.id,
       [pat("p_hd"), cons_pat(), pat("p_tl")],
     ),
   ]),
 ),
 (
   FunctionExp.function_var_group,
   init_options([
     (FunctionExp.function_exp.id, [pat("p")]),
     (FunctionExp.function_var_exp.id, [pat("x")]),
   ]),
 ),
 (
   FunctionExp.function_tuple_group,
   init_options([
     (FunctionExp.function_exp.id, [pat("p")]),
     (
       FunctionExp.function_tuple_exp.id,
       [pat("p1"), comma_pat(), pat("...")],
     ),
   ]),
 ),
 (
   FunctionExp.function_tuple_2_group,
   init_options([
     (FunctionExp.function_exp.id, [pat("p")]),
     (
       FunctionExp.function_tuple_exp.id,
       [pat("p1"), comma_pat(), pat("...")],
     ),
     (
       FunctionExp.function_tuple2_exp.id,
       [pat("p1"), comma_pat(), pat("p2")],
     ),
   ]),
 ),
 (
   FunctionExp.function_tuple_3_group,
   init_options([
     (FunctionExp.function_exp.id, [pat("p")]),
     (
       FunctionExp.function_tuple_exp.id,
       [pat("p1"), comma_pat(), pat("...")],
     ),
     (
       FunctionExp.function_tuple3_exp.id,
       [pat("p1"), comma_pat(), pat("p2"), comma_pat(), pat("p3")],
     ),
   ]),
 ),
 (
   FunctionExp.function_tag_group,
   init_options([
     (FunctionExp.function_exp.id, [pat("p")]),
     (FunctionExp.function_tag_exp.id, [pat("C")]),
   ]),
 ),
 (
   FunctionExp.function_ap_group,
   init_options([
     (FunctionExp.function_exp.id, [pat("p")]),
     (
       FunctionExp.function_ap_exp.id,
       [pat("p_con"), mk_ap_pat([[pat("p_arg")]])],
     ),
   ]),
 ),
 (TupleExp.tuple_exp_group, init_options([(TupleExp.tuple_exp.id, [])])),
 (
   TupleExp.tuple_exp_2_group,
   init_options([
     (TupleExp.tuple_exp.id, [exp("e1"), comma_exp(), exp("...")]),
     (TupleExp.tuple_exp_size2.id, [exp("e1"), comma_exp(), exp("e2")]),
   ]),
 ),
 (
   TupleExp.tuple_exp_3_group,
   init_options([
     (TupleExp.tuple_exp.id, [exp("e1"), comma_exp(), exp("...")]),
     (
       TupleExp.tuple_exp_size3.id,
       [exp("e1"), comma_exp(), exp("e2"), comma_exp(), exp("e3")],
     ),
   ]),
 ),
 (
   LetExp.let_base_exp_group,
   init_options([(LetExp.let_base_exp.id, [])]),
 ),
 (
   LetExp.let_empty_hole_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (LetExp.let_empty_hole_exp.id, [pat("EmptyHole")]),
   ]),
 ),
 (
   LetExp.let_multi_hole_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (LetExp.let_multi_hole_exp.id, [pat("INVALID")]),
   ]),
 ),
 (
   LetExp.let_wild_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (LetExp.let_wild_exp.id, [pat("_")]),
   ]),
 ),
 (
   LetExp.let_int_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (LetExp.let_int_exp.id, [pat("IntLit")]),
   ]),
 ),
 (
   LetExp.let_float_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (LetExp.let_float_exp.id, [pat("FloatLit")]),
   ]),
 ),
 (
   LetExp.let_bool_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (LetExp.let_bool_exp.id, [pat("BoolLit")]),
   ]),
 ),
 (
   LetExp.let_str_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (LetExp.let_str_exp.id, [pat("StringLit")]),
   ]),
 ),
 (
   LetExp.let_triv_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (LetExp.let_triv_exp.id, [pat("triv")]),
   ]),
 ),
 (
   LetExp.let_listlit_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (LetExp.let_listlit_exp.id, [pat("p1"), comma_pat(), pat("...")]),
   ]),
 ),
 (
   LetExp.let_listnil_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (LetExp.let_listnil_exp.id, [pat("nil")]),
   ]),
 ),
 (
   LetExp.let_cons_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (LetExp.let_cons_exp.id, [pat("p_hd"), cons_pat(), pat("p_tl")]),
   ]),
 ),
 (
   LetExp.let_var_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (LetExp.let_var_exp.id, [pat("x")]),
   ]),
 ),
 (
   LetExp.let_tuple_base_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (LetExp.let_tuple_exp.id, [pat("p1"), comma_pat(), pat("...")]),
   ]),
 ),
 (
   LetExp.let_tuple2_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (LetExp.let_tuple_exp.id, [pat("p1"), comma_pat(), pat("...")]),
     (LetExp.let_tuple2_exp.id, [pat("p1"), comma_pat(), pat("p2")]),
   ]),
 ),
 (
   LetExp.let_tuple3_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (LetExp.let_tuple_exp.id, [pat("p1"), comma_pat(), pat("...")]),
     (
       LetExp.let_tuple3_exp.id,
       [pat("p1"), comma_pat(), pat("p2"), comma_pat(), pat("p3")],
     ),
   ]),
 ),
 (
   LetExp.let_tag_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (LetExp.let_tag_exp.id, [pat("C")]),
   ]),
 ),
 (
   LetExp.let_ap_exp_group,
   init_options([
     (LetExp.let_base_exp.id, [pat("p")]),
     (
       LetExp.let_ap_exp.id,
       [pat("p_con"), mk_ap_pat([[pat("p_arg")]])],
     ),
   ]),
 ),
 (AppExp.funapp_exp_group, init_options([(AppExp.funapp_exp.id, [])])),
 (AppExp.conapp_exp_group, init_options([(AppExp.conapp_exp.id, [])])),
 (IfExp.if_exp_group, init_options([(IfExp.if_exp.id, [])])),
 (SeqExp.seq_exp_group, init_options([(SeqExp.seq_exp.id, [])])),
 (TestExp.test_group, init_options([(TestExp.test_exp.id, [])])),
 (
   OpExp.int_unary_minus_group,
   init_options([(OpExp.int_unary_minus_exp.id, [])]),
 ),
 (OpExp.int_plus_group, init_options([(OpExp.int_plus_exp.id, [])])),
 (OpExp.int_minus_group, init_options([(OpExp.int_minus_exp.id, [])])),
 (OpExp.int_times_group, init_options([(OpExp.int_times_exp.id, [])])),
 (OpExp.int_power_group, init_options([(OpExp.int_power_exp.id, [])])),
 (OpExp.int_divide_group, init_options([(OpExp.int_divide_exp.id, [])])),
 (OpExp.int_lt_group, init_options([(OpExp.int_lt_exp.id, [])])),
 (OpExp.int_lte_group, init_options([(OpExp.int_lte_exp.id, [])])),
 (OpExp.int_gt_group, init_options([(OpExp.int_gt_exp.id, [])])),
 (OpExp.int_gte_group, init_options([(OpExp.int_gte_exp.id, [])])),
 (OpExp.int_eq_group, init_options([(OpExp.int_eq_exp.id, [])])),
 (OpExp.float_plus_group, init_options([(OpExp.float_plus_exp.id, [])])),
 (
   OpExp.float_minus_group,
   init_options([(OpExp.float_minus_exp.id, [])]),
 ),
 (
   OpExp.float_times_group,
   init_options([(OpExp.float_times_exp.id, [])]),
 ),
 (
   OpExp.float_power_group,
   init_options([(OpExp.float_power_exp.id, [])]),
 ),
 (
   OpExp.float_divide_group,
   init_options([(OpExp.float_divide_exp.id, [])]),
 ),
 (OpExp.float_lt_group, init_options([(OpExp.float_lt_exp.id, [])])),
 (OpExp.float_lte_group, init_options([(OpExp.float_lte_exp.id, [])])),
 (OpExp.float_gt_group, init_options([(OpExp.float_gt_exp.id, [])])),
 (OpExp.float_gte_group, init_options([(OpExp.float_gte_exp.id, [])])),
 (OpExp.float_eq_group, init_options([(OpExp.float_eq_exp.id, [])])),
 (OpExp.bool_and_group, init_options([(OpExp.bool_and_exp.id, [])])),
 (OpExp.bool_or_group, init_options([(OpExp.bool_or_exp.id, [])])),
 (OpExp.str_eq_group, init_options([(OpExp.str_eq_exp.id, [])])),
 (CaseExp.case_exp_group, init_options([(CaseExp.case_exp.id, [])])),
 // Rules
 // Patterns
 (
   HolePat.empty_hole_pat_group,
   init_options([(HolePat.empty_hole_pat.id, [])]),
 ),
 (
   HolePat.multi_hole_pat_group,
   init_options([(HolePat.multi_hole_pat.id, [])]),
 ),
 (
   TerminalPat.wild_pat_group,
   init_options([(TerminalPat.wild_pat.id, [])]),
 ),
 (
   TerminalPat.intlit_pat_group,
   init_options([(TerminalPat.intlit_pat.id, [])]),
 ),
 (
   TerminalPat.floatlit_pat_group,
   init_options([(TerminalPat.floatlit_pat.id, [])]),
 ),
 (
   TerminalPat.boollit_pat_group,
   init_options([(TerminalPat.boollit_pat.id, [])]),
 ),
 (
   TerminalPat.strlit_pat_group,
   init_options([(TerminalPat.strlit_pat.id, [])]),
 ),
 (
   TerminalPat.triv_pat_group,
   init_options([(TerminalPat.triv_pat.id, [])]),
 ),
 (
   TerminalPat.var_pat_group,
   init_options([(TerminalPat.var_pat.id, [])]),
 ),
 (
   TerminalPat.tag_pat_group,
   init_options([(TerminalPat.tag_pat.id, [])]),
 ),
 (
   ListPat.listlit_pat_group,
   init_options([(ListPat.listlit_pat.id, [])]),
 ),
 (
   ListPat.listnil_pat_group,
   init_options([(ListPat.listnil_pat.id, [])]),
 ),
 (
   ListPat.cons_pat_group,
   init_options([(ListPat.cons_base_pat.id, [])]),
 ),
 (
   ListPat.cons2_pat_group,
   init_options([
     (ListPat.cons_base_pat.id, [pat("p_tl")]),
     (ListPat.cons2_pat.id, [pat("p_snd"), cons_pat(), pat("p_tl")]),
   ]),
 ),
 (TuplePat.tuple_pat_group, init_options([(TuplePat.tuple_pat.id, [])])),
 (
   TuplePat.tuple_pat_2_group,
   init_options([
     (TuplePat.tuple_pat.id, [pat("p1"), comma_pat(), pat("...")]),
     (TuplePat.tuple_pat_size2.id, [pat("p1"), comma_pat(), pat("p2")]),
   ]),
 ),
 (
   TuplePat.tuple_pat_3_group,
   init_options([
     (TuplePat.tuple_pat.id, [pat("p1"), comma_pat(), pat("...")]),
     (
       TuplePat.tuple_pat_size3.id,
       [pat("p1"), comma_pat(), pat("p2"), comma_pat(), pat("p3")],
     ),
   ]),
 ),
 (AppPat.ap_pat_group, init_options([(AppPat.ap_pat.id, [])])),
 (
   TypAnnPat.typann_pat_group,
   init_options([(TypAnnPat.typann_pat.id, [])]),
 ),
 // Types
 (
   HoleTyp.empty_hole_typ_group,
   init_options([(HoleTyp.empty_hole_typ.id, [])]),
 ),
 (
   HoleTyp.multi_hole_typ_group,
   init_options([(HoleTyp.multi_hole_typ.id, [])]),
 ),
 (
   TerminalTyp.int_typ_group,
   init_options([(TerminalTyp.int_typ.id, [])]),
 ),
 (
   TerminalTyp.float_typ_group,
   init_options([(TerminalTyp.float_typ.id, [])]),
 ),
 (
   TerminalTyp.bool_typ_group,
   init_options([(TerminalTyp.bool_typ.id, [])]),
 ),
 (
   TerminalTyp.str_typ_group,
   init_options([(TerminalTyp.str_typ.id, [])]),
 ),
 (
   TerminalTyp.var_typ_group,
   init_options([(TerminalTyp.var_typ.id, [])]),
 ),
 (ListTyp.list_typ_group, init_options([(ListTyp.list_typ.id, [])])),
 (ArrowTyp.arrow_typ_group, init_options([(ArrowTyp.arrow_typ.id, [])])),
 (
   ArrowTyp.arrow3_typ_group,
   init_options([
     (ArrowTyp.arrow_typ.id, [typ("ty_out")]),
     (ArrowTyp.arrow3_typ.id, [typ("ty_arg2"), arrow(), typ("ty_out")]),
   ]),
 ),
 (TupleTyp.tuple_typ_group, init_options([(TupleTyp.tuple_typ.id, [])])),
 (
   TupleTyp.tuple2_typ_group,
   init_options([
     (TupleTyp.tuple_typ.id, [typ("ty1"), comma_typ(), typ("...")]),
     (TupleTyp.tuple2_typ.id, [typ("ty1"), comma_typ(), typ("ty2")]),
   ]),
 ),
 (
   TupleTyp.tuple3_typ_group,
   init_options([
     (TupleTyp.tuple_typ.id, [typ("ty1"), comma_typ(), typ("...")]),
     (
       TupleTyp.tuple3_typ.id,
       [typ("ty1"), comma_typ(), typ("ty2"), comma_typ(), typ("ty3")],
     ),
   ]),
 ),*/
//};
/*[@deriving (show({with_path: false}), sexp, yojson)]
  type persistent_example = {
    sub_id: string,
    feedback: feedback_option,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent_form = {
    id: string,
    explanation_feedback: feedback_option,
    examples: list(persistent_example),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent_form_group = {
    id: string,
    current_selection: int,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent_state = {
    show: bool,
    highlight: bool,
    specificity_open: bool,
    forms: list(persistent_form),
    groups: list(persistent_form_group),
  };

  let persist =
      ({show, highlight, specificity_open, forms, groups, _}: t)
      : persistent_state => {
    let persist_example = ({sub_id, feedback, _}: example): persistent_example => {
      {sub_id, feedback};
    };
    let persist_form = ({id, explanation, examples, _}: form): persistent_form => {
      let {feedback, _}: explanation = explanation;
      {
        id,
        explanation_feedback: feedback,
        examples: List.map(persist_example, examples),
      };
    };

    {
      show,
      highlight,
      specificity_open,
      forms: List.map(persist_form, forms),
      groups:
        List.map(
          ((group_id, group: form_group)) =>
            {id: group_id, current_selection: group.current_selection},
          groups,
        ),
    };
  };

  // TODO Make more robust to added messages
  let unpersist =
      ({show, highlight, specificity_open, forms, groups}: persistent_state): t => {
    let unpersist_examples = (persistent_examples, examples) => {
      let new_examples =
        List.filter(
          (example: example): bool =>
            Option.is_none(
              List.find_opt(
                ({sub_id, _}: persistent_example) => sub_id == example.sub_id,
                persistent_examples,
              ),
            ),
          examples,
        );
      List.filter_map(
        ({sub_id, feedback}: persistent_example) => {
          let init_example_opt = get_example_opt(sub_id, examples);
          switch (init_example_opt) {
          | Some(init_example) =>
            Some({
              sub_id,
              term: init_example.term,
              message: init_example.message,
              feedback,
            })
          | None => None
          };
        },
        persistent_examples,
      )
      @ new_examples;
    };

    let new_forms =
      List.filter(
        (form: form): bool =>
          Option.is_none(
            List.find_opt(({id, _}: persistent_form) => id == form.id, forms),
          ),
        init.forms,
      );
    let forms_unpersist =
      List.filter_map(
        ({id, explanation_feedback, examples}: persistent_form) => {
          let init_form_opt = get_form_opt(id, init.forms);
          switch (init_form_opt) {
          | Some(init_form) =>
            Some({
              id,
              syntactic_form: init_form.syntactic_form,
              expandable_id: init_form.expandable_id,
              explanation: {
                message: init_form.explanation.message,
                feedback: explanation_feedback,
              },
              examples: unpersist_examples(examples, init_form.examples),
            })
          | None => None
          };
        },
        forms,
      )
      @ new_forms;

    let new_groups =
      List.filter(
        ((group_id, _)): bool =>
          Option.is_none(
            List.find_opt(
              ({id, _}: persistent_form_group) => id == group_id,
              groups,
            ),
          ),
        init.groups,
      );
    let groups_unpersist =
      List.filter_map(
        ({id, current_selection}: persistent_form_group) => {
          let init_group_opt = get_group_opt(id, init);
          switch (init_group_opt) {
          | Some((_, init_group)) =>
            Some((id, {options: init_group.options, current_selection}))
          | None => None
          };
        },
        groups,
      )
      @ new_groups;
    {
      show,
      highlight,
      specificity_open,
      forms: forms_unpersist,
      groups: groups_unpersist,
    };
  };

  let serialize = (explainThisModel: t): string => {
    persist(explainThisModel)
    |> sexp_of_persistent_state
    |> Sexplib.Sexp.to_string;
  };

  let deserialize = (data: string) => {
    Sexplib.Sexp.of_string(data) |> persistent_state_of_sexp |> unpersist;
  };*/
