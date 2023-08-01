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
