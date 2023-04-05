open Sexplib.Std;
open ExplainThisForm;

[@deriving (show({with_path: false}), sexp, yojson)]
type update =
  | ToggleShow
  | ToggleHighlight
  | SpecificityOpen(bool)
  | ToggleExplanationFeedback(group_id, form_id, feedback_option)
  | ToggleExampleFeedback(group_id, form_id, example_id, feedback_option)
  | UpdateGroupSelection(group_id, form_id);

let set_update =
    (docLangMessages: ExplainThisMessages.t, u: update): ExplainThisMessages.t => {
  let update_feedback = (old_feedback, new_feedback) =>
    switch (old_feedback, new_feedback) {
    | (Unselected, _) => new_feedback
    | (ThumbsUp, ThumbsUp)
    | (ThumbsDown, ThumbsDown) => Unselected
    | (ThumbsUp, ThumbsDown)
    | (ThumbsDown, ThumbsUp)
    | (_, Unselected) => new_feedback
    };
  let update_messages =
      (
        group_id: group_id,
        form_id: form_id,
        toggle_feedback_form,
        toggle_feedback_form_option,
      ) => {
    let messages = docLangMessages.messages;
    switch (group_id, form_id) {
    | (EmptyHoleExp, EmptyHoleExp) => {
        ...messages,
        empty_hole_exp_group: {
          ...messages.empty_hole_exp_group,
          empty_hole_exp:
            toggle_feedback_form(
              messages.empty_hole_exp_group.empty_hole_exp,
            ),
        },
      }
    | (MultiHoleExp, MultiHoleExp) => {
        ...messages,
        multi_hole_exp_group: {
          ...messages.multi_hole_exp_group,
          multi_hole_exp:
            toggle_feedback_form(
              messages.multi_hole_exp_group.multi_hole_exp,
            ),
        },
      }
    | (TrivExp, TrivExp) => {
        ...messages,
        triv_exp_group: {
          ...messages.triv_exp_group,
          triv_exp: toggle_feedback_form(messages.triv_exp_group.triv_exp),
        },
      }
    | (BoolExp, BoolExp) => {
        ...messages,
        bool_exp_group: {
          ...messages.bool_exp_group,
          bool_exp: toggle_feedback_form(messages.bool_exp_group.bool_exp),
        },
      }
    | (IntExp, IntExp) => {
        ...messages,
        int_exp_group: {
          ...messages.int_exp_group,
          int_exp: toggle_feedback_form(messages.int_exp_group.int_exp),
        },
      }
    | (FloatExp, FloatExp) => {
        ...messages,
        float_exp_group: {
          ...messages.float_exp_group,
          float_exp: toggle_feedback_form(messages.float_exp_group.float_exp),
        },
      }
    | (StringExp, StringExp) => {
        ...messages,
        string_exp_group: {
          ...messages.string_exp_group,
          string_exp:
            toggle_feedback_form(messages.string_exp_group.string_exp),
        },
      }
    | (VarExp, VarExp) => {
        ...messages,
        var_exp_group: {
          ...messages.var_exp_group,
          var_exp: toggle_feedback_form(messages.var_exp_group.var_exp),
        },
      }
    | (TagExp, TagExp) => {
        ...messages,
        tag_exp_group: {
          ...messages.tag_exp_group,
          tag_exp: toggle_feedback_form(messages.tag_exp_group.tag_exp),
        },
      }
    | (FunctionExp, FunctionExp) => {
        ...messages,
        function_exp_group: {
          ...messages.function_exp_group,
          function_exp:
            toggle_feedback_form(messages.function_exp_group.function_exp),
        },
      }
    | (FunctionEmptyHole, FunctionExp) => {
        ...messages,
        function_empty_hole_group: {
          ...messages.function_empty_hole_group,
          function_exp:
            toggle_feedback_form_option(
              messages.function_empty_hole_group.function_exp,
            ),
        },
      }
    | (FunctionEmptyHole, FunctionEmptyHole) => {
        ...messages,
        function_empty_hole_group: {
          ...messages.function_empty_hole_group,
          function_empty_hole_exp:
            toggle_feedback_form_option(
              messages.function_empty_hole_group.function_empty_hole_exp,
            ),
        },
      }
    | (FunctionMultiHole, FunctionExp) => {
        ...messages,
        function_multi_hole_group: {
          ...messages.function_multi_hole_group,
          function_exp:
            toggle_feedback_form_option(
              messages.function_multi_hole_group.function_exp,
            ),
        },
      }
    | (FunctionMultiHole, FunctionMultiHole) => {
        ...messages,
        function_multi_hole_group: {
          ...messages.function_multi_hole_group,
          function_multi_hole_exp:
            toggle_feedback_form_option(
              messages.function_multi_hole_group.function_multi_hole_exp,
            ),
        },
      }
    | (FunctionWild, FunctionExp) => {
        ...messages,
        function_wild_group: {
          ...messages.function_wild_group,
          function_exp:
            toggle_feedback_form_option(
              messages.function_wild_group.function_exp,
            ),
        },
      }
    | (FunctionWild, FunctionWild) => {
        ...messages,
        function_wild_group: {
          ...messages.function_wild_group,
          function_wild_exp:
            toggle_feedback_form_option(
              messages.function_wild_group.function_wild_exp,
            ),
        },
      }
    | (FunctionInt, FunctionExp) => {
        ...messages,
        function_int_group: {
          ...messages.function_int_group,
          function_exp:
            toggle_feedback_form_option(
              messages.function_int_group.function_exp,
            ),
        },
      }
    | (FunctionInt, FunctionInt) => {
        ...messages,
        function_int_group: {
          ...messages.function_int_group,
          function_intlit_exp:
            toggle_feedback_form_option(
              messages.function_int_group.function_intlit_exp,
            ),
        },
      }
    | (FunctionFloat, FunctionExp) => {
        ...messages,
        function_float_group: {
          ...messages.function_float_group,
          function_exp:
            toggle_feedback_form_option(
              messages.function_float_group.function_exp,
            ),
        },
      }
    | (FunctionFloat, FunctionFloat) => {
        ...messages,
        function_float_group: {
          ...messages.function_float_group,
          function_floatlit_exp:
            toggle_feedback_form_option(
              messages.function_float_group.function_floatlit_exp,
            ),
        },
      }
    | (FunctionBool, FunctionExp) => {
        ...messages,
        function_bool_group: {
          ...messages.function_bool_group,
          function_exp:
            toggle_feedback_form_option(
              messages.function_bool_group.function_exp,
            ),
        },
      }
    | (FunctionBool, FunctionBool) => {
        ...messages,
        function_bool_group: {
          ...messages.function_bool_group,
          function_boollit_exp:
            toggle_feedback_form_option(
              messages.function_bool_group.function_boollit_exp,
            ),
        },
      }
    | _ =>
      raise(
        Invalid_argument(
          "Form id "
          ++ Sexplib.Sexp.to_string(
               ExplainThisForm.sexp_of_form_id(form_id),
             )
          ++ " is not handled for group id "
          ++ Sexplib.Sexp.to_string(
               ExplainThisForm.sexp_of_group_id(group_id),
             ),
        ),
      )
    };
  };
  switch (u) {
  | ToggleShow => {...docLangMessages, show: !docLangMessages.show}
  | ToggleHighlight => {
      ...docLangMessages,
      highlight: !docLangMessages.highlight,
    }
  | SpecificityOpen(b) => {...docLangMessages, specificity_open: b}
  | ToggleExplanationFeedback(group_id, form_id, feedback_option) =>
    let toggle_explanation_feedback_form = (form: form) => {
      ...form,
      explanation: {
        ...form.explanation,
        feedback: update_feedback(form.explanation.feedback, feedback_option),
      },
    };
    let toggle_explanation_feedback_form_option = (form_option: form_option) => {
      ...form_option,
      form: toggle_explanation_feedback_form(form_option.form),
    };
    {
      ...docLangMessages,
      messages:
        update_messages(
          group_id,
          form_id,
          toggle_explanation_feedback_form,
          toggle_explanation_feedback_form_option,
        ),
    };
  | ToggleExampleFeedback(group_id, form_id, example_id, feedback_option) =>
    let rec update_example = examples => {
      switch (examples) {
      | [] =>
        raise(
          Invalid_argument(
            "Example id "
            ++ Sexplib.Sexp.to_string(
                 ExplainThisForm.sexp_of_example_id(example_id),
               )
            ++ " is not an example for Form id "
            ++ Sexplib.Sexp.to_string(
                 ExplainThisForm.sexp_of_form_id(form_id),
               )
            ++ " in group id "
            ++ Sexplib.Sexp.to_string(
                 ExplainThisForm.sexp_of_group_id(group_id),
               ),
          ),
        )
      | [x, ...xs] =>
        if (x.sub_id == example_id) {
          let new_example = {
            ...x,
            feedback: update_feedback(x.feedback, feedback_option),
          };
          [new_example, ...xs];
        } else {
          [x, ...update_example(xs)];
        }
      };
    };
    let toggle_example_feedback_form = (form: form) => {
      {...form, examples: update_example(form.examples)};
    };
    let toggle_example_feedback_form_option = (form_option: form_option) => {
      ...form_option,
      form: toggle_example_feedback_form(form_option.form),
    };
    {
      ...docLangMessages,
      messages:
        update_messages(
          group_id,
          form_id,
          toggle_example_feedback_form,
          toggle_example_feedback_form_option,
        ),
    };
  | UpdateGroupSelection(group_id, new_selection_id) =>
    let set_new_selection = form_option => {
      ...form_option,
      selected: form_option.form.id == new_selection_id,
    };
    let messages = docLangMessages.messages;
    let new_messages =
      switch (group_id) {
      | EmptyHoleExp
      | MultiHoleExp
      | TrivExp
      | BoolExp
      | IntExp
      | FloatExp
      | StringExp
      | VarExp
      | TagExp
      | FunctionExp =>
        raise(
          Invalid_argument(
            "Group id "
            ++ Sexplib.Sexp.to_string(
                 ExplainThisForm.sexp_of_group_id(group_id),
               )
            ++ "does not handle group selections",
          ),
        )
      | FunctionEmptyHole =>
        let group = messages.function_empty_hole_group;
        print_endline(
          "Group id "
          ++ Sexplib.Sexp.to_string(
               ExplainThisForm.sexp_of_group_id(group_id),
             )
          ++ "trying to update with new selection id "
          ++ Sexplib.Sexp.to_string(
               ExplainThisForm.sexp_of_form_id(new_selection_id),
             ),
        );
        {
          ...messages,
          function_empty_hole_group: {
            id: group.id,
            function_exp: set_new_selection(group.function_exp),
            function_empty_hole_exp:
              set_new_selection(group.function_empty_hole_exp),
          },
        };
      | FunctionMultiHole =>
        let group = messages.function_multi_hole_group;
        {
          ...messages,
          function_multi_hole_group: {
            id: group.id,
            function_exp: set_new_selection(group.function_exp),
            function_multi_hole_exp:
              set_new_selection(group.function_multi_hole_exp),
          },
        };
      | FunctionWild =>
        let group = messages.function_wild_group;
        {
          ...messages,
          function_wild_group: {
            id: group.id,
            function_exp: set_new_selection(group.function_exp),
            function_wild_exp: set_new_selection(group.function_wild_exp),
          },
        };
      | FunctionInt =>
        let group = messages.function_int_group;
        {
          ...messages,
          function_int_group: {
            id: group.id,
            function_exp: set_new_selection(group.function_exp),
            function_intlit_exp: set_new_selection(group.function_intlit_exp),
          },
        };
      | FunctionFloat =>
        let group = messages.function_float_group;
        {
          ...messages,
          function_float_group: {
            id: group.id,
            function_exp: set_new_selection(group.function_exp),
            function_floatlit_exp:
              set_new_selection(group.function_floatlit_exp),
          },
        };
      | FunctionBool =>
        let group = messages.function_bool_group;
        {
          ...messages,
          function_bool_group: {
            id: group.id,
            function_exp: set_new_selection(group.function_exp),
            function_boollit_exp:
              set_new_selection(group.function_boollit_exp),
          },
        };
      };
    {...docLangMessages, messages: new_messages};
  };
};
