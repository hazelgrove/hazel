open ExplainThisForm;
open ExplainThisModel;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type update =
  | SpecificityOpen(bool)
  | ToggleExplanationFeedback(group_id, form_id, feedback_option)
  | ToggleExampleFeedback(group_id, form_id, example_id, feedback_option)
  | UpdateGroupSelection(group_id, form_id);

let set_update =
    (explainThisModel: ExplainThisModel.t, u: update)
    : Updated.t(ExplainThisModel.t) => {
  (
    switch (u) {
    | SpecificityOpen(b) => {...explainThisModel, specificity_open: b}
    | ToggleExplanationFeedback(group_id, form_id, feedback_option) =>
      let (pre, form, post) =
        ListUtil.split(explainThisModel.forms, f =>
          f.form == form_id && f.group == group_id
        );
      let form =
        switch (form) {
        | Some(form) =>
          let feedback =
            switch (form.explanation_feedback, feedback_option) {
            | (Some(ThumbsUp), ThumbsDown)
            | (Some(ThumbsDown), ThumbsUp)
            | (None, _) => Some(feedback_option)
            | (Some(ThumbsUp), ThumbsUp)
            | (Some(ThumbsDown), ThumbsDown) => None
            };
          {...form, explanation_feedback: feedback};
        | None => {
            group: group_id,
            form: form_id,
            explanation_feedback: Some(feedback_option),
            examples: [],
          }
        };
      {...explainThisModel, forms: pre @ [form] @ post};
    | ToggleExampleFeedback(group_id, form_id, example_id, feedback_option) =>
      let (pre_form, form, post_form) =
        ListUtil.split(explainThisModel.forms, f =>
          f.form == form_id && f.group == group_id
        );
      let form: form_model =
        switch (form) {
        | Some(form) =>
          let (pre_example, example, post_example) =
            ListUtil.split(form.examples, e => e.sub_id == example_id);
          let examples: list(example_model) =
            switch (example) {
            | Some(example) =>
              switch (example.feedback, feedback_option) {
              | (ThumbsUp, ThumbsDown)
              | (ThumbsDown, ThumbsUp) =>
                pre_example
                @ [{...example, feedback: feedback_option}]
                @ post_example
              | (ThumbsUp, ThumbsUp)
              | (ThumbsDown, ThumbsDown) => pre_example @ post_example
              }
            | None =>
              pre_example
              @ [{sub_id: example_id, feedback: feedback_option}]
              @ post_example
            };
          {...form, examples};
        | None => {
            group: group_id,
            form: form_id,
            explanation_feedback: None,
            examples: [{sub_id: example_id, feedback: feedback_option}],
          }
        };
      {...explainThisModel, forms: pre_form @ [form] @ post_form};
    | UpdateGroupSelection(group_id, form_id) =>
      let (pre_group, _group, post_group) =
        ListUtil.split(explainThisModel.groups, g => g.group == group_id);
      {
        ...explainThisModel,
        groups:
          pre_group @ [{group: group_id, selected: form_id}] @ post_group,
      };
    }
  )
  |> Updated.return_quiet(~logged=true);
};
