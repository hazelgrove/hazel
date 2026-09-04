open Haz3lcore;
open ExplainThisForm;
open Util_web;

[@deriving (show({with_path: false}), sexp, yojson)]
type feedback_option =
  | ThumbsUp
  | ThumbsDown;

[@deriving (show({with_path: false}), sexp, yojson)]
type example_model = {
  sub_id: example_id,
  feedback: feedback_option,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type form_model = {
  group: group_id,
  form: form_id,
  explanation_feedback: option(feedback_option),
  examples: list(example_model),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type group_model = {
  group: group_id,
  selected: form_id,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  specificity_open: bool,
  forms: list(form_model),
  groups: list(group_model),
};

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type highlight =
    | NoHighlight
    | One(Id.t)
    | All;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    show: bool,
    show_feedback: bool,
    highlight,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type highlight_action =
    | Toggle
    | Hover(Id.t)
    | UnsetHover;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleShowFeedback
    | SetHighlight(highlight_action);
};

let init: t = {
  specificity_open: false,
  forms: [],
  groups: [],
};

let get_explanation_feedback =
    (group_id: group_id, form_id: form_id, model: t): option(feedback_option) => {
  let forms =
    List.filter(
      (form: form_model) => form.group == group_id && form.form == form_id,
      model.forms,
    );
  /* A (group, form) pair should appear at most once, but this reads persisted
     state that a previous version wrote, so tolerate a duplicate rather than
     taking down the sidebar over a stale thumbs-up. */
  switch (forms) {
  | [] => None
  | [form, ..._] => form.explanation_feedback
  };
};

let get_example_feedback =
    (group_id: group_id, form_id: form_id, example_id: example_id, model: t)
    : option(feedback_option) => {
  let forms =
    List.filter(
      (form: form_model) => form.group == group_id && form.form == form_id,
      model.forms,
    );

  switch (forms) {
  | [] => None
  | [form, ..._] =>
    let examples =
      List.filter(
        (example: example_model) => example.sub_id == example_id,
        form.examples,
      );
    switch (examples) {
    | [] => None
    | [example, ..._] => Some(example.feedback)
    };
  };
};

/* Falls back to the most specific form rather than raising: `form_id` can come
   from persisted state naming a form that has since been renamed or removed. */
let get_form_in_group = (form_id: form_id, group: group): option(form) =>
  switch (List.find_opt((form: form) => form.id == form_id, group.forms)) {
  | Some(form) => Some(form)
  | None => List.nth_opt(group.forms, 0)
  };

let get_selected_option = (group: group, model: t): option(form) => {
  let selected =
    List.filter(
      (group': group_model) => group'.group == group.id,
      model.groups,
    );
  switch (selected) {
  /* No recorded selection means the most specific form. A group should have at
     most one selection; if stale state carries more, honour the first. */
  | [] => List.nth_opt(group.forms, 0)
  | [selected, ..._] => get_form_in_group(selected.selected, group)
  };
};

/* Only forms that name an anchor can appear in the specificity menu, since the
   menu is drawn at that anchor. No reachable multi-form group has an anchorless
   form today; skipping rather than raising over one keeps a future addition from
   taking down the sidebar. */
let get_options = (group: group): list((form_id, Segment.t)) =>
  List.length(group.forms) < 2
    ? []
    : List.rev(
        List.filter_map(
          (form: form) =>
            Option.map(
              ((_anchor, segment)) => (form.id, segment),
              form.expandable_id,
            ),
          group.forms,
        ),
      );

/* `None` means the group has no forms at all, which no group constructor can
   produce — but saying so in the type is what lets this module be free of
   raises. */
let get_form_and_options =
    (group: group, model: t): (option(form), list((form_id, Segment.t))) => (
  get_selected_option(group, model),
  get_options(group),
);

// To prevent OCaml thinking t is a recursive type lower down
[@deriving (show({with_path: false}), yojson, sexp)]
type explainthismodel = t;

module Store =
  Store.F({
    [@deriving (show({with_path: false}), yojson, sexp)]
    type t = explainthismodel;
    let default = () => init;
    let key = Store.ExplainThis;
  });
