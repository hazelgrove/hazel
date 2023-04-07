open Sexplib.Std;
module Sexp = Sexplib.Sexp;
open Haz3lcore;
open ExplainThisForm;
open Util;

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
  show: bool,
  highlight: bool,
  specificity_open: bool,
  forms: list(form_model),
  groups: list(group_model),
};

let init: t = {
  show: true,
  highlight: true,
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
  switch (forms) {
  | [] => None
  | [form] => form.explanation_feedback
  | _ =>
    raise(
      Invalid_argument(
        "Each form, group pair should only appear once, but "
        ++ Sexp.to_string(sexp_of_form_id(form_id))
        ++ ", "
        ++ Sexp.to_string(sexp_of_group_id(group_id))
        ++ " appears "
        ++ string_of_int(List.length(forms))
        ++ " times",
      ),
    )
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
  | [form] =>
    let examples =
      List.filter(
        (example: example_model) => example.sub_id == example_id,
        form.examples,
      );
    switch (examples) {
    | [] => None
    | [example] => Some(example.feedback)
    | _ =>
      raise(
        Invalid_argument(
          "Each group, form, example triple should only appear once, but "
          ++ Sexp.to_string(sexp_of_group_id(group_id))
          ++ ", "
          ++ Sexp.to_string(sexp_of_form_id(form_id))
          ++ ", "
          ++ Sexp.to_string(sexp_of_example_id(example_id))
          ++ " appears "
          ++ string_of_int(List.length(examples))
          ++ " times",
        ),
      )
    };
  | _ =>
    raise(
      Invalid_argument(
        "Each group, form pair should only appear once, but "
        ++ Sexp.to_string(sexp_of_group_id(group_id))
        ++ ", "
        ++ Sexp.to_string(sexp_of_form_id(form_id))
        ++ " appears "
        ++ string_of_int(List.length(forms))
        ++ " times",
      ),
    )
  };
};

let get_form_in_group = (form_id: form_id, group: group): form => {
  OptUtil.get_or_raise(
    Invalid_argument(
      "Form "
      ++ Sexp.to_string(sexp_of_form_id(form_id))
      ++ " is not in group "
      ++ Sexp.to_string(sexp_of_group_id(group.id)),
    ),
    List.find_opt((form: form) => form.id == form_id, group.forms),
  );
};

let get_selected_option = (group: group, model: t): form => {
  let selected =
    List.filter(
      (group': group_model) => group'.group == group.id,
      model.groups,
    );
  switch (selected, group.forms) {
  | ([], [form, ..._fs]) => form
  | ([selected], _) => get_form_in_group(selected.selected, group)
  | ([_f1, _f2, ..._fs], _) =>
    raise(
      Invalid_argument(
        "Each group should have only one selection, but group "
        ++ Sexp.to_string(sexp_of_group_id(group.id))
        ++ " has "
        ++ string_of_int(List.length(selected))
        ++ "forms selected",
      ),
    )
  | (_, []) =>
    raise(Invalid_argument("Each group must have at least one form"))
  };
};

let get_options = (group: group): list((form_id, Segment.t)) =>
  if (List.length(group.forms) < 2) {
    [];
  } else {
    List.rev(
      List.map(
        (form: form) =>
          (
            form.id,
            snd(
              OptUtil.get_or_raise(
                Invalid_argument(
                  "Forms used for group options must specify expandable",
                ),
                form.expandable_id,
              ),
            ),
          ),
        group.forms,
      ),
    );
  };

let get_form_and_options =
    (group: group, model: t): (form, list((form_id, Segment.t))) => {
  (get_selected_option(group, model), get_options(group));
};
