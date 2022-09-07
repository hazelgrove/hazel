open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type feedback_option =
  | ThumbsUp
  | ThumbsDown
  | Unselected;

[@deriving (show({with_path: false}), sexp, yojson)]
type example = {
  sub_id: string,
  term: Segment.t,
  message: string,
  feedback: feedback_option,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type explanation = {
  message: string,
  feedback: feedback_option,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type form = {
  id: string,
  syntactic_form: Segment.t,
  expandable_id: option(Id.t),
  explanation,
  examples: list(example),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type form_group = {
  options: list((string, Segment.t)),
  current_selection: int,
};

let cons = () => Example.mk_monotile(Form.get("cons_exp"));
let exp = v =>
  Example.mk_monotile(Form.mk(Form.ss, [v], Mold.(mk_op(Exp, []))));
let int = n => Example.mk_monotile(Form.mk_atomic(Exp, n));

let cons_exp: form = {
  let nil = () => exp("nil");
  let left = exp("EXP_hd");
  let right = exp("EXP_tl");
  let example_1 = {
    sub_id: "example_1",
    term: [int("1"), cons(), nil()],
    message: "A single element list of 1.",
    feedback: Unselected,
  };
  let example_2 = {
    sub_id: "example_2",
    term: [int("1"), cons(), int("2"), cons(), nil()],
    message: "A list with two elements, 1 and 2.",
    feedback: Unselected,
  };
  let explanation = {
    message: "Cons operator to make list with [*head*](%i) and [*tail*](%i)",
    feedback: Unselected,
  };
  {
    id: "cons_exp",
    syntactic_form: [left, cons(), right],
    expandable_id: None,
    explanation,
    examples: [example_1, example_2],
  };
};

// Just have a flat list of forms w/ their explanations and examples
// Keep track of options/groups in a separate structure
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  show: bool,
  highlight: bool,
  specificity_open: bool,
  forms: list(form),
  groups: list((string, form_group)),
};

let get_form_and_options = (group_id, doc: t) => {
  let (_, form_group) = List.find(((id, _)) => id == group_id, doc.groups);
  let (selected_id, _) =
    List.nth(form_group.options, form_group.current_selection);
  let form = List.find(({id, _}) => id == selected_id, doc.forms);
  (form, form_group.options);
};

let get_example = (example_sub_id, docs) =>
  List.find(({sub_id, _}) => sub_id == example_sub_id, docs);

let get_form = (form_id, docs) =>
  List.find(({id, _}) => id == form_id, docs);

let rec update_form = (new_form, docs) => {
  switch (docs) {
  | [] => []
  | [x, ...xs] =>
    if (x.id == new_form.id) {
      [new_form, ...xs];
    } else {
      update_form(new_form, xs);
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
      update_example(new_example, xs);
    }
  };
};

let init = {
  show: true,
  highlight: true,
  specificity_open: false,
  forms: [cons_exp],
  groups: [
    ("cons_exp", {options: [(cons_exp.id, [])], current_selection: 0}),
  ],
};

[@deriving (show({with_path: false}), sexp, yojson)]
type update =
  | ToggleShow
  | ToggleHighlight
  | SpecificityOpen(bool)
  | ToggleExplanationFeedback(string, feedback_option)
  | ToggleExampleFeedback(string, string, feedback_option);

let set_update = (docLangMessages: t, u: update): t => {
  switch (u) {
  | ToggleShow => {...docLangMessages, show: !docLangMessages.show}
  | ToggleHighlight => {
      ...docLangMessages,
      highlight: !docLangMessages.highlight,
    }
  | SpecificityOpen(b) => {...docLangMessages, specificity_open: b}
  | ToggleExplanationFeedback(id, feedback_option) =>
    let form = get_form(id, docLangMessages.forms);
    let explanation =
      switch (form.explanation.feedback, feedback_option) {
      | (Unselected, _) => {...form.explanation, feedback: feedback_option}
      | (ThumbsUp, ThumbsUp)
      | (ThumbsDown, ThumbsDown) => {
          ...form.explanation,
          feedback: Unselected,
        }
      | (ThumbsUp, ThumbsDown)
      | (ThumbsDown, ThumbsUp)
      | (_, Unselected) => {...form.explanation, feedback: feedback_option}
      };
    {
      ...docLangMessages,
      forms: update_form({...form, explanation}, docLangMessages.forms),
    };
  | ToggleExampleFeedback(id, sub_id, feedback_option) =>
    let form = get_form(id, docLangMessages.forms);
    let example = get_example(sub_id, form.examples);
    let new_example =
      switch (example.feedback, feedback_option) {
      | (Unselected, _) => {...example, feedback: feedback_option}
      | (ThumbsUp, ThumbsUp)
      | (ThumbsDown, ThumbsDown) => {...example, feedback: Unselected}
      | (ThumbsUp, ThumbsDown)
      | (ThumbsDown, ThumbsUp)
      | (_, Unselected) => {...example, feedback: feedback_option}
      };
    {
      ...docLangMessages,
      forms:
        update_form(
          {...form, examples: update_example(new_example, form.examples)},
          docLangMessages.forms,
        ),
    };
  };
};
