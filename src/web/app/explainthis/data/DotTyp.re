open ExplainThisForm;
open Example;

let dot_typ_example: example = {
  sub_id: DotTyp,
  term: mk_example({|module M = { type T = Int } in
let x : M.T = 1 in x|}),
  message: "The type M.T projects the type member T from module M, resolving to Int.",
};

let _typ_module = typ("M");
let _typ_label = typ("T");

let dot_typ: form = {
  let explanation = "Projects a type member from a module or labeled tuple type. The left side names the module and the right side names the type alias exported by that module.";
  {
    id: DotTyp,
    syntactic_form: [_typ_module, dot_typ(), _typ_label],
    expandable_id: None,
    explanation,
    examples: [dot_typ_example],
  };
};

let dot: group = {
  id: DotTyp,
  forms: [dot_typ],
};
