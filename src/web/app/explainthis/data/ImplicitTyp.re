open ExplainThisForm;
open Example;

let single: Simple.t = {
  group_id: ImplicitTyp,
  form_id: ImplicitTyp,
  abstract: (
    [
      typ("implicit"),
      space(),
      typ("S"),
      space(),
      typ(":"),
      space(),
      typ("SIG"),
    ],
    [],
  ),
  explanation: "An implicit module binder in a function type: a parameter component of signature SIG that callers may omit, resolved at each call from the implicit instances in scope. S names the module's type members in the later components and in the result, as in (implicit S : SHOW, S.T) -> String.",
  examples: [
    {
      sub_id: ImplicitTyp1,
      term:
        mk_example(
          "type SHOW = {\ntype T;\nlet show : T -> String\n} in\nlet implicit ShowInt = {\ntype T = Int;\nlet show = string_of_int\n} in\nlet show : (implicit S : SHOW, S.T) -> String =\nfun (implicit S : SHOW, x : S.T) -> S.show(x) in\nshow(3)",
        ),
      message: "The annotation names the implicit component; S.T is the type of the explicit one and refers to the instance chosen at each call.",
    },
  ],
};
