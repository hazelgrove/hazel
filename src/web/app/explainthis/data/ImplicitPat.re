open ExplainThisForm;
open Example;

let single: Simple.t = {
  group_id: ImplicitPat,
  form_id: ImplicitPat,
  abstract: (
    [
      pat("implicit"),
      space(),
      pat("S"),
      space(),
      typeann(),
      space(),
      typ("SIG"),
    ],
    [],
  ),
  explanation: "An implicit module binder. As a function parameter component, S is a module of signature SIG that callers may leave out: a call with only the explicit arguments resolves S to the unique implicit instance in scope whose signature fits and whose type members agree with the other arguments, while passing a module in S's position supplies it explicitly. As a let binder, implicit declares the module an instance for such resolution.",
  examples: [
    {
      sub_id: ImplicitPat1,
      term:
        mk_example(
          "type SHOW = {\ntype T;\nlet show : T -> String\n} in\nlet implicit ShowInt = {\ntype T = Int;\nlet show = string_of_int\n} in\nlet show = fun (implicit S : SHOW, x : S.T) -> S.show(x) in\nshow(3)",
        ),
      message: "show(3) resolves S to ShowInt, the instance whose T is Int; show(ShowInt, 3) would pass it explicitly.",
    },
  ],
};
