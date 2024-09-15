open Haz3lcore;
open Virtual_dom.Vdom;
open Util;
open ExplainThisForm;

open DrvSyntax;

let highlight = (msg: list(Node.t), id: Id.t, mapping: ColorSteps.t): Node.t => {
  let (c, _) = ColorSteps.get_color(id, mapping);
  let classes = Attr.class_("highlight-" ++ c);
  let attrs = [classes];
  Node.span(~attrs, msg);
};
let rec repr = (p: int, prop: t, ~color_map: ColorSteps.t): Node.t => {
  let p' = precedence(prop);
  let repr = repr(p', ~color_map);
  let repr_aba = (as_: list(string), bs: list(t)): Node.t =>
    Aba.mk(as_, bs) |> Aba.join(Node.text, repr) |> Node.span;
  let repr_aba_tight = (as_: list(string), bs: list(t)) =>
    Aba.mk(as_, bs) |> Aba.join(Node.text, repr) |> Node.span;
  let repr_binop = (op: string, a: t, b: t) =>
    repr_aba(["", op, ""], [a, b]);
  let repr_preop = (op, a: t) => repr_aba([op, ""], [a]);
  let repr_postop = (op, a: t) => repr_aba(["", op], [a]);
  (
    switch (IdTagged.term_of(prop)) {
    | Hole(s) => Printf.sprintf("[%s]", s) |> Node.text
    | Atom(s) => s |> Node.text
    | And(a, b) => repr_binop("∧", a, b)
    | Or(a, b) => repr_binop("∨", a, b)
    | Impl(a, b) when IdTagged.term_of(b) == Falsity => repr_postop("¬", a)
    | Impl(a, b) => repr_binop("⊃", a, b)
    | Truth => "⊤" |> Node.text
    | Falsity => "⊥" |> Node.text
    | Ctx(ctx) =>
      if (List.length(ctx) == 0) {
        "·" |> Node.text;
      } else {
        ctx |> List.map(repr) |> Node.span;
      }
    | Entail(a, b) => repr_binop("⊢", a, b)
    | NumLit(i) => string_of_int(i) |> Node.text
    | Val(a) => repr_postop(".val", a)
    | Neg(a) => repr_preop("-", a)
    | Plus(a, b) => repr_binop("+", a, b)
    | Minus(a, b) => repr_binop("-", a, b)
    | Times(a, b) => repr_binop("*", a, b)
    | Lt(a, b) => repr_binop("<", a, b)
    | Gt(a, b) => repr_binop(">", a, b)
    | Eq(a, b) => repr_binop("==", a, b)
    | Eval(a, b) => repr_binop("⇓", a, b)
    | Num => "Num" |> Node.text
    | Bool => "Bool" |> Node.text
    | Arrow(a, b) => repr_binop("→", a, b)
    | Prod(a, b) => repr_binop("×", a, b)
    | Unit => "Unit" |> Node.text
    | Sum(a, b) => repr_binop("+", a, b)
    | TVar(x) => x |> Node.text
    | Rec(x, a) => repr_aba(["rec", "→", ""], [x, a])
    | True => "True" |> Node.text
    | False => "False" |> Node.text
    | If(a, b, c) => repr_aba(["if", "then", "else", ""], [a, b, c])
    | Var(x) => x |> Node.text
    | Let(x, a, b) => repr_aba(["let", "→", "in", ""], [x, a, b])
    | LetAnn(x, t, a, b) =>
      repr_aba(["let", ":", "→", "in", ""], [x, t, a, b])
    | Fix(x, a) => repr_aba(["fix", "→", ""], [x, a])
    | FixAnn(x, t, a) => repr_aba(["fix", ":", "→", ""], [x, t, a])
    | Fun(x, a) => repr_aba(["fun", "→", ""], [x, a])
    | FunAnn(x, t, a) => repr_aba(["fun", ":", "→", ""], [x, t, a])
    | Ap(a, b) => repr_aba_tight(["", "(", ")"], [a, b])
    | Pair(a, b) => repr_aba_tight(["(", ",", ")"], [a, b])
    | Triv => "()" |> Node.text
    | PrjL(a) => repr_postop(".fst", a)
    | PrjR(a) => repr_postop(".snd", a)
    | LetPair(x, y, a, b) =>
      repr_aba(["let (", ",", ") =", "in", ""], [x, y, a, b])
    | InjL(a) => repr_preop("L", a)
    | InjR(a) => repr_preop("R", a)
    | Case(a, x, b, y, c) =>
      repr_aba(
        ["case", "of L", "→", "else R", "→", ""],
        [a, x, b, y, c],
      )
    | Roll(a) => repr_aba_tight(["roll(", ")"], [a])
    | Unroll(a) => repr_aba_tight(["unroll(", ")"], [a])
    | TPat(x) => x |> Node.text
    | Pat(x) => x |> Node.text
    | HasTy(a, b) => repr_binop(":", a, b)
    | Syn(a, b) => repr_binop("⇒", a, b)
    | Ana(a, b) => repr_binop("⇐", a, b)
    }
  )
  |> (
    x =>
      p < p' ? Node.span([Node.text("("), x, Node.text(")")]) : Fun.id(x)
  )
  |> (
    x =>
      switch (
        Haz3lcore.Id.Map.find_opt(IdTagged.rep_id(prop), fst(color_map))
      ) {
      | None => x
      | Some(_) => highlight([x], IdTagged.rep_id(prop), color_map)
      }
  );
};

let repr = repr(P.min);

let copy_color_map =
    (terms: list(RuleVerify.t), (map, idx): ColorSteps.t): ColorSteps.t => {
  (
    List.fold_left(
      (new_map, RuleVerify.{self, ghost}) => {
        let id = IdTagged.rep_id(self);
        let ghost_id = ghost |> Option.get |> IdTagged.rep_id;
        let color = Haz3lcore.Id.Map.find_opt(id, map) |> Option.get;
        Haz3lcore.Id.Map.add(ghost_id, color, new_map);
      },
      Haz3lcore.Id.Map.empty,
      terms,
    ),
    idx,
  );
};

let copy_color_map =
    (failure: RuleVerify.failure, color_map: ColorSteps.t): ColorSteps.t => {
  let terms: list(RuleVerify.t) =
    switch (failure) {
    | PremiseMismatch(_) => []
    | FailUnbox(_, p) => [p]
    | NotAList(p) => [p]
    | NotEqual(p1, p2) => [p1, p2]
    | FailTest(p, op) =>
      [p]
      @ (
        switch (op) {
        | Neg(p) => [p]
        | Plus(p1, p2) => [p1, p2]
        | Minus(p1, p2) => [p1, p2]
        | Times(p1, p2) => [p1, p2]
        | Lt(p) => [p]
        | NotLt(p) => [p]
        | Gt(p) => [p]
        | NotGt(p) => [p]
        | Eq(p) => [p]
        | NotEq(p) => [p]
        | Subst((p1, p2), p3) => [p1, p2, p3]
        | Subst2((p1, p2), (p3, p4), p5) => [p1, p2, p3, p4, p5]
        | SubstTy((p1, p2), p3) => [p1, p2, p3]
        | Cons(p1, p2) => [p1, p2]
        | ConsHasTy((p1, p2), p3) => [p1, p2, p3]
        | ConsHasTy2((p1, p2), (p3, p4), p5) => [p1, p2, p3, p4, p5]
        | Mem(p) => [p]
        | MemHasTy(p1, p2) => [p1, p2]
        }
      )
    };
  copy_color_map(terms, color_map);
};

let conclusion_view = (~syntax: DrvSyntax.t, ~color_map: ColorSteps.t) =>
  Node.div(
    ~attrs=[Attr.class_("deduction-concl")],
    [repr(syntax, ~color_map)],
  );

let rule_to_label =
  fun
  | Some(rule) => Rule.repr(rule)
  | None => "?";

let label_view = (~label) =>
  Node.div(~attrs=[Attr.class_("deduction-label")], [Node.text(label)]);

let premises_view =
    (~syntaxes: list(DrvSyntax.t), ~rule, ~color_map: ColorSteps.t) => {
  let label = rule_to_label(rule);
  Node.div(
    ~attrs=[Attr.class_("deduction-prems-label")],
    [
      Node.div(
        ~attrs=[Attr.class_("deduction-prems")],
        List.map(
          syntax =>
            Node.div(
              ~attrs=[Attr.class_("deduction-prem")],
              [repr(syntax, ~color_map)],
            ),
          syntaxes,
        ),
      ),
    ]
    @ [label_view(~label)],
  );
};

let rule_example_view =
    (
      ~info: Haz3lschool.ProofGrade.VerifiedTree.info,
      ~color_map: ColorSteps.t,
    )
    : Node.t => {
  let (rule, res) = (info.rule, info);
  let color_map =
    switch (res.res) {
    | Correct => color_map
    | Incorrect(failure) => copy_color_map(failure, color_map)
    | Pending(_) => color_map
    };
  let {prems, concl} = res.ghost |> Option.get;
  Node.div(
    ~attrs=[Attr.class_("section"), Attr.class_("syntactic-form")],
    [
      Node.div(
        ~attrs=[Attr.class_("section-title")],
        [
          Node.text(
            switch (rule) {
            | Some(rule) => "Rule " ++ Rule.show(rule)
            | None => "Rule ?"
            },
          ),
        ],
      ),
      premises_view(~syntaxes=prems, ~rule, ~color_map),
      conclusion_view(~syntax=concl, ~color_map),
    ],
  );
};

let rule_example_view =
    (
      ~info: option(Haz3lschool.ProofGrade.VerifiedTree.info),
      ~color_map: ColorSteps.t,
    ) =>
  switch (info) {
  | Some(info) => rule_example_view(~info, ~color_map)
  | None => Node.div([])
  };

let premise_mismatch: group = {
  id: DrvPremiseMismatch,
  forms: [
    {
      id: DrvPremiseMismatch,
      syntactic_form: [],
      expandable_id: None,
      explanation: "",
      examples: [],
    },
  ],
};

let failunbox: DrvSyntax.cls => group =
  cls => {
    id: DrvFailUnbox,
    forms: [
      {
        id: DrvFailUnbox,
        syntactic_form: [],
        expandable_id: None,
        explanation:
          "Failed to unbox [*term*](%s), expected "
          ++ DrvSyntax.show_cls(cls)
          ++ ".",
        examples: [],
      },
    ],
  };

let notalist: group = {
  id: DrvNotAList,
  forms: [
    {
      id: DrvNotAList,
      syntactic_form: [],
      expandable_id: None,
      explanation: "[*ctx*](%s) is not a list.",
      examples: [],
    },
  ],
};

let notequal: group = {
  id: DrvNotEqual,
  forms: [
    {
      id: DrvNotEqual,
      syntactic_form: [],
      expandable_id: None,
      explanation: "[*term1*](%s) is not equal to [*term2*](%s).",
      examples: [],
    },
  ],
};

let failtest: (test_id, string) => group =
  (id, explanation) => {
    id: DrvFailTest(id),
    forms: [
      {
        id: DrvFailTest(id),
        syntactic_form: [],
        expandable_id: None,
        explanation,
        examples: [],
      },
    ],
  };

// type operation =
//   | Neg(t)
//   | Plus(t, t)
//   | Minus(t, t)
//   | Times(t, t)
//   | Lt(t)
//   | NotLt(t)
//   | Gt(t)
//   | NotGt(t)
//   | Eq(t)
//   | NotEq(t)
//   | Subst((t, t), t)
//   | Subst2((t, t), (t, t), t)
//   | SubstTy((t, t), t)
//   | Cons(t, t)
//   | ConsHasTy((t, t), t)
//   | ConsHasTy2((t, t), (t, t), t)
//   | Mem(t)
//   | MemHasTy(t, t);

let neg: group =
  failtest(Neg, "Expect [*n*](%s) to be the negation of [*n1*](%s).");

let plus: group =
  failtest(Plus, "Expect [*n*](%s) to be [*n1*](%s) plus [*n2*](%s).");

let minus: group =
  failtest(Minus, "Expect [*n*](%s) to be [*n1*](%s) minus [*n2*](%s).");

let times: group =
  failtest(Times, "Expect [*n*](%s) to be [*n1*](%s) times [*n2*](%s).");

let lt: group = failtest(Lt, "Expect [*n*](%s) to be less than [*n1*](%s).");

let notlt: group =
  failtest(NotLt, "Expect [*n*](%s) to be not less than [*n1*](%s).");

let gt: group =
  failtest(Gt, "Expect [*n*](%s) to be greater than [*n1*](%s).");

let notgt: group =
  failtest(NotGt, "Expect [*n*](%s) to be not greater than [*n1*](%s).");

let eq: group = failtest(Eq, "Expect [*n*](%s) to be equal to [*n1*](%s).");

let noteq: group =
  failtest(NotEq, "Expect [*n*](%s) to be not equal to [*n1*](%s).");

let subst: group =
  failtest(
    Subst,
    "Expect [*e*](%s) to be [*e1*](%s) after substituting [*x*](%s) by [*v1*](%s).",
  );

let subst2: group =
  failtest(
    Subst2,
    "Expect [*e*](%s) to be [*e1*](%s) after substituting [*x*](%s) by [*v1*](%s) and [*y*](%s) by [*v2*](%s).",
  );

let substty: group =
  failtest(
    SubstTy,
    "Expect [*e*](%s) to be [*e1*](%s) after substituting [*a*](%s) by [*t*](%s).",
  );

let cons: group =
  failtest(
    Cons,
    "Expect [*ctx*](%s) to be [*ctx1*](%s) extended by [*x*](%s).",
  );

let conshasty: group =
  failtest(
    ConsHasTy,
    "Expect [*ctx*](%s) to be [*ctx1*](%s) extended by [*x*](%s) : [*t1*](%s).",
  );

let conshasty2: group =
  failtest(
    ConsHasTy2,
    "Expect [*ctx*](%s) to be [*ctx1*](%s) extended by [*x*](%s) : [*t1*](%s) and [*y*](%s) : [*t2*](%s).",
  );

let mem: group =
  failtest(Mem, "Expect [*p*](%s) to be a member of [*ctx*](%s).");

let memhasty: group =
  failtest(
    MemHasTy,
    "Expect [*x*](%s) : [*t*](%s) to be a member of [*ctx*](%s).",
  );
