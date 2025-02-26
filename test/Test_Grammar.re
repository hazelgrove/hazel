open Haz3lcore;
open Alcotest;
let qcheck_map_annotation_test =
  QCheck.Test.make(
    ~name="Map annotation to something and back",
    ~count=100,
    QCheck.make(
      ~print=Haz3lmenhir.AST.show_exp,
      Haz3lmenhir.AST.gen_exp_sized(7),
    ),
    exp => {
      let indicated_exp = Haz3lmenhir.Conversion.Exp.of_menhir_ast(exp);
      let core_exp =
        Grammar.map_exp_annotation(
          _ => IdTagged.IdTag.fresh(),
          indicated_exp,
        );
      let _ = [%derive.show: Exp.t](core_exp); // Gets coverage for show
      Grammar.equal_exp_t(
        (==),
        Grammar.map_exp_annotation(Fun.id, core_exp),
        core_exp,
      );
    },
  );

/*
 * Constructs a sample expression from a given class expression.
 * This is used to make sure we have a factory function for every expression
 * and to make sure that we can actually construct one for every class.
 * The correspondence is tested below.
 */
let sample_expression = (cls_exp: Exp.cls): Grammar.UnitGrammar.exp => {
  Grammar.UnitGrammar.(
    Exp.(
      switch (cls_exp) {
      | Invalid => invalid("invalid")
      | EmptyHole => empty_hole()
      | MultiHole => multi_hole([Exp(empty_hole()), Exp(empty_hole())])
      | DynamicErrorHole => dynamic_error_hole(empty_hole(), DivideByZero)
      | FailedCast => failed_cast(empty_hole(), Typ.int(), Typ.string())
      | Deferral => deferral(InAp)
      | Undefined => undefined()
      | Bool => bool(true)
      | Int => int(1)
      | Float => float(2.)
      | String => string("hello")
      | ListLit => list_lit([])
      | Constructor => constructor("A", None)
      | Fun => fn(Pat.var("x"), var("x"), None, None)
      | TypFun => typ_fun(TPat.var("x"), empty_hole(), None)
      | Label => label("label")
      | TupLabel => tup_label(label("label"), empty_hole())
      | Tuple => tuple([])
      | Dot => dot(empty_hole(), empty_hole())
      | Var => var("x")
      | Let => let_(Pat.empty_hole(), empty_hole(), empty_hole())
      | FixF => fix_f(Pat.empty_hole(), empty_hole(), None)
      | TyAlias =>
        ty_alias(
          TPat.empty_hole(),
          Typ.unknown(Hole(EmptyHole)),
          empty_hole(),
        )
      | Ap => ap(Forward, empty_hole(), empty_hole())
      | TypAp => typ_ap(empty_hole(), Typ.unknown(Hole(EmptyHole)))
      | DeferredAp => deferred_ap(empty_hole(), [empty_hole()])
      | If => if_(empty_hole(), empty_hole(), empty_hole())
      | Seq => seq(empty_hole(), empty_hole())
      | Test => test(empty_hole())
      | Filter =>
        filter(StepperFilter.residue(0, (Step, One)), empty_hole())
      | Closure =>
        module M = {
          include VarBstMap.Ordered;
        };

        closure(closure_environment(Id.mk(), M.empty), empty_hole());
      | Parens => parens(empty_hole())
      | Cons => cons(empty_hole(), empty_hole())
      | UnOp(op) => un_op(op, empty_hole())
      | BinOp(op) => bin_op(op, empty_hole(), empty_hole())
      | BuiltinFun => builtin_fun("string_compare")
      | Match => match(empty_hole(), [])
      | Cast => cast(empty_hole(), Typ.int(), Typ.string())
      | ListConcat => list_concat(empty_hole(), empty_hole())
      }
    )
  );
};

let tests = (
  "Grammar",
  [
    test_case(
      "Expression classes are correct",
      `Quick,
      () => {
        let exp_classes = Exp.all_of_cls;
        let cls_testable =
          testable(Fmt.using(Exp.show_cls, Fmt.string), Exp.equal_cls);
        List.iter(
          cls =>
            check(
              cls_testable,
              Exp.show_cls(cls) ++ " Equivalency",
              cls,
              Exp.cls_of_term(sample_expression(cls).term),
            ),
          exp_classes,
        );
      },
    ),
    QCheck_alcotest.to_alcotest(qcheck_map_annotation_test),
  ],
);
