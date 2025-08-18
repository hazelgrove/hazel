open Haz3lcore;
open Alcotest;
let qcheck_map_annotation_test =
  QCheck.Test.make(
    ~name="Map annotation to something and back",
    ~count=100,
    Haz3lmenhir.AST.arb_exp(7),
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
      | Atom(Bool) => bool(true)
      | Atom(Int) => int(1)
      | Atom(SInt) => sint(1)
      | Atom(Float) => float(2.)
      | Atom(String) => string("hello")
      | Atom(Nat) => nat(Bigint.one)
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
      | Use => use(Typ.unknown(Hole(EmptyHole)), empty_hole())
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

        closure(
          closure_environment(~callstack=[], Id.mk(), M.empty),
          empty_hole(),
        );
      | Parens => parens(empty_hole())
      | Probe => probe(empty_hole(), Probe.empty)
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

let sample_pattern = (cls_pat: Pat.cls): Grammar.UnitGrammar.pat => {
  Grammar.UnitGrammar.(
    Pat.(
      switch (cls_pat) {
      | Invalid => invalid("invalid")
      | EmptyHole => empty_hole()
      | MultiHole => multi_hole([Pat(empty_hole()), Pat(empty_hole())])
      | Atom(Bool) => bool(true)
      | Atom(Int) => int(1)
      | Atom(SInt) => sint(1)
      | Atom(Float) => float(2.)
      | Atom(String) => string("hello")
      | Atom(Nat) => nat(Bigint.one)
      | ListLit => list_lit([])
      | Constructor => constructor("A", None)
      | Var => var("x")
      | Tuple => tuple([])
      | Cons => cons(empty_hole(), empty_hole())
      | Label => label("label")
      | TupLabel => tup_label(label("label"), empty_hole())
      | Parens => parens(empty_hole())
      | Probe => probe(empty_hole(), Probe.empty)
      | Ap => ap(empty_hole(), empty_hole())
      | Cast => cast(empty_hole(), Typ.int(), Typ.string())
      | Wild => wild()
      }
    )
  );
};

let sample_type = (cls_typ: Typ.cls): Grammar.UnitGrammar.typ => {
  Grammar.UnitGrammar.(
    Typ.(
      switch (cls_typ) {
      | Invalid => unknown(Hole(Invalid("invalid")))
      | Atom(Bool) => bool()
      | Atom(Int) => int()
      | Atom(SInt) => sint()
      | Atom(Float) => float()
      | Atom(String) => string()
      | Atom(Nat) => nat()
      | List => list(unknown(Hole(EmptyHole)))
      | Arrow => arrow(unknown(Hole(EmptyHole)), unknown(Hole(EmptyHole)))
      | Var => var("x")
      | Prod => prod([])
      | TupLabel =>
        tup_label(unknown(Hole(EmptyHole)), unknown(Hole(EmptyHole)))
      | Parens => parens(unknown(Hole(EmptyHole)))
      | Ap => ap(unknown(Hole(EmptyHole)), unknown(Hole(EmptyHole)))
      | Rec => rec_(TPat.var("x"), unknown(Hole(EmptyHole)))
      | Forall => forall(TPat.var("x"), unknown(Hole(EmptyHole)))
      | EmptyHole => unknown(Hole(EmptyHole))
      | SynSwitch => unknown(SynSwitch)
      | Internal => unknown(Internal)
      | Label => label("label")
      | MultiHole => unknown(Hole(MultiHole([])))
      | Sum => sum([])
      | Constructor => assert(false) // Excluded because there is no Typ constructor
      }
    )
  );
};

let sample_tpat = (cls_tpat: TPat.cls): Grammar.UnitGrammar.tpat => {
  Grammar.UnitGrammar.(
    TPat.(
      switch (cls_tpat) {
      | Invalid => invalid("invalid")
      | EmptyHole => empty_hole()
      | Var => var("x")
      | MultiHole => multi_hole([TPat(empty_hole()), TPat(empty_hole())])
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
    test_case(
      "Pattern classes are correct",
      `Quick,
      () => {
        let pat_classes = Pat.all_of_cls;
        let cls_testable =
          testable(Fmt.using(Pat.show_cls, Fmt.string), Pat.equal_cls);
        List.iter(
          cls =>
            check(
              cls_testable,
              Pat.show_cls(cls) ++ " Equivalency",
              cls,
              Pat.cls_of_term(sample_pattern(cls).term),
            ),
          pat_classes,
        );
      },
    ),
    test_case(
      "Type classes are correct",
      `Quick,
      () => {
        let typ_classes = Typ.all_of_cls;
        let cls_testable =
          testable(Fmt.using(Typ.show_cls, Fmt.string), Typ.equal_cls);
        List.iter(
          (cls: Typ.cls) => {
            switch (cls) {
            | Constructor => ()
            | _ =>
              check(
                cls_testable,
                Typ.show_cls(cls) ++ " Equivalency",
                cls,
                Typ.cls_of_term(sample_type(cls).term.typ),
              )
            }
          },
          typ_classes,
        );
      },
    ),
    test_case(
      "Type pattern classes are correct",
      `Quick,
      () => {
        let tpat_classes = TPat.all_of_cls;
        let cls_testable =
          testable(Fmt.using(TPat.show_cls, Fmt.string), TPat.equal_cls);
        List.iter(
          cls =>
            check(
              cls_testable,
              TPat.show_cls(cls) ++ " Equivalency",
              cls,
              TPat.cls_of_term(sample_tpat(cls).term),
            ),
          tpat_classes,
        );
      },
    ),
    QCheck_alcotest.to_alcotest(qcheck_map_annotation_test),
  ],
);
