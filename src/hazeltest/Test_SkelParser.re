open Tezt;
open Tezt.Base;

let mvar = MetaVarGen.init;

let register_exp_test = (title, tags, seq, skel) =>
  Test.register(
    ~__FILE__, ~title, ~tags=["hazelcore", "skelparser"] @ tags, () =>
    UHExp.associate(seq) == skel ? unit : Test.fail("skel parser failed!")
  );

let register_typ_test = (title, tags, seq, skel) =>
  Test.register(
    ~__FILE__, ~title, ~tags=["hazelcore", "skelparser"] @ tags, () =>
    UHTyp.associate(seq) == skel ? unit : Test.fail("skel parser failed!")
  );

// 1
let () =
  register_exp_test(
    "single operand test",
    [],
    Seq.S(UHExp.IntLit(NotInHole, "1"), Seq.E),
    Skel.Placeholder(0),
  );

// 1 + 2
let () =
  register_exp_test(
    "simple addition test",
    [],
    Seq.S(
      UHExp.IntLit(NotInHole, "1"),
      Seq.A(Operators_Exp.Plus, Seq.S(IntLit(NotInHole, "2"), E)),
    ),
    Skel.BinOp(
      NotInHole,
      Operators_Exp.Plus,
      Skel.Placeholder(0),
      Skel.Placeholder(1),
    ),
  );

// _
let () =
  register_exp_test(
    "single hole test",
    [],
    Seq.S(UHExp.EmptyHole(mvar), E),
    Skel.Placeholder(0),
  );

// _ + 2
let () =
  register_exp_test(
    "addition w/ left hole",
    [],
    Seq.S(
      UHExp.EmptyHole(mvar),
      Seq.A(Operators_Exp.Plus, Seq.S(IntLit(NotInHole, "2"), E)),
    ),
    Skel.BinOp(
      NotInHole,
      Operators_Exp.Plus,
      Skel.Placeholder(0),
      Skel.Placeholder(1),
    ),
  );

// 1 - 2 + 2 * 3 :: 4 :: []
let () = {
  let cons_seq =
    Seq.S(
      UHExp.IntLit(NotInHole, "3"),
      Seq.A(
        Operators_Exp.Cons,
        Seq.S(
          UHExp.IntLit(NotInHole, "4"),
          Seq.A(Operators_Exp.Cons, Seq.S(UHExp.ListNil(NotInHole), Seq.E)),
        ),
      ),
    );
  register_exp_test(
    "operator precedence test",
    [],
    Seq.S(
      UHExp.IntLit(NotInHole, "1"),
      Seq.A(
        Operators_Exp.Minus,
        Seq.S(
          IntLit(NotInHole, "2"),
          Seq.A(
            Operators_Exp.Plus,
            Seq.S(
              IntLit(NotInHole, "2"),
              Seq.A(Operators_Exp.Times, cons_seq),
            ),
          ),
        ),
      ),
    ),
    Skel.BinOp(
      NotInHole,
      Operators_Exp.Cons,
      Skel.BinOp(
        NotInHole,
        Operators_Exp.Plus,
        Skel.BinOp(
          NotInHole,
          Operators_Exp.Minus,
          Skel.Placeholder(0),
          Skel.Placeholder(1),
        ),
        Skel.BinOp(
          NotInHole,
          Operators_Exp.Times,
          Skel.Placeholder(2),
          Skel.Placeholder(3),
        ),
      ),
      Skel.BinOp(
        NotInHole,
        Operators_Exp.Cons,
        Skel.Placeholder(4),
        Skel.Placeholder(5),
      ),
    ),
  );
};

// 1 - _ + 2 * 3.2 :: 4 :: []
let () = {
  let cons_seq =
    Seq.S(
      UHExp.FloatLit(NotInHole, "3.2"),
      Seq.A(
        Operators_Exp.Cons,
        Seq.S(
          UHExp.IntLit(NotInHole, "4"),
          Seq.A(Operators_Exp.Cons, Seq.S(UHExp.ListNil(NotInHole), Seq.E)),
        ),
      ),
    );
  register_exp_test(
    "holey operator precedence test",
    [],
    Seq.S(
      UHExp.IntLit(NotInHole, "1"),
      Seq.A(
        Operators_Exp.Minus,
        Seq.S(
          UHExp.EmptyHole(mvar),
          Seq.A(
            Operators_Exp.Plus,
            Seq.S(
              IntLit(NotInHole, "2"),
              Seq.A(Operators_Exp.Times, cons_seq),
            ),
          ),
        ),
      ),
    ),
    Skel.BinOp(
      NotInHole,
      Operators_Exp.Cons,
      Skel.BinOp(
        NotInHole,
        Operators_Exp.Plus,
        Skel.BinOp(
          NotInHole,
          Operators_Exp.Minus,
          Skel.Placeholder(0),
          Skel.Placeholder(1),
        ),
        Skel.BinOp(
          NotInHole,
          Operators_Exp.Times,
          Skel.Placeholder(2),
          Skel.Placeholder(3),
        ),
      ),
      Skel.BinOp(
        NotInHole,
        Operators_Exp.Cons,
        Skel.Placeholder(4),
        Skel.Placeholder(5),
      ),
    ),
  );
};

// Utils.run_n_times(
//   10_000_000,
//   "precedence op skel",
//   UHExp.associate,
//   precedence_op_seq,
// );

// Int + Int -> Int || Int -> Int
let () = {
  let second_half =
    Seq.S(
      UHTyp.Int,
      Seq.A(
        Operators_Typ.Sum,
        Seq.S(
          UHTyp.Int,
          Seq.A(Operators_Typ.Arrow, Seq.S(UHTyp.Int, Seq.E)),
        ),
      ),
    );
  register_typ_test(
    "type precedence test",
    [],
    Seq.S(
      UHTyp.Int,
      Seq.A(
        Operators_Typ.Prod,
        Seq.S(UHTyp.Int, Seq.A(Operators_Typ.Arrow, second_half)),
      ),
    ),
    Skel.BinOp(
      NotInHole,
      Operators_Typ.Prod,
      Skel.Placeholder(0),
      Skel.BinOp(
        NotInHole,
        Operators_Typ.Arrow,
        Skel.Placeholder(1),
        Skel.BinOp(
          NotInHole,
          Operators_Typ.Arrow,
          Skel.BinOp(
            NotInHole,
            Operators_Typ.Sum,
            Skel.Placeholder(2),
            Skel.Placeholder(3),
          ),
          Skel.Placeholder(4),
        ),
      ),
    ),
  );
};
