// Test suite for the skel parser.
let mvar = fst(MetaVarGen.init);

let%test "single operand test" = {
  // 1
  let single_op_seq = Seq.S(UHExp.IntLit(NotInHole, "1"), Seq.E);
  let single_op_skel = Skel.Placeholder(0);

  UHExp.associate(single_op_seq) == single_op_skel;
};

let%test "simple addition test" = {
  // 1 + 2
  let simple_add_seq =
    Seq.S(
      UHExp.IntLit(NotInHole, "1"),
      Seq.A(Operators_Exp.Plus, Seq.S(IntLit(NotInHole, "2"), E)),
    );
  let simple_add_skel =
    Skel.BinOp(
      NotInHole,
      Operators_Exp.Plus,
      Skel.Placeholder(0),
      Skel.Placeholder(1),
    );

  UHExp.associate(simple_add_seq) == simple_add_skel;
};

let%test "single hole test" = {
  // _
  let single_hole_seq = Seq.S(UHExp.EmptyHole(mvar), E);
  let single_hole_skel = Skel.Placeholder(0);

  UHExp.associate(single_hole_seq) == single_hole_skel;
};

let%test "addition w/ left hole" = {
  // _ + 2
  let add_l_hole_seq =
    Seq.S(
      UHExp.EmptyHole(mvar),
      Seq.A(Operators_Exp.Plus, Seq.S(IntLit(NotInHole, "2"), E)),
    );
  let add_l_hole_skel =
    Skel.BinOp(
      NotInHole,
      Operators_Exp.Plus,
      Skel.Placeholder(0),
      Skel.Placeholder(1),
    );

  UHExp.associate(add_l_hole_seq) == add_l_hole_skel;
};

let%test "operator precedence test" = {
  // 1 - 2 + 2 * 3 :: 4 :: []
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

  let precedence_op_seq =
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
    );

  let precedence_op_skel =
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
    );

  UHExp.associate(precedence_op_seq) == precedence_op_skel;
};

let%test "holey operator precedence test" = {
  // 1 - _ + 2 * 3.2 :: 4 :: []
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

  let precedence_op_seq =
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
    );

  let precedence_op_skel =
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
    );

  // Utils.run_n_times(
  //   10_000_000,
  //   "precedence op skel",
  //   UHExp.associate,
  //   precedence_op_seq,
  // );

  UHExp.associate(precedence_op_seq) == precedence_op_skel;
};

let%test "type precedence test" = {
  // Int + Int -> Int || Int -> Int
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

  let type_precedence_seq =
    Seq.S(
      UHTyp.Int,
      Seq.A(
        Operators_Typ.Prod,
        Seq.S(UHTyp.Int, Seq.A(Operators_Typ.Arrow, second_half)),
      ),
    );

  let precedence_op_skel =
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
    );

  UHTyp.associate(type_precedence_seq) == precedence_op_skel;
};
