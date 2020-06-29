// 
// Tests for the new shunting yard implementation
// of the skel parser.
let mvar = MetaVarGen.init;

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
