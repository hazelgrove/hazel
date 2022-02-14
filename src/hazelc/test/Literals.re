open Case;

module Bool_Lit_True =
  Make({
    let expr = String("true");
    let expect = Ok("true");
  });

module BoolLit_False =
  Make({
    let expr = String("false");
    let expect = Ok("false");
  });

module IntLit_0 =
  Make({
    let expr = String("0");
    let expect = Ok("0");
  });

module IntLit_5 =
  Make({
    let expr = String("5");
    let expect = Ok("5");
  });

module FloatLit_1_01 =
  Make({
    let expr = String("1.01");
    let expect = Ok("1.01");
  });

module UnitLit =
  Make({
    // TODO: No way to represent triv in text yet
    let expr = DH(Triv);
    let expect = Ok("void");
  });

module ListNil =
  Make({
    let expr = String("[]");
    let expect = Ok("[]");
  });

module ListCons =
  Make({
    let expr = String("true::[]");
    let expect = Ok("[true, ...[]]");
  });
