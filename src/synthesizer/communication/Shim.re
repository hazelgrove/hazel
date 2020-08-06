open Synthesiscore.Types;
open Synthesiscore;
open UHExp;
module IntMap = Map.Make(Int);

/*
    The goal is to convert a UHExp into an exp.

    This requires conversion of lines into expressions.
 */

let hashVar = Id.of_string; /*v => {
  Array.init(Js.String.length(v), i => Js.String.charCodeAt(i, v))
  |> Array.to_list
  |> List.fold_left((accum, chr) => accum lsl 5 - accum + chr, 0);
};*/

let rec hTypeToType_ = (t: HTyp.t): type_ => {
  HTyp.(
    switch (t) {
    | Hole => Any_t
    | Int => Int_t
    | Float => Float_t
    | Bool => Bool_t
    | Arrow(t1, t2) => Function_t(hTypeToType_(t1), hTypeToType_(t2))
    | Prod(ts) => unfoldTypes(ts)
    | Sum(t1, t2) => Pair_t(hTypeToType_(t1), hTypeToType_(t2))
    | _ => failwith("Not yet implemented")
    }
  );
}

and unfoldTypes = (ts: list(HTyp.t)) => {
  switch (ts) {
  | [] => failwith("Product type needs at least two types")
  | [t] => hTypeToType_(t)
  | [t, ...xs] => Pair_t(hTypeToType_(t), unfoldTypes(xs))
  };
}

and uhTypToType_ = (t: UHTyp.t): type_ => UHTyp.expand(t) |> hTypeToType_

and collapseBlock = (block: list(exp)): exp => {
  switch (block) {
  | [] => failwith("Empty block")
  | [e] => e
  | [e, ...xs] =>
    switch (e) {
    | Function(_, x, t, e') =>
      Application(
        Function(IdGenerator.getId(), x, t, collapseBlock(xs)),
        e',
      )
    | _ =>
      failwith("Expression lines before let lines are currently unsupported")
    }
  };
}

and uHExpToExp = (e: UHExp.t): list(exp) => {
  switch (e) {
  | [] => []
  | [EmptyLine, ...xs] => uHExpToExp(xs)
  | [ExpLine(seq), ...xs] => [opSeqToExp(seq), ...uHExpToExp(xs)]
  | [LetLine(pat, t, block), ...xs] =>
    let t =
      switch (t) {
      | Some(x) => x
      | None => failwith("The type must be supplied for the let bind.")
      };
    let bind =
      switch (opPatToExp(pat)) {
      | Var(x) => x
      | _ =>
        failwith(
          "Pattern must be a single variable for now. Holes in let binds not yet supported in synthesizer",
        )
      };
    let exp = uHExpToExp(block) |> collapseBlock;
    [Function(0, bind, uhTypToType_(t), exp), ...uHExpToExp(xs)];
  | _ => failwith("Not yet implemented")
  };
}

and operandToExp = (op: UHExp.operand): exp => {
  switch (op) {
  | EmptyHole(x) => Hole(x)
  | InvalidText(x, y) => failwith(y)
  | Var(_, _, v) => Var(hashVar(v))
  | IntLit(_, s) => Int(int_of_string(s))
  | FloatLit(_, s) => Float(float_of_string(s))
  | BoolLit(_, b) => Bool(b)
  | ListNil(_) => Ctor(0, List, Unit)
  | AssertLit(_) => Unit
  | Lam(_, pat, Some(t), block) =>
    switch (opPatToExp(pat)) {
    | Hole(x) => Hole(x)
    | Var(v) =>
      Function(
        IdGenerator.getId(),
        v,
        uhTypToType_(t),
        collapseBlock(uHExpToExp(block)),
      )
    | _ => failwith("Function pattern should only be hole or var.")
    }
  // Need help figuring out injections
  | Inj(_, _, _) => Unit
  | Case(_, e, rules) =>
    switch (getType(e)) {
    | D(adt) =>
      let constructors = Tools.lookup(adt, sigma);
      let branches =
        List.map2(
          ((n, typ), rule) => {
            let Rule(pat, uhexp) = rule;
            let pattern =
              switch (opPatToExp(pat)) {
              | Var(v) => V(v)
              | _ => failwith("Expected var within branch")
              };
            (n, (pattern, uHExpToExp(uhexp) |> collapseBlock));
          },
          constructors,
          rules,
        );
      Case(uHExpToExp(e) |> collapseBlock, branches);
    | _ => failwith("Type needs to be some adt")
    }
  | Parenthesized(e) => uHExpToExp(e) |> collapseBlock
  | ApPalette(_, _, _, _) => Unit
  };
}

and patOperandToExp = (op: UHPat.operand): exp => {
  switch (op) {
  | EmptyHole(m) => Hole(m)
  | Wild(_) => Unit
  | InvalidText(_, _) => Unit
  | Var(_, _, v) => Var(hashVar(v))
  | IntLit(_, s) => Int(int_of_string(s))
  | FloatLit(_, s) => Float(float_of_string(s))
  | BoolLit(_, b) => Bool(b)
  | ListNil(_) => Unit
  | Parenthesized(t) => opPatToExp(t)
  // Same as above, I don't know exactly what inj is.
  | Inj(_, _, _) => Unit
  };
}

and getType = (op: UHExp.t): type_ => {
  switch (Statics_Exp.syn(Contexts.empty, op)) {
  | None => failwith("Typing could not be accomplished")
  | Some(htyp) => hTypeToType_(htyp)
  };
}

and seqToExp = (seq: Seq.t(UHExp.operand, Operators_Exp.t)): exp => {
  let S(operand, affix) = seq;
  let o = operandToExp(operand);
  switch (affix) {
  | E => o
  | A(op, seq') =>
    switch (op) {
    | Operators_Exp.Plus => Plus(o, seqToExp(seq'))
    | Operators_Exp.Minus => Minus(o, seqToExp(seq'))
    | Operators_Exp.Times => Times(o, seqToExp(seq'))
    | Operators_Exp.Divide => Divide(o, seqToExp(seq'))
    | Operators_Exp.FPlus => FPlus(o, seqToExp(seq'))
    | Operators_Exp.FMinus => FMinus(o, seqToExp(seq'))
    | Operators_Exp.FTimes => FTimes(o, seqToExp(seq'))
    | Operators_Exp.FDivide => FDivide(o, seqToExp(seq'))
    | Operators_Exp.LessThan => LessThan(o, seqToExp(seq'))
    | Operators_Exp.GreaterThan => GreaterThan(o, seqToExp(seq'))
    | Operators_Exp.Equals => Equals(o, seqToExp(seq'))
    | Operators_Exp.FLessThan => FLessThan(o, seqToExp(seq'))
    | Operators_Exp.FGreaterThan => FGreaterThan(o, seqToExp(seq'))
    | Operators_Exp.FEquals => FEquals(o, seqToExp(seq'))
    | Operators_Exp.And => And(o, seqToExp(seq'))
    | Operators_Exp.Or => Or(o, seqToExp(seq'))
    }
  };
}

and patSeqToExp = (seq: Seq.t(UHPat.operand, Operators_Pat.t)): exp => {
  let S(operand, affix) = seq;
  let o = patOperandToExp(operand);
  switch (affix) {
  | E => o
  | A(op, seq') =>
    switch (op) {
    | Operators_Pat.Comma => Pair(o, patSeqToExp(seq'))
    | Operators_Pat.Space => Application(o, patSeqToExp(seq'))
    | Operators_Pat.Cons => Ctor(0, List, Pair(o, patSeqToExp(seq')))
    }
  };
}

and opPatToExp = (pat: UHPat.t): exp => {
  let OpSeq(_, seq) = pat;
  patSeqToExp(seq);
}

and opSeqToExp = (seq: OpSeq.t(UHExp.operand, Operators_Exp.t)): exp => {
  let OpSeq(_, seq') = seq;
  seqToExp(seq');
};

let rec findAssertion = (e: UHExp.t) => {
  switch (e) {
  | [] => None
  | [ExpLine(s), ...xs] =>
    switch (findAssert_h(s)) {
    | Some((e1, e2)) => Some((e1, e2))
    | None => findAssertion(xs)
    }
  | [_, ...xs] => findAssertion(xs)
  };
}

and findAssert_h = s => {
  let OpSeq(_, seq) = s;
  findAssert_hh(seq);
}

and findAssert_hh = seq => {
  switch (seq) {
  | S(op, aff) =>
    switch (op) {
    | AssertLit(_, e1, e2) => Some((e1, e2))
    | _ =>
      switch (aff) {
      | E => None
      | A(_, seq') => findAssert_hh(seq')
      }
    }
  };
};

let processAssertion = (e: UHExp.t) => {
  switch (findAssertion(e)) {
  | None => (Unit, Eunit)
  | Some((e1, e2)) =>
    let cast = uHExpToExp(e2) |> collapseBlock |> Typecasting.expToEx;
    let ex =
      switch (cast) {
      | Some(x) => x
      | None => failwith("Casting second UHExp to example failed")
      };
    let exp = uHExpToExp(e1) |> collapseBlock;
    (exp, ex);
  };
};

let rec expToUHExp = (exp: Synthesiscore.Types.exp): UHExp.t => {
  [EmptyLine];
};

let rec holeFillingToIntMap =
        (hf: Synthesiscore.Types.hole_fillings): IntMap.t(UHExp.t) => {
  IntMap.(hfHelper(hf));
}

and hfHelper = hf => {
  switch (hf) {
  | [] => IntMap.empty
  | [(x, e), ...xs] => hfHelper(xs) |> IntMap.add(x, expToUHExp(e))
  };
};

/*and expToSeq = (e: exp):Seq.t => {
      switch (e) {
          | Int(x) => S(IntL

  };
  */

//TODO missing links

let hazelToSynthesizer = (e: UHExp.t): Synthesiscore.Types.input => {
  let (exp, ex) = processAssertion(e);
  (exp, ex);
};

let synthesizerToHazel =
    (_fillings: Synthesiscore.Types.output): IntMap.t(UHExp.operand) =>
  IntMap.(
    empty
    |> add(0, IntLit(NotInHole, "1729"))
    |> add(17, IntLit(NotInHole, "12"))
  );
