open Types;
open IdGenerator;
open UHExp;

/*
    The goal is to convert a UHExp into an exp.

    This requires conversion of lines into expressions.
 */

let hashVar = v => {
  Array.init(Js.String.length(v), i => Js.String.charCodeAt(i, v))
  |> Array.to_list
  |> List.fold_left((accum, chr) => accum lsl 5 - accum + chr, 0);
};

let rec hTypeToType_ = (t: Htyp.t): type_ => {
  Htyp.(
    switch (t) {
    | Hole => Any_t
    | Int => Int_t
    | Float => Float_t
    | Bool => Bool_t
    | Arrow(t1, t2) => Function_t(hTypeToType_(t1), hTypeToType_(t2))
    | Sum(t1, t2) => Pair_t(hTypeToType_(t1), hTypeToType_(t2))
    | _ => failwith("Not yet implemented")
    }
  );
}

and UHExpToExp =
  (e: UHExp.t) => (
    {
      switch (e) {
      | [] => []
      | [EmptyLine, ...xs] => UHExpToExp(xs)
      | [ExpLine(seq), ...xs] => [opSeqToExp(seq), ...UHExpToExp(xs)]
      | [LetLine(pat, t, block), ...xs] =>
        let element = opSeqToExp(pat);
        [element, ...UHExpToExp(xs)];
      | _ => failwith("Not yet implemented")
      };
    }:
      list(exp)
  )

and operandToExp = (op: UHExp.operand): exp => {
  switch (op) {
  | EmptyHole(x) => Hole(x)
  | InvalidText(x, y) => failwith(y)
  | Var(_, _, v) => Var(hashVar(v))
  | IntLit(_, s) => Int(int_of_string(s))
  | FloatLit(_, s) => Float(float_of_string(s))
  | BoolLit(_, s) => Bool(bool_of_string(s))
  | ListNil(_) => Ctor(0, List, Unit)
  | AssertList(_) => Unit
  | Lam(_, pat, Some(t), block) =>
    switch (pat) {
    | (EmptyHole(m), _) => Hole(m)
    | (Var(_, _, v), _) =>
      Function(hashVar(v), hTypeToType(t), UHExpToExp(block))
    | _ => failwith("Function pattern should only be hole or var.")
    }
  | Inj(_, _) => Unit
  | Case(_, e, rules) =>
    switch (getType(e)) {
    | D(adt) =>
      let constructors = Tools.lookup(adt, sigma);
      let branches =
        List.map2(
          ((n, typ), rule) => {
            let Rule(pat, uhexp) = rule;
            let pattern =
              switch (pat) {
              | Var(_, _, v) => V(hashVar(v))
              | _ => failwith("Expected var within branch")
              };
            (n, (pattern, UHExpToExp(uhexp)));
          },
          constructors,
          rules,
        );
      Case(UHExpToExp(e), branches);
    | _ => failwith("Type needs to be some adt")
    }
  | Parenthesized(e) => UHExpToExp(e)
  | ApPalette(_, _, _, _) => Unit
  };
}

and getType = (op: UHExp.operand): type_ => {
  switch (Statics_Exp.syn(Contexts.empty, op)) {
  | None => failwith("Typing could not be accomplished")
  | Some(htyp) => hTypeToType_(htyp)
  };
}

and seqToExp = (seq: Seq.t): exp => {
  let S(operand, affix) = seq;
  let o = operandToExp(operand);
  switch (affix) {
  | E => o
  | A(op, seq') =>
    switch (op) {
    | None => o
    | Plus => Plus(o, seqToExp(seq'))
    | Minus => Minus(o, seqToExp(seq'))
    | Times => Times(o, seqToExp(seq'))
    | Divide => Divide(o, seqToExp(seq'))
    | FPlus => FPlus(o, seqToExp(seq'))
    | FMinus => FMinus(o, seqToExp(seq'))
    | FTimes => FTimes(o, seqToExp(seq'))
    | FDivide => FDivide(o, seqToExp(seq'))
    | LessThan => LessThan(o, seqToExp(seq'))
    | GreaterThan => GreaterThan(o, seqToExp(seq'))
    | Equals => Equals(o, seqToExp(seq'))
    | FLessThan => FLessThan(o, seqToExp(seq'))
    | FGreaterThan => FGreaterThan(o, seqToExp(seq'))
    | FEquals => FEquals(o, seqToExp(seq'))
    | And => And(o, seqToExp(seq'))
    | Or => Or(o, seqToExp(seq'))
    }
  };
}

and opSeqToExp = (seq: OpSeq.t): exp => {
  let OpSeq(_, seq') = seq;
  seqToExp(seq');
};

/*and expToSeq = (e: exp):Seq.t => {
      switch (e) {
          | Int(x) => S(IntL

  };
  */
