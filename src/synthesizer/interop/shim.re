open ../engine/project/src/Types;
open ../../hazelcore/UHExp;

let rec hTypeToType_ = (t: ../../hazelcore/Htyp.t):type_ => {
    open ../../hazelcore/Htyp;
    switch (t) {
        | Hole => Any_t 
        | Int => Int_t 
        | Float => Float_t 
        | Bool => Bool_t 
        | Arrow(t1, t2) => Function_t(hTypeToType_(t1), hTypeToType_(t2))
        | Sum(t1, t2) => Pair_t(hTypeToType_(t1), hTypeToType_(t2))
        | _ => failwith("Not yet implemented")
    }
}

and UHExpToExp = (e: UHExp.t):exp => {
    switch (e) {
        | [] => Unit 
        | [EmptyLine, ...xs] => UHExpToExp(xs)
        | [ExpLine(seq), ...xs] => Pair(opSeqToExp(seq), UHExpToExp(xs))
        | _ => failwith("Not yet implemented")
    }
}

and operandToExp = (op: UHExp.operand):Exp => {
    switch (op) {
        | EmptyHole(x) => Hole(0)
        | InvalidText(x, y) => failwith(y)
        | Var(_, _, v) => Var(0)
        | IntLit(_, s) => Int(int_of_string(s))
        | FloatLit(_, s) => Float(float_of_string(s))
        | BoolLit(_, s) => Bool(bool_of_string(s))
        | ListNil(_) => Ctor(0, List, Unit)
        | AssertList(_) => Unit 
        | Lam(_, _, _) => Unit 
        | Inj(_, )) => Unit
        | Case(_, _, _) => Unit 
        | Parenthesized(e) => UHExpToExp(e)
        | ApPalette(_, _, _, _) => Unit 
        }
}

and seqToExp = (seq: Seq.t):Exp => {
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
    }
}

and opSeqToExp = (seq: OpSeq.t):exp => {
    let OpSeq(_, seq') = seq;
    seqToExp(seq');
};

