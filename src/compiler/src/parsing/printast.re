open Format;
open Parsetree;

let line = (i:int, f:formatter, ~s:'a=?, unit) => {
  fprintf(f, "%s", (String.make(((2*i) mod 72), ' ')));
  switch(s) {
      |Some(ss) => fprintf(f, "%s", ss);
      |_ => ();
  }
}

let expression = (i:int, f:formatter, e:Parsetree.expression) => {
    let edesc = e.pexp_desc;
    switch(edesc) {
        | PExpId(eloc) => {
            let name = Identifier.string_of_ident(eloc.txt);
            line(i, f, ~s=name, ());
        }
//   | PExpConstant(constant)
//   | PExpTuple(list(expression))
//   | PExpArray(list(expression))
//   | PExpArrayGet(expression, expression)
//   | PExpArraySet(expression, expression, expression)
//   | PExpRecord(list((loc(Identifier.t), expression)))
//   | PExpRecordGet(expression, loc(Identifier.t))
//   | PExpRecordSet(expression, loc(Identifier.t), expression)
//   | PExpLet(rec_flag, mut_flag, list(value_binding))
//   | PExpMatch(expression, list(match_branch))
//   | PExpPrim1(prim1, expression)
//   | PExpPrim2(prim2, expression, expression)
//   | PExpPrimN(primn, list(expression))
//   | PExpIf(expression, expression, expression)
//   | PExpWhile(expression, expression)
//   | PExpFor(
//       option(expression),
//       option(expression),
//       option(expression),
//       expression,
//     )
//   | PExpContinue
//   | PExpBreak
//   | PExpConstraint(expression, parsed_type)
//   | PExpLambda(list(pattern), expression)
//   | PExpApp(expression, list(expression))
//   | PExpBlock(list(expression))
//   | PExpBoxAssign(expression, expression)
//   | PExpAssign(expression, expression)
//   | /** Used for modules without body expressions */
//     PExpNull 
    }
}