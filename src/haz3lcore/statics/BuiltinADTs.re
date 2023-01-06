// let alftyp: Typ.t = Var("ALFTyp");
// let alfexpr: Typ.t = Var("ALFExpr");
// let option_alftyp: Typ.t = Var("Option_ALFTyp");
// let adts: list(Typ.adt) = [
//   (
//     "Color",
//     [
//       {tag: "Red", typ: Prod([])},
//       {tag: "Green", typ: Prod([])},
//       {tag: "Blue", typ: Prod([])},
//     ],
//   ),
//   (
//     "Option_ALFTyp",
//     [{tag: "None", typ: Prod([])}, {tag: "Some", typ: alftyp}],
//   ),
//   (
//     "ALFTyp",
//     {
//       [
//         {tag: "Num", typ: Prod([])},
//         {tag: "Bool", typ: Prod([])},
//         {tag: "Arrow", typ: Prod([alftyp, alftyp])},
//         {tag: "Prod", typ: Prod([alftyp, alftyp])},
//       ];
//     },
//   ),
//   (
//     "ALFExpr",
//     {
//       [
//         {tag: "NumLit", typ: Int},
//         {tag: "Plus", typ: Prod([alfexpr, alfexpr])},
//         {tag: "Times", typ: Prod([alfexpr, alfexpr])},
//         {tag: "Minus", typ: Prod([alfexpr, alfexpr])},
//         {tag: "Eq", typ: Prod([alfexpr, alfexpr])},
//         {tag: "Lt", typ: Prod([alfexpr, alfexpr])},
//         {tag: "Gt", typ: Prod([alfexpr, alfexpr])},
//         {tag: "Neg", typ: alfexpr},
//         {tag: "BoolLit", typ: Bool},
//         {tag: "If", typ: Prod([alfexpr, alfexpr, alfexpr])},
//         {tag: "Var", typ: String},
//         {tag: "Let", typ: Prod([String, alfexpr, alfexpr])},
//         {tag: "Fun", typ: Prod([String, alftyp, alfexpr])},
//         {tag: "Ap", typ: Prod([alfexpr, alfexpr])},
//         {tag: "Pair", typ: Prod([alfexpr, alfexpr])},
//         {tag: "PrjL", typ: alfexpr},
//         {tag: "PrjR", typ: alfexpr},
//         {tag: "LetPair", typ: Prod([String, String, alfexpr, alfexpr])},
//       ];
//     },
//   ),
// ];
