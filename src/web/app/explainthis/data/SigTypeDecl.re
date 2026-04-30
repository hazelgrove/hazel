open Haz3lcore;
open ExplainThisForm;
open Example;

let _tpat = tpat("T");
let _typ = typ("ty");

let sig_type_decl_coloring_ids =
    (~tpat_id: Id.t, ~typ_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_tpat), tpat_id),
  (Piece.id(_typ), typ_id),
];

let sig_type_decl_form: form = {
  let explanation = "A signature type declaration defines a [*type alias*](%s) for the [*type*](%s) within the signature. Note: type declarations in signatures are currently limited — they are parsed but not yet used during type checking. See the Modules documentation for details.";
  {
    id: SigTypeDecl,
    syntactic_form: [
      mk_sig_type([[space(), _tpat, space()]]),
      space(),
      _typ,
    ],
    expandable_id: None,
    explanation,
    examples: [
      {
        sub_id: SigType1,
        term:
          mk_example(
            "let m : {\ntype T = Int;\nlet x : T\n} = {\ntype T = Int;\nlet x = 5\n} in m",
          ),
        message: "A signature with a type alias T for Int used to annotate field x. Note: T appears as a static error in `let x : T` because type declarations in signatures are not yet used during type checking.",
      },
    ],
  };
};

let sig_type_decls: group = {
  id: SigTypeDecl,
  forms: [sig_type_decl_form],
};
