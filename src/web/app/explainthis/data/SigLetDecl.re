open Haz3lcore;
open ExplainThisForm;
open Example;

let _pat = pat("p");

let sig_let_decl_coloring_ids =
    (~pat_id: Id.t): list((Id.t, Id.t)) => [(Piece.id(_pat), pat_id)];

let sig_let_decl_form: form = {
  let explanation = "A signature let declaration specifies the expected type of a [*field*](%s) of a module matching this signature.";
  {
    id: SigLetDecl,
    syntactic_form: [mk_sig_let([]), space(), _pat],
    expandable_id: None,
    explanation,
    examples: [
      {
        sub_id: SigLet1,
        term:
          mk_example(
            "let m : {\nlet x : Int;\nlet y : Bool\n} = {\nlet x = 1;\nlet y = true\n} in m",
          ),
        message: "A signature with two let declarations specifying the types of fields x and y.",
      },
    ],
  };
};

let sig_let_decls: group = {
  id: SigLetDecl,
  forms: [sig_let_decl_form],
};
