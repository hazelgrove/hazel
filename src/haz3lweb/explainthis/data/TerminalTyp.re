open Example;
open ExplainThisForm;

let int_typ: form = {
  let explanation = "The `Int` type classifies 32-bit signed integer values.";
  {
    id: IntTyp,
    syntactic_form: [typ("Int")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let float_typ: form = {
  let explanation = "The `Float` type classifies 64-bit floating-point values according to the IEEE 754 standard.";
  {
    id: FloatTyp,
    syntactic_form: [typ("Float")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let bool_typ: form = {
  let explanation = "The `Bool` type classifies boolean values.";
  {
    id: BoolTyp,
    syntactic_form: [typ("Bool")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let str_typ: form = {
  let explanation = "The `String` type classifies string values.";
  {
    id: StrTyp,
    syntactic_form: [typ("String")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let var_typ = (name: string): form => {
  let explanation = "`%s` is a type variable.";
  {
    id: VarTyp,
    syntactic_form: [name |> abbreviate |> typ],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let module_typ: form = {
  let explanation = "Module Signature Type. Specifies the structure and specifications of a module, detailing the types and relationships of its members, including variables and type aliases.";
  {
    id: ModuleTyp,
    syntactic_form: [typ("{ "), exp("x1"), typ(" : ty1, ... }")],
    expandable_id: None,
    explanation,
    examples: [
      {
        sub_id: ModuleTyp,
        term:
          mk_example(
            "module M : {\nType T = Int,\nx : T\n} =\ntype T = Int in\nlet x:T = 1 in\nin\nM.x",
          ),
        message: {|
      The module M is annotated to have a type member T which is alias of Int, and a member x of type T.
              |},
      },
    ],
  };
};

let int: group = {id: IntTyp, forms: [int_typ]};

let float: group = {id: FloatTyp, forms: [float_typ]};

let bool: group = {id: BoolTyp, forms: [bool_typ]};

let str: group = {id: StrTyp, forms: [str_typ]};

let var = (name: string): group => {id: VarTyp, forms: [var_typ(name)]};

let moduletyp: group = {id: ModuleTyp, forms: [module_typ]};
