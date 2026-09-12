/* The syntactic class of a Fumola term, which is what the cursor inspector
   names when the cursor is inside a Fumola program.

   Before this existed, Statics put nothing in the info map for a Fumola
   subterm, so the inspector fell through to its default and reported
   "Whitespace or Comment" for every one of them -- `$leaf` included. This is
   the Fumola counterpart of Drv.Any.cls, and it is deliberately coarse: one
   class per form the tiles can build, named as a reader of the program would
   name it rather than as the constructor spells it. */

[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type t =
  /* holes and the boundary */
  | EmptyHole
  | MultiHole
  | Invalid
  | Hazel
  /* atoms */
  | InstanceName
  | Var
  | Int
  | Float
  | Char
  | Text
  | Bool
  | Null
  | Unit
  | QuotedId
  | Prim
  /* structure */
  | Parens
  | Tuple
  | Block
  | Array
  /* application and access */
  | Ap
  | Proj
  | Index
  | Bang
  /* operators */
  | Variant
  | Opt
  | UnOp
  | Not
  | Unquote
  | BinOp
  | RelOp
  | And
  | Or
  /* control */
  | If
  | Switch
  | Case
  | Assert
  | Ignore
  | Return
  /* the adapton core */
  | Thunk
  | Force
  | Put
  | Get
  | DoPutForce
  | DoGoto
  | DoWithin
  /* declarations */
  | Exp
  | Let
  | Var_
  | Func
  | Import
  /* patterns */
  | PatVar
  | PatWild
  | PatLit
  | PatParens
  | PatTuple
  | PatVariant
  | PatOpt;

/* Prefixed "Fumola" by Cls.show, so these read as the form alone. */
let show: t => string =
  fun
  | EmptyHole => "Empty Hole"
  | MultiHole => "Multi Hole"
  | Invalid => "Invalid Token"
  | Hazel => "Hazel Expression"
  | InstanceName => "Instance Name"
  | Var => "Variable Reference"
  | Int => "Integer Literal"
  | Float => "Float Literal"
  | Char => "Character Literal"
  | Text => "Text Literal"
  | Bool => "Boolean Literal"
  | Null => "Null Literal"
  | Unit => "Unit Literal"
  | QuotedId => "Quoted Name"
  | Prim => "Primitive"
  | Parens => "Parenthesized Term"
  | Tuple => "Tuple Literal"
  | Block => "Block"
  | Array => "Array Literal"
  | Ap => "Application"
  | Proj => "Projection"
  | Index => "Index"
  | Bang => "Unwrap"
  | Variant => "Variant"
  | Opt => "Option"
  | UnOp => "Unary Operation"
  | Not => "Negation"
  | Unquote => "Unquote"
  | BinOp => "Binary Operation"
  | RelOp => "Comparison"
  | And => "Conjunction"
  | Or => "Disjunction"
  | If => "If Expression"
  | Switch => "Switch Expression"
  | Case => "Case"
  | Assert => "Assertion"
  | Ignore => "Ignore"
  | Return => "Return"
  | Thunk => "Thunk"
  | Force => "Force"
  | Put => "Put"
  | Get => "Get"
  | DoPutForce => "Put and Force"
  | DoGoto => "Goto Navigation"
  | DoWithin => "Within Navigation"
  | Exp => "Expression Declaration"
  | Let => "Let Declaration"
  | Var_ => "Var Declaration"
  | Func => "Function Declaration"
  | Import => "Import Declaration"
  | PatVar => "Pattern Variable"
  | PatWild => "Wildcard Pattern"
  | PatLit => "Literal Pattern"
  | PatParens => "Parenthesized Pattern"
  | PatTuple => "Tuple Pattern"
  | PatVariant => "Variant Pattern"
  | PatOpt => "Option Pattern";

let of_lit: FumolaGrammar.lit => t =
  fun
  | Nat(_) => Int
  | Float(_) => Float
  | Char(_) => Char
  | Text(_) => Text
  | Bool(_) => Bool
  | Null => Null
  | Unit => Unit;

let of_exp_term: FumolaTermBase.exp_term => t =
  fun
  | Hole(EmptyHole) => EmptyHole
  | Hole(MultiHole(_)) => MultiHole
  | Hole(Invalid(_)) => Invalid
  | Hazel(_) => Hazel
  | Var(_) => Var
  | Lit(l) => of_lit(l)
  | Paren(_) => Parens
  | Tuple(_) => Tuple
  | Block(_) => Block
  | Array(_, _) => Array
  | QuotedId(_) => QuotedId
  | Prim(_) => Prim
  | Ap(_, _) => Ap
  | Proj(_, _) => Proj
  | Index(_, _) => Index
  | Bang(_) => Bang
  | Variant(_, _) => Variant
  | Opt(_) => Opt
  | Un(_, _) => UnOp
  | Not(_) => Not
  | Unquote(_) => Unquote
  | Bin(_, _, _) => BinOp
  | Rel(_, _, _) => RelOp
  | And(_, _) => And
  | Or(_, _) => Or
  | If(_, _, _) => If
  | Switch(_, _) => Switch
  | Assert(_) => Assert
  | Ignore(_) => Ignore
  | Return(_) => Return
  | Thunk(_) => Thunk
  | Force(_) => Force
  | Put(_, _) => Put
  | Get(_) => Get
  | DoPutForce(_, _) => DoPutForce
  | DoNav(Goto, _, _, _) => DoGoto
  | DoNav(Within, _, _, _) => DoWithin;

let of_dec: FumolaTermBase.dec => t =
  d =>
    switch (d.term) {
    | DHole(EmptyHole) => EmptyHole
    | DHole(MultiHole(_)) => MultiHole
    | DHole(Invalid(_)) => Invalid
    | DExp(_) => Exp
    | DLet(_, _) => Let
    | DVar(_, _) => Var_
    | DFunc(_, _, _) => Func
    | DImport(_, _) => Import
    };

let of_pat: FumolaTermBase.pat => t =
  p =>
    switch (p.term) {
    | PHole(EmptyHole) => EmptyHole
    | PHole(MultiHole(_)) => MultiHole
    | PHole(Invalid(_)) => Invalid
    | PVar(_) => PatVar
    | PWild => PatWild
    | PLit(_) => PatLit
    | PParen(_) => PatParens
    | PTuple(_) => PatTuple
    | PVariant(_, _) => PatVariant
    | POpt(_) => PatOpt
    };
