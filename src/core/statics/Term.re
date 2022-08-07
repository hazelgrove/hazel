open Sexplib.Std;

/* TERM

   These data structures define the term structures on which
   the static and dynamic semantics of the language are based.
   Each sort has a corresponding U<Sort> module.

   The contained cls type lists the terms of that sort, and
   should be in 1-1 correspondance with the term type which
   is used to build composite terms.

   This is wrapped in a record type to associate a unique id
   with each term. These unique ids are the same as from the
   tile structure from the syntax module, as there is a 1-1
   correspondance between terms and tiles.

   The below functions exist to parse tile structure into
   term structure. The language syntax, as determined by
   Syntax.Form, is an open, data-driven system, so adding
   a syntactic form there will not trigger a static error
   here; you must remember to add a case below for each
   new form added tot the syntax.

   TODO: add tests to check if there are forms and/or terms
   without correponding syntax classes */

module UTyp = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | Int
    | Float
    | Bool
    | Arrow
    | Prod
    | Parens;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(Piece.t)
    | EmptyHole
    | Int
    | Float
    | Bool
    | Arrow(t, t)
    | Prod(t, t)
    | Parens(t)
  and t = {
    id: Id.t,
    term,
  };

  let cls_of_term: term => cls =
    fun
    | Invalid(_) => Invalid
    | EmptyHole => EmptyHole
    | Int => Int
    | Float => Float
    | Bool => Bool
    | Arrow(_) => Arrow
    | Prod(_) => Prod
    | Parens(_) => Parens;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid Type"
    | EmptyHole => "Empty Type Hole"
    | Int
    | Float
    | Bool => "Concrete Type"
    | Arrow => "Arrow Type"
    | Prod => "Product Type"
    | Parens => "Parenthesized Type Term";
};

module UPat = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | Wild
    | Int
    | Float
    | Bool
    | Var
    | Pair
    | Parens
    | TypeAnn;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(Piece.t)
    | EmptyHole
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | Var(Token.t)
    | Pair(t, t)
    | Parens(t)
    | TypeAnn(t, UTyp.t)
  and t = {
    id: Id.t,
    term,
  };

  let cls_of_term: term => cls =
    fun
    | Invalid(_) => Invalid
    | EmptyHole => EmptyHole
    | Wild => Wild
    | Int(_) => Int
    | Float(_) => Float
    | Bool(_) => Bool
    | Var(_) => Var
    | Pair(_) => Pair
    | Parens(_) => Parens
    | TypeAnn(_) => TypeAnn;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid Pattern"
    | EmptyHole => "Empty Pattern Hole"
    | Wild => "Wildcard Pattern"
    | Int => "Integer Literal"
    | Float => "Float Literal"
    | Bool => "Boolean Literal"
    | Var => "Pattern Variable"
    | Pair => "Pair Pattern"
    | Parens => "Parenthesized Pattern"
    | TypeAnn => "Type Annotation";
};

module UExp = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bool =
    | And
    | Or;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_int =
    | Plus
    | Minus
    | Times
    | Divide
    | LessThan
    | GreaterThan
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_float =
    | Plus
    | Minus
    | Times
    | Divide
    | LessThan
    | GreaterThan
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin =
    | Int(op_int)
    | Float(op_float)
    | Bool(op_bool);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | Bool
    | Int
    | Float
    | Fun
    | FunAnn
    | Pair
    | Var
    | Let
    | LetAnn
    | Ap
    | If
    | Seq
    | Test
    | Parens
    | BinOp(op_bin);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(Piece.t) //everything? text? keyword?
    //| InvalidSegment(Segment.t)
    | EmptyHole
    | Bool(bool)
    | Int(int)
    | Float(float)
    | Fun(UPat.t, t)
    | FunAnn(UPat.t, UTyp.t, t)
    | Pair(t, t)
    | Var(Token.t)
    | Let(UPat.t, t, t)
    | LetAnn(UPat.t, UTyp.t, t, t)
    | Ap(t, t)
    //| ApBuiltin(Token.t, list(t))
    // maybe everything with fn semantics should be a builtin e.g. plus??
    | If(t, t, t)
    | Seq(t, t)
    | Test(t)
    | Parens(t)
    | BinOp(op_bin, t, t)
  and t = {
    id: Id.t,
    term,
  };

  let cls_of_term: term => cls =
    fun
    | Invalid(_) => Invalid
    | EmptyHole => EmptyHole
    | Bool(_) => Bool
    | Int(_) => Int
    | Float(_) => Float
    | Fun(_) => Fun
    | FunAnn(_) => FunAnn
    | Pair(_) => Pair
    | Var(_) => Var
    | Let(_) => Let
    | LetAnn(_) => LetAnn
    | Ap(_) => Ap
    | If(_) => If
    | Seq(_) => Seq
    | Test(_) => Test
    | Parens(_) => Parens
    | BinOp(op, _, _) => BinOp(op);

  let show_op_bool: op_bool => string =
    fun
    | And => "Boolean Conjunction"
    | Or => "Boolean Disjunction";

  let show_op_int: op_int => string =
    fun
    | Plus => "Integer Addition"
    | Minus => "Integer Subtraction"
    | Times => "Integer Multiplication"
    | Divide => "Integer Division"
    | LessThan => "Integer Less Than"
    | GreaterThan => "Integer Greater Than"
    | Equals => "Integer Equality";

  let show_op_float: op_float => string =
    fun
    | Plus => "Float Addition"
    | Minus => "Float Subtraction"
    | Times => "Float Multiplication"
    | Divide => "Float Division"
    | LessThan => "Float Less Than"
    | GreaterThan => "Float Greater Than"
    | Equals => "Float Equality";

  let show_binop: op_bin => string =
    fun
    | Int(op) => show_op_int(op)
    | Float(op) => show_op_float(op)
    | Bool(op) => show_op_bool(op);

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid Expression"
    | EmptyHole => "Empty Expression Hole"
    | Bool => "Boolean Literal"
    | Int => "Integer Literal"
    | Float => "Float Literal"
    | Fun => "Function Literal"
    | FunAnn => "Annotated Function Literal"
    | Pair => "Pair Literal"
    | Var => "Variable Reference"
    | Let => "Let Expression"
    | LetAnn => "Annotated Let Expression"
    | Ap => "Function Application"
    | If => "If Expression"
    | Seq => "Sequence Expression"
    | Test => "Test (Effectful)"
    | Parens => "Parenthesized Expression"
    | BinOp(op) => show_binop(op);
};

/* Converts a syntactic type into a semantic type */
let rec utyp_to_ty: UTyp.t => Typ.t =
  utyp =>
    switch (utyp.term) {
    | Invalid(_)
    | EmptyHole => Unknown(Internal) //TODO: is this correct?
    | Int => Int
    | Float => Float
    | Bool => Bool
    | Arrow(u1, u2) => Arrow(utyp_to_ty(u1), utyp_to_ty(u2))
    | Prod(u1, u2) => Prod(utyp_to_ty(u1), utyp_to_ty(u2))
    | Parens(u1) => utyp_to_ty(u1)
    };

let piece_and_kids = (ps: Segment.t, skel: Skel.t): (Piece.t, list(Skel.t)) => {
  let at = List.nth(ps);
  switch (skel) {
  | Op(idx) => (at(idx), [])
  | Pre(idx, skel') => (at(idx), [skel'])
  | Post(skel', idx) => (at(idx), [skel'])
  | Bin(skel_l, idx, skel_r) => (at(idx), [skel_l, skel_r])
  };
};

type ty_temp =
  | Exp(UExp.t)
  | Pat(UPat.t)
  | Typ(UTyp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type temp1 = list(Sort.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type temp2 = list(Skel.t);

/* Converts syntactic segments into terms */
let rec of_seg_and_skel = (ps: Segment.t, skel: Skel.t): UExp.t => {
  let (p, kids) = piece_and_kids(ps, skel);
  of_piece(p, List.map(of_seg_and_skel(ps), kids));
}
and uexp_of_seg = (ps: Segment.t): UExp.t => {
  //NOTE(andrew): filter out incomplete tiles for now
  //TODO(andrew): better approach which still provides feedback inside incomplete tile children
  let ps = List.filter(Piece.is_complete, ps);
  ps |> Segment.skel |> of_seg_and_skel(ps);
}
and of_seg_and_skel_pat = (ps: Segment.t, skel: Skel.t): UPat.t => {
  let (p, kids) = piece_and_kids(ps, skel);
  //TODO(andrew): fix this utter nonsense
  let sorts =
    switch (p) {
    | Whitespace(_) => []
    | Grout(_) => [] //TODO(andrew): handle concave
    | Tile({
        mold: {
          nibs: (
            {sort: sort_l, shape: shape_l},
            {sort: sort_r, shape: shape_r},
          ),
          _,
        },
        _,
      }) =>
      switch (shape_l, shape_r) {
      | (Convex, Convex) => []
      | (Convex, Concave(_)) => [sort_r]
      | (Concave(_), Convex) => [sort_l]
      | (Concave(_), Concave(_)) => [sort_l, sort_r]
      }
    };
  let guy: list(ty_temp) =
    switch (sorts, kids) {
    | _ when !Piece.is_complete(p) => [] //TODO(HACK)
    | ([], []) => []
    | ([Pat], [p]) => [Pat(of_seg_and_skel_pat(ps, p))]
    | ([Typ], [t]) => [Typ(of_seg_and_skel_typ(ps, t))]
    | ([Pat, Typ], [p, ty]) => [
        Pat(of_seg_and_skel_pat(ps, p)),
        Typ(of_seg_and_skel_typ(ps, ty)),
      ]
    | ([Pat, Pat], [p1, p2]) => [
        Pat(of_seg_and_skel_pat(ps, p1)),
        Pat(of_seg_and_skel_pat(ps, p2)),
      ]
    | ([Typ, Typ], [t1, t2]) => [
        Typ(of_seg_and_skel_typ(ps, t1)),
        Typ(of_seg_and_skel_typ(ps, t2)),
      ]
    | _ =>
      print_endline(show_temp1(sorts));
      print_endline(show_temp2(kids));
      // in particular, need to handle incomplete tiles
      failwith("Term nonsense TODO(andrew)");
    };
  of_piece_pat(p, guy);
}
and upat_of_seg = (ps: Segment.t): UPat.t =>
  ps |> Segment.skel |> of_seg_and_skel_pat(ps)
and of_seg_and_skel_typ = (ps: Segment.t, skel: Skel.t): UTyp.t => {
  let (p, kids) = piece_and_kids(ps, skel);
  of_piece_typ(p, List.map(of_seg_and_skel_typ(ps), kids));
}
and utyp_of_seg = (ps: Segment.t): UTyp.t =>
  ps |> Segment.skel |> of_seg_and_skel_typ(ps)
and of_piece = (p: Piece.t, children_h: list(UExp.t)): UExp.t => {
  let invalid = (p: Piece.t): UExp.t => {id: (-1), term: Invalid(p)};
  switch (p) {
  | Whitespace(_) => invalid(p)
  | Grout({id, shape}) =>
    switch (shape) {
    | Convex => {id, term: EmptyHole}
    | Concave => {id, term: EmptyHole}
    //TODO(andrew): do something better with concave holes
    }
  | Tile({id, label, children, mold: _, shards: _} as t) =>
    // TODO(andrew): do better than switching label
    let term: UExp.term =
      switch (/*mold.out,*/ label, children_h, children) {
      | _ when !Tile.is_complete(t) =>
        /* TODO(andrew): more principled handling of incomplete tiles  */
        EmptyHole
      | (["true"], [], []) => Bool(true) //TODO(andrew):generify
      | (["false"], [], []) => Bool(false)
      /* WARNING: is_float must come first because is_int's regexp is strictly more general */
      | ([t], [], []) when Form.is_float(t) => Float(float_of_string(t))
      | ([t], [], []) when Form.is_int(t) => Int(int_of_string(t))
      | ([t], [], []) when Form.is_var(t) => Var(t)
      | ([","], [l, r], []) => Pair(l, r)
      | (["+"], [l, r], []) => BinOp(Int(Plus), l, r)
      | (["-"], [l, r], []) => BinOp(Int(Minus), l, r)
      | (["*"], [l, r], []) => BinOp(Int(Times), l, r)
      | (["/"], [l, r], []) => BinOp(Int(Divide), l, r)
      | (["<"], [l, r], []) => BinOp(Int(LessThan), l, r)
      | ([">"], [l, r], []) => BinOp(Int(GreaterThan), l, r)
      | (["=="], [l, r], []) => BinOp(Int(Equals), l, r)
      | (["+."], [l, r], []) => BinOp(Float(Plus), l, r)
      | (["-."], [l, r], []) => BinOp(Float(Minus), l, r)
      | (["*."], [l, r], []) => BinOp(Float(Times), l, r)
      | (["/."], [l, r], []) => BinOp(Float(Divide), l, r)
      | (["<."], [l, r], []) => BinOp(Float(LessThan), l, r)
      | ([">."], [l, r], []) => BinOp(Float(GreaterThan), l, r)
      | (["==."], [l, r], []) => BinOp(Float(Equals), l, r)
      | (["&&"], [l, r], []) => BinOp(Bool(And), l, r)
      | (["||"], [l, r], []) => BinOp(Bool(Or), l, r)
      | ([";"], [l, r], []) => Seq(l, r)
      | (["test", "end"], [], [test]) => Test(uexp_of_seg(test))
      | (["fun", "->"], [body], [pat]) => Fun(upat_of_seg(pat), body)
      | (["funann", ":", "->"], [body], [pat, typ]) =>
        FunAnn(upat_of_seg(pat), utyp_of_seg(typ), body)
      | (["let", "=", "in"], [body], [pat, def]) =>
        Let(upat_of_seg(pat), uexp_of_seg(def), body)
      | (["letann", ":", "=", "in"], [body], [pat, typ, def]) =>
        LetAnn(upat_of_seg(pat), utyp_of_seg(typ), uexp_of_seg(def), body)
      | (["if", "then", "else"], [alt], [cond, conseq]) =>
        If(uexp_of_seg(cond), uexp_of_seg(conseq), alt)
      | (["(", ")"], [fn], [arg]) => Ap(fn, uexp_of_seg(arg))
      | (["(", ")"], [], [body]) => Parens(uexp_of_seg(body))
      | _ => Invalid(p)
      };
    {id, term};
  };
}
and of_piece_pat = (p: Piece.t, children_h: list(ty_temp)): UPat.t => {
  let invalid: UPat.t = {id: (-1), term: Invalid(p)};
  switch (p) {
  | Whitespace(_) => invalid
  | Grout({id, shape}) =>
    switch (shape) {
    | Convex => {id, term: EmptyHole}
    | Concave => invalid
    }
  | Tile({id, label, children, mold: _, shards: _} as t) =>
    // TODO(andrew): do better than switching label
    let term: UPat.term =
      switch (/*mold.out,*/ label, children_h, children) {
      | _ when !Tile.is_complete(t) => Invalid(p)
      | (["(", ")"], [], [body]) => Parens(upat_of_seg(body))
      | ([","], [Pat(l), Pat(r)], []) => Pair(l, r)
      | (["true"], [], []) => Bool(true) //TODO(andrew):generify
      | (["false"], [], []) => Bool(false)
      | ([":"], [Pat(p), Typ(ty)], []) => TypeAnn(p, ty)
      /* WARNING: is_float must come first because is_int's regexp is strictly more general */
      | ([t], [], []) when Form.is_float(t) => Float(float_of_string(t))
      | ([t], [], []) when Form.is_int(t) => Int(int_of_string(t))
      | ([t], [], []) when Form.is_var(t) => Var(t)
      | ([t], [], []) when Form.is_wild(t) => Wild
      | _ => Invalid(p)
      };
    {id, term};
  };
}
and of_piece_typ = (p: Piece.t, children_h: list(UTyp.t)): UTyp.t => {
  let invalid: UTyp.t = {id: (-1), term: Invalid(p)};
  switch (p) {
  | Whitespace(_) => invalid
  | Grout({id, shape}) =>
    switch (shape) {
    | Convex => {id, term: EmptyHole}
    | Concave => invalid
    }
  | Tile({id, label, children, mold: _, shards: _} as t) =>
    // TODO(andrew): do better than switching label
    let term: UTyp.term =
      switch (/*mold.out,*/ label, children_h, children) {
      | _ when !Tile.is_complete(t) => Invalid(p)
      | (["Int"], [], []) => Int
      | (["Float"], [], []) => Float
      | (["Bool"], [], []) => Bool
      | (["->"], [l, r], []) => Arrow(l, r)
      | ([","], [l, r], []) => Prod(l, r)
      | (["(", ")"], [], [body]) => Parens(utyp_of_seg(body))
      | _ => Invalid(p)
      };
    {id, term};
  };
};

let of_zipper = (z: Zipper.t): UExp.t => z |> Zipper.zip |> uexp_of_seg;

let uexp_of_seg =
  Core_kernel.Memo.general(~cache_size_bound=1000, uexp_of_seg);
