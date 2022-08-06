open Sexplib.Std;

/* cls = SYNTAX CLASSES: source of truth for supported forms
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
    | Prod;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(Piece.t)
    | EmptyHole
    | Int
    | Float
    | Bool
    | Arrow(t, t)
    | Prod(t, t)
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
    | Prod(_) => Prod;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid Type"
    | EmptyHole => "Empty Type Hole"
    | Int
    | Float
    | Bool => "Concrete Type"
    | Arrow => "Arrow Type"
    | Prod => "Product Type";
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
    | Pair;

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
    | Pair(_) => Pair;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid Pattern"
    | EmptyHole => "Empty Pattern Hole"
    | Wild => "Wildcard Pattern"
    | Int => "Integer Literal"
    | Float => "Float Literal"
    | Bool => "Boolean Literal"
    | Var => "Pattern Variable"
    | Pair => "Pair Pattern";
};

module UExp = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type exp_op_int =
    | Plus
    | Lt;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type exp_op_float =
    | Plus
    | Lt;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type exp_op_bool =
    | And;

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
    | OpInt(exp_op_int)
    | OpFloat(exp_op_float)
    | OpBool(exp_op_bool);

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
    | OpInt(exp_op_int, t, t)
    | OpFloat(exp_op_float, t, t)
    | OpBool(exp_op_bool, t, t)
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
    | OpInt(x, _, _) => OpInt(x)
    | OpFloat(x, _, _) => OpFloat(x)
    | OpBool(x, _, _) => OpBool(x);

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
    | OpInt(Plus) => "Integer Addition"
    | OpInt(Lt) => "Integer Less-Than"
    | OpFloat(Plus) => "Float Addition"
    | OpFloat(Lt) => "Float Less-Than"
    | OpBool(And) => "Boolean Conjunction";
};

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
    };

let piece_and_kids = (ps, skel: Skel.t): (Piece.t, list(Skel.t)) => {
  let at = List.nth(ps);
  switch (skel) {
  | Op(idx) => (at(idx), [])
  | Pre(idx, skel') => (at(idx), [skel'])
  | Post(skel', idx) => (at(idx), [skel'])
  | Bin(skel_l, idx, skel_r) => (at(idx), [skel_l, skel_r])
  };
};

let rec of_seg_and_skel = (ps: Segment.t, skel: Skel.t): UExp.t => {
  let (p, kids) = piece_and_kids(ps, skel);
  of_piece(p, List.map(of_seg_and_skel(ps), kids));
}
and uhexp_of_seg = (ps: Segment.t): UExp.t =>
  ps |> Segment.skel |> of_seg_and_skel(ps)
and of_seg_and_skel_pat = (ps: Segment.t, skel: Skel.t): UPat.t => {
  let (p, kids) = piece_and_kids(ps, skel);
  of_piece_pat(p, List.map(of_seg_and_skel_pat(ps), kids));
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
    | Concave => invalid(p)
    }
  | Tile({id, label, children, mold: _, shards: _} as t) =>
    // TODO(andrew): do better than switching label
    let term: UExp.term =
      switch (/*mold.out,*/ label, children_h, children) {
      | _ when !Tile.is_complete(t) => Invalid(p)
      | (["true"], [], []) => Bool(true) //TODO(andrew):generify
      | (["false"], [], []) => Bool(false)
      /* WARNING: is_float must come first because is_int's regexp is strictly more general */
      | ([t], [], []) when Form.is_float(t) => Float(float_of_string(t))
      | ([t], [], []) when Form.is_int(t) => Int(int_of_string(t))
      | ([t], [], []) when Form.is_var(t) => Var(t)
      | ([","], [l, r], []) => Pair(l, r)
      | (["+"], [l, r], []) => OpInt(Plus, l, r)
      | (["+."], [l, r], []) => OpFloat(Plus, l, r)
      | (["<"], [l, r], []) => OpInt(Lt, l, r)
      | (["<."], [l, r], []) => OpFloat(Lt, l, r)
      | (["&&"], [l, r], []) => OpBool(And, l, r)
      | (["fun", "->"], [body], [pat]) => Fun(upat_of_seg(pat), body)
      | (["funann", ":", "->"], [body], [pat, typ]) =>
        FunAnn(upat_of_seg(pat), utyp_of_seg(typ), body)
      | (["let", "=", "in"], [body], [pat, def]) =>
        Let(upat_of_seg(pat), uhexp_of_seg(def), body)
      | (["letann", ":", "=", "in"], [body], [pat, typ, def]) =>
        LetAnn(upat_of_seg(pat), utyp_of_seg(typ), uhexp_of_seg(def), body)
      | (["if", "then", "else"], [alt], [cond, conseq]) =>
        If(uhexp_of_seg(cond), uhexp_of_seg(conseq), alt)
      | (["(", ")"], [fn], [arg]) => Ap(fn, uhexp_of_seg(arg))
      //TODO(andrew): more cases
      | _ => Invalid(p)
      };
    {id, term};
  };
}
and of_piece_pat = (p: Piece.t, children_h: list(UPat.t)): UPat.t => {
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
      | ([","], [l, r], []) => Pair(l, r)
      | (["true"], [], []) => Bool(true) //TODO(andrew):generify
      | (["false"], [], []) => Bool(false)
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
      | _ => Invalid(p)
      };
    {id, term};
  };
};

let of_zipper = (z: Zipper.t): UExp.t => z |> Zipper.zip |> uhexp_of_seg;
