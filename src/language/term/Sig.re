[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | SigLet
  | SigType
  | SigModule;

include TermBase.Sig;

let fresh: term => t = IdTagged.fresh;

let rep_id: t => Id.t = IdTagged.rep_id;

let hole = (tms: list(TermBase.Any.t)): TermBase.Sig.term =>
  switch (tms) {
  | [] => EmptyHole
  | [_, ..._] => MultiHole(tms)
  };

let cls_of_term: Grammar.sig_term('a) => cls =
  fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | SigLet(_) => SigLet
  | SigType(_, _) => SigType
  | SigModule(_) => SigModule;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid signature"
  | MultiHole => "Broken signature"
  | EmptyHole => "Signature hole"
  | SigLet => "Let declaration"
  | SigType => "Type declaration"
  | SigModule => "Module declaration";

let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp(),
  };

/* ==================== Member view ====================
   A signature type is a dependent record: later items may mention earlier
   type members by their bare name. Canonical items are
   `SigLet(Asc(Var x, τ))`, `SigModule(Asc(Var m, τ))` and
   `SigType(Var T, τ)`; a `module m : S` member is a value member. In a
   closed signature (Typ.close) every item declares a member, and no two
   members share a name and kind. */

[@deriving (show({with_path: false}), sexp, yojson)]
type member =
  | Val(Var.t, TermBase.Typ.t)
  | TypeManifest(Var.t, TermBase.Typ.t);

/* What a written item declares. */
[@deriving show({with_path: false})]
type reading =
  | Member(member)
  /* A hole or invalid piece where the member would be named. */
  | Incomplete
  /* A `let` whose pattern is not a name, `let (a, b) : (Int, Int)`. */
  | NotAName;

let rec read_pat = (p: TermBase.Pat.t): reading =>
  switch (p.term) {
  | Parens(p) => read_pat(p)
  | Asc(p, ty) =>
    switch (read_pat(p)) {
    | Member(Val(x, _)) => Member(Val(x, ty))
    | reading => reading
    }
  | Var(x) => Member(Val(x, TempGrammar.Typ.unknown(Internal)))
  | EmptyHole
  | MultiHole(_)
  | Invalid(_) => Incomplete
  | _ => NotAName
  };

let read_mpat = (mp: TermBase.MPat.t): reading =>
  switch (mp.term) {
  | Var(x) => Member(Val(x, TempGrammar.Typ.unknown(Internal)))
  | Asc(inner, ty) =>
    switch (MPat.name(inner)) {
    | Some(x) => Member(Val(x, ty))
    | None => Incomplete
    }
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => Incomplete
  };

let read_item = (item: t): reading =>
  switch (item.term) {
  | SigLet(p) => read_pat(p)
  | SigModule(mp) => read_mpat(mp)
  | SigType({term: Var(name), _}, ty) => Member(TypeManifest(name, ty))
  | SigType(_, _)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => Incomplete
  };

let member_of_item = (item: t): option(member) =>
  switch (read_item(item)) {
  | Member(m) => Some(m)
  | Incomplete
  | NotAName => None
  };

let members = (items: list(t)): list(member) =>
  List.filter_map(member_of_item, items);

/* An open signature reached an operation that reads its members' types.
   Those read closed signatures only; Typ.close makes one where a type is
   read from the syntax, so an open one here is a bug in that reading. */
exception OpenSignature;

/* The items of a signature in either reading, for a syntactic walk that
   does not read a member's type: printing, collecting ids, counting. */
let items = (s: TermBase.signature): list(t) =>
  switch (s) {
  | Open(items) => items
  | Closed(items) => ClosedSig.items(items)
  };

/* The items of a closed signature, for an operation that reads their types. */
let closed = (s: TermBase.signature): ClosedSig.t(t) =>
  switch (s) {
  | Open(_) => raise(OpenSignature)
  | Closed(items) => items
  };

let closed_members = (s: TermBase.signature): list(member) =>
  members(ClosedSig.items(closed(s)));

let item_of_member = (m: member): t =>
  switch (m) {
  | Val(x, ty) => TempGrammar.(Sig.sig_let(Pat.asc(Pat.var(x), ty)))
  | TypeManifest(name, ty) => TempGrammar.(Sig.sig_type(TPat.var(name), ty))
  };

/* `module m : ty` — a sub-module member. */
let module_item = (name: Var.t, ty: TermBase.Typ.t): t =>
  TempGrammar.(Sig.sig_module(MPat.asc(MPat.var(name), ty)));

/* Apply [f] to the type carried by a well-formed item, keeping the item's
   form (`let`, `type` or `module`). An item written without a type, `let x`,
   is read as `let x : ?`: [f] is applied to `?`, and the item is ascribed
   the result unless that is still `?`. */
let map_typ = (f: TermBase.Typ.t => TermBase.Typ.t, item: t): t => {
  let refined = (): option(TermBase.Typ.t) =>
    switch (f(TempGrammar.Typ.unknown(Internal))) {
    | {term: Unknown(_), _} => None
    | ty => Some(ty)
    };
  let rec pat_map = (p: TermBase.Pat.t): TermBase.Pat.t =>
    switch (p.term) {
    | Asc(p', ty) => {
        ...p,
        term: (Asc(p', f(ty)): TermBase.Pat.term),
      }
    | Parens(p') => {
        ...p,
        term: (Parens(pat_map(p')): TermBase.Pat.term),
      }
    | Projector(d, p') => {
        ...p,
        term: (Projector(d, pat_map(p')): TermBase.Pat.term),
      }
    | TupLabel(l, p') => {
        ...p,
        term: (TupLabel(l, pat_map(p')): TermBase.Pat.term),
      }
    | Var(x) =>
      switch (refined()) {
      | Some(ty) => {
          ...p,
          term: (Asc(TempGrammar.Pat.var(x), ty): TermBase.Pat.term),
        }
      | None => p
      }
    | _ => p
    };
  let mpat_map = (mp: TermBase.MPat.t): TermBase.MPat.t =>
    switch (mp.term) {
    | Asc(inner, ty) => {
        ...mp,
        term: (Asc(inner, f(ty)): TermBase.MPat.term),
      }
    | Var(x) =>
      switch (refined()) {
      | Some(ty) => {
          ...mp,
          term: (Asc(TempGrammar.MPat.var(x), ty): TermBase.MPat.term),
        }
      | None => mp
      }
    | Invalid(_)
    | EmptyHole
    | MultiHole(_) => mp
    };
  switch (item.term) {
  | SigLet(p) => {
      ...item,
      term: SigLet(pat_map(p)),
    }
  | SigType(tp, ty) => {
      ...item,
      term: SigType(tp, f(ty)),
    }
  | SigModule(mp) => {
      ...item,
      term: SigModule(mpat_map(mp)),
    }
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => item
  };
};

/* The type an item is written with, the one [map_typ] maps. */
let typ_of_item = (item: t): option(TermBase.Typ.t) => {
  let rec of_pat = (p: TermBase.Pat.t) =>
    switch (p.term) {
    | Asc(_, ty) => Some(ty)
    | Parens(p)
    | Projector(_, p)
    | TupLabel(_, p) => of_pat(p)
    | _ => None
    };
  switch (item.term) {
  | SigLet(p) => of_pat(p)
  | SigType(_, ty) => Some(ty)
  | SigModule({term: Asc(_, ty), _}) => Some(ty)
  | SigModule(_)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => None
  };
};

let value_names = (ms: list(member)): list(Var.t) =>
  List.filter_map(
    fun
    | Val(x, _) => Some(x)
    | TypeManifest(_) => None,
    ms,
  );

let type_names = (ms: list(member)): list(Var.t) =>
  List.filter_map(
    fun
    | TypeManifest(x, _) => Some(x)
    | Val(_) => None,
    ms,
  );

/* Whether two members are the same name of the same kind: values and
   modules share a namespace, type members have their own. */
let same_kind = (a: member, b: member) =>
  switch (a, b) {
  | (Val(x, _), Val(y, _))
  | (TypeManifest(x, _), TypeManifest(y, _)) => String.equal(x, y)
  | (Val(_), TypeManifest(_))
  | (TypeManifest(_), Val(_)) => false
  };

let find_value = (ms: list(member), name: Var.t): option(TermBase.Typ.t) =>
  List.find_map(
    fun
    | Val(x, ty) when String.equal(x, name) => Some(ty)
    | Val(_)
    | TypeManifest(_) => None,
    ms,
  );

let find_type_def = (ms: list(member), name: Var.t): option(TermBase.Typ.t) =>
  List.find_map(
    fun
    | TypeManifest(x, ty) when String.equal(x, name) => Some(ty)
    | Val(_)
    | TypeManifest(_) => None,
    ms,
  );
