open Sexplib.Std;

[@deriving sexp]
type t =
  | Tag(TagErrStatus.t, string)
  | EmptyTagHole(MetaVar.t);

let compare = compare;

let eq = (tag1: t, tag2: t): bool =>
  switch (tag1, tag2) {
  | (Tag(_, _), EmptyTagHole(_))
  | (EmptyTagHole(_), Tag(_, _)) => false
  | (Tag(_, t1), Tag(_, t2)) => String.equal(t1, t2)
  | (EmptyTagHole(u1), EmptyTagHole(u2)) => MetaVar.eq(u1, u2)
  };

let new_TagHole = (u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (u, u_gen) = u_gen |> MetaVarGen.next;
  (EmptyTagHole(u), u_gen);
};

let is_majuscule_letter = (c: char): bool => {
  let code = Char.code(c);
  Char.code('A') <= code && code <= Char.code('Z');
};

let is_minuscule_letter = (c: char): bool => {
  let code = Char.code(c);
  Char.code('a') <= code && code <= Char.code('z');
};

let is_numeric_digit = (c: char): bool => {
  let code = Char.code(c);
  Char.code('0') <= code && code <= Char.code('9');
};

let is_punctuation: char => bool =
  fun
  | '_'
  | '\'' => true
  | _ => false;

let is_tag_char = (c: char): bool => {
  is_majuscule_letter(c)
  || is_minuscule_letter(c)
  || is_numeric_digit(c)
  || is_punctuation(c);
};

let is_tag_name = (str: string): bool => {
  switch (str |> String.to_seq |> List.of_seq) {
  | [] => false
  | [c] => is_majuscule_letter(c)
  | [c, ...cs] => is_majuscule_letter(c) && List.for_all(is_tag_char, cs)
  };
};

let is_complete: t => bool =
  fun
  | Tag(NotInTagHole, _) => true
  | Tag(_)
  | EmptyTagHole(_) => false;

let is_valid: t => bool =
  fun
  | Tag(InTagHole(InvalidName, _), _) => false
  | Tag(InTagHole(_), _)
  | Tag(NotInTagHole, _)
  | EmptyTagHole(_) => true;

module OrderedType = {
  type nonrec t = t;

  let compare = (tag1: t, tag2: t): int =>
    switch (tag1, tag2) {
    | (EmptyTagHole(_), Tag(_)) => (-1)
    | (Tag(_), EmptyTagHole(_)) => 1
    | (EmptyTagHole(u1), EmptyTagHole(u2)) => Int.compare(u1, u2)
    | (Tag(_, t1), Tag(_, t2)) => String.compare(t1, t2)
    };
};

module Set = Set.Make(OrderedType);
module Map = Map.Make(OrderedType);

let consistent = (tag1: t, tag2: t): bool =>
  switch (tag1, tag2) {
  // TagCEHole1
  | (EmptyTagHole(_), _) => true
  // TagCEHole2
  | (_, EmptyTagHole(_)) => true
  // TagCNEHole1
  | (Tag(InTagHole(_), _), _) => true
  // TagCNEHole2
  | (_, Tag(InTagHole(_), _)) => true
  // TagCRefl
  | (_, _) => eq(tag1, tag2)
  };

let fix_holes = (tag: t, dups: Set.t, u_gen: MetaVarGen.t): (t, MetaVarGen.t) =>
  switch (tag) {
  | Tag(_, t) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let status: TagErrStatus.t =
      !is_tag_name(t)
        ? InTagHole(InvalidName, u)
        : Set.mem(tag, dups) ? InTagHole(Duplicate, u) : NotInTagHole;
    (Tag(status, t), u_gen);
  | EmptyTagHole(_) => (tag, u_gen)
  };
