include Base.Space;

let empty: t = [];
let is_empty: t => bool = (==)(empty);

let mk_elem = shape => {
  let id = Id.Gen.next();
  {id, shape};
};

let to_lexemes: t => list(Lexeme.t) = List.map(s => Lexeme.S(s));

let split_cursor = (_: t) => failwith("todo split_cursor");
