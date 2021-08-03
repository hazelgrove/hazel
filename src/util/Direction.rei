type t =
  | L
  | R;

let pick: (t, ('a, 'a)) => ('a, 'a);
let unpick: (t, ('a, 'a)) => ('a, 'a); 
