open Util;

/**
 * higher precedence means lower int representation
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;

[@deriving show({with_path: false})]
type precedence_element =
  | Unquote
  | Ap
  | Neg
  | Power
  | Mult
  | Not
  | Plus
  | Cons
  | Concat
  | Eqs
  | And
  | Or
  | Ann
  | If
  | Fun
  | Semi
  | Let
  | Filter
  | RuleArr
  | RulePre
  | RuleSep
  | Case
  | Comma
  | TypePlus
  | TypeArrow
  | TypeProd;

let ordering: list(list(precedence_element)) = [
  [Unquote],
  [Ap],
  [Neg],
  [TypePlus],
  [TypeArrow],
  [Power],
  [Mult, Not],
  [Plus],
  [Cons],
  [Concat],
  [Eqs],
  [And],
  [Or],
  [Ann],
  [If],
  [Fun],
  [Comma, TypeProd],
  [Semi],
  [Let],
  [Filter],
  [RuleArr],
  [RulePre],
  [RuleSep],
  [Case],
];

let ordering_indexed: list((precedence_element, int)) =
  ordering
  |> List.mapi((i, l) => l |> List.map(e => (e, i + 1)))
  |> List.concat;

let assoc_ordering_index: precedence_element => int =
  e => List.assoc(e, ordering_indexed);

let max: t = 0;
let unquote = assoc_ordering_index(Unquote);
let ap = assoc_ordering_index(Ap);
let neg = assoc_ordering_index(Neg);
let power = assoc_ordering_index(Power);
let mult = assoc_ordering_index(Mult);
let not_ = assoc_ordering_index(Not);
let plus = assoc_ordering_index(Plus);
let cons = assoc_ordering_index(Cons);
let concat = assoc_ordering_index(Concat);
let eqs = assoc_ordering_index(Eqs);
let and_ = assoc_ordering_index(And);
let or_ = assoc_ordering_index(Or);
let ann = assoc_ordering_index(Ann);
let if_ = assoc_ordering_index(If);
let fun_ = assoc_ordering_index(Fun);
let semi = assoc_ordering_index(Semi);
let let_ = assoc_ordering_index(Let);
let filter = assoc_ordering_index(Filter);
let rule_arr = assoc_ordering_index(RuleArr);
let rule_pre = assoc_ordering_index(RulePre);
let rule_sep = assoc_ordering_index(RuleSep);
let case_ = assoc_ordering_index(Case);

let comma = assoc_ordering_index(Comma);

let type_plus = assoc_ordering_index(TypePlus);
let type_arrow = assoc_ordering_index(TypeArrow);
let type_prod = assoc_ordering_index(TypeProd);

let min: t = List.length(ordering) + 1;

let compare = (p1: t, p2: t): int =>
  (-1) * Int.compare((p1 :> int), (p2 :> int));
// let min = (p1: t, p2: t): t => max(p1, p2);

let associativity_map: IntMap.t(Direction.t) =
  [
    (mult, Direction.Left),
    (plus, Left),
    (power, Right),
    (cons, Right),
    (concat, Right),
    (ann, Left),
    (eqs, Left),
    (type_arrow, Right),
  ]
  |> List.to_seq
  |> IntMap.of_seq;

let associativity = (p: t): option(Direction.t) =>
  IntMap.find_opt(p, associativity_map);
