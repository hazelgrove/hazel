open Alcotest;
open Language;

let testable_error_map =
  testable(
    Fmt.using(Statics.Map.show_errors, Fmt.string),
    Statics.Map.equal_errors,
  );

let statics = Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)));

let parse_menhir = (s: string) => {
  MenhirParser.Conversion.Exp.of_menhir_ast(
    MenhirParser.Interface.parse_program(s),
  );
};

let has_errors =
    (
      ~actual_errors_filter=?,
      name: string,
      exp: string,
      errors: list(Info.error),
    ) => {
  test_case(
    name,
    `Quick,
    () => {
      let indicated_exp: Grammar.exp_t(bool) = parse_menhir(exp);
      let (e, ids) =
        MenhirParser.Conversion.Exp.get_indicated_ids(indicated_exp);

      let (s, _) = statics(e);
      let errors_map = Statics.Map.collect_errors(s);
      let actual_errors =
        switch (actual_errors_filter) {
        | Some(f) => f(errors_map)
        | None => errors_map
        };
      let expected_errors = Id.Map.of_list(List.combine(ids, errors));

      Alcotest.check(
        testable_error_map,
        "Static Errors",
        expected_errors,
        actual_errors,
      );
    },
  );
};

let has_non_common_errors =
  has_errors(
    ~actual_errors_filter=
      Statics.Map.filter((_, err) =>
        switch (err) {
        | Info.Pat(Info.Common(_)) => false
        | Info.Exp(Info.Common(_)) => false
        | _ => true
        }
      ),
  );

let no_errors = (name: string, exp: string) => has_errors(name, exp, []);

let bare_let =
  no_errors("Bare let has no error on pattern", "let x = 1 in x");

let bare_fun = no_errors("Bare fun has no error on pattern", "fun x -> x");

let annotated_let =
  no_errors("Annotated let has no error on pattern", "let x : Int = 1 in x");

let annotated_fun =
  no_errors("Annotated fun has no error on pattern", "fun (x : Int) -> x");

let let_tuple =
  no_errors(
    "Let binding a tuple has no error on pattern",
    "let (x, y, z) = (1, 2, 3) in x",
  );

let fun_tuple =
  no_errors(
    "Fun binding a tuple has no error on pattern",
    "fun (x, y, z) -> x",
  );

let annotated_let_tuple =
  no_errors(
    "Annotated let binding a tuple has no error on pattern",
    "let (x, y, z): (Int, Int, Int) = (1, 2, 3) in x",
  );

let annotated_fun_tuple =
  no_errors(
    "Annotated fun binding a tuple has no error on pattern",
    "fun ((x, y, z) : (Int, Int, Int)) -> x",
  );

let peanut_1a =
  no_errors(
    "Peanut Figure 1a: Exhaustive + Irredundant Tree",
    {|
type Tree = +Empty + Leaf(Int) + Node([Tree]) in
let f = fun (x : Tree) ->
  case x
    | Node([]) => Empty
    | Node([x]) => Node([f(x), Empty])
    | Node([x, y]) => Node([f(x), f(y)])
    | Node(x::y::tl) => Node(f(x)::[f(Node(y::tl))])
    | Leaf(x) => Leaf(x)
    | Empty => Empty
  end
in ?|},
  );

let peanut_1b =
  has_errors(
    "Peanut Figure 1b: Inexhaustive + Redundant (Second Pattern)",
    {|
type Tree = +Empty + Leaf(Int) + Node([Tree]) in
let f = fun (x : Tree) ->
  {{{case x
    | Node(x::y::tl) => Node(f(x)::[f(Node(y::tl))])
    | {{{Node([x, y])}}} => Node([f(x), f(y)])
    | Node([x]) => Node([f(x), Empty])
    | Node([]) => Empty
    | Empty => Empty
  end}}}
in ?|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(
              ap(constructor("Leaf", None), wild())
            ),
          ),
        ),
      ),
      Info.Pat(Redundant(None)),
    ],
  );

let peanut_2a =
  no_errors(
    "Peanut Figure 2a: Indeterminately Exhaustive",
    {|
let odd_length : [Int] -> Bool =
  fun xs ->
    case xs
    | [] => false
    | x::? => true
    end
in ?|},
  );

let peanut_2b =
  has_errors(
    "Peanut Figure 2b: Necessarily Exhaustive",
    {|
let odd_length : [Int] -> Bool =
  fun xs ->
    {{{case xs
      | [] => false
      | x::?::? => true
    end}}} in ?
|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(cons(wild(), list_lit([]))),
          ),
        ),
      ),
    ],
  );

let peanut_2c =
  no_errors(
    "Peanut Figure 2c: Necessarily Exhaustive",
    {|
let not : Bool -> Bool = ? in
let odd_length : [Int] -> Bool =
  fun xs ->
    case xs
      | [] => false
      | x::? => true
      | x::tl => not(odd_length(tl))
    end in ?
|},
  );

let peanut_3a =
  no_errors(
    "Peanut Figure 3a: Necessarily Irredundant (first two patterns) + Indeterminately Redundant (third pattern)",
    {|
let odd_length : [Int] -> Bool =
  fun xs ->
    case xs
      | [] => false
      | x::? => true
      | x::y::tl => odd_length(tl)
    end in ?|},
  );

let peanut_3b =
  has_errors(
    "Peanut Figure 3b: Necessarily Redundant (third pattern)",
    {|
let odd_length : [Int] -> Bool =
  fun xs ->
    case xs
      | [] => false
      | x::tl => odd_length(tl)
      | {{{x::?}}} => true
    end in ?|},
    [Info.Pat(Redundant(None))],
  );

let loooong_list =
  no_errors(
    "Loooong List: Exhaustive",
    {|let x : [Int] = ? in
case x
| [] => 1
| [_] => 2
| [_, _] => 3
| [_, _, _] => 4
| [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => 5
| [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => 6
| _ => 7
end|},
  );

let list_inexhaustive_nil =
  has_errors(
    "List: Inexhaustive Nil",
    {|
    let x : [Int] = ? in
    {{{case x
    | _::_ => 1
    end}}}
|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(IdTagged.FreshGrammar.Pat.(list_lit([]))),
        ),
      ),
    ],
  );

let list_inexhaustive_cons =
  has_errors(
    "List: Inexhaustive Cons",
    {|
      let x : [Int] = ? in
      {{{case x
      | [] => 0
      | a::[] => 1
      end}}}
|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(cons(wild(), cons(wild(), wild()))),
          ),
        ),
      ),
    ],
  );

let list_inexhaustive_cons_long =
  has_errors(
    "List: Inexhaustive Cons Long",
    {|
      let x : [Int] = ? in
      {{{case x
      | [] => 0
      | _::[] => 0
      | _::_::[] => 1
      | _::_::_::_::[] => 1
      end}}}
|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(
              cons(
                wild(),
                cons(
                  wild(),
                  cons(wild(), cons(wild(), cons(wild(), wild()))),
                ),
              )
            ),
          ),
        ),
      ),
    ],
  );

let list_inexhaustive_tuple_with_elt =
  has_errors(
    "List: Inexhaustive In Tuple with Second Element",
    {|
  let x : ([Int], Int) = ? in
  {{{case x
  | (hd::[], _) => 0
  end}}}
  |},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(
              tuple([cons(wild(), cons(wild(), wild())), wild()])
            ),
          ),
        ),
      ),
    ],
  );

let list_inexhaustive_triple =
  has_errors(
    "List: Inexhaustive Triple",
    {|
    let x : ([Int], [Int], Int) = ? in
    {{{case x
    | (_::_, [], _) => 0
    | ([], [], _) => 0
    end}}}
      |},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(
              tuple([cons(wild(), wild()), cons(wild(), wild()), wild()])
            ),
          ),
        ),
      ),
    ],
  );

let list_inexhaustive_triple_elt_first =
  has_errors(
    "List: Inexhaustive In Triple with First Element",
    {|
    let x : (Int, [Int], [Int]) = ? in
    {{{case x
    | (0, _, []) => 0
    | (0, _, _::_) => 1
    end}}}
    |},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(
              tuple([big_int(Bigint.of_int(1)), wild(), wild()])
            ),
          ),
        ),
      ),
    ],
  );

let list_inexhaustive_middle_quad =
  has_errors(
    "List: Inexhaustive List Middle Element In Quad",
    {|
    let x : ([Int], [Int], Int, [Int]) = ? in
    {{{case x
    | (_::_, [], _, []) => 1
    | (_::_, [], _, _::_) => 2
    end}}}
    |},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(
              tuple([
                cons(wild(), wild()),
                cons(wild(), wild()),
                wild(),
                wild(),
              ])
            ),
          ),
        ),
      ),
    ],
  );

let integers_exhaustive =
  no_errors(
    "Integers: Exhaustive",
    {|
let x : Int = ? in
case x
  | 1 => 1
  | 2 => 2
  | _ => 3
end|},
  );

let integers_non_exhaustive =
  has_errors(
    "Integers: Non-Exhaustive",
    {|
let x : Int = ? in
{{{case x
  | 1 => 1
  | 2 => 2
end}}}|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(big_int(Bigint.of_int(0))),
          ),
        ),
      ),
    ],
  );

let integers_tuple_non_exhaustive =
  has_errors(
    "Integers: Non-Exhaustive Tuple",
    {|
let x : (Int, Int) = ? in
{{{case x
  | (0, 0) => 1
  | (0, _) => 2
end}}}|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(
              tuple([big_int(Bigint.of_int(1)), wild()])
            ),
          ),
        ),
      ),
    ],
  );

let integers_redundant =
  has_errors(
    "Integers: Redundant",
    {|
let x : Int = ? in
case x
  | 1 => 1
  | 2 => 2
  | {{{1}}} => 1
  | _ => 3
end|},
    [Info.Pat(Redundant(None))],
  );

let floats_exhaustive =
  no_errors(
    "Floats: Exhaustive",
    {|
let x : Float = ? in
case x
  | 1.0 => 1
  | 2.0 => 2
  | _ => 3
end|},
  );

let floats_non_exhaustive =
  has_errors(
    "Floats: Non-Exhaustive",
    {|
let x : Float = ? in
{{{case x
  | 1.0 => 1
  | 2.0 => 2
end}}}|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(IdTagged.FreshGrammar.Pat.(float(0.0))),
        ),
      ),
    ],
  );

let floats_tuple_non_exhaustive =
  has_errors(
    "Floats: Non-Exhaustive Tuple",
    {|
let x : (Float, Float) = ? in
{{{case x
  | (0.0, 0.0) => 1
  | (0, _) => 2
end}}}|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(tuple([float(1.0), wild()])),
          ),
        ),
      ),
    ],
  );

let floats_redundant =
  has_errors(
    "Floats: Redundant",
    {|
let x : Float = ? in
case x
  | 1.0 => 1
  | 2.0 => 2
  | {{{1.0}}} => 1
  | _ => 3
end|},
    [Info.Pat(Redundant(None))],
  );

let strings_exhaustive =
  no_errors(
    "Strings: Exhaustive",
    {|
let x : String = ? in
case x
  | "ABC" => 1
  | "" => 2
  | _ => 3
end|},
  );

let strings_non_exhaustive =
  has_errors(
    "Strings: Non-Exhaustive",
    {|
let x : String = ? in
{{{case x
  | "ABC" => 1
  | "" => 2
end}}}|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(IdTagged.FreshGrammar.Pat.(string("*"))),
        ),
      ),
    ],
  );

let strings_non_exhaustive_empty =
  has_errors(
    "Strings: Non-Exhaustive Empty",
    {|
let x : String = ? in
{{{case x
  | "ABC" => 1
  | "a" => 2
end}}}|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(IdTagged.FreshGrammar.Pat.(string(""))),
        ),
      ),
    ],
  );

let strings_non_exhaustive_tuple =
  has_errors(
    "Strings: Non-Exhaustive Tuple",
    {|
let x : (String, String) = ? in
{{{case x
  | ("", "")  => 1
  | ("", "*")  => 2
  | ("", _)  => 3
end}}}|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(tuple([string("*"), wild()])),
          ),
        ),
      ),
    ],
  );

let strings_redundant =
  has_errors(
    "Strings: Redundant",
    {|
let x : String = ? in
case x
  | "ABC" => 1
  | "" => 2
  | {{{""}}} => 2
  | {{{"ABC"}}} => 1
  | _ => 3
end|},
    [Info.Pat(Redundant(None)), Info.Pat(Redundant(None))],
  );

let bools_exhaustive =
  no_errors(
    "Bools: Exhaustive",
    {|
let x : Bool = ? in
case x
  | true => 1
  | false => 2
end|},
  );

let bools_non_exhaustive =
  has_errors(
    "Bools: Non-Exhaustive",
    {|
let x : Bool = ? in
let y = {{{case x
  | true => 1
end}}} in
let z = {{{case x
  | false => 2
end}}} in ?
|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(IdTagged.FreshGrammar.Pat.(bool(true))),
        ),
      ),
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(IdTagged.FreshGrammar.Pat.(bool(false))),
        ),
      ),
    ],
  );

let bools_redundant =
  has_errors(
    "Bools: Redundant",
    {|
let x : Bool = ? in
case x
  | true => 1
  | false => 2
  | {{{true}}} => 2
  | {{{false}}} => 1
  | {{{_}}} => 3
end|},
    [
      Info.Pat(Redundant(None)),
      Info.Pat(Redundant(None)),
      Info.Pat(Redundant(None)),
    ],
  );

let unit_exhaustive =
  no_errors(
    "Unit: Exhaustive",
    {|
let x : () = ? in
case x
| () => 1
end
    |},
  );

let unit_redundant =
  has_errors(
    "Unit: Redundant",
    {|
let x : () = ? in
case x
| () => 1
| {{{()}}} => 1
| {{{_}}} => 2
end
    |},
    [Info.Pat(Redundant(None)), Info.Pat(Redundant(None))],
  );

let rank_compare =
  has_errors(
    "Rank Compare -- Andrew's Example from Issue #1473",
    {|
type Rank =
  + Ace
  + King
  + Queen
  + Jack in
let rank_compare: (Rank, Rank) -> Int =
  fun (r1, r2) ->
    case (r1, r2)
      | (Ace, Ace) => 0
      | (Ace, _) => -1
      | (_, Ace) => 1
      | (King, King) => 0
      | (King, _) => -1
      | (_, King) => 1
      | (Queen, Queen) => 0
      | (Queen, _) => -1
      | (_, Queen) => 1
      | (Jack, Jack) => 0
      | {{{(Jack, _)}}} => -1
      | {{{_}}} => 1
end in ?
|},
    [Info.Pat(Redundant(None)), Info.Pat(Redundant(None))],
  );

let rank_compare_inexhaustive =
  has_errors(
    "Rank Compare -- Andrew's Example from Issue #1473 with cases removed",
    {|
type Rank =
  + Ace
  + King
  + Queen
  + Jack in
let rank_compare: (Rank, Rank) -> Int =
  fun (r1, r2) ->
    {{{case (r1, r2)
      | (Ace, Ace) => 0
      | (Ace, _) => -1
      | (_, Ace) => 1
      | (King, King) => 0
      | (King, _) => -1
      | (_, King) => 1
      | (Queen, Queen) => 0
      | (Queen, _) => -1
      | (_, Queen) => 1
end}}} in ?
|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(
              tuple([wild(), constructor("Jack", None)])
            ),
          ),
        ),
      ),
    ],
  );

let rank_let_inexhaustive =
  has_errors(
    "Rank Compare -- Let Inexhaustive",
    {|
type Rank =
  + Ace
  + King
  + Queen
  + Jack in
let x : Rank = ? in
{{{let Ace = x in
?}}}|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(IdTagged.FreshGrammar.Pat.(constructor("Jack", None))),
        ),
      ),
    ],
  );

let rank_fun_inexhaustive =
  has_errors(
    "Rank Compare -- Fun Inexhaustive",
    {|
type Rank =
  + Ace
  + King
  + Queen
  + Jack in
let x : Rank = ? in
let f = {{{fun Ace -> ?}}} in
?|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(IdTagged.FreshGrammar.Pat.(constructor("Jack", None))),
        ),
      ),
    ],
  );

let nested_constructors_inexhaustive =
  has_errors(
    "Nested Constructors: Inexhaustive",
    {|
type Tree = +Empty + Leaf(Int) + Node([Tree]) in
let f = fun (x : Tree) ->
  {{{case x
    | Node([]) => Empty
    | Node([Node(_)]) => Empty
    | Node([x, y]) => Node([f(x), f(y)])
    | Node(x::y::tl) => Node(f(x)::[f(Node(y::tl))])
    | Leaf(x) => Leaf(x)
    | Empty => Empty
  end}}}
in ?|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(
              ap(constructor("Node", None), cons(wild(), list_lit([])))
            ),
          ),
        ),
      ),
    ],
  );

let nested_constructors_exhaustive =
  no_errors(
    "Nested Constructors: Inexhaustive",
    {|
type Tree = +Empty + Leaf(Int) + Node([Tree]) in
let f = fun (x : Tree) ->
  case x
    | Node([]) => Empty
    | Node([Node(_)]) => Empty
    | Node([Empty]) => Empty
    | Node([Leaf(_)]) => Empty
    | Node([x, y]) => Node([f(x), f(y)])
    | Node(x::y::tl) => Node(f(x)::[f(Node(y::tl))])
    | Leaf(x) => Leaf(x)
    | Empty => Empty
  end
in ?|},
  );

let multiple_holes_irredundant =
  no_errors(
    "Multiple Holes: Irredundant",
    {|
type Tree = +Empty + Leaf(Int) + Node([Tree]) in
let f = fun (x : Tree) ->
  case x
    | Node([]) => Empty
    | ? => Empty
    | ? => Empty
  end
in ?|},
  );

let unknown_scrutinee_exhaustive =
  no_errors(
    "Unknown Scrutinee is Exhaustive",
    {|
type Tree = +Empty + Leaf(Int) + Node([Tree]) in
let f = fun (x : ?) ->
  case x
    | Node([]) => Empty
  end
in ?|},
  );

let unknown_scrutinee_redundant_vars =
  has_errors(
    "Unknown Scrutinee is Exhaustive",
    {|
type Tree = +Empty + Leaf(Int) + Node([Tree]) in
let f = fun (x : ?) ->
  case x
    | x => Empty
    | {{{x}}} => Empty
    | {{{_}}} => Empty
  end
in ?|},
    [Info.Pat(Redundant(None)), Info.Pat(Redundant(None))],
  );

let unknown_scrutinee_tuples_of_many_lengths =
  has_errors(
    "Unknown Scrutinee: Tuples of Many Lengths",
    {|
    let x : ? = ? in
    case x
    | (x, y) => 1
    | (x, y, z) => 2
    | {{{(x, y, z)}}} => 3
    | {{{(a, b)}}} => 4
    end|},
    [Info.Pat(Redundant(None)), Info.Pat(Redundant(None))],
  );

let partially_unknown_scrutinee_inexhaustive =
  has_errors(
    "Partially Unknown Scrutinee: Inxhaustive",
    {|
type Tree = +Empty + Leaf(?) + Node([Tree]) in
let f = fun (x : Tree) ->
  {{{case x
    | Empty => 1
    | Leaf(_) => 2
  end}}}
in ?|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(
              ap(constructor("Node", None), wild())
            ),
          ),
        ),
      ),
    ],
  );

let partially_unknown_scrutinee_redundancy =
  has_errors(
    "Partially Unknown Scrutinee: Redundancy",
    {|
type Tree = +Empty + Leaf(?) + Node([Tree]) in
let f = fun (x : Tree) ->
  {{{case x
    | Empty => 1
    | Leaf(_) => 2
    | {{{Leaf(x)}}} => 3
  end}}}
in ?|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(
              ap(constructor("Node", None), wild())
            ),
          ),
        ),
      ),
      Info.Pat(Redundant(None)),
    ],
  );

let labeled_tuple_exhaustiveness =
  no_errors(
    "Labeled Tuple Exhaustiveness Stress Test",
    {|
type ATree = +A + B + C + D in
type BTree = +E + F + G + H in
type Tuple = (x=ATree, y=BTree, z=ATree) in
let f = fun (tpl : Tuple) ->
  case tpl
    | (A, E, A) => 1
    | (x=B, y=E, z=C) => 2
    | (y=_, x=C, _) => 3
    | (z=C, y=_, x=D) => 5
    | (B, _, _) => 6
    | (x=A, _, _) => 7
    | (x=D, z=_, y=_) => 8
  end
in ?|},
  );

let labeled_tuple_inexhaustiveness =
  has_errors(
    "Labeled Tuple Inexhaustiveness Stress Test",
    {|
type ATree = +A + B + C + D in
type BTree = +E + F + G + H in
type Tuple = (x=ATree, y=BTree, z=ATree) in
let f = fun (tpl : Tuple) ->
  {{{case tpl
    | (A, E, A) => 1
    | (x=B, y=E, z=C) => 2
    | (y=_, x=C, _) => 3
    | (z=C, y=_, x=D) => 5
    | (B, _, _) => 6
    | (x=A, _, _) => 7
  end}}}
in ?|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(
              tuple([
                tup_label(label("x"), constructor("D", None)),
                tup_label(label("y"), wild()),
                tup_label(label("z"), constructor("A", None)),
              ])
            ),
          ),
        ),
      ),
    ],
  );

let labeled_tuple_redundancy =
  has_errors(
    "Labeled Tuple Redundancy Stress Test",
    {|
type ATree = +A + B + C + D in
type BTree = +E + F + G + H in
type Tuple = (x=ATree, y=BTree, z=ATree) in
let f = fun (tpl : Tuple) ->
  {{{case tpl
    | (A, E, A) => 1
    | {{{(x=A, y=E, z=A)}}} => 2
    | {{{(y=E, x=A, z=A)}}} => 3
    | {{{(A, z=A, y=E)}}} => 4
    | {{{(x=A, E, A)}}} => 5
  end}}}
in ?|},
    [
      Info.Exp(
        InexhaustiveMatch(
          None,
          Grammar.Pat(
            IdTagged.FreshGrammar.Pat.(
              tuple([
                tup_label(label("x"), constructor("A", None)),
                tup_label(label("y"), constructor("E", None)),
                tup_label(label("z"), constructor("B", None)),
              ])
            ),
          ),
        ),
      ),
      Info.Pat(Redundant(None)),
      Info.Pat(Redundant(None)),
      Info.Pat(Redundant(None)),
      Info.Pat(Redundant(None)),
    ],
  );

let labeled_tuple_additional_error = {
  test_case("Labeled Tuple Additional Error Test", `Quick, () => {
    Test_Statics_Prelude.(
      annotated_tree_test(
        {|case () | _ => ""| {{{([], a=?)}}} => "" end|},
        FTemp.Typ.(string()),
        FIError.(
          Exp.(
            match(
              tuple([]),
              [
                (Pat.wild(), string("")),
                (
                  Pat.(
                    tuple(
                      ~ann=
                        Some(
                          Pat(
                            Redundant(
                              Some(
                                Common(
                                  TupleLabelError({
                                    malformed_labels: [],
                                    duplicate_labels: [],
                                    invalid_labels: ["a"],
                                    typ:
                                      FTemp.Typ.(
                                        prod([
                                          list(
                                            unknown(
                                              FTemp.TypeProvenance.internal(),
                                            ),
                                          ),
                                          unknown(
                                            FTemp.TypeProvenance.internal(),
                                          ),
                                        ])
                                      ),
                                  }),
                                ),
                              ),
                            ),
                          ),
                        ),
                      [
                        list_lit([]),
                        tup_label(
                          ~ann=
                            Some(
                              Pat(
                                Common(
                                  TupleLabelError({
                                    malformed_labels: [],
                                    duplicate_labels: [],
                                    invalid_labels: ["a"],
                                    typ:
                                      FTemp.Typ.(
                                        tup_label(
                                          label("a"),
                                          unknown(
                                            FTemp.TypeProvenance.internal(),
                                          ),
                                        )
                                      ),
                                  }),
                                ),
                              ),
                            ),
                          label(
                            ~ann=
                              Some(
                                Pat(Common(NoType(InvalidLabel("a", [])))),
                              ),
                            "a",
                          ),
                          empty_hole(),
                        ),
                      ],
                    )
                  ),
                  string(""),
                ),
              ],
            )
          )
        ),
      )
    )
  });
};

let function_scrutinee =
  has_errors(
    "Function Scrutinee",
    {|let f: Int -> Int = fun x -> x in
case f
| g => g
| {{{h}}} => h
end|},
    [Info.Pat(Redundant(None))],
  );

let type_function_scrutinee =
  has_errors(
    "Function Scrutinee",
    {|let f: poly a -> a -> a = typfun a -> fun x -> x in
case f
| g => g
| {{{h}}} => h
end|},
    [Info.Pat(Redundant(None))],
  );

// tests a weird edge case where the label has no argument
let fun_labeled_tuple =
  no_errors(
    "Exhaustive fun w/ labeled tuple",
    {|
      let _ = fun (a=_) -> 0 in ?
    |},
  );

let exhaustive_ints_with_wilds =
  no_errors(
    "Exhaustive Int Tuples with Wilds",
    {|
      let f : (Int, Int, Int) -> Bool =
      fun x ->
        case x
          | (0, _, 0) => false
          | _ => false
        end
      in ?
    |},
  );

let exhaustive_strings_with_wilds =
  no_errors(
    "Exhaustive Strings Tuples with Wilds",
    {|
      let f : (String, String, String) -> Bool =
      fun x ->
        case x
          | ("", _, "") => false
          | _ => false
        end
      in ?
    |},
  );

let exhaustive_bools_with_wilds =
  no_errors(
    "Exhaustive Bools Tuples with Wilds",
    {|
      let f : (Bool, Bool, Bool) -> Bool =
      fun x ->
        case x
          | (false, _, false) => false
          | _ => false
        end
      in ?
    |},
  );

let tests = (
  "Pattern Coverage Checker",
  [
    bare_let,
    bare_fun,
    annotated_let,
    annotated_fun,
    let_tuple,
    fun_tuple,
    annotated_let_tuple,
    annotated_fun_tuple,
    peanut_1a,
    peanut_1b,
    peanut_2a,
    peanut_2b,
    peanut_2c,
    peanut_3a,
    peanut_3b,
    loooong_list,
    list_inexhaustive_nil,
    list_inexhaustive_cons,
    list_inexhaustive_cons_long,
    list_inexhaustive_tuple_with_elt,
    list_inexhaustive_triple,
    list_inexhaustive_triple_elt_first,
    list_inexhaustive_middle_quad,
    integers_exhaustive,
    integers_non_exhaustive,
    integers_tuple_non_exhaustive,
    integers_redundant,
    floats_exhaustive,
    floats_non_exhaustive,
    floats_tuple_non_exhaustive,
    floats_redundant,
    strings_exhaustive,
    strings_non_exhaustive,
    strings_non_exhaustive_empty,
    strings_non_exhaustive_tuple,
    strings_redundant,
    bools_exhaustive,
    bools_non_exhaustive,
    bools_redundant,
    unit_exhaustive,
    unit_redundant,
    rank_compare,
    rank_compare_inexhaustive,
    rank_let_inexhaustive,
    rank_fun_inexhaustive,
    nested_constructors_inexhaustive,
    multiple_holes_irredundant,
    unknown_scrutinee_exhaustive,
    unknown_scrutinee_redundant_vars,
    unknown_scrutinee_tuples_of_many_lengths,
    partially_unknown_scrutinee_inexhaustive,
    partially_unknown_scrutinee_redundancy,
    labeled_tuple_exhaustiveness,
    labeled_tuple_inexhaustiveness,
    labeled_tuple_redundancy,
    labeled_tuple_additional_error,
    function_scrutinee,
    fun_labeled_tuple,
    exhaustive_ints_with_wilds,
    exhaustive_strings_with_wilds,
    exhaustive_bools_with_wilds,
  ],
);
