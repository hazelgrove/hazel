open Test_Statics_Prelude;
open FTemp;
open Typ;

let tests = [
  fully_consistent_typecheck(
    "nested_sum_constructors",
    {|
case (? : (rec t -> +Z+S(t)))
  | S(S(x)) => 1
  | _ => 2
end
        |},
    Some(int()),
  ),
  fully_consistent_typecheck(
    "type hole",
    {|
    type ? = ? in ?
    |},
    Some(unknown(Type, Language.Hole.temp(EmptyHole), Atom)),
  ),
  fully_consistent_typecheck(
    "single null sum type",
    {|
    type SingleNull = +One in ?
    |},
    Some(unknown(Type, Language.Hole.temp(EmptyHole), Atom)),
  ),
  fully_consistent_typecheck(
    "single sum type",
    {|
    type Single = +F(Int) in ?
    |},
    Some(unknown(Type, Language.Hole.temp(EmptyHole), Atom)),
  ),
  fully_consistent_typecheck(
    "partial sum type",
    {|
    type Partial = Ok(?) + ? in ?
    |},
    Some(unknown(Type, Language.Hole.temp(EmptyHole), Atom)),
  ),
  fully_consistent_typecheck(
    "double alias",
    {|
    type GoodSum = A + B + C(Int) in
    type DoubleAlias = GoodSum in ?
    |},
    Some(unknown(Type, Language.Hole.temp(EmptyHole), Atom)),
  ),
  fully_consistent_typecheck(
    "vertical leading",
    {|
    type GoodSum = A + B + C(Int) in
    type VerticalLeading =
      + A
      + B(GoodSum)
      + C(Bool->Bool)
    in ?
    |},
    Some(unknown(Type, Language.Hole.temp(EmptyHole), Atom)),
  ),
  inconsistent_typecheck(
    "tuple as type name",
    // #err: invalid type name#
    {|
    type (?, ?) = ? in ?
    |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "not a sum",
    // #err: invalid type name#
    {|
    type NotASum = NotInSum(Bool) in ?
    |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "shadows base type",
    // #err: shadows base type#
    {|
    type Bool = ? in ?
    |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "unbound type var",
    // #err: unbound type var#
    {|
    type BadCons =
      + Um(Unbound)
    in ?
    |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "base type instead of constructor",
    // #err: expected cons found type#
    {|
    type BadCons =
      + Bool
    in ?
    |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "base type instead of constructor in ap",
    // #err: expected cons found type#
    {|
    type BadCons =
      + Int(Int)
    in ?
    |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "parenthesis instead of constructor",
    // #err: expected cons found type#
    {|
    type BadCons =
      + (?)(Int)
    in ?
    |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "ap instead of constructor",
    // #err: expected cons found app#
    {|
    type BadCons =
      + A(Bool)(Int)
    in ?
    |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "unbound type var in compound alias",
    // #err: unbound type var#
    {|
    type CompoundAlias = (Int, Anonymous + Sum) in
    let _ = (1, Sum) in ?
    |}
    |> parse_exp,
  ),
  fully_consistent_typecheck(
    "analytic sum types",
    {|
    type CompoundAlias = (Int, Anonymous + Sum) in
    let _: CompoundAlias = (1, Sum) in ?
    |},
    Some(unknown(Type, Language.Hole.temp(EmptyHole), Atom)),
  ),
  inconsistent_typecheck(
    "unbound type var in function",
    // #err: not defined#
    {|
    type Yorp = Int -> (Inside + Ouside) in
    let _ = fun _ -> Inside in ?
    |}
    |> parse_exp,
  ),
  fully_consistent_typecheck(
    "analytic sum types in function",
    {|
    type Yorp = Int -> (Inside + Ouside) in
    let _: Yorp = fun _ -> Inside in ?
    |},
    Some(unknown(Type, Language.Hole.temp(EmptyHole), Atom)),
  ),
  inconsistent_typecheck(
    "unbound type var in sum",
    // #err: not defined#
    {|
    type Gargs = [BigGuy + Small] in
    let _ = BigGuy in ?
    |}
    |> parse_exp,
  ),
  fully_consistent_typecheck(
    "analytic sum types in sum",
    {|
    type Gargs = [BigGuy + Small] in
    let _: Gargs = [BigGuy] in ?
    |},
    Some(unknown(Type, Language.Hole.temp(EmptyHole), Atom)),
  ),
  fully_consistent_typecheck(
    "analytic sum types in sum with cons",
    {|
    type Gargs = [BigGuy + Small] in
    let _: Gargs = BigGuy :: [BigGuy] in ?
    |},
    Some(unknown(Type, Language.Hole.temp(EmptyHole), Atom)),
  ),
  inconsistent_typecheck(
    "unbound type var in sum with cons",
    // #err: not bound#
    {|
    let a:Bad = 0 in a == 0
    |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "non-sum types cant be recursive",
    // #err: not bound#
    {|
    type Lol = Lol in ?
    |} |> parse_exp,
  ),
  fully_consistent_typecheck(
    "analytic shadowing",
    {|
    type Tork1 = +Blob in
    type Tork2 = +Blob in
    let x:Tork1 = Blob in ?
    |},
    Some(unknown(Type, Language.Hole.temp(EmptyHole), Atom)),
  ),
  fully_consistent_typecheck(
    "exp tests: happy",
    {|
    type GoodSum = A + B + C(Int) in
    type DoubleAlias = GoodSum in
    type YoDawg =  Yo(Int) + Bo(Int)+ Dawg(Bool) in
    let _ = Yo(1) in
    let _ : YoDawg = Yo(2) in
    let _ : +Yo(Bool) = Yo(true) in
    let _ : (Yo + Dawg, Int) = (Dawg,5) in
    let _ : DoubleAlias = C(4) in ?
    |},
    Some(unknown(Type, Language.Hole.temp(EmptyHole), Atom)),
  ),
  inconsistent_typecheck(
    "inconsistent type with arrow",
    // #err: incons with arrow#
    {|
    let _ = 2(1) in ?
    |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "undefined constructor",
    // #err: cons undefined#
    {|
    let _ = Undefined(1) in ?
    |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "type incons with string",
    // #err: type incons#
    {|
    type GoodSum = A + B + C(Int) in
    type VerticalLeading =
      + A
      + B(GoodSum)
      + C(Bool->Bool)
    in
    let _ = B("lol") in ?
    |}
    |> parse_exp,
  ),
  inconsistent_typecheck(
    "constructor missing argument",
    // #err: type incons#
    {|
    let _ : +Yo(Bool) = Yo in ?
    |} |> parse_exp,
  ),
  fully_consistent_typecheck(
    "pattern constructor tests: happy",
    {|
    type YoDawg =  Yo(Int) + Bo(Int)+ Dawg(Bool) in
    case Dawg(true) | Yo(1) => ? | _ => ? end;
    case Yo(1) | Yo(1) : YoDawg => ? | _ => ? end;
    case Yo(1):(+Yo(Int)) | Yo(1) => ? | _ => ? end;
    case Yo :(+Yo) | Yo : +Yo => ? end;
    |},
    Some(unknown(Type, Language.Hole.temp(EmptyHole), Atom)),
  ),
  inconsistent_typecheck(
    "pattern constructor tests: errors",
    // #err: incons with arrow#
    {|
    let 2(1) = 3 in ?
    |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "pattern constructor tests: undefined",
    // #err: cons undefined#
    {|
    let NotDefined(1) = 3 in ?
    |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "pattern constructor tests: type incons",
    // #err: type incons#
    {|
    type YoDawg =  Yo(Int) + Bo(Int)+ Dawg(Bool) in
    let Yo = Dawg in ?
    |}
    |> parse_exp,
  ),
  inconsistent_typecheck(
    "pattern constructor tests: type incons",
    // #err: type incons#
    {|
    type YoDawg =  Yo(Int) + Bo(Int)+ Dawg(Bool) in
    let Yo(true) = Dawg(true) in ?
    |}
    |> parse_exp,
  ),
  inconsistent_typecheck(
    "pattern constructor tests: type incons",
    // #err: type incons#
    {|
    type YoDawg =  Yo(Int) + Bo(Int)+ Dawg(Bool) in
    let Yo: YoDawg = Yo(1) in ?
    |}
    |> parse_exp,
  ),
  inconsistent_typecheck(
    "pattern constructor tests: type incons",
    // #err: type incons#
    {|
    type YoDawg =  Yo(Int) + Bo(Int)+ Dawg(Bool) in
    let Yo(1): +Yo = Yo in ?
    |}
    |> parse_exp,
  ),
  inconsistent_typecheck(
    "pattern constructor tests: type incons",
    // #err: type incons#
    {|
    type YoDawg =  Yo(Int) + Bo(Int)+ Dawg(Bool) in
    let Yo(1): +Yo(Bool) = Yo(true) in ?
    |}
    |> parse_exp,
  ),
  inconsistent_typecheck(
    "constructor with unneeded argument",
    // #err: type incons#
    {|
    let _ : +Yo = Yo("lol") in ?
    |} |> parse_exp,
  ),
  // ======================== KNOWN BUGS ==============================
  skip_known_bug(
    // inconsistent_typecheck(
    "bad type name",
    // #err: invalid type name#
    {|
    type badTypeName = ? in ?
    |},
    // |> parse_exp,
  ),
  // Issue #1458
  skip_known_bug(
    // inconsistent_typecheck(
    "duplicate constructors",
    // #err: already used#
    {|
    type Dupes =
      + Guy(Bool)
      + Guy(Int)
      + Guy
    in ?
    |},
    // |> parse_exp,
  ),
  // Issue #1609
  skip_known_bug(
    // inconsistent_typecheck(
    "invalid constructor",
    // #err: invalid#
    {|
    type BadCons =
      + notvalid
    in ?
    |},
    // |> parse_exp,
  ),
];
