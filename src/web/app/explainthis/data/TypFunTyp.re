open Haz3lcore;
open Example;
open ExplainThisForm;

let option_ex = {
  sub_id: TypFunTyp1,
  term:
    mk_example(
      "type Opt = typfun a -> + Nothing + Just(a) in \n let x : Opt(Int) = Just(1) in x",
    ),
  message: "Opt is a type function: Opt(Int) is the type + Nothing + Just(Int). The alias could equally be written type Opt(a) = + Nothing + Just(a).",
};

let curried_ex = {
  sub_id: TypFunTyp2,
  term:
    mk_example(
      "type Res = typfun e -> typfun o -> + Err(e) + Ok(o) in \n type StrRes = Res(String) in \n let r : StrRes(Int) = Ok(1) in r",
    ),
  message: "Nested type functions take their arguments one at a time, so Res(String) is itself a type function, which StrRes(Int) applies to Int.",
};

/* `typfun a -> t`, `typfun a, b -> t`: a type-level function. Not a
   proper type (its kind is an arrow), so it is used by applying it to
   type arguments, `T(Int)`, never as the type of a value. */
let typfun_typ =
    (~binders: list((string, Id.t)), ~tbody_id: Id.t, ~kind: string): form => {
  let tpat_ids = List.map(~f=snd, binders);
  let n = List.length(tpat_ids);
  let (binder_seg, binder_pieces) = tpat_list("a", n);
  let typ_body = typ("ty");
  {
    id: TypFunTyp,
    syntactic_form: [
      mk_typfun([[space()] @ binder_seg @ [space()]]),
      space(),
      typ_body,
    ],
    colorings:
      zip_colorings(binder_pieces, tpat_ids)
      @ [(Piece.id(typ_body), tbody_id)],
    expandable_id: None,
    explanation:
      Printf.sprintf(
        "A type function (kind `%s`): applied to %s, it gives the [*body*](%s) with the arguments substituted for %s. It is not itself the type of any value: write `T(%s)` to get one. Usually written through an alias, `type T(%s) = …`. Compare `poly`, the type of values that work at every type.",
        kind,
        n == 1 ? "a type argument" : Printf.sprintf("%d type arguments", n),
        Id.to_string(tbody_id),
        named_links(binders),
        n == 1
          ? "Int" : String.concat(~sep=", ", List.init(n, ~f=_ => "Int")),
        n == 1
          ? "a"
          : String.concat(
              ~sep=", ",
              List.init(n, ~f=i => Printf.sprintf("a%d", i + 1)),
            ),
      ),
    examples: [option_ex, curried_ex],
  };
};

let typfun =
    (~binders: list((string, Id.t)), ~tbody_id: Id.t, ~kind: string): group =>
  singleton(typfun_typ(~binders, ~tbody_id, ~kind));
