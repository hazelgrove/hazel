open Alcotest;
open Haz3lcore;

/* The card projector reads a card or a hand through Parens and through any
   projector already on a card, so projecting a hand that holds a projected
   card does not fail to initialise. */

let state =
  testable(CardTypes.pp_state, (a, b) =>
    CardTypes.show_state(a) == CardTypes.show_state(b)
  );

let parse = (s: string): Language.Exp.t =>
  switch (Parser.to_term(s, ~root=Language.Sort.Exp)) {
  | Some(e) => e
  | None => fail("failed to parse " ++ s)
  };

/* Projects a card over the first card-shaped tuple, the way projecting one
   card of a hand in the editor leaves it. */
let project_first_card = (e: Language.Exp.t): Language.Exp.t => {
  let (module P) = ProjectorInit.to_module(Card);
  let done_ = ref(false);
  Language.Exp.map_term(
    ~f_exp=
      (continue, e) =>
        switch (e.term) {
        | Parens({term: Tuple([_, _]), _}) when ! done_^ =>
          done_ := true;
          let model =
            switch (P.init(Exp(e))) {
            | Some(m) => m
            | None => fail("card should init on a card")
            };
          {
            ...e,
            term:
              Projector(
                {
                  kind: Card,
                  model,
                },
                e,
              ),
          };
        | _ => continue(e)
        },
    e,
  );
};

let hand: CardTypes.state = (Exp, Hand([(Hearts, Ace), (Spades, Two)]));

let tests = [
  (
    "CardSyntax",
    [
      test_case("plain hand", `Quick, () =>
        check(
          option(state),
          "hand",
          Some(hand),
          CardSyntax.any_to_state(
            Exp(parse("[(Hearts, Ace), (Spades, Two)]")),
          ),
        )
      ),
      test_case("hand holding a projected card", `Quick, () =>
        check(
          option(state),
          "hand",
          Some(hand),
          CardSyntax.any_to_state(
            Exp(
              project_first_card(parse("[(Hearts, Ace), (Spades, Two)]")),
            ),
          ),
        )
      ),
      test_case("parenthesized hand", `Quick, () =>
        check(
          option(state),
          "hand",
          Some(hand),
          CardSyntax.any_to_state(
            Exp(parse("([(Hearts, Ace), (Spades, Two)])")),
          ),
        )
      ),
    ],
  ),
];
