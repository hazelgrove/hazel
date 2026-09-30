open Alcotest;
open Haz3lcore;
open Language;
open CardTypes;

/* Exercise the same read -> choose -> SetSyntax -> read path as the card
   picker. Projector syntax has a storage wrapper in addition to any
   parentheses needed by the value itself (notably a card's tuple). */
let info = p =>
  ProjectorInfo.mk_info(
    p,
    ~sample_focus=Sample.Focus.init,
    ~statics=Statics.Map.empty,
    ~dynamics=Dynamics.Map.empty,
    ~elaborated=None,
  );

let edit_card = (z, update) => {
  let parsed = MakeTerm.from_zip_for_sem(z, ~root=Exp);
  let id = List.hd(parsed.projector_list);
  let p = Id.Map.find(id, parsed.projectors);
  let before_info = info(p);
  let expected =
    CardProj.update(CardProj.SyntaxTerm.get(before_info), update);
  let seg = CardProj.SyntaxTerm.put(before_info, expected) |> Option.get;
  let z =
    ProjectorPerform.go(
      parsed.term_data,
      SetSyntax(0, Card, seg),
      z,
      parsed.projector_list,
      [],
      ~elaborated=parsed.term,
      ~root=Exp,
    )
    |> Result.get_ok;
  let after = MakeTerm.from_zip_for_sem(z, ~root=Exp);
  check(
    bool,
    "projector identity retained",
    true,
    Id.equal(id, List.hd(after.projector_list)),
  );
  let actual =
    CardProj.SyntaxTerm.get(info(Id.Map.find(id, after.projectors)));
  check(bool, "chosen card remains readable", true, expected == actual);
  z;
};

let case_ = (name, source, updates) =>
  test_case(
    name,
    `Quick,
    () => {
      let z =
        PersistentZipper.parse_text(~source="card picker", ~root=Exp, source)
        |> Option.get;
      ignore(List.fold_left(edit_card, z, updates));
    },
  );

let tests = (
  "Projector edits",
  [
    case_(
      "single card can be changed repeatedly",
      "^^card((Spades, Jack))",
      [ReplaceCard((Clubs, Ace)), ReplaceCard((Hearts, Two))],
    ),
    case_(
      "hand changes only the chosen card",
      "^^card([(Spades, Jack), (Diamonds, Queen), (Hearts, Two)])",
      [
        ReplaceCardInHand(1, (Clubs, Ace)),
        ReplaceCardInHand(0, (Hearts, King)),
      ],
    ),
    case_(
      "single-card hand keeps its list",
      "^^card([(Spades, Jack)])",
      [ReplaceCardInHand(0, (Clubs, Ace))],
    ),
    case_(
      "pattern card supports wildcard suit and rank",
      "fun card -> case card | ^^card((Hearts, _)) => true | _ => false end",
      [ReplaceCard((UnknownS, Ace)), ReplaceCard((Clubs, UnknownR))],
    ),
    case_(
      "pattern hand preserves the other cards",
      "fun hand -> case hand | ^^card([(Hearts, _), (Clubs, _)]) => true | _ => false end",
      [ReplaceCardInHand(1, (UnknownS, Ace))],
    ),
  ],
);
