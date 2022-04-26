let demo = (
  "simple expression",
  UHExp.(
    Block.wrap'(
      Seq.mk(
        intlit("1"),
        [
          (Operators_Exp.Plus, intlit("2")),
          (Operators_Exp.Times, intlit("3")),
        ],
      )
      |> mk_OpSeq,
    )
  ),
);

let example_to_card = ((name: string, e: UHExp.t)): CardInfo.t => {
  name,
  caption: Virtual_dom.Vdom.Node.div([], []),
  init_zexp: ZExp.place_before(e),
};

let demostack: CardstackInfo.t = {
  title: "Demo",
  cards: [example_to_card(demo)],
};
