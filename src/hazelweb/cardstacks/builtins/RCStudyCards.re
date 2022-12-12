/*
                                                                                                 module Vdom = Virtual_dom.Vdom;
                                                                                                 open OpSeq;
                                                                                                 open OpSeqUtil;

                                                                                                 let span = Vdom.Node.span;
                                                                                                 let txt = Vdom.Node.text;
                                                                                                 let p = Vdom.Node.p;

                                                                                                 let code = s => span([Vdom.Attr.classes(["code"])], [txt(s)]);
                                                                                                 let images_dir = "img/RCStudy/"

                                                                                                 let append_case =
                                                                                                   UHExp.(
                                                                                                     case(
                                                                                                       wrap_in_block(var("xs")),
                                                                                                       [
                                                                                                         Rule(UHPat.listnil(), wrap_in_block(var("ys"))),
                                                                                                         Rule(
                                                                                                           Pat.mk_OpSeq(ExpOpExp(UHPat.var("z"), Cons, UHPat.var("zs"))),
                                                                                                           wrap_in_block(
                                                                                                             Exp.mk_OpSeq(
                                                                                                               ExpOpExp(
                                                                                                                 var("z"),
                                                                                                                 Cons,
                                                                                                                 Parenthesized(
                                                                                                                   wrap_in_block(
                                                                                                                     Exp.mk_OpSeq(
                                                                                                                       SeqOpExp(
                                                                                                                         ExpOpExp(var("append"), Space, var("zs")),
                                                                                                                         Space,
                                                                                                                         var("ys"),
                                                                                                                       ),
                                                                                                                     ),
                                                                                                                   ),
                                                                                                                 ),
                                                                                                               ),
                                                                                                             ),
                                                                                                           ),
                                                                                                         ),
                                                                                                       ],
                                                                                                     )
                                                                                                   );
                                                                                                 let append_lam =
                                                                                                   UHExp.(
                                                                                                     lam(
                                                                                                       UHPat.var("xs"),
                                                                                                       wrap_in_block(lam(UHPat.var("ys"), wrap_in_block(append_case))),
                                                                                                     )
                                                                                                   );
                                                                                                 let append_letline =
                                                                                                   UHExp.(
                                                                                                     letline(
                                                                                                       UHPat.var("append"),
                                                                                                       ~ann=
                                                                                                         Typ.mk_OpSeq(
                                                                                                           SeqOpExp(
                                                                                                             ExpOpExp(List(Int), Arrow, List(Int)),
                                                                                                             Arrow,
                                                                                                             List(Int),
                                                                                                           ),
                                                                                                         ),
                                                                                                       wrap_in_block(append_lam),
                                                                                                     )
                                                                                                   );

                                                                                                 let exercise_number = n =>
                                                                                                   span(
                                                                                                     [Vdom.Attr.classes(["card-exercise-number"])],
                                                                                                     [txt("Exercise " ++ string_of_int(n) ++ ".   ")],
                                                                                                   );

                                                                                                 let centered_code = s =>
                                                                                                   p([Vdom.Attr.create("style", "text-align: center;")], [code(s)]);

                                                                                                 let set_img = path =>
                                                                                                   Vdom.Node.create(
                                                                                                     "img",
                                                                                                     [
                                                                                                       Vdom.Attr.create("src", path),
                                                                                                       Vdom.Attr.create("style", "margin-left: -40px; margin-right: -40px;"),
                                                                                                     ],
                                                                                                     [],
                                                                                                   );

                                                                                                 let set_img_container = (~width, img1, img2, img3) =>
                                                                                                   Vdom.Node.div(
                                                                                                     [
                                                                                                       Vdom.Attr.create(
                                                                                                         "style",
                                                                                                         "width: "
                                                                                                         ++ string_of_int(width)
                                                                                                         ++ "%; "
                                                                                                         ++ "display: flex; "
                                                                                                         ++ "flex-direction: row; "
                                                                                                         ++ "justify-content: center;",
                                                                                                       ),
                                                                                                     ],
                                                                                                     [img1, img2, img3],
                                                                                                   );

                                                                                                 let cards: list(Card.t) =
                                                                                                   Vdom.[
                                                                                                     {
                                                                                                       caption:
                                                                                                         span(
                                                                                                           [],
                                                                                                           [
                                                                                                             exercise_number(1),
                                                                                                             txt(
                                                                                                               "Suppose we are implementing a combat game "
                                                                                                               ++ "and, specifically, defining the function ",
                                                                                                             ),
                                                                                                             centered_code("damage : (Bool, Int) -> Int"),
                                                                                                             txt("The input tuple of type "),
                                                                                                             code("(Bool, Int)"),
                                                                                                             txt(
                                                                                                               " represents an enemy attack dealt to the "
                                                                                                               ++ "current player, consisting of a ",
                                                                                                             ),
                                                                                                             code("Bool"),
                                                                                                             txt(" indicating whether the attack is a melee attack, and a "),
                                                                                                             code("Int"),
                                                                                                             txt(
                                                                                                               " representing the critical hit multiplier. The output type of ",
                                                                                                             ),
                                                                                                             code("Int"),
                                                                                                             txt(" is the damage points inflicted upon the current player."),
                                                                                                             p(
                                                                                                               [],
                                                                                                               [
                                                                                                                 txt(
                                                                                                                   "Take a moment to understand the current implementation, "
                                                                                                                   ++ "then click \'Next\' to begin the exercise.",
                                                                                                                 ),
                                                                                                               ],
                                                                                                             ),
                                                                                                           ],
                                                                                                         ),
                                                                                                       init_zexp:
                                                                                                         UHExp.Block(
                                                                                                           [
                                                                                                             UHExp.letline(
                                                                                                               UHPat.var("damage"),
                                                                                                               ~ann=
                                                                                                                 UHTyp.(
                                                                                                                   ExpOpExp(
                                                                                                                     Parenthesized(ExpOpExp(Bool, Prod, Int) |> Typ.mk_OpSeq),
                                                                                                                     Arrow,
                                                                                                                     Int,
                                                                                                                   )
                                                                                                                   |> Typ.mk_OpSeq
                                                                                                                 ),
                                                                                                               UHExp.(
                                                                                                                 wrap_in_block(
                                                                                                                   lam(
                                                                                                                     UHPat.(
                                                                                                                       Parenthesized(
                                                                                                                         ExpOpExp(var("is_melee"), Comma, var("crit_hit"))
                                                                                                                         |> Pat.mk_OpSeq,
                                                                                                                       )
                                                                                                                     ),
                                                                                                                     wrap_in_block(
                                                                                                                       case(
                                                                                                                         var("is_melee") |> wrap_in_block,
                                                                                                                         [
                                                                                                                           Rule(
                                                                                                                             UHPat.boollit(false),
                                                                                                                             intlit(5) |> wrap_in_block,
                                                                                                                           ),
                                                                                                                           Rule(
                                                                                                                             UHPat.boollit(true),
                                                                                                                             SeqOpExp(
                                                                                                                               ExpOpExp(intlit(2), Times, var("crit_hit")),
                                                                                                                               Plus,
                                                                                                                               intlit(1),
                                                                                                                             )
                                                                                                                             |> Exp.mk_OpSeq
                                                                                                                             |> wrap_in_block,
                                                                                                                           ),
                                                                                                                         ],
                                                                                                                       ),
                                                                                                                     ),
                                                                                                                   ),
                                                                                                                 )
                                                                                                               ),
                                                                                                             ),
                                                                                                           ],
                                                                                                           EmptyHole(-1),
                                                                                                         )
                                                                                                         |> ZExp.place_before_block,
                                                                                                     },
                                                                                                     {
                                                                                                       caption:
                                                                                                         span(
                                                                                                           [],
                                                                                                           [
                                                                                                             exercise_number(1),
                                                                                                             txt(
                                                                                                               "Suppose we are implementing a combat game "
                                                                                                               ++ "and, specifically, defining the function ",
                                                                                                             ),
                                                                                                             centered_code("damage : (Bool, Int) -> Int"),
                                                                                                             txt("The input tuple of type "),
                                                                                                             code("(Bool, Int)"),
                                                                                                             txt(
                                                                                                               " represents an enemy attack dealt to the "
                                                                                                               ++ "current player, consisting of a ",
                                                                                                             ),
                                                                                                             code("Bool"),
                                                                                                             txt(" indicating whether the attack is a melee attack, and a "),
                                                                                                             code("Int"),
                                                                                                             txt(
                                                                                                               " representing the critical hit multiplier. The output type of ",
                                                                                                             ),
                                                                                                             code("Int"),
                                                                                                             txt(" is the damage points inflicted upon the current player."),
                                                                                                             p(
                                                                                                               [Attr.classes(["card-exercise"])],
                                                                                                               [
                                                                                                                 txt("Suppose we have in scope the current player's"),
                                                                                                                 code(" defense_score : Int "),
                                                                                                                 txt(
                                                                                                                   "and wish to integrate it into the damage calculation. "
                                                                                                                   ++ "Modify the body of ",
                                                                                                                 ),
                                                                                                                 code("damage"),
                                                                                                                 txt(" so that the "),
                                                                                                                 code("case"),
                                                                                                                 txt(" expression is bound to a new variable called "),
                                                                                                                 code("attack_score"),
                                                                                                                 txt(", then return twice the difference between "),
                                                                                                                 code("attack_score"),
                                                                                                                 txt(" and "),
                                                                                                                 code("defense_score"),
                                                                                                                 txt("."),
                                                                                                               ],
                                                                                                             ),
                                                                                                           ],
                                                                                                         ),
                                                                                                       init_zexp:
                                                                                                         UHExp.Block(
                                                                                                           [
                                                                                                             UHExp.letline(
                                                                                                               UHPat.var("defense_score"),
                                                                                                               ~ann=UHTyp.Int,
                                                                                                               UHExp.EmptyHole(-1) |> UHExp.Block.wrap',
                                                                                                             ),
                                                                                                             UHExp.EmptyLine,
                                                                                                             UHExp.letline(
                                                                                                               UHPat.var("damage"),
                                                                                                               ~ann=
                                                                                                                 UHTyp.(
                                                                                                                   ExpOpExp(
                                                                                                                     Parenthesized(ExpOpExp(Bool, Prod, Int) |> Typ.mk_OpSeq),
                                                                                                                     Arrow,
                                                                                                                     Int,
                                                                                                                   )
                                                                                                                   |> Typ.mk_OpSeq
                                                                                                                 ),
                                                                                                               UHExp.(
                                                                                                                 wrap_in_block(
                                                                                                                   lam(
                                                                                                                     UHPat.(
                                                                                                                       Parenthesized(
                                                                                                                         ExpOpExp(var("is_melee"), Comma, var("crit_hit"))
                                                                                                                         |> Pat.mk_OpSeq,
                                                                                                                       )
                                                                                                                     ),
                                                                                                                     wrap_in_block(
                                                                                                                       case(
                                                                                                                         var("is_melee") |> wrap_in_block,
                                                                                                                         [
                                                                                                                           Rule(
                                                                                                                             UHPat.boollit(false),
                                                                                                                             intlit(5) |> wrap_in_block,
                                                                                                                           ),
                                                                                                                           Rule(
                                                                                                                             UHPat.boollit(true),
                                                                                                                             SeqOpExp(
                                                                                                                               ExpOpExp(intlit(2), Times, var("crit_hit")),
                                                                                                                               Plus,
                                                                                                                               intlit(1),
                                                                                                                             )
                                                                                                                             |> Exp.mk_OpSeq
                                                                                                                             |> wrap_in_block,
                                                                                                                           ),
                                                                                                                         ],
                                                                                                                       ),
                                                                                                                     ),
                                                                                                                   ),
                                                                                                                 )
                                                                                                               ),
                                                                                                             ),
                                                                                                           ],
                                                                                                           EmptyHole(-1),
                                                                                                         )
                                                                                                         |> ZExp.place_before_block,
                                                                                                     },
                                                                                                     {
                                                                                                       caption:
                                                                                                         span(
                                                                                                           [],
                                                                                                           [
                                                                                                             exercise_number(2),
                                                                                                             txt(
                                                                                                               "Suppose we are implementing a slot machine game. "
                                                                                                               ++ "We represent the state of the slot machine as "
                                                                                                               ++ "3 lists of numbers, each representing a spinning "
                                                                                                               ++ "reel of the slot machine. Below we have implemented "
                                                                                                               ++ "the function",
                                                                                                             ),
                                                                                                             centered_code(
                                                                                                               "step : (List(Int), List(Int), List(Int)) -> (List(Int), List(Int), List(Int))",
                                                                                                             ),
                                                                                                             txt(
                                                                                                               "that takes an inital slot machine state and returns "
                                                                                                               ++ "the new state after a single time step. We have "
                                                                                                               ++ "designed this slot machine so that the middle reel "
                                                                                                               ++ "spins twice as fast as the other two reels.",
                                                                                                             ),
                                                                                                             p(
                                                                                                               [],
                                                                                                               [
                                                                                                                 txt(
                                                                                                                   "Take a moment to read and understand the current "
                                                                                                                   ++ "implementation, then click 'Next' to begin the exercise.",
                                                                                                                 ),
                                                                                                               ],
                                                                                                             ),
                                                                                                           ],
                                                                                                         ),
                                                                                                       init_zexp:
                                                                                                         UHExp.Block(
                                                                                                           [
                                                                                                             append_letline,
                                                                                                             UHExp.EmptyLine,
                                                                                                             UHExp.letline(
                                                                                                               UHPat.var("rotate"),
                                                                                                               ~ann=
                                                                                                                 UHTyp.(
                                                                                                                   ExpOpExp(List(Int), Arrow, List(Int)) |> Typ.mk_OpSeq
                                                                                                                 ),
                                                                                                               UHExp.(
                                                                                                                 wrap_in_block(
                                                                                                                   lam(
                                                                                                                     UHPat.var("xs"),
                                                                                                                     wrap_in_block(
                                                                                                                       case(
                                                                                                                         var("xs") |> wrap_in_block,
                                                                                                                         [
                                                                                                                           Rule(UHPat.listnil(), listnil() |> wrap_in_block),
                                                                                                                           Rule(
                                                                                                                             UHPat.(
                                                                                                                               ExpOpExp(var("y"), Cons, var("ys"))
                                                                                                                               |> Pat.mk_OpSeq
                                                                                                                             ),
                                                                                                                             UHExp.(
                                                                                                                               SeqOpExp(
                                                                                                                                 ExpOpExp(var("append"), Space, var("ys")),
                                                                                                                                 Space,
                                                                                                                                 Parenthesized(
                                                                                                                                   ExpOpExp(var("y"), Cons, listnil())
                                                                                                                                   |> Exp.mk_OpSeq
                                                                                                                                   |> wrap_in_block,
                                                                                                                                 ),
                                                                                                                               )
                                                                                                                               |> Exp.mk_OpSeq
                                                                                                                               |> wrap_in_block
                                                                                                                             ),
                                                                                                                           ),
                                                                                                                         ],
                                                                                                                       ),
                                                                                                                     ),
                                                                                                                   ),
                                                                                                                 )
                                                                                                               ),
                                                                                                             ),
                                                                                                             UHExp.EmptyLine,
                                                                                                             UHExp.letline(
                                                                                                               UHPat.var("step"),
                                                                                                               ~ann=
                                                                                                                 UHTyp.(
                                                                                                                   ExpOpExp(
                                                                                                                     Parenthesized(
                                                                                                                       SeqOpExp(
                                                                                                                         ExpOpExp(List(Int), Prod, List(Int)),
                                                                                                                         Prod,
                                                                                                                         List(Int),
                                                                                                                       )
                                                                                                                       |> Typ.mk_OpSeq,
                                                                                                                     ),
                                                                                                                     Arrow,
                                                                                                                     Parenthesized(
                                                                                                                       SeqOpExp(
                                                                                                                         ExpOpExp(List(Int), Prod, List(Int)),
                                                                                                                         Prod,
                                                                                                                         List(Int),
                                                                                                                       )
                                                                                                                       |> Typ.mk_OpSeq,
                                                                                                                     ),
                                                                                                                   )
                                                                                                                   |> Typ.mk_OpSeq
                                                                                                                 ),
                                                                                                               UHExp.(
                                                                                                                 Block(
                                                                                                                   [EmptyLine],
                                                                                                                   lam(
                                                                                                                     UHPat.(
                                                                                                                       Parenthesized(
                                                                                                                         SeqOpExp(
                                                                                                                           ExpOpExp(var("reel1"), Comma, var("reel2")),
                                                                                                                           Comma,
                                                                                                                           var("reel3"),
                                                                                                                         )
                                                                                                                         |> Pat.mk_OpSeq,
                                                                                                                       )
                                                                                                                     ),
                                                                                                                     Block(
                                                                                                                       [EmptyLine],
                                                                                                                       Parenthesized(
                                                                                                                         wrap_in_block(
                                                                                                                           SeqOpExp(
                                                                                                                             SeqOpExp(
                                                                                                                               SeqOpExp(
                                                                                                                                 SeqOpExp(
                                                                                                                                   ExpOpExp(
                                                                                                                                     var("rotate"),
                                                                                                                                     Space,
                                                                                                                                     var("reel1"),
                                                                                                                                   ),
                                                                                                                                   Comma,
                                                                                                                                   var("rotate"),
                                                                                                                                 ),
                                                                                                                                 Space,
                                                                                                                                 Parenthesized(
                                                                                                                                   ExpOpExp(
                                                                                                                                     var("rotate"),
                                                                                                                                     Space,
                                                                                                                                     var("reel2"),
                                                                                                                                   )
                                                                                                                                   |> Exp.mk_OpSeq
                                                                                                                                   |> wrap_in_block,
                                                                                                                                 ),
                                                                                                                               ),
                                                                                                                               Comma,
                                                                                                                               var("rotate"),
                                                                                                                             ),
                                                                                                                             Space,
                                                                                                                             var("reel3"),
                                                                                                                           )
                                                                                                                           |> Exp.mk_OpSeq,
                                                                                                                         ),
                                                                                                                       ),
                                                                                                                     ),
                                                                                                                   ),
                                                                                                                 )
                                                                                                               ),
                                                                                                             ),
                                                                                                           ],
                                                                                                           UHExp.EmptyHole(-1),
                                                                                                         )
                                                                                                         |> ZExp.place_before_block,
                                                                                                     },
                                                                                                     {
                                                                                                       caption:
                                                                                                         span(
                                                                                                           [],
                                                                                                           [
                                                                                                             exercise_number(2),
                                                                                                             txt(
                                                                                                               "Suppose we are implementing a slot machine game. "
                                                                                                               ++ "We represent the state of the slot machine as "
                                                                                                               ++ "3 lists of numbers, each representing a spinning "
                                                                                                               ++ "reel of the slot machine. Below we have implemented "
                                                                                                               ++ "the function",
                                                                                                             ),
                                                                                                             centered_code(
                                                                                                               "step : (List(Int), List(Int), List(Int)) -> (List(Int), List(Int), List(Int))",
                                                                                                             ),
                                                                                                             txt(
                                                                                                               "that takes an inital slot machine state and returns "
                                                                                                               ++ "the new state after a single time step. We have "
                                                                                                               ++ "designed this slot machine so that the middle reel "
                                                                                                               ++ "spins twice as fast as the other two reels.",
                                                                                                             ),
                                                                                                             p(
                                                                                                               [Attr.classes(["card-exercise"])],
                                                                                                               [
                                                                                                                 txt("Modify the function "),
                                                                                                                 code("rotate"),
                                                                                                                 txt(" so that it takes an additional argument "),
                                                                                                                 code("k"),
                                                                                                                 txt(" of type "),
                                                                                                                 code("Int"),
                                                                                                                 txt(" and rotates the given list "),
                                                                                                                 code("k"),
                                                                                                                 txt(" times. Then refactor the body of "),
                                                                                                                 code("step"),
                                                                                                                 txt(" to utilize the new signature of "),
                                                                                                                 code("rotate"),
                                                                                                                 txt(" correctly."),
                                                                                                               ],
                                                                                                             ),
                                                                                                           ],
                                                                                                         ),
                                                                                                       init_zexp:
                                                                                                         UHExp.Block(
                                                                                                           [
                                                                                                             append_letline,
                                                                                                             UHExp.EmptyLine,
                                                                                                             UHExp.letline(
                                                                                                               UHPat.var("rotate"),
                                                                                                               ~ann=
                                                                                                                 UHTyp.(
                                                                                                                   SeqOpExp(
                                                                                                                     ExpOpExp(Int, Arrow, List(Int)),
                                                                                                                     Arrow,
                                                                                                                     List(Int),
                                                                                                                   )
                                                                                                                   |> Typ.mk_OpSeq
                                                                                                                 ),
                                                                                                               UHExp.(
                                                                                                                 wrap_in_block(
                                                                                                                   lam(
                                                                                                                     UHPat.var("xs"),
                                                                                                                     wrap_in_block(
                                                                                                                       case(
                                                                                                                         var("xs") |> wrap_in_block,
                                                                                                                         [
                                                                                                                           Rule(UHPat.listnil(), listnil() |> wrap_in_block),
                                                                                                                           Rule(
                                                                                                                             UHPat.(
                                                                                                                               ExpOpExp(var("y"), Cons, var("ys"))
                                                                                                                               |> Pat.mk_OpSeq
                                                                                                                             ),
                                                                                                                             UHExp.(
                                                                                                                               SeqOpExp(
                                                                                                                                 ExpOpExp(var("append"), Space, var("ys")),
                                                                                                                                 Space,
                                                                                                                                 Parenthesized(
                                                                                                                                   ExpOpExp(var("y"), Cons, listnil())
                                                                                                                                   |> Exp.mk_OpSeq
                                                                                                                                   |> wrap_in_block,
                                                                                                                                 ),
                                                                                                                               )
                                                                                                                               |> Exp.mk_OpSeq
                                                                                                                               |> wrap_in_block
                                                                                                                             ),
                                                                                                                           ),
                                                                                                                         ],
                                                                                                                       ),
                                                                                                                     ),
                                                                                                                   ),
                                                                                                                 )
                                                                                                               ),
                                                                                                             ),
                                                                                                             UHExp.EmptyLine,
                                                                                                             UHExp.letline(
                                                                                                               UHPat.var("step"),
                                                                                                               ~ann=
                                                                                                                 UHTyp.(
                                                                                                                   ExpOpExp(
                                                                                                                     Parenthesized(
                                                                                                                       SeqOpExp(
                                                                                                                         ExpOpExp(List(Int), Prod, List(Int)),
                                                                                                                         Prod,
                                                                                                                         List(Int),
                                                                                                                       )
                                                                                                                       |> Typ.mk_OpSeq,
                                                                                                                     ),
                                                                                                                     Arrow,
                                                                                                                     Parenthesized(
                                                                                                                       SeqOpExp(
                                                                                                                         ExpOpExp(List(Int), Prod, List(Int)),
                                                                                                                         Prod,
                                                                                                                         List(Int),
                                                                                                                       )
                                                                                                                       |> Typ.mk_OpSeq,
                                                                                                                     ),
                                                                                                                   )
                                                                                                                   |> Typ.mk_OpSeq
                                                                                                                 ),
                                                                                                               UHExp.(
                                                                                                                 Block(
                                                                                                                   [EmptyLine],
                                                                                                                   lam(
                                                                                                                     UHPat.(
                                                                                                                       Parenthesized(
                                                                                                                         SeqOpExp(
                                                                                                                           ExpOpExp(var("reel1"), Comma, var("reel2")),
                                                                                                                           Comma,
                                                                                                                           var("reel3"),
                                                                                                                         )
                                                                                                                         |> Pat.mk_OpSeq,
                                                                                                                       )
                                                                                                                     ),
                                                                                                                     Block(
                                                                                                                       [EmptyLine],
                                                                                                                       Parenthesized(
                                                                                                                         wrap_in_block(
                                                                                                                           SeqOpExp(
                                                                                                                             SeqOpExp(
                                                                                                                               SeqOpExp(
                                                                                                                                 SeqOpExp(
                                                                                                                                   ExpOpExp(
                                                                                                                                     var("rotate"),
                                                                                                                                     Space,
                                                                                                                                     var("reel1"),
                                                                                                                                   ),
                                                                                                                                   Comma,
                                                                                                                                   var("rotate"),
                                                                                                                                 ),
                                                                                                                                 Space,
                                                                                                                                 Parenthesized(
                                                                                                                                   ExpOpExp(
                                                                                                                                     var("rotate"),
                                                                                                                                     Space,
                                                                                                                                     var("reel2"),
                                                                                                                                   )
                                                                                                                                   |> Exp.mk_OpSeq
                                                                                                                                   |> wrap_in_block,
                                                                                                                                 ),
                                                                                                                               ),
                                                                                                                               Comma,
                                                                                                                               var("rotate"),
                                                                                                                             ),
                                                                                                                             Space,
                                                                                                                             var("reel3"),
                                                                                                                           )
                                                                                                                           |> Exp.mk_OpSeq,
                                                                                                                         ),
                                                                                                                       ),
                                                                                                                     ),
                                                                                                                   ),
                                                                                                                 )
                                                                                                               ),
                                                                                                             ),
                                                                                                           ],
                                                                                                           UHExp.EmptyHole(-1),
                                                                                                         )
                                                                                                         |> ZExp.place_before_block,
                                                                                                     },
                                                                                                     {
                                                                                                       caption:
                                                                                                         span(
                                                                                                           [],
                                                                                                           [
                                                                                                             exercise_number(3),
                                                                                                             txt(
                                                                                                               "Suppose we are implementing a variant of the game SET. "
                                                                                                               ++ "The face of each card has three properties: a color, "
                                                                                                               ++ "a shape, and a texture. Each face property of a card "
                                                                                                               ++ "can take on one of three different values, e.g., the "
                                                                                                               ++ "color may be red, green, or purple.",
                                                                                                             ),
                                                                                                             set_img_container(
                                                                                                               ~width=100,
                                                                                                               set_img(images_dir ++ "redovalshaded.png"),
                                                                                                               set_img(images_dir ++ "greensquigglefilled.png"),
                                                                                                               set_img(images_dir ++ "purplediamondempty.png"),
                                                                                                             ),
                                                                                                             txt(
                                                                                                               "Three cards form a SET when, for each face property, the "
                                                                                                               ++ "values across the three cards are either all the same "
                                                                                                               ++ "or all different. In the figure below, the left three "
                                                                                                               ++ "cards form a SET because the colors are all "
                                                                                                               ++ "different, the shapes are all the same, and the textures "
                                                                                                               ++ "are all different. The right three cards do not "
                                                                                                               ++ "form a SET because the first two cards have the same "
                                                                                                               ++ "texture but the third has a different texture.",
                                                                                                             ),
                                                                                                             Node.div(
                                                                                                               [
                                                                                                                 Attr.create(
                                                                                                                   "style",
                                                                                                                   "width: 100%; display: flex; flex-direction: row;",
                                                                                                                 ),
                                                                                                               ],
                                                                                                               [
                                                                                                                 set_img_container(
                                                                                                                   ~width=50,
                                                                                                                   set_img(images_dir ++ "purplesquigglefilled.png"),
                                                                                                                   set_img(images_dir ++ "redsquiggleempty.png"),
                                                                                                                   set_img(images_dir ++ "greensquiggleshaded.png"),
                                                                                                                 ),
                                                                                                                 set_img_container(
                                                                                                                   ~width=50,
                                                                                                                   set_img(images_dir ++ "greendiamondfilled.png"),
                                                                                                                   set_img(images_dir ++ "greenovalfilled.png"),
                                                                                                                   set_img(images_dir ++ "greensquiggleempty.png"),
                                                                                                                 ),
                                                                                                               ],
                                                                                                             ),
                                                                                                             txt("We model each card as a 3-tuple of type "),
                                                                                                             code("(Int, Int, Int)"),
                                                                                                             txt(
                                                                                                               ", where each tuple position represents a face property. "
                                                                                                               ++ "In this exercise, we will implement a function",
                                                                                                             ),
                                                                                                             centered_code(
                                                                                                               "is_set : (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> Bool",
                                                                                                             ),
                                                                                                             txt("that returns whether three given cards form a SET."),
                                                                                                             p(
                                                                                                               [],
                                                                                                               [
                                                                                                                 txt("When you are ready, click 'Next' to begin the exercise."),
                                                                                                               ],
                                                                                                             ),
                                                                                                           ],
                                                                                                         ),
                                                                                                       init_zexp: {
                                                                                                         let num_triple =
                                                                                                           UHTyp.(
                                                                                                             Parenthesized(
                                                                                                               SeqOpExp(ExpOpExp(Int, Prod, Int), Prod, Int) |> Typ.mk_OpSeq,
                                                                                                             )
                                                                                                           );
                                                                                                         UHExp.Block(
                                                                                                           [
                                                                                                             UHExp.letline(
                                                                                                               UHPat.var("is_set"),
                                                                                                               ~ann=
                                                                                                                 UHTyp.(
                                                                                                                   SeqOpExp(
                                                                                                                     SeqOpExp(
                                                                                                                       ExpOpExp(num_triple, Arrow, num_triple),
                                                                                                                       Arrow,
                                                                                                                       num_triple,
                                                                                                                     ),
                                                                                                                     Arrow,
                                                                                                                     Bool,
                                                                                                                   )
                                                                                                                   |> Typ.mk_OpSeq
                                                                                                                 ),
                                                                                                               UHExp.(Block([EmptyLine], EmptyHole(-1))),
                                                                                                             ),
                                                                                                             UHExp.EmptyLine,
                                                                                                           ],
                                                                                                           UHExp.EmptyHole(-1),
                                                                                                         )
                                                                                                         |> ZExp.place_before_block;
                                                                                                       },
                                                                                                     },
                                                                                                     {
                                                                                                       caption:
                                                                                                         span(
                                                                                                           [],
                                                                                                           [
                                                                                                             exercise_number(3),
                                                                                                             txt(
                                                                                                               "Suppose we are implementing a variant of the game SET. "
                                                                                                               ++ "The face of each card has three properties: a color, "
                                                                                                               ++ "a shape, and a texture. Each face property of a card "
                                                                                                               ++ "can take on one of three different values, e.g., the "
                                                                                                               ++ "color may be red, green, or purple.",
                                                                                                             ),
                                                                                                             set_img_container(
                                                                                                               ~width=100,
                                                                                                               set_img(images_dir ++ "redovalshaded.png"),
                                                                                                               set_img(images_dir ++ "greensquigglefilled.png"),
                                                                                                               set_img(images_dir ++ "purplediamondempty.png"),
                                                                                                             ),
                                                                                                             txt(
                                                                                                               "Three cards form a SET when, for each face property, the "
                                                                                                               ++ "values across the three cards are either all the same "
                                                                                                               ++ "or all different. In the figure below, the left three "
                                                                                                               ++ "cards form a SET because the colors are all "
                                                                                                               ++ "different, the shapes are all the same, and the textures "
                                                                                                               ++ "are all different. The right three cards do not "
                                                                                                               ++ "form a SET because the first two cards have the same "
                                                                                                               ++ "texture but the third has a different texture.",
                                                                                                             ),
                                                                                                             Node.div(
                                                                                                               [
                                                                                                                 Attr.create(
                                                                                                                   "style",
                                                                                                                   "width: 100%; display: flex; flex-direction: row;",
                                                                                                                 ),
                                                                                                               ],
                                                                                                               [
                                                                                                                 set_img_container(
                                                                                                                   ~width=50,
                                                                                                                   set_img(images_dir ++ "purplesquigglefilled.png"),
                                                                                                                   set_img(images_dir ++ "redsquiggleempty.png"),
                                                                                                                   set_img(images_dir ++ "greensquiggleshaded.png"),
                                                                                                                 ),
                                                                                                                 set_img_container(
                                                                                                                   ~width=50,
                                                                                                                   set_img"lled.png"),
                                                                                                                   set_img"d.png"),
                                                                                                                   set_img"mpty.png"),
                                                                                                                 ),
                                                                                                               ],
                                                                                                             ),
                                                                                                             txt("We model each card as a 3-tuple of type "),
                                                                                                             code("(Int, Int, Int)"),
                                                                                                             txt(
                                                                                                               ", where each tuple position represents a face property. "
                                                                                                               ++ "In this exercise, we will implement a function",
                                                                                                             ),
                                                                                                             centered_code(
                                                                                                               "is_set : (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> Bool",
                                                                                                             ),
                                                                                                             txt("that returns whether three given cards form a SET."),
                                                                                                             p(
                                                                                                               [],
                                                                                                               [
                                                                                                                 txt("Your goal is to implement the function "),
                                                                                                                 code("is_set"),
                                                                                                                 txt("."),
                                                                                                                 Node.ul(
                                                                                                                   [],
                                                                                                                   [
                                                                                                                     Node.li(
                                                                                                                       [],
                                                                                                                       [
                                                                                                                         txt("In scope are pre-defined helper functions "),
                                                                                                                         code("same"),
                                                                                                                         txt(" and "),
                                                                                                                         code("different"),
                                                                                                                         txt(
                                                                                                                           ", each of which takes two face property values and "
                                                                                                                           ++ "determines whether they are the same or different, "
                                                                                                                           ++ "respectively. ",
                                                                                                                         ),
                                                                                                                       ],
                                                                                                                     ),
                                                                                                                     Node.li(
                                                                                                                       [],
                                                                                                                       [
                                                                                                                         txt(
                                                                                                                           "Use these helper functions to implement the functions ",
                                                                                                                         ),
                                                                                                                         code("all_same"),
                                                                                                                         txt(" and "),
                                                                                                                         code("all_different"),
                                                                                                                         txt(
                                                                                                                           ", each of which takes three property values and determines "
                                                                                                                           ++ "whether they are all same or all different, respectively.",
                                                                                                                         ),
                                                                                                                       ],
                                                                                                                     ),
                                                                                                                     Node.li(
                                                                                                                       [],
                                                                                                                       [
                                                                                                                         txt("Use "),
                                                                                                                         code("all_same"),
                                                                                                                         txt(" and "),
                                                                                                                         code("all_different"),
                                                                                                                         txt(" to implement the function "),
                                                                                                                         code("is_set"),
                                                                                                                         txt(
                                                                                                                           ", which takes three cards and determines whether they "
                                                                                                                           ++ "form a SET.",
                                                                                                                         ),
                                                                                                                       ],
                                                                                                                     ),
                                                                                                                   ],
                                                                                                                 ),
                                                                                                               ],
                                                                                                             ),
                                                                                                           ],
                                                                                                         ),
                                                                                                       init_zexp: {
                                                                                                         let num_triple =
                                                                                                           UHTyp.(
                                                                                                             Parenthesized(
                                                                                                               SeqOpExp(ExpOpExp(Int, Prod, Int), Prod, Int) |> Typ.mk_OpSeq,
                                                                                                             )
                                                                                                           );
                                                                                                         UHExp.Block(
                                                                                                           [
                                                                                                             UHExp.letline(
                                                                                                               UHPat.var("same"),
                                                                                                               ~ann=
                                                                                                                 UHTyp.(
                                                                                                                   SeqOpExp(ExpOpExp(Int, Arrow, Int), Arrow, Bool)
                                                                                                                   |> Typ.mk_OpSeq
                                                                                                                 ),
                                                                                                               UHExp.(EmptyHole(-1) |> wrap_in_block),
                                                                                                             ),
                                                                                                             UHExp.letline(
                                                                                                               UHPat.var("different"),
                                                                                                               ~ann=
                                                                                                                 UHTyp.(
                                                                                                                   SeqOpExp(ExpOpExp(Int, Arrow, Int), Arrow, Bool)
                                                                                                                   |> Typ.mk_OpSeq
                                                                                                                 ),
                                                                                                               UHExp.(EmptyHole(-1) |> wrap_in_block),
                                                                                                             ),
                                                                                                             UHExp.EmptyLine,
                                                                                                             UHExp.letline(
                                                                                                               UHPat.var("all_same"),
                                                                                                               ~ann=
                                                                                                                 UHTyp.(
                                                                                                                   SeqOpExp(
                                                                                                                     SeqOpExp(ExpOpExp(Int, Arrow, Int), Arrow, Int),
                                                                                                                     Arrow,
                                                                                                                     Bool,
                                                                                                                   )
                                                                                                                   |> Typ.mk_OpSeq
                                                                                                                 ),
                                                                                                               UHExp.(EmptyHole(-1) |> wrap_in_block),
                                                                                                             ),
                                                                                                             UHExp.letline(
                                                                                                               UHPat.var("all_different"),
                                                                                                               ~ann=
                                                                                                                 UHTyp.(
                                                                                                                   SeqOpExp(
                                                                                                                     SeqOpExp(ExpOpExp(Int, Arrow, Int), Arrow, Int),
                                                                                                                     Arrow,
                                                                                                                     Bool,
                                                                                                                   )
                                                                                                                   |> Typ.mk_OpSeq
                                                                                                                 ),
                                                                                                               UHExp.(EmptyHole(-1) |> wrap_in_block),
                                                                                                             ),
                                                                                                             UHExp.EmptyLine,
                                                                                                             UHExp.letline(
                                                                                                               UHPat.var("is_set"),
                                                                                                               ~ann=
                                                                                                                 UHTyp.(
                                                                                                                   SeqOpExp(
                                                                                                                     SeqOpExp(
                                                                                                                       ExpOpExp(num_triple, Arrow, num_triple),
                                                                                                                       Arrow,
                                                                                                                       num_triple,
                                                                                                                     ),
                                                                                                                     Arrow,
                                                                                                                     Bool,
                                                                                                                   )
                                                                                                                   |> Typ.mk_OpSeq
                                                                                                                 ),
                                                                                                               UHExp.(Block([EmptyLine], EmptyHole(-1))),
                                                                                                             ),
                                                                                                           ],
                                                                                                           UHExp.EmptyHole(-1),
                                                                                                         )
                                                                                                         |> ZExp.place_before_block;
                                                                                                       },
                                                                                                     },
                                                                                                   ];

                                                                                                 let cardstack: CardStack.t = {title: "RC Study Questions", cards};
                                                                                                 */
