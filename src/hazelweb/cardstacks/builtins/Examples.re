// Examples can be easily added from Hazel by
// exporting the program (in Text Editor), and passing
// the resulting string to `parse`

let parse = (str: string): UHExp.t => {
  let result = Hazeltext.Parsing.ast_of_string(str);
  Stdlib.Result.get_ok(result);
};

let just_hole: UHExp.t = parse("?");

let holey_lambda: UHExp.t = parse("(fun ? {?}) ?");

let let_line: UHExp.t = parse("let y = ? in

  let x = ? in
  x;
  y");

let map_example: UHExp.t =
  parse(
    "let map : (Int -> Int) -> [Int] -> [Int] =
     fun f {
         fun xs {
             case xs
               | [] => []
               | y::ys => (f y)::(map f ys)
               end
           }
       }
   in
   ?",
  );

let qsort_example: UHExp.t =
  parse(
    "let append : [Int] -> [Int] -> [Int] =
     fun xs {
         fun ys {
             case xs
               | [] => ys
               | z::zs => z::(append zs ys)
               end
           }
       }
   in

   let partition : (Int -> Bool) -> [Int] -> ([Int], [Int]) =
     fun f {
         fun xs {
             case xs
               | [] => ([], [])
               | y::ys =>
                 let (ys1, ys2) = partition f ys in
                   case f y
                   | true => (y::ys1, ys2)
                   | false => (ys1, y::ys2)
                   end
               end
           }
       }
   in

   qsort (4::2::6::5::3::1::7::[])",
  );

let rec qsort_n = (n: int): UHExp.t =>
  if (n == 0) {
    [];
  } else {
    [
      UHExp.letline(
        OpSeq.wrap(UHPat.var("qsort" ++ Int.to_string(n))),
        qsort_example,
      ),
      ...qsort_n(n - 1),
    ];
  };

let inconsistent_branches: UHExp.t =
  parse("
case 1
| 0 => true
| 1 => 1.
| 2 => 2.
end ");

let typfun: UHExp.t = {
  let operand_wrap = operand =>
    operand |> OpSeq.wrap |> (x => UHExp.ExpLine(x));
  [
    UHExp.LetLine(
      UHPat.var("id") |> OpSeq.wrap,
      [
        UHExp.typfun(
          TPat.of_string("X"),
          [
            UHExp.lam(
              OpSeq.wrap(UHPat.var("x")),
              [UHExp.var("x") |> operand_wrap],
            )
            |> operand_wrap,
          ],
        )
        |> operand_wrap,
      ],
    ),
    UHExp.Parenthesized([
      UHExp.typapp(
        [UHExp.var("id") |> operand_wrap],
        OpSeq.wrap(UHTyp.Int),
      )
      |> operand_wrap,
    ])
    |> operand_wrap,
  ];
};

let examples = [
  ("hole", just_hole),
  // ("lambda", holey_lambda),
  // ("let", let_line),
  // ("map", map_example),
  // ("quicksort", qsort_example),
  // ("inconsistent branches", inconsistent_branches),
  // ("typfun", typfun),
];

let example_to_card = ((name: string, e: UHExp.t)): CardInfo.t => {
  name,
  caption: Virtual_dom.Vdom.Node.div([], []),
  init_zexp: ZExp.place_before(e),
};

let cardstack: CardstackInfo.t = {
  title: "examples",
  cards: List.map(example_to_card, examples),
};

// let tests = [
//   ("quicksort x1", qsort_n(1)),
//   ("quicksort x10", qsort_n(10)),
//   ("quicksort x100", qsort_n(100)),
// ];

let tests = [];

let teststack: CardstackInfo.t = {
  title: "tests",
  cards: List.map(example_to_card, tests),
};
