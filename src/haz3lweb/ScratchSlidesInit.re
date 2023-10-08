let slide0: ScratchSlide.persistent_state = LanguageRefSlide.slide;

let empty: ScratchSlide.persistent_state = (
  1,
  {
    zipper: "((selection((focus Left)(content())))(backpack())(relatives((siblings(()((Grout((id 0)(shape Convex))))))(ancestors())))(caret Outer))",
    backup_text: "",
  },
);

let paper_intro_example: ScratchSlide.persistent_state = (
  350,
  {
    zipper: "((selection((focus Left)(content())))(backpack())(relatives((siblings(((Secondary((id 5)(content(Whitespace\" \"))))(Tile((id 7)(label(x))(mold((out Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id 8)(content(Whitespace\" \"))))(Tile((id 20)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave 11))(sort Typ))))))(shards(0))(children())))(Secondary((id 64)(content(Whitespace\" \"))))(Grout((id 23)(shape Convex))))((Secondary((id 22)(content(Whitespace\" \")))))))(ancestors((((id 4)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards((0)(1 2)))(children(()(((Secondary((id 65)(content(Whitespace\" \"))))(Grout((id 26)(shape Convex)))(Secondary((id 24)(content(Whitespace\" \")))))))))(((Secondary((id 157)(content(Comment\"# Put cursor on ! to see suggestions in the #\"))))(Secondary((id 158)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id 213)(content(Comment\"# cursor inspector below. Hover over suggestions #\"))))(Secondary((id 214)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id 311)(content(Comment\"# to tentatively accept them and see resulting errors. #\"))))(Secondary((id 312)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id 349)(content(Comment\"# Click on a suggestion to accept it #\"))))(Secondary((id 83)(content(Whitespace\"\\226\\143\\142\")))))((Secondary((id 28)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id 34)(label(if then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id 35)(content(Whitespace\" \"))))(Tile((id 37)(label(x))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id 40)(content(Whitespace\" \")))))((Secondary((id 44)(content(Whitespace\" \"))))(Tile((id 45)(label(x))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id 46)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id 48)(label(x))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id 51)(content(Whitespace\" \"))))(Tile((id 52)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave 5))(sort Exp))))))(shards(0))(children())))(Secondary((id 54)(content(Whitespace\" \"))))(Tile((id 55)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id 58)(content(Whitespace\" \")))))))))(Secondary((id 62)(content(Whitespace\" \"))))(Tile((id 63)(label(0))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0))(children()))))))))))(caret Outer))",
    backup_text: "# Put cursor on ! to see suggestions in the #\n# cursor inspector below. Hover over suggestions #\n# to tentatively accept them and see resulting errors. #\n# Click on a suggestion to accept it #\nlet x :   =   in\nif x then x(x + 1) else 0",
  },
);

let type_hole_inference_intro_example: ScratchSlide.persistent_state = (
  507,
  {
    zipper: "((selection((focus Left)(content())))(backpack())(relatives((siblings(((Secondary((id 361)(content(Comment\"# Put cursor on ! to see suggestions in the #\"))))(Secondary((id 362)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id 411)(content(Comment\"# cursor inspector below. Hover over suggestions #\"))))(Secondary((id 412)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id 467)(content(Comment\"# to tentatively accept them and see resulting errors. #\"))))(Secondary((id 468)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id 505)(content(Comment\"# Click on a suggestion to accept it #\")))))((Secondary((id 59)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id 4)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id 5)(content(Whitespace\" \"))))(Tile((id 7)(label(f))(mold((out Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort Pat))))))(shards(0))(children())))(Tile((id 8)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave 11))(sort Typ))))))(shards(0))(children())))(Secondary((id 56)(content(Whitespace\" \"))))(Grout((id 11)(shape Convex)))(Secondary((id 10)(content(Whitespace\" \")))))((Secondary((id 12)(content(Whitespace\" \"))))(Tile((id 13)(label(2))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id 16)(content(Whitespace\" \")))))))))(Secondary((id 18)(content(Whitespace\" \"))))(Tile((id 21)(label(if then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id 57)(content(Whitespace\" \"))))(Grout((id 27)(shape Convex)))(Secondary((id 58)(content(Whitespace\" \")))))((Secondary((id 29)(content(Whitespace\" \"))))(Tile((id 32)(label(f))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id 33)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id 37)(label(2))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0))(children()))))))))(Secondary((id 40)(content(Whitespace\" \")))))))))(Secondary((id 55)(content(Whitespace\" \"))))(Grout((id 54)(shape Convex))))))(ancestors())))(caret Outer))",
    backup_text: "# Put cursor on ! to see suggestions in the #\n# cursor inspector below. Hover over suggestions #\n# to tentatively accept them and see resulting errors. #\n# Click on a suggestion to accept it #\nlet f:   = 2 in if   then f(2) else  ",
  },
);

let num_empty = 0;

let init_data = [
  paper_intro_example,
  type_hole_inference_intro_example,
  slide0,
  ...List.init(num_empty, _ => empty),
];

assert(List.length(init_data) > 0);

let init = (): Editors.scratch => (
  0,
  init_data |> List.map(ScratchSlide.unpersist),
);

let init_nth = n => {
  let data = List.nth(init_data, n);
  ScratchSlide.unpersist(data);
};
