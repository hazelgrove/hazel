open Tezt;
open Tezt.Base;

module Parsing = Hazeltext.Parsing;
module Parse = Hazeltext.Parse;
module Print = Hazeltext.Print;
module UHDoc_Exp = UHDoc_Exp.Make(Memo.DummyMemo);

let parse = (text: string): option(UHExp.block) =>
  switch (Parsing.ast_of_string(text)) {
  | Ok(ast) => Some(ast)
  | Error(_) => None
  };

let test_parse = (text: string) =>
  try({
    let ast_a = parse(text);
    switch (ast_a) {
    | Some(ast) =>
      let doc =
        Lazy.force(UHDoc_Exp.mk, ~memoize=true, ~enforce_inline=false, ast);
      let lay = Pretty.LayoutOfDoc.layout_of_doc(~width=100, ~pos=0, doc);
      let l =
        switch (lay) {
        | Some(l) => l
        | None => Test.fail("parse failed: no layout")
        };
      let printed_text = Print.string_of_layout(l);
      let ast_b = parse(printed_text);
      ast_a == ast_b;
    | None => false
    };
  }) {
  | Failure(_) => false
  };

let register_test = (title, tags, text) =>
  Test.register(~__FILE__, ~title, ~tags=["hazeltext"] @ tags, () =>
    test_parse(text) ? unit : Test.fail("parse failed!")
  );

let register_negative_test = (title, tags, text) =>
  Test.register(~__FILE__, ~title, ~tags=["hazeltext"] @ tags, () =>
    test_parse(text) ? Test.fail("parse did not fail!") : unit
  );

let () = register_test("basic types", [], "1; two; 3.0; true; false");
let () = register_test("let basic", [], "let a = 1 in a");
let () = register_test("let type annotation", [], "let a : Int = 1 in a");
let () = register_test("basic lambda", [], "fun f {f}");
let () = register_test("multiline", [], "let a = 1 in a");
let () = register_test("comment", [], "#Comment\n 3") /* Currently, the final line must be an Exp line */;
let () = register_negative_test("bad comment", [], "#Comment \n 3; #Comment") /* The program must end in an expr line of some sort */;
let () = register_negative_test("only comment", [], "# Comment");
let () = register_negative_test("only empty", [], "\n");
let () =
  register_test("func app", [], "let f = fun x { fun y { x + y } } in f 1 2");

let () =
  register_test(
    "func app 2",
    [],
    {|let f = fun x {fun y { x + y }} in
      let g = fun g { fun x { fun y { g x y } }} in
      g f 1 2|},
  );

let () =
  register_test(
    "mult",
    [],
    {|let mult : [Int] -> Int =
          fun list {
            case list
             | hd::[] => hd
             | hd::md::[] => hd * md
             | hd::tl => (mult tl)
            end
          }
     in
     mult (4::3::[])|},
  );

let () =
  register_test(
    "map",
    [],
    {|
       let map : (Int -> Int) -> [Int] -> [Int] =
           fun f {
             fun xs {
               case xs
               | [] => []
               | y::ys => (f y)::(map f ys)
               end
             }
           }
       in map|},
  );

let () =
  register_test(
    "case type annot",
    [],
    {|
       let a =
         fun f {
           case f
            | 1 : Int => 1
            | 2 => 2
           end
         }
       in a 1|},
  );

let () =
  register_test(
    "pat type annotation",
    [],
    {|let c = fun a : Int, b : Float {1, true} in
      let d : (Int), (Bool), e : Float = c (2, 1.0), 2.0 in
      let d : (Int, Bool), e : Float = c (2, 1.0), 2.0 in
      (d, e)|},
  );

let () =
  register_negative_test(
    "incorrect pat type annotation",
    [],
    "let a : (Int, b: Bool) = ? in a",
  );

let () =
  register_test(
    "exp shapes",
    [],
    {|
      # EXP

      let a : (Int, (Int, Bool)), b : Float = ((1, (1, true)), 1.0) in
      let c = a::(2, (2, false))::[] in
      let d =
      case c
      | a::_::[] => 0
      | _ => 1
      end
     in
     let e = fun x {x+1} in
     (e, 'x3, ?)|},
  );

let () =
  register_test(
    "float ops",
    [],
    {|
      let a : Float = 1.0 -. 2.0 in
      let b = a +. 1.0 /. 2.0 *. 3.0 in
      let c = b ==. 0. in
      let d = a >. b in
      let e = b <. b in
      e
      |},
  );

let () =
  register_test(
    "identifier characters",
    [],
    "let __a = 3 in let 0a = 4 in let 'b = 5 in let c' = 6 in c'",
  );

let () =
  register_negative_test(
    "multiple type annotations",
    [],
    "let a : Int : Float = 3 in a",
  );

let () =
  register_test(
    "block within case",
    [],
    {|case 2; 3 | ? => ? end;
      case
      let a = 3 in
      let b = 2 in
      a+b
      | 5 => true
      | _ => false
      end|},
  );

let () =
  register_test(
    "case in function position",
    [],
    "case true\n    | true => fun f {f+1}\n   end 2",
  );

let () =
  register_test("and, or", [], "((a && b || c) && d || e) && (f || g)");
