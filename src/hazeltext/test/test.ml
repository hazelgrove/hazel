module Parsing = Hazeltext.Parsing
module Parse = Hazeltext.Parse
module Print = Hazeltext.Print
module UHDoc_Exp = UHDoc_Exp.Make (Memo.DummyMemo)

let parse text : UHExp.block option =
  let lexbuf = Lexing.from_string text in
  let ast, _ = Parsing.ast_of_lexbuf lexbuf in
  ast

let test_parse text : bool =
  (*Get the first AST*)
  try
    let ast_a = parse text in
    match ast_a with
    | Some ast -> (
        let doc =
          Lazy.force UHDoc_Exp.mk ~memoize:true ~enforce_inline:false ast
        in
        let lay = Pretty.LayoutOfDoc.layout_of_doc ~width:100 ~pos:0 doc in
        try
          let l = match lay with Some l -> l | None -> raise Print.NoLayout in
          let printed_text = Print.string_of_layout l in
          (*Get the seconds AST*)
          let ast_b = parse printed_text in
          ast_a = ast_b
        with Print.NoLayout -> false)
    | None -> false
  with Failure _ -> false

let test_incorrect text = test_parse text = false

let%test "basic types" = test_parse "1; two; 3.0; true; false"

let%test "let basic" = test_parse "let a = 1 in a"

let%test "let type annotation" = test_parse "let a : Int = 1 in a"

let%test "basic lambda" = test_parse "\\f.{f}"

let%test "multiline" = test_parse "let a =\n 1\n in\n a"

let%test "comment" = test_parse "#Comment\n 3"

(* Currently, the final line must be an Exp line *)
let%test "bad comment" = test_incorrect "#Comment \n 3; #Comment"

let%test "mult" =
  test_parse
    "\n\
    \    let mult : [Int] -> Int =\n\
    \      \\list.{\n\
    \        case list\n\
    \        | hd::[] => hd\n\
    \        | hd::md::[] => hd * md\n\
    \        | hd::tl => (mult tl)\n\
    \        end\n\
    \      }\n\
    \    in\n\
    \    mult (4::3::[])"

let%test "map" =
  test_parse
    "\n\
    \  let map : (Int -> Int) -> [Int] -> [Int] =\n\
    \      \\f.{\n\
    \        \\xs.{\n\
    \          case xs\n\
    \          | [] => []\n\
    \          | y::ys => (f y)::(map f ys)\n\
    \          end\n\
    \        }\n\
    \      }\n\
    \  in map"

let%test "case type annot" =
  test_parse
    "\n\
    \  let a =\n\
    \    \\f.{\n\
    \      case f\n\
    \       | 1 : Int => 1\n\
    \       | 2 => 2\n\
    \      end\n\
    \    }\n\
    \  in a 1"

let%test "pat type annotation" =
  test_parse
    "let c = λa : Int, b : Float.{1, true} in\n\
     let d : (Int), (Bool), e : Float = c (2, 1.0), 2.0 in\n\
     let d : (Int, Bool), e : Float = c (2, 1.0), 2.0 in\n\
     (d, e)"

let%test "incorrect pat type annotation" =
  test_incorrect "let a : (Int, b: Bool) = ? in a"
