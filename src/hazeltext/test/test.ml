module Parsing = Hazeltext.Parsing
module Parse = Hazeltext.Parse
module Print = Hazeltext.Print
module UHDoc_Exp = UHDoc_Exp.Make(Memo.DummyMemo)

let parse text: UHExp.block option =
  let lexbuf = Lexing.from_string text in
  let (ast, _) = Parsing.ast_of_lexbuf lexbuf in
  ast

let test_parse text: bool =
 (*Get the first AST*)
  let ast_a = parse text in
  match ast_a with
  | Some(ast) ->
    let doc = Lazy.force UHDoc_Exp.mk ~memoize:true ~enforce_inline:false ast in
    let lay = Pretty.LayoutOfDoc.layout_of_doc ~width:100 ~pos:0 doc in
    let l = match lay with
    | Some l -> l
    | None -> Pretty.Layout.Text("")
    in
    let printed_text = Print.string_of_layout l in
    (*Get the seconds AST*)
    let ast_b = parse printed_text in
    ast_a = ast_b
  | None -> false

let%test "basic types" =
  test_parse "1; two; 3.0; true; false"

let%test "let basic" =
  test_parse "let a = 1 in a"

let%test "let type annotation" =
  test_parse "let a : Int = 1 in a"

let%test "basic lambda" =
  test_parse "\\f.{f}"

let%test "multiline" =
  test_parse "let a =\n 1\n in\n a"

let%test "comment" =
  test_parse "#Comment\n 3"

(* Currently, the final line must be an Exp line *)
let%test "bad comment" =
  test_parse "#Comment \n 3; #Comment"
  = false

let%test "mult" =
  test_parse "
    let mult : [Int] -> Int =
      \\list.{
        case list
        | hd::[] => hd
        | hd::md::[] => hd * md
        | hd::tl => (mult tl)
        end
      }
    in
    mult (4::3::[])"

let%test "map" =
  test_parse "
  let map : (Int -> Int) -> [Int] -> [Int] =
      \\f.{
        \\xs.{
          case xs
          | [] => []
          | y::ys => (f y)::(map f ys)
          end
        }
      }
  in map"
