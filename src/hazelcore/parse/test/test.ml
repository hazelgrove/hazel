type t =
{
  name: string;
  valid: bool;
  text: string;
}

let tests = [
  {name="Basic types"; valid=true; text="1; two; 3.0; true; false"};
  {name="Let Basic"; valid=true; text="let a = 1 in a"};
  {name="Let type annotation"; valid=true; text="let a : Int = 1 in a"};
  {name="Basic Lambda"; valid=true; text="\\f.{f}"};
  {name="Multiline"; valid=true; text="let a =\n 1\n in\n a"};
  {name="Comment"; valid=true; text="#Comment\n 3"};
  (* Currently, the final line must be an Exp line *)
  {name="Bad comment"; valid=false; text="#Comment\n 3;\n#Comment"};
  {name="Mult"; valid=true; text="
    let mult : [Int] -> Int =
      \\list.{
        case list
        | hd::[] => hd
        | hd::md::[] => hd * md
        | hd::tl => (mult tl)
        end
      }
    in
    mult (4::3::[])"};
  {name="Map"; valid=true; text="
    let map : (Int -> Int) -> [Int] -> [Int] =
      \\f.{
        \\xs.{
          case xs
          | [] => []
          | y::ys => (f y)::(map f ys)
          end
        }
      }
    in
    map"}
]

module Parsing = Parser.Parsing
module Parse = Parser.Parse
module Print = Parser.Print
module UHDoc_Exp = UHDoc_Exp.Make(Memo.DummyMemo)

let compare_strings original parsed =
  let _ = print_endline original in
  let _ = print_endline parsed in
  (*
  let _ = print_endline (String.escaped original) in
  let _ = print_endline (String.escaped parsed) in
  *)
        original = parsed

let parse text: UHExp.line list =
  let lexbuf = Lexing.from_string text in
  Parsing.parse lexbuf (Parse.Incremental.main lexbuf.lex_curr_p)

let test_parse text: bool =
 (*Get the first AST*)
  let ast = parse text in
  let doc = Lazy.force UHDoc_Exp.mk ~memoize:true ~enforce_inline:false ast in
  let lay = Pretty.LayoutOfDoc.layout_of_doc ~width:100 ~pos:0 doc in
  let l = match lay with
  | Some l -> l
  | None -> Pretty.Layout.Text("")
  in
  let printed_text = Print.string_of_layout l in
  let _ = compare_strings text printed_text in
  (*Get the seconds AST*)
  let ast_b = parse printed_text in
  ast = ast_b

let thing test =
  match test with
  | {text = t; valid = v; _} ->
  let success = test_parse t in
success = v

(*
let%test _ =
  let run_test (tests: t list) = List.iter thing tests; true
  in run_test tests
  *)

let%test _ = thing (List.nth tests 0)
let%test _ = thing (List.nth tests 1)
let%test _ = thing (List.nth tests 2)
let%test _ = thing (List.nth tests 3)
let%test _ = thing (List.nth tests 4)
let%test _ = thing (List.nth tests 5)
let%test _ = thing (List.nth tests 6)
let%test _ = thing (List.nth tests 7)
let%test _ = thing (List.nth tests 8)
