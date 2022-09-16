(*
open Text.Parsing

let%test "a" =
  let a = ast_of_string "let a = 4 in a" in
  match a with
  | Ok b ->
      let c = Sexplib.Sexp.to_string_hum (Haz3lcore.Term.UExp.sexp_of_t b) in
      print_endline c;
      true
  | Error e ->
      print_endline ("Error: " ^ e);
      false
      *)

open Text.Parsing

let test_parse text : bool =
  let ast = ast_of_string text in
  match ast with
  | Ok _ ->
      (*
      let c = Sexplib.Sexp.to_string_hum (Haz3lcore.Term.UExp.sexp_of_t b) in
      print_endline c;
      *)
      true
  | Error e ->
      print_endline ("Error: " ^ e);
      false

let test_incorrect text = test_parse text = false

let%test "let basic" = test_parse "let a = 1 in a"
let%test "let type annotation" = test_parse "let a : Int = 1 in a"
let%test "basic lambda" = test_parse "fun f -> f"
let%test "multiline" = test_parse "let a =\n 1\n in\n a"
let%test "something" = test_parse "let a : [Int] = [1] in a"

let%test "basic let fun" =
  test_parse "let a : Int -> Int = fun x -> x + 1 in a(4)"
let%test "annotated fun" = test_parse "fun (x : Int) -> x"
let%test "annotated fun2" = test_parse "fun (x : Int -> Int) -> x"

let%test "major" =
  test_parse
    "let a = 2 in\n\
     let b : Bool = true in\n\
     let g : Int -> Int =\n\
    \  fun x -> x + 1\n\
     in\n\
     let x =\n\
    \  fun q -> if q < 0 then false else true in\n\
     let f =\n\
    \  fun (x : Int) -> x + 5 < 0 in\n\
     true && f(a) && f(4) && (g(5) == 6)\n\
    \  "
(*
let%test "basic types" = test_parse "1; two; 3.0; true; false"
let%test "comment" = test_parse "#Comment\n 3"
(* Currently, the final line must be an Exp line *)
let%test "bad comment" = test_incorrect "#Comment \n 3; #Comment"
(* The program must end in an expr line of some sort *)
let%test "only comment" = test_incorrect "# Comment"
let%test "only empty" = test_incorrect "\n"
*)

(*
let%test "mult" =
  test_parse
    "    let mult : [Int] -> Int =\n\
    \      fun list {\n\
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
    \      fun f {\n\
    \        fun xs {\n\
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
    \    fun f {\n\
    \      case f\n\
    \       | 1 : Int => 1\n\
    \       | 2 => 2\n\
    \      end\n\
    \    }\n\
    \  in a 1"

let%test "pat type annotation" =
  test_parse
    "let c = fun a : Int, b : Float {1, true} in\n\
     let d : (Int), (Bool), e : Float = c (2, 1.0), 2.0 in\n\
     let d : (Int, Bool), e : Float = c (2, 1.0), 2.0 in\n\
     (d, e)"

let%test "incorrect pat type annotation" =
  test_incorrect "let a : (Int, b: Bool) = ? in a"

let%test "exp shapes" =
  test_parse
    "let a : (Int, (Int, Bool)), b : Float = ((1, (1, true)), 1.0) in\n\
     let c = a::(2, (2, false))::[] in\n\
     let d =\n\
    \  case c\n\
    \  | a::_::[] => 0\n\
    \  | _ => 1\n\
    \  end\n\
     in\n\
     let e = fun x {x+1} in\n\
     (e, 'x3, ?)"

let%test "float ops" =
  test_parse
    "\n\
    \    let a : Float = 1.0 -. 2.0 in\n\
    \    let b = a +. 1.0 /. 2.0 *. 3.0 in\n\
    \    let c = b ==. 0. in\n\
    \    let d = a >. b in\n\
    \    let e = b <. b in\n\
    \    e\n\
    \  "

let%test "identifier characters" =
  test_parse
    "\nlet __a = 3 in\nlet 0a = 4 in\nlet 'b = 5 in\nlet c' = 6 in\nc'\n  "

let%test "multiple type annotations" =
  test_incorrect "let a : Int : Float = 3 in a"

let%test "block within case" =
  test_parse
    "case 2; 3 | ? => ? end;\n\
     case\n\
     let a = 3 in\n\
     let b = 2 in\n\
     a+b\n\
     | 5 => true\n\
     | _ => false\n\
     end"

let%test "case in function position" =
  test_parse "case true\n    | true => fun f {f+1}\n   end 2"

let%test "and, or" = test_parse "((a && b || c) && d || e) && (f || g)"
*)
