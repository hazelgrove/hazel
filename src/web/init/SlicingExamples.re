// Worked examples from the type-slicing paper, loaded as the default scratchpads.

let parse_digit = {|type Option(A) = None + Some(A) in
type Digit = Zero + One + Two + Three + Four + Five + Six + Seven + Eight + Nine in
type Pin = (Digit, Digit, Digit, Digit) in
let parse_digit = fun c : String -> case c
  | "0" => Some@<Digit>(Zero)               | "1" => Some(One)   | "2" => Some(Two)
  | "3" => Some(Three) | "4" => Some(Four)  | "5" => Some(Five)  | "6" => Some(Six)
  | "7" => Some(Seven) | "8" => Some(Eight) | "9" => Some(Nine)  | _ => None end in

parse_digit("1")|};

let parse_pin = {|type Option(A) = None + Some(A) in
type Digit = Zero + One + Two + Three + Four + Five + Six + Seven + Eight + Nine in
type Pin = (Digit, Digit, Digit, Digit) in
let parse_digit = fun c : String -> case c
  | "0" => Some@<Digit>(Zero)               | "1" => Some(One)   | "2" => Some(Two)
  | "3" => Some(Three) | "4" => Some(Four)  | "5" => Some(Five)  | "6" => Some(Six)
  | "7" => Some(Seven) | "8" => Some(Eight) | "9" => Some(Nine)  | _ => None end in

parse_digit("1") ;

let seq = abs A -> abs B -> fun (p : String -> Option((String, A))) ->
    fun (f : A -> Option((String, B))) -> fun (s : String) -> case p(s)
  | None => None | Some(s2, a) => f(a) end in

let digit_parser = fun (s : String) -> case parse_digit(string_sub(s, 0, 1))
  | Some(d) => Some(string_sub(s, 1, string_length(s) - 1), d) | None => None end in

let parse_pin = fun (s : String) ->
    seq@<Digit>@<Pin>(digit_parser)(fun (d1 : Digit) ->
      seq@<Digit>@<Pin>(digit_parser)(
        fun (d2 : Digit) -> fun (s2 : String) -> Some(s2, (d1, d2))
      )(s))(s) in
parse_pin("1234")|};

let pin_parser = {|type Option(A) = None + Some(A) in
type Digit =
  Zero + One + Two + Three + Four + Five + Six + Seven + Eight + Nine in
type Pin = (Digit, Digit, Digit, Digit) in
let parse_digit = fun c : String ->
  case c
  | "0" => Some@<Digit>(Zero)
  | "1" => Some(One)
  | "2" => Some(Two)
  | "3" => Some(Three)
  | "4" => Some(Four)
  | "5" => Some(Five)
  | "6" => Some(Six)
  | "7" => Some(Seven)
  | "8" => Some(Eight)
  | "9" => Some(Nine)
  | _ => None
  end in
let seq = abs A -> abs B ->
  fun (p : String -> Option((String, A))) ->
  fun (f : A -> Option((String, B))) ->
  fun (s : String) ->
    case p(s)
    | None => None
    | Some((s2, a)) => f(a)
    end in
let digit_parser = fun (s : String) ->
  case parse_digit(string_sub(s, 0, 1))
  | Some(d) => Some((string_sub(s, 1, string_length(s) - 1), d))
  | None => None
  end in
let parse_pin = fun (s : String) ->
  seq@<Digit>@<Pin>(digit_parser)(fun (d1 : Digit) ->
  seq@<Digit>@<Pin>(digit_parser)(fun (d2 : Digit) ->
  seq@<Digit>@<Pin>(digit_parser)(fun (d3 : Digit) ->
  seq@<Digit>@<Pin>(digit_parser)(fun (d4 : Digit) ->
  fun (s2 : String) -> Some((s2, (d1, d2, d3, d4))))(s))(s))(s))(s) in
parse_pin("1234")|};

let all: list((string, string)) = [
  ("Digit parser", parse_digit),
  ("Pin parser", parse_pin),
  ("Pin parser (full)", pin_parser),
];
