
## Notation
- `?` empty hole
- `!e!` nonempty hole around expression `e`


- construction
  - `!x!` (free var in synthetic position)
  - `!x! * 32`
    (free var in analytic position, int lit in analytic position)
  - `!let!` (reserved keyword)
  - `123` (int lit synthetic)
  - `let x = ? in x` (bound var in synthetic position)
  - `let x = ? in x + 1` (bound var in analytic position)


```
let b = (32 + !c! - !true!) < (003 * 5_000 / 50_00_000_) && (0xAA + 0X9A + 0o745 + 0O7 + 0b1111 + 0B000) = (!0xGGG! + !0o888! + !0b222! + !999999999999999!) in
  (* make sure 32 in synthetic position works in the process of typing it,
     c in free var hole, true in type inconsistency hole,
     last group of special integer forms should all be invalid text holes
     change < to > too
     TODO: unary negation tests *)
let c = (47. *. !a! +. 99.0) <. (0.5 -. NaN /. Inf) || (1e5 +. 1E0 +. 1_000e1_) = (0XA.A +. 0xEp9 -. !9.a!) in
  (* change <. to >. too
     TODO: unary negation tests
     TODO: negative exponent tests *)
let d = c -. c in
  (* test for bound var, make sure it works when it is just c in synthetic position too *)

# ^ empty line
\x.{x +. d}
# ^ expression line in middle of block

let e =
  !case ?
  | ? => 1.
  | ? => true
  end!
in (* check cursor inspector on case *)
let f : String =
  case ↵
    []
  | !1! => ↵
    !true!
  | ?::? => "hello"
  end
in
let g : Int, h = ? in (* make sure comma after Int introduces new pattern *)
# test for tuple completion
let h : (Int, Bool, String) = ↵

  (?, ?, ?)
in (* check for user newline tuple completion upon opening parentheses *)

let i = 1::!2.!::!true!::[] in

let map : (Int => Int) => List(Int) => List(Int) = \f.\xs.
  case xs
  | [] => []
  | y::ys => f y::map f ys
  end
in (* check cursor inspector that map is in context in body *)

let j = 1 + 2 in
(* first type 12, then apply + between... *)

!g! h


# ^ empty lines trailing concluding expression
```