let defaults = [
  "let int_lits : Int = 1 in
let negation = -1 in
let arithmetic = 1*2 + 8/4 in
let int_comparison = (10 == 10, 1 < 2, 2 <= 3, 3 > 2, 2 >= 1) in

let float_lits : Float = 1.5 in
let float_artih = 1. *. 2. +. 8. /. 4. in
let float_comparison = (10. ==. 10., 1. <. 2., 2. <=. 3., 3. >. 2., 2. >=. 1.) in

let booleans : (Bool, Bool) = (true, false) in
let conditionals =
let (x, y) = (2 + 2, 3 + 3) in
if y > x then 1
else 2
in

let tuples : (Int, Bool, (Bool, Int)) = (1, true, (false, 3)) in
let (a, b, (c, d)) = tuples in

let y : (Int, Int, Int) -> Int =
fun (m, x, b) -> m * x + b
in

let double_recursively : Int -> Int =
fun n ->
if n == 0 then 0
else double_recursively(n - 1) + 2
in

let empty_list : [Int] = nil in
let non_empty_list : [Int] = 1::2::3::nil in
let list_literals : [Int] = [1, 2, 3] in
let length : [Int] -> Int =
fun xs ->
case xs
| nil => 0
| hd::tl => 1 + length(tl)
end
in
let has_at_least_two_elements : [Int] -> Bool =
fun xs ->
case xs
| nil => false
| hd::nil => false
| a::b::nil => true
end
in
",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
];

let init = (): Editors.scratch => {
  (0, List.map(ScratchSlide.init, defaults));
};
