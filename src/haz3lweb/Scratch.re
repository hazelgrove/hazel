let defaults = [
  "let a = 2 in
let b : Bool = 2 in
let g : Int -> Int =
fun x -> x + 1
in
let x =
fun q -> if q < 0 then a else true in
let f =
fun x : Int -> x + 5 < 0 in
true && f(a) && f(b) && g(true)",
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
