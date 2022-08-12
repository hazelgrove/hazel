let defaults = [
  "let a = 2 in
letann b : Bool = 2 in
letann g : Int -> Int =
fun x -> x + 1
in
let x =
fun q -> if q < 0 then a else true in
let f =
funann x : Int -> x + 5 < 0 in
true && f(a) && f(b) && g(true)",
  "blog",
  "2525",
];

let init: Model.study = {
  let (id_gen, zs) =
    List.fold_left(
      ((acc_id, acc_zs), str) => {
        switch (Printer.zipper_of_string(acc_id, str)) {
        | None => (acc_id, acc_zs @ [Core.Zipper.init(0)])
        | Some((z, new_id)) => (new_id, acc_zs @ [z])
        }
      },
      (0, []),
      defaults,
    );
  (id_gen, 0, List.map(Model.mk_editor, zs));
};
