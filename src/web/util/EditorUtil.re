open Core;

let editor_of_code = (init_id, code: CodeString.t) => {
  switch (Printer.zipper_of_string(init_id, code)) {
  | None => None
  | Some((z, new_id)) => Some((new_id, Editor.init(z)))
  };
};

let editors_for =
    (xs: list('a), f: 'a => option(string))
    : (Id.t, int, list(('a, option(Editor.t)))) => {
  let (id_gen, zs) =
    List.fold_left(
      ((acc_id, acc_zs), a) => {
        switch (f(a)) {
        | Some(str) =>
          switch (Printer.zipper_of_string(acc_id, str)) {
          | None => (acc_id, acc_zs @ [(a, Some(Zipper.init(0)))])
          | Some((z, new_id)) => (new_id, acc_zs @ [(a, Some(z))])
          }
        | None => (acc_id, acc_zs @ [(a, None)])
        }
      },
      (0, []),
      xs,
    );
  (
    id_gen,
    0,
    List.map(
      ((a, sz)) =>
        switch (sz) {
        | Some(z) => (a, Some(Editor.init(z)))
        | None => (a, None)
        },
      zs,
    ),
  );
};

let editors_of_strings = (xs: list(string)) => {
  let (id, i, aes) = editors_for(xs, x => Some(x));
  (id, i, List.map(((_, oe)) => Option.get(oe), aes));
};
