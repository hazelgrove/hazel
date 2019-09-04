/* TODO: we might want to just use this API?

    https://github.com/rgrinberg/ocaml-mtl/blob/master/lib/mtl.ml

   Though it's a bit heavy, especially with the extra type parameter...

   In any case, that's a good reference. */

module type MONAD = {
  [@deriving sexp]
  type t('a);
  let return: 'a => t('a);
  let bind: (t('a), 'a => t('b)) => t('b);
};
