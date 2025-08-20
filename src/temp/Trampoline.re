// This module defines the stack machine for the evaluator.
module Trampoline = {
  type t('a) =
    | Bind(t('b), 'b => t('a)): t('a)
    | Next(unit => t('a)): t('a)
    | Done('a): t('a);

  type callstack('a, 'b) =
    | Finished: callstack('a, 'a)
    | Continue('a => t('b), callstack('b, 'c)): callstack('a, 'c);

  let rec run: type a b. (t(b), callstack(b, a)) => a =
    (t: t(b), callstack: callstack(b, a)) => (
      switch (t) {
      | Bind(t, f) => (run(t, Continue(f, callstack)): a)
      | Next(f) => run(f(), callstack)
      | Done(x) =>
        switch (callstack) {
        | Finished => x
        | Continue(f, callstack) => run(f(x), callstack)
        }
      }: a
    );

  let run = run(_, Finished);

  let return = x => Done(x);

  let bind = (t, f) => Bind(t, f);

  module Syntax = {
    let (let.trampoline) = (x, f) => bind(x, f);
  };
};
