module T = {
  [@deriving sexp]
  type t('success) =
    | Succeeded('success)
    | CursorEscaped(Side.t)
    | Failed;

  let map =
    `Custom(
      (x, f) =>
        switch (x) {
        | Succeeded(a) => Succeeded(f(a))
        | CursorEscaped(s) => CursorEscaped(s)
        | Failed => Failed
        },
    );

  let bind = (x, f) =>
    switch (x) {
    | Succeeded(a) => f(a)
    | CursorEscaped(s) => CursorEscaped(s)
    | Failed => Failed
    };

  let return = a => Succeeded(a);
};
include T;
include Monads.Make(T);

let rescue_escaped = (f: Side.t => t('a)) =>
  fun
  | Succeeded(a) => Succeeded(a)
  | CursorEscaped(side) => f(side)
  | Failed => Failed;

let of_option =
  fun
  | Some(x) => Succeeded(x)
  | None => Failed;
