open Util;

/* Slider over an Int literal. See SliderCore for the shared implementation. */
module M =
  SliderCore.Make({
    let name = "Slider";
    let literal = "integer";
    type t = Bigint.t;
    let of_atom = (a: Language.Atom.t) =>
      switch (a) {
      | Int(i) => Some(i)
      | _ => None
      };
    let to_atom = (v: string): Language.Atom.t => Int(Bigint.of_string(v));
    let to_string = Bigint.to_string;
  });
