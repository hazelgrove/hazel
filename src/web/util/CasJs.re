open Js_of_ocaml;

/* Gives the checker the Algebrite this JS context loads as the global
 * `Algebrite`. The page loads it in a module script, which runs after
 * the app starts, so it is looked up at each use. Where it comes from
 * `script` instead, the script is loaded at the first use, so a program
 * with no `algebra` step never loads it. */
let install = (~script: option(string)=?, ()): unit => {
  let script = ref(script);
  Language.Cas.run :=
    (
      input => {
        switch (script^) {
        | Some(url) =>
          script := None;
          Js.Unsafe.global##importScripts(Js.string(url));
        | None => ()
        };
        let algebrite = Js.Unsafe.global##.Algebrite;
        String.equal(Js.to_string(Js.typeof(algebrite)), "undefined")
          ? None : Some(Js.to_string(algebrite##run(Js.string(input))));
      }
    );
};
