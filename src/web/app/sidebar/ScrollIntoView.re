open Virtual_dom.Vdom;

/* Scroll the element carrying this attribute into view when it appears.

   A panel that answers a click by opening a row somewhere else in a long list
   has only half answered it: the row is open and the reader is still looking
   at where they were. Following a pointer should land on what it names.

   `nearest` in both axes, so a row already on screen is left where it is
   rather than yanked to the middle of the pane. */
module Hook =
  Attr.Hooks.Make({
    module State = Unit;
    module Input = {
      /* The key of the row being scrolled to. Carried so that moving focus
         from one row to another within a list that is already mounted counts
         as a change, rather than looking like the same hook standing still. */
      type t = string;
      let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_string;
      let combine = (_, newer) => newer;
    };
    let scroll = el =>
      Js_of_ocaml.Js.Unsafe.coerce(el)##scrollIntoView(
        Js_of_ocaml.Js.Unsafe.obj([|
          (
            "block",
            Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string("nearest")),
          ),
          (
            "inline",
            Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string("nearest")),
          ),
        |]),
      );
    let init = (_, _el) => ();
    let on_mount = (_, (), el) => scroll(el);
    let update = (~old_input, ~new_input, (), el) =>
      if (old_input != new_input) {
        scroll(el);
      };
    let destroy = (_, (), _el) => ();
  });

/* [name] separates one caller's hook from another's, so two panels scrolling
   two different lists do not read as the same attribute moving. */
let attr = (~name: string, key: string): Attr.t =>
  Attr.create_hook(name, Hook.create(key));
