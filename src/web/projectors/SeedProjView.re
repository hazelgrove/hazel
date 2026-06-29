open Virtual_dom.Vdom;
open Haz3lcore;
open ProjectorBase;
open ProjectorViewBase;

/* Minimal view: `^^seed(N)` is a CLI tooling concept (resolved at run time), so
   in the editor it simply renders its current seed value. */
module V: ProjectorView = {
  module L = SeedProjector.M;

  let focusable = Focusable.non;

  let view = ({model, _}: View.args(L.model, L.action)) =>
    View.mk(
      Node.span(
        ~attrs=[Attr.classes(["seed-projector"])],
        [Node.text(string_of_int(SeedProjector.seed_of(model)))],
      ),
    );
};
