open Virtual_dom.Vdom;
open Haz3lcore;
open ProjectorBase;
open ProjectorViewBase;

module V: ProjectorView = {
  module L = CheckboxProj.M;

  let focusable = Focusable.non;

  let view = ({info, parent, _}: View.args(L.model, L.action)) =>
    View.mk(
      Node.input(
        ~attrs=
          [
            Attr.create("type", "checkbox"),
            Attr.on_input((_, _) =>
              parent(SetSyntax(CheckboxProj.toggle(info)))
            ),
          ]
          @ (CheckboxProj.get(info) ? [Attr.checked] : []),
        (),
      ),
    );
};
