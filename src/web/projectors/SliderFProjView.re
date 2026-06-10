open Virtual_dom.Vdom;
open Haz3lcore;
open ProjectorBase;
open ProjectorViewBase;

module V: ProjectorView = {
  module L = SliderFProj.M;

  let focusable = Focusable.non;

  let view = ({info, parent, _}: View.args(L.model, L.action)) =>
    View.mk(
      Util.WebUtil.range(
        ~attrs=[
          Attr.on_input((_, v) =>
            parent(SetSyntax(SliderFProj.put(info, v)))
          ),
        ],
        info |> SliderFProj.get |> Printf.sprintf("%.2f"),
      ),
    );
};
