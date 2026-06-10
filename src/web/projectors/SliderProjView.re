open Virtual_dom.Vdom;
open Util;
open Haz3lcore;
open ProjectorBase;
open ProjectorViewBase;

module V: ProjectorView = {
  module L = SliderProj.M;

  let focusable = Focusable.non;

  let view = ({info, parent, _}: View.args(L.model, L.action)) =>
    View.mk(
      WebUtil.range(
        ~attrs=[
          Attr.on_input((_, v) =>
            parent(SetSyntax(SliderProj.put(info, v)))
          ),
        ],
        info |> SliderProj.get |> Bigint.to_string,
      ),
    );
};
