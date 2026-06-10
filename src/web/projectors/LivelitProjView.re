open Virtual_dom.Vdom;
open Haz3lcore;
open ProjectorBase;
open ProjectorViewBase;
open Language;

module V: ProjectorView = {
  module L = LivelitProj.M;

  let focus_pointer = (id: Id.t) => {
    JsUtil.get_elem_by_id(Id.cls(id))##focus;
  };

  let focusable =
    Focusable.{
      pointer: Some(focus_pointer),
      keyboard: None,
    };

  let view = ({info, parent, _}: View.args(L.model, L.action)) => {
    let ctx =
      switch (info.statics) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => Ctx.empty
      };

    let node =
      switch (LivelitProj.get_model(info)) {
      | Some((ll_name, model)) =>
        let ll = Ctx.lookup_livelit(ctx, ll_name);

        switch (ll) {
        | Some(ll) =>
          let action_callback = (action: LivelitCtx.action_exp) => {
            let new_model = ll.update(action, model);

            let updated_segment =
              info.utility.lift_syntax(
                ~inline=true,
                LivelitProj.replace_model_term(new_model),
                info.syntax,
              );

            switch (updated_segment) {
            | Some(s) => parent(SetSyntax(s))
            | None =>
              print_endline("Warning - LivelitProj.view: lift_syntax failed");
              Ui_effect.Ignore;
            };
          };

          switch (LivelitViews.find(ll.name)) {
          | Some(view) =>
            let list_contents = view(model, action_callback);
            Node.div(
              ~attrs=[Attr.class_(ll_name), Attr.id(Id.cls(info.id))],
              [list_contents],
            );
          | None =>
            print_endline("Warning - LivelitProj.view: no view registered");
            Node.text("No livelit view found");
          };
        | None =>
          print_endline("Warning - LivelitProj.view: not found in context");
          Node.text("No livelit found");
        };
      | None =>
        print_endline("Warning - LivelitProj.view: get is empty");
        Node.text("No livelit found");
      };

    View.mk(node);
  };
};
