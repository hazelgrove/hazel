open ProjectorBase;
open Virtual_dom.Vdom;
open Node;

let mk = (_: fold): core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = fold;
     let model = ();
     let can_project = _ => true;
     let placeholder = _ => Inline(2);
     let update = _ => Fold();
     let view = (~info as _, ~inject) =>
       div(
         ~attrs=[Attr.on_double_click(_ => inject(Remove))],
         [text("⋱")],
       );
     let keymap = (_, _): option(ProjectorBase.action) => None;
   });
