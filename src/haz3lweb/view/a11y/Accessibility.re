open Virtual_dom.Vdom;
//open Util.Web;

let inputField_id = "a11y-input";
let outputArea_id = "a11y-output";

let input_view = (id, input): Node.t => {
  Node.div(~attr=Attr.id(id), [Node.text(":" ++ input)]);
};

let output_view = (id, content): Node.t => {
  Node.div(~attr=Attr.id(id), [Node.text(content)]);
};

let view = (~_inject, model: AccessibilityModel.t): Node.t => {
  Node.div(
    ~attr=Attr.classes(["a11y"]),
    [
      input_view(inputField_id, model.input),
      output_view(
        outputArea_id,
        model.query_result |> Option.value(~default=""),
      ),
    ],
  );
};
