open Virtual_dom.Vdom;
open Node;
let img = create("img");

let slide = (header, content) =>
  div(
    ~key="slide",
    ~attrs=[Attr.class_("slide")],
    [
      h1(
        ~key="header",
        ~attrs=[Attr.class_("slide-header")],
        [text(header)],
      ),
      div(~key="content", ~attrs=[Attr.class_("slide-content")], content),
    ],
  );

let code = content => span(~attrs=[Attr.class_("code")], [text(content)]);

let em = content => span(~attrs=[Attr.class_("em")], [text(content)]);

let get_content =
  fun
  | "Tuples" =>
    Some(
      slide(
        "Tuples",
        [
          p([
            text(
              "Tuples are ordered collections of values that can optionally include labels for some or all elements. Labels enable more expressive programming by allowing access via label rather than position.",
            ),
          ]),
        ],
      ),
    )
  | "Tables" =>
    Some(
      slide(
        "Tables",
        [
          p([
            text(
              "Tables in Hazel are represented as lists of labeled tuples, where each tuple corresponds to a row and the labels correspond to column names. This structure enables familiar table-like operations such as projection, filtering, and transformation.",
            ),
          ]),
          p([
            text(
              "Label-based projection works the same way as it does on individual tuples, but automatically broadcasts across the list to extract a column of values.",
            ),
          ]),
        ],
      ),
    )
  | "Pattern Matching on Tuples" =>
    Some(slide("Pattern Matching on Tuples", []))
  | "Recursion" => Some(slide("Recursion", []))
  | "Lists" => Some(slide("Lists", []))
  | "Pattern Matching on Lists" =>
    Some(slide("Pattern Matching on Lists", []))
  | "Recursion on Lists: length" =>
    Some(slide("Recursion on Lists: length", []))
  | "Recursion on Lists: sum" => Some(slide("Recursion on Lists: sum", []))
  | "Recursion on Lists: num_zeros" =>
    Some(slide("Recursion on Lists: num_zeros", []))
  | "Higher-Order Functions" => Some(slide("Higher-Order Functions", []))
  | _ => None;
