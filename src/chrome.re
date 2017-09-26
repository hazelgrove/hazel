open View;

open Tyxml_js;

let view ((rs, rf): Model.rp) => {
  /* pp view */
  let pp_view_width = 30;
  let pp_rs =
    React.S.map
      (
        fun (zexp, _) => {
          let prettified =
            Pretty.HTML_Of_SDoc.html_of_sdoc (
              Pretty.PP.sdoc_of_doc pp_view_width (PPView.of_zexp zexp)
            );
          [prettified]
        }
      )
      rs;
  let pp_view = R.Html5.div (ReactiveData.RList.from_signal pp_rs);
  /* htype view */
  let htype_rs =
    React.S.map
      (
        fun (_, htype) => {
          let pp_view = PPView.of_htype htype;
          let sdoc = Pretty.PP.sdoc_of_doc pp_view_width pp_view;
          let prettified = Pretty.HTML_Of_SDoc.html_of_sdoc sdoc;
          [prettified]
        }
      )
      rs;
  let htype_view = R.Html5.div (ReactiveData.RList.from_signal htype_rs);
  Tyxml_js.To_dom.of_div
    Html5.(
      div [
        div a::[a_class ["top-bar"]] [span a::[a_class ["logo-text"]] [pcdata "Hazel"]],
        div
          a::[a_class ["main-area"]]
          [
            div
              a::[a_class ["page-area"]]
              [
                div
                  a::[a_class ["page"]]
                  [
                    h1 [pcdata "Welcome to Hazel!"],
                    hr (),
                    p [pcdata "Introductory paragraph"],
                    div a::[a_class ["ModelExp"]] [pp_view],
                    div [span [pcdata "Type: "], htype_view]
                  ]
              ],
            div a::[a_class ["sidebar"]] [Action_palette.make_palette (rs, rf)]
          ],
        div
          a::[a_class ["container"], a_id "footerContainer"]
          [
            p [
              pcdata "Source (OCaml): ",
              a
                a::[a_href "https://github.com/hazelgrove/hazel"]
                [pcdata "https://github.com/hazelgrove/hazel"]
            ],
            p [
              pcdata "A project of ",
              a a::[a_href "http://hazelgrove.org"] [pcdata "Hazel Grove"],
              pcdata "."
            ]
          ]
      ]
    )
};
