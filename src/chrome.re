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
        div
          a::[a_class ["jumbotron"]]
          [
            div
              a::[a_class ["headerTextAndLogo"]]
              [
                div a::[a_class ["display-3"]] [pcdata "Hazel"],
                div
                  a::[a_class ["logoDiv"]]
                  [
                    img
                      a::[a_id "logo"]
                      alt::"Logo"
                      src::(Xml.uri_of_string "imgs/hazel-logo.png")
                      ()
                  ]
              ],
            div
              a::[a_class ["subtext"]]
              [pcdata "(a structure editor rooted in the principles of type theory)"],
            br (),
            div
              a::[a_class ["typeLbl"]]
              [pcdata ("Pretty-printed (width:" ^ string_of_int pp_view_width ^ "):")],
            div a::[a_class ["ModelExp"]] [pp_view],
            br (),
            div
              a::[a_class ["subtext", "ModelType"]]
              [div a::[a_class ["typeLbl"]] [pcdata "Synthesizes H-type: "], htype_view]
          ],
        Action_palette.make_palette (rs, rf),
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
