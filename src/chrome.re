open View;

open Tyxml_js;

open Semantics.Core;

let view ((rs, rf): Model.rp) => {
  /* pp view */
  let pp_view_width = 50;
  let pp_rs =
    React.S.map
      (
        fun ((zexp, _), _) => {
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
        fun ((_, htype), _) => {
          let pp_view = PPView.of_htype htype;
          let sdoc = Pretty.PP.sdoc_of_doc pp_view_width pp_view;
          let prettified = Pretty.HTML_Of_SDoc.html_of_sdoc sdoc;
          [prettified]
        }
      )
      rs;
  let htype_view = R.Html5.div (ReactiveData.RList.from_signal htype_rs);
  /* result view */
  let result_rs =
    React.S.map
      (
        fun ((zexp, _), _) => {
          let e = ZExp.erase zexp;
          let expanded = Dynamics.DHExp.syn_expand () Ctx.empty e;
          switch expanded {
          | Dynamics.DHExp.DoesNotExpand => [Html5.(pcdata "(does not expand)")] /* should never happen! */
          | Dynamics.DHExp.Expands d ty delta =>
            let result = Dynamics.Evaluator.evaluate () delta d;
            switch result {
            | Dynamics.Evaluator.InvalidInput => [Html5.pcdata "(internal error: invalid input)"]
            | Dynamics.Evaluator.CastError => [Html5.pcdata "(cast error)"]
            | Dynamics.Evaluator.Value d_val
            | Dynamics.Evaluator.Indet d_val =>
              let pp_view = PPView.of_dhexp d_val;
              let sdoc = Pretty.PP.sdoc_of_doc pp_view_width pp_view;
              let prettified = Pretty.HTML_Of_SDoc.html_of_sdoc sdoc;
              [prettified]
            }
          }
        }
      )
      rs;
  let result_view = R.Html5.div (ReactiveData.RList.from_signal result_rs);
  Tyxml_js.To_dom.of_div
    Html5.(
      div [
        div a::[a_class ["top-bar"]] [span a::[a_class ["logo-text"]] [pcdata "Hazel"]],
        div
          a::[a_class ["main-area"]]
          [
            div [
              div
                a::[a_class ["page-area"]]
                [
                  div
                    a::[a_class ["page"]]
                    [
                      h1 [pcdata "Welcome to Hazel"],
                      hr (),
                      p [
                        pcdata "Hazel is an experimental structure editor for a simple typed expression language."
                      ],
                      div a::[a_class ["ModelExp"]] [pp_view],
                      div
                        a::[a_class ["cell-status"]]
                        [
                          div a::[a_class ["result-label"]] [pcdata "Result: "],
                          div
                            a::[a_class ["type-indicator"]]
                            [
                              div a::[a_class ["type-label"]] [pcdata "Type: "],
                              div a::[a_class ["htype-view"]] [htype_view]
                            ]
                        ],
                      div a::[a_class ["result-view"]] [result_view]
                    ]
                ],
              div a::[a_class ["sidebar"]] [Action_palette.make_palette (rs, rf)]
            ]
          ]
      ]
    )
};
