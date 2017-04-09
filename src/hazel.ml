open Tyxml_js
open Hazel_semantics
open Hazel_model
open Hazel_model.Model
open Hazel_view

open React

module Ev = Dom_html.Event

module Util = struct
  let force_opt opt = match opt with Some x -> x | _ -> assert false
end

module JSUtil = struct
  let forceGetElementById id = 
    let doc = Dom_html.document in  
    Js.Opt.get (doc##getElementById (Js.string id))
      (fun () -> assert false)

  let listen_to ev elem f =
    Dom_html.addEventListener
      elem
      ev
      (Dom_html.handler f)
      Js._false

  let listen_to_t ev elem f =
    listen_to ev elem (fun evt ->
        f evt; Js._true)

  (* create an input and a reactive signal tracking its
   * string value *)
  let r_input id placeholder_str =
    let rs, rf = S.create "" in
    let i_elt = Html5.(input ~a:[
        a_id id; 
        a_class ["form-control"]; 
        a_placeholder placeholder_str] ()) in
    let i_dom = To_dom.of_input i_elt in
    let _ = listen_to_t Ev.input i_dom (fun _ ->
        rf (Js.to_string (i_dom##.value))) in
    ((rs, rf), i_elt, i_dom)

  module KeyCombo : 
  sig
    type t
    val make : string -> int -> t
    val to_string : t -> string
    val keyCode : t -> int
  end
  = struct
    type t = string * int
    let make str keyCode = (str, keyCode)
    let keyCode (str, keyCode) = keyCode
    let to_string (str, keyCode) = str
  end

  module KeyCombos = struct
    let _kc = KeyCombo.make

    let enter = _kc "Enter" 13
    let esc = _kc "Esc" 27
    let number_1 = _kc "1" 49
    let number_2 = _kc "2" 50
    let number_3 = _kc "3" 51
    let p = _kc "p" 112
    let x = _kc "x" 120
    let greaterThan = _kc ">" 62 
    let n = _kc "n" 110 
    let s = _kc "s" 115 
    let dot = _kc "." 46 
    let colon = _kc ":" 58 
    let v = _kc "v" 118 
    let backslash = _kc "\\" 92 
    let openParens = _kc "(" 40 
    let pound = _kc "#" 35 
    let plus = _kc "+" 43 
    let l = _kc "l" 108 
    let r = _kc "r" 114 
    let c = _kc "c" 99 
    let qmark = _kc "?" 63
    let equals = _kc "=" 61
  end

  let get_keyCode (evt : Dom_html.keyboardEvent Js.t) = 
    Js.Optdef.get (evt##.which) (fun () -> assert false)
end

(* generates the action palette *)
module ActionPalette = struct
  let make_palette ((rs, rf) : Model.rp) =
    (* start by defining a bunch of helpers *)

    (* performs the top-level action and updates the signal *)
    let doAction action =
      rf (Action.performSyn Ctx.empty action (React.S.value rs)) in

    let module KC = JSUtil.KeyCombo in 
    let module KCs = JSUtil.KeyCombos in 

    (* helper function for constructing action buttons with no textbox *)
    let action_button action btn_label key_combo =
      let _ = JSUtil.listen_to_t Ev.keypress Dom_html.document (fun evt ->
          if JSUtil.get_keyCode evt = (KC.keyCode key_combo) then
            try
              doAction action
            with Action.InvalidAction -> ()
          else ()) in
      Html5.(button ~a:[a_class ["btn";"btn-outline-primary"];
                        a_onclick (fun _ ->
                            doAction action;
                            true);
                        R.filter_attrib
                          (a_disabled ())
                          (S.map (fun m ->
                               try
                                 let _ = Action.performSyn Ctx.empty action m in false
                               with Action.InvalidAction -> true
                             ) rs)
                       ] [
               pcdata (btn_label ^ " [" ^ (KC.to_string key_combo) ^ "]")]) in

    (* actions that take an input. the conversion function
     * goes from a string (the input value) to an arg option 
     * where arg is the action argument. *)
    let action_input_button action conv btn_label input_id key_combo placeholder_str = 
      (* create reactive input box *)
      let (i_rs, i_rf), i_elt, i_dom = JSUtil.r_input input_id placeholder_str in
      let clear_input () =
        i_dom##.value := (Js.string "");
        i_rf "" (* need to manually update rf because r_input only reacts to UI input *) in
      let button_elt = Html5.(button ~a:[
          a_class ["btn"; "btn-default"];
          a_id (input_id ^ "_button");
          a_onclick (fun _ ->
              let arg = Util.force_opt (conv (React.S.value i_rs)) in
              doAction (action arg);
              clear_input ();
              true
            );
          R.filter_attrib (* filters out the disabled attribute *)
            (a_disabled ())
            (S.l2 (fun s m -> (* S.l2 creates a signal from two signals *)
                 match conv s with
                   Some arg ->
                   begin try
                       let _ = Action.performSyn Ctx.empty (action arg) m in false
                     with Action.InvalidAction -> true (* filter disbled attr out if invalid action *)
                   end
                 | _ -> true) i_rs rs)
        ] [
          pcdata (btn_label ^ " [" ^ (KC.to_string key_combo) ^ "]")]) in
      let button_dom = To_dom.of_button button_elt in
      (* listen for the key combo at the document level *)
      let _ = JSUtil.listen_to Ev.keypress Dom_html.document (fun evt ->
          let evt_key = JSUtil.get_keyCode evt in
          (* let _ = Firebug.console##log evt_key in *) 
          if evt_key = KC.keyCode key_combo then
            begin
              i_dom##focus;
              Dom_html.stopPropagation evt;
              Js._false
            end
          else
            Js._true
        ) in
      (* respond to enter and esc inside the input box *)
      let _ = JSUtil.listen_to Ev.keyup i_dom (fun evt ->
          let evt_key = JSUtil.get_keyCode evt in
          if evt_key = (KC.keyCode KCs.enter) then 
            begin
              button_dom##click;
              i_dom##blur;
              Js._false
            end
          else if evt_key = (KC.keyCode KCs.esc) then 
            begin
              i_dom##blur;
              Js._false
            end
          else Js._true) in 
      (* stop propagation of keys when focus is in input box *)
      let _ = JSUtil.listen_to Ev.keypress i_dom (fun evt ->
          Dom_html.stopPropagation evt; Js._true) in
      Html5.(div ~a:[a_class ["input-group"]] [
          span ~a:[a_class ["input-group-btn"]] [
            button_elt];i_elt;
        ]) in

    (* actions that take two inputs. the conversion function
     * goes from a pair of strings to an arg option where arg is
     * the action argument. *)
    let action_input_input_button action conv btn_label input_id key_combo placeholder_str_1 placeholder_str_2 =
      (* analagous to action_input_button, but with two input boxes. 
       * could define an n-ary version of this, but this is probably more clear for now *)
      let input_id_1 = (input_id ^ "_1") in
      let input_id_2 = (input_id ^ "_2") in
      let (i_rs_1, i_rf_1), i_elt_1, i_dom_1 = JSUtil.r_input input_id_1 placeholder_str_1 in
      let (i_rs_2, i_rf_2), i_elt_2, i_dom_2 = JSUtil.r_input input_id_2 placeholder_str_2 in
      let clear_input () =
        i_dom_1##.value := (Js.string ""); i_rf_1 "";
        i_dom_2##.value := (Js.string ""); i_rf_2 "" in
      let button_elt = Html5.(button ~a:[
          a_class ["btn"; "btn-default"];
          a_id (input_id ^ "_button");
          a_onclick (fun _ ->
              let i1 = React.S.value i_rs_1 in
              let i2 = React.S.value i_rs_2 in
              let arg = Util.force_opt (conv (i1, i2)) in
              doAction (action arg);
              clear_input ();
              true
            );
          R.filter_attrib
            (a_disabled ())
            (S.l3 (fun s1 s2 m ->
                 match conv (s1, s2) with
                   Some arg ->
                   begin try
                       let _ = Action.performSyn Ctx.empty (action arg) m in false
                     with Action.InvalidAction -> true
                   end
                 | _ -> true) i_rs_1 i_rs_2 rs)
        ] [
          pcdata (btn_label ^ " [" ^ (KC.to_string key_combo) ^ "]")]) in
      let button_dom = To_dom.of_button button_elt in
      let _ = JSUtil.listen_to Ev.keypress Dom_html.document (fun evt ->
          let evt_key = JSUtil.get_keyCode evt in
          if evt_key = (KC.keyCode key_combo) then
            begin
              Firebug.console##log "in c";  
              i_dom_1##focus;
              Dom_html.stopPropagation evt;
              Js._false
            end
          else
            Js._true
        ) in
      let i_keyup_listener i_dom = JSUtil.listen_to Ev.keyup i_dom (fun evt ->
          let evt_key = JSUtil.get_keyCode evt in
          if evt_key = (KC.keyCode KCs.enter) then 
            begin
              button_dom##click;
              i_dom##blur;
              Js._false
            end
          else if evt_key = (KC.keyCode KCs.esc) then 
            begin
              i_dom##blur;
              Js._false
            end
          else 
            begin
              Dom_html.stopPropagation evt; Js._true
            end
        ) in
      let _ = i_keyup_listener i_dom_1 in
      let _ = i_keyup_listener i_dom_2 in
      let i_keypress_listener i_dom = 
        JSUtil.listen_to Ev.keypress i_dom (fun evt ->
            Dom_html.stopPropagation evt; Js._true) in
      let _ = i_keypress_listener i_dom_1 in 
      let _ = i_keypress_listener i_dom_2 in 
      Html5.(div ~a:[a_class ["input-group"]] [
          span ~a:[a_class ["input-group-btn"]] [
            button_elt];
          i_elt_1;
          i_elt_2;
        ]) in

    (* now construct the action palette entries*)
    (* movement *)
    let moveChild1 = action_button (Action.Move (Action.Child 1)) "move child 1" KCs.number_1 in 
    let moveChild2 = action_button (Action.Move (Action.Child 2)) "move child 2" KCs.number_2 in 
    let moveChild3 = action_button (Action.Move (Action.Child 3)) "move child 3" KCs.number_3 in 
    let moveParent = action_button (Action.Move (Action.Parent)) "move parent" KCs.p in 

    (* deletion *)
    let delete = action_button (Action.Del) "del" KCs.x in 

    (* type construction *)
    let constructArrow = action_button (Action.Construct Action.SArrow) "construct arrow" KCs.greaterThan in 
    let constructNum = action_button (Action.Construct Action.SNum) "construct num" KCs.n in 
    let constructSum = action_button (Action.Construct Action.SSum) "construct sum" KCs.s in 

    (* finishing *)
    let finish = action_button (Action.Finish) "finish" KCs.dot in 

    (* expression construction *)
    let constructAsc = action_button (Action.Construct Action.SAsc) "construct asc" KCs.colon in 
    let constructLet = action_input_button 
        (fun v -> Action.Construct (Action.SLet v))
        (fun s -> match String.compare s "" with 0 -> None | _ -> Some s)
        "construct let" "var_input" KCs.equals "Enter var + press Enter" in 
    let constructVar = action_input_button
        (fun v -> Action.Construct (Action.SVar v))
        (fun s -> match String.compare s "" with 0 -> None | _ -> Some s)
        "construct var" "var_input" KCs.v "Enter var + press Enter" in 
    let constructLam = action_input_button
        (fun v -> Action.Construct (Action.SLam v))
        (fun s -> match String.compare s "" with 0 -> None | _ -> Some s)
        "construct lam" "lam_input" KCs.backslash "Enter var + press Enter" in 
    let constructAp = action_button (Action.Construct Action.SAp) "construct ap" KCs.openParens in 
    let constructLit = action_input_button
        (fun n -> Action.Construct (Action.SLit n))
        (fun s ->
           try
             let i = int_of_string s in
             if i < 0 then None else Some i
           with Failure _ -> None)
        "construct lit" "lit_input" KCs.pound "Enter num + press Enter" in 
    let constructPlus = action_button (Action.Construct Action.SPlus) "construct plus" KCs.plus in 
    let constructInjL = action_button (Action.Construct (Action.SInj HExp.L)) "construct inj L" KCs.l in 
    let constructInjR = action_button (Action.Construct (Action.SInj HExp.R)) "construct inj R" KCs.r in 
    let constructCase = action_input_input_button
        (fun (v1,v2) -> Action.Construct (Action.SCase (v1,v2)))
        (fun (s1, s2) ->
           let s1_empty = String.compare s1 "" in
           let s2_empty = String.compare s2 "" in
           match s1_empty, s2_empty with
           | 0, _ -> None
           | _, 0 -> None
           | _ -> Some (s1, s2))
        "construct case" "case_input" KCs.c 
        "Enter var + press Tab"
        "Enter var + press Enter" in 
    let constructNEHole = action_button (Action.Construct Action.SNEHole) "construct neHole" KCs.qmark in 

    (* finally, put it all together into the action palette *)
    Html5.(div ~a:[a_class ["row";"marketing"]] [
        div ~a:[a_class ["col-lg-3"; "col-md-3"; "col-sm-3"]] [
          div ~a:[a_class ["panel";"panel-default"]] [
            div ~a:[a_class ["panel-title"]] [pcdata "Movement"];
            div ~a:[a_class ["panel-body"]] [
              (moveChild1);
              br ();
              (moveChild2);
              br ();
              (moveChild3);
              br ();
              (moveParent);
            ]
          ];
          div ~a:[a_class ["panel";"panel-default"]] [
            div ~a:[a_class ["panel-title"]] [pcdata "Deletion"];
            div ~a:[a_class ["panel-body"]] [
              (delete);
            ]
          ]
        ];
        div ~a:[a_class ["col-lg-3"; "col-md-3"; "col-sm-3"]] [
          div ~a:[a_class ["panel";"panel-default"]] [
            div ~a:[a_class ["panel-title"]] [pcdata "Type Construction"];
            div ~a:[a_class ["panel-body"]] [
              (constructArrow);
              br ();
              (constructNum);
              br ();
              (constructSum);
              br ();]
          ];
          div ~a:[a_class ["panel";"panel-default"]] [
            div ~a:[a_class ["panel-title"]] [pcdata "Finishing"];
            div ~a:[a_class ["panel-body"]] [
              (finish)
            ]
          ]
        ]
        ;
        div ~a:[a_class ["col-lg-6"; "col-md-6"; "col-sm-6"]] [
          div ~a:[a_class ["panel";"panel-default"]] [
            div ~a:[a_class ["panel-title"]] [pcdata "Expression Construction"];
            div ~a:[a_class ["panel-body"]] [
              (constructAsc);
              br ();
              (constructLet);
              br ();
              (constructVar);
              (constructLam);
              (constructAp);
              br ();
              (constructLit);
              (constructPlus);
              br ();
              (constructInjL);
              br ();
              (constructInjR);
              br ();
              (constructCase);
              (constructNEHole)
            ]
          ]
        ]
      ])
end

(* generates the view for the whole application *)
module AppView = struct
  let view ((rs, rf) : Model.rp) =
    (* pp view *) 
    let pp_view_width = 30 in 
    let pp_rs = React.S.map (fun (zexp, _) -> 
        let prettified = Pretty.HTML_Of_SDoc.html_of_sdoc (
            Pretty.PP.sdoc_of_doc pp_view_width (PPView.of_zexp zexp)) in 
        [prettified]) rs in 
    let pp_view = (R.Html5.div (ReactiveData.RList.from_signal pp_rs)) in 

    (* htype view *)
    let htype_rs = React.S.map (fun (_, htype) -> 
        let pp_view = PPView.of_htype htype in 
        let sdoc = Pretty.PP.sdoc_of_doc pp_view_width pp_view in 
        let prettified = Pretty.HTML_Of_SDoc.html_of_sdoc sdoc in 
        [prettified]) rs in 
    let htype_view = (R.Html5.div (ReactiveData.RList.from_signal htype_rs)) in 

    Tyxml_js.To_dom.of_div Html5.(
        div [
          div ~a:[a_class ["jumbotron"]] [
            div ~a:[a_class ["headerTextAndLogo"]] [
              div ~a:[a_class ["display-3"]] [pcdata "Hazel"];
              div ~a:[a_class ["logoDiv"]] [
                img ~a:[a_id "logo"] ~alt:("Logo") ~src:(Xml.uri_of_string ("imgs/hazel-logo.png")) ()
              ]
            ]; 
            div ~a:[a_class ["subtext"]] [
              pcdata "(a structure editor rooted in the principles of type theory)"]; br (); 

            div ~a:[a_class ["typeLbl"]] [pcdata 
                                            ("Pretty-printed (width=" ^ (string_of_int pp_view_width) ^ "):")]; 
            div ~a:[a_class ["ModelExp"]] [pp_view]; br ();
            div ~a:[a_class ["subtext"; "ModelType"]] [
              div ~a:[a_class ["typeLbl"]] [pcdata "Synthesizes H-type: "];
              htype_view]];
          ActionPalette.make_palette (rs, rf);
          div ~a:[a_class ["container"]; a_id "footerContainer"] [
            p [
              pcdata "Source (OCaml): "; 
              a ~a:[a_href "https://github.com/hazelgrove/hazel"] [
                pcdata "https://github.com/hazelgrove/hazel"
              ]];
            p [
              pcdata "A project of "; 
              a ~a:[a_href "http://hazelgrove.org"] [
                pcdata "Hazel Grove"
              ];
              pcdata "."]
          ]
        ])
end

(* execution starts here *)
let _ = JSUtil.listen_to_t 
    Dom_html.Event.domContentLoaded
    Dom_html.document
    (fun _ -> 
       let rs, rf = React.S.create (Model.empty) in
       let parent = JSUtil.forceGetElementById "container" in 
       Dom.appendChild parent (AppView.view (rs, rf)))



