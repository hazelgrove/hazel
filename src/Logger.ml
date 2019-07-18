module Js = Js_of_ocaml.Js
module Dom_html = Js_of_ocaml.Dom_html


let array_empty : (Js.js_string Js.t) Js.js_array Js.t Js.constr = Js.array_empty 
let action_log = new%js array_empty
let action_log_global = "action_log"
let _ = Js.Unsafe.set
          Dom_html.window
          action_log_global
          action_log

let init_log () = ()

let append (s : string) = 
  let _ = action_log##push(Js.string(s)) in  
  ()
