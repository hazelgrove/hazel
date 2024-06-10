open Js_of_ocaml;

let options = (schedule_action: UpdateAction.t => unit) => [|
  [%js
    {
      val id = "Go To Definition";
      val title = "Go to Definition";
      val mdIcon = "arrow_forward";
      val handler =
        () => {
          schedule_action(PerformAction(Jump(BindingSiteOfIndicatedVar)));
        }
    }
  ],
  [%js
    {
      val id = "Proceed To Next Hole";
      val title = "Proceed To Next Hole";
      val mdIcon = "arrow_forward";
      val handler =
        () => {
          schedule_action(PerformAction(MoveToNextHole(Right)));
        }
    }
  ],
  [%js
    {
      val id = "Undo";
      val title = "Undo";
      val mdIcon = "undo";
      val handler =
        () => {
          schedule_action(Undo);
        }
    }
  ],
  [%js
    {
      val id = "Redo";
      val title = "Redo";
      val mdIcon = "redo";
      val handler =
        () => {
          schedule_action(Redo);
        }
    }
  ],
  [%js
    {
      val id = "Reparse Current Editor";
      val title = "Reparse Current Editor";
      val mdIcon = "refresh";
      val handler =
        () => {
          schedule_action(ReparseCurrentEditor);
        }
    }
  ],
|];

let elem = () => JsUtil.get_elem_by_id("ninja-keys");

let initialize = opts => Js.Unsafe.set(elem(), "data", Js.array(opts));
