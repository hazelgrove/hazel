module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

/* janky user action logging */

let array_empty: Js.constr(Js.t(Js.js_array(Js.t(Js.js_string)))) = Js.array_empty;
let action_log = {
  %js
  new array_empty;
};
let action_log_global = "action_log";
Js.Unsafe.set(Dom_html.window, action_log_global, action_log);

let append = (s: string) => {
  let _ = action_log##push(Js.string(s));
  ();
};

/* robust system event logging */

let reporter = ppf => {
  let report = (src, level, ~over, k, msgf) => {
    let k = _ => {
      over();
      k();
    };
    let timestamped = (h, tags, k, ppf, fmt) => {
      let ts =
        {
          open OptUtil.Syntax;
          let* tags = tags;
          Logs.Tag.find(Log.timestamp_tag, tags);
        }
        |> Option.value(~default=0.0);
      Format.kfprintf(
        k,
        ppf,
        "%a %+04.0f %s @[" ^^ fmt ^^ "@]@.",
        Logs.pp_header,
        (level, h),
        ts,
        Logs.Src.name(src),
      );
    };
    msgf((~header=?, ~tags=?, fmt) =>
      timestamped(header, tags, k, ppf, fmt)
    );
  };
  {Logs.report: report};
};

let init_log = () => {
  /* Logs.set_reporter(Logs_browser.console_reporter()); */
  Logs.set_reporter(reporter(Format.std_formatter));
  Logs.set_level(Some(Logs.Debug));
};
