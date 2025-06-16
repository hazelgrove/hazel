open Util;

let format_time_diff = (prior: float): string => {
  let now = JsUtil.timestamp();
  let diff_seconds = (now -. prior) /. 1000.0;
  let diff_mins = floor(diff_seconds /. 60.0);
  let diff_hours = floor(diff_mins /. 60.0);
  let diff_days = floor(diff_hours /. 24.0);

  if (diff_mins < 1.0) {
    Printf.sprintf("<1 min ago");
  } else if (diff_mins < 60.0) {
    diff_mins < 2.0
      ? Printf.sprintf("%.0f min ago", diff_mins)
      : Printf.sprintf("%.0f mins ago", diff_mins);
  } else if (diff_hours < 24.0) {
    diff_hours < 2.0
      ? Printf.sprintf("%.0f hour ago", diff_hours)
      : Printf.sprintf("%.0f hours ago", diff_hours);
  } else {
    diff_days < 2.0
      ? Printf.sprintf("%.0f day ago", diff_days)
      : Printf.sprintf("%.0f days ago", diff_days);
  };
};
