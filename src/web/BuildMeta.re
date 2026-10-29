/* Human-readable identification of the running Hazel build, derived from
 * the generated BuildInfo module. */

let suffix =
  (BuildInfo.ahead ? "-ahead" : "") ++ (BuildInfo.dirty ? "-dirty" : "");

let label = BuildInfo.branch ++ "@" ++ BuildInfo.commit_short ++ suffix;

/* A "straight" dev build: on dev with no local commits or changes. */
let is_clean_dev =
  BuildInfo.branch == "dev" && !BuildInfo.dirty && !BuildInfo.ahead;
