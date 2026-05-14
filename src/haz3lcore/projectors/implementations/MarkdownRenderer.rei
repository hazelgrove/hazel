/* MarkdownRenderer - Render a string sample value as rendered Markdown. */
[@deriving (show({with_path: false}), sexp, yojson)]
type m = {raw: bool};
[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | ToggleRaw;
type v = string;

include
  RichProbe.RichProbe with
    type model = m and type action = a and type value = v;
