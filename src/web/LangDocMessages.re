open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type update =
  | ToggleShow
  | ToggleHighlight
  | SpecificityOpen(bool);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  show: bool,
  highlight: bool,
  specificity_open: bool,
};

let init = {show: true, highlight: true, specificity_open: false};

let set_update = (docLangMessages: t, u: update): t => {
  switch (u) {
  | ToggleShow => {...docLangMessages, show: !docLangMessages.show}
  | ToggleHighlight => {
      ...docLangMessages,
      highlight: !docLangMessages.highlight,
    }
  | SpecificityOpen(b) => {...docLangMessages, specificity_open: b}
  };
};
