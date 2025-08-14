open Haz3lcore;
open Language;
let content = [%blob "Datasheet.md"];

let content: string = content |> Util.StringUtil.escape_linebreaks;
let string_exp = IdTagged.FreshGrammar.Exp.string(content);
let segment =
  ProjectorInit.init(
    TextArea,
    Segment.parenthesize(
      ExpToSegment.exp_to_segment(
        ~settings=ExpToSegment.Settings.editable(~inline=true),
        string_exp,
      ),
    ),
    Exp(string_exp),
  )
  |> Option.get;
let slide = (
  "[B2T2] Datasheet",
  PersistentZipper.persist({
    selection: Selection.mk([]),
    relatives: {
      siblings: ([], [segment]),
      ancestors: [],
    },
    caret: Outer,
  }),
);
