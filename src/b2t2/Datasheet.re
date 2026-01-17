open Haz3lcore;
open Language;
let content = [%blob "Datasheet.md"];

let content: string = content |> Util.StringUtil.escape_linebreaks;
let string_exp = IdTagged.FreshGrammar.Exp.string(content);
let z =
  ProjectorInit.init(
    TextArea,
    Segment.parenthesize(
      ExpToSegment.exp_to_segment(
        ~settings=ExpToSegment.Settings.editable(~inline=Compound),
        string_exp,
      ),
    ),
    Exp(string_exp),
  )
  |> Option.get
  |> (p => [p])
  |> Zipper.unzip;
let slide = ("B2T2 / Datasheet", PersistentSegment.persist(z));
