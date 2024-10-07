open Haz3lcore;
let format = (~inline: bool, s: string): string => {
  let seg =
    ExpToSegment.exp_to_segment(
      ~settings=ExpToSegment.Settings.of_core(~inline, CoreSettings.on),
      MakeTerm.from_zip_for_sem(Option.get(Printer.zipper_of_string(s))).
        term,
    );
  let zipper = Zipper.unzip(seg);
  Printer.zipper_to_string(~holes=Some("?"), zipper);
};
