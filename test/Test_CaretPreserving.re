open Alcotest;
open Haz3lcore;

/* CaretPreserving.transform must round-trip caret AND selection under
   an identity transform — including selections that abut the buffer
   end, where there is no right neighbor to track (formatting used to
   drop the selection and park the caret at buffer start). */

let roundtrip = (~name, init) =>
  test_case(
    name,
    `Quick,
    () => {
      let z = Test_Editing.mk_zipper(init);
      let z = CaretPreserving.transform(z, Fun.id);
      check(string, name, init, Test_Editing.printer(z));
    },
  );

let tests = [
  (
    "CaretPreserving: identity round-trips",
    [
      roundtrip(~name="caret only", "1 + ¦2"),
      roundtrip(~name="token selected at EOF, focus left", "1 + ¦2§"),
      roundtrip(~name="token selected at EOF, focus right", "1 + §2¦"),
      roundtrip(~name="char selected at EOF", "ab¦c§"),
      roundtrip(~name="selection mid-buffer", "1 ¦+§ 2"),
      roundtrip(~name="selection at buffer start", "¦1§ + 2"),
      roundtrip(~name="whole buffer selected", "§1 + 2¦"),
    ],
  ),
];
