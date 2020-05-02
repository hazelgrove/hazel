let nbsp = "\xC2\xA0"; // UTF-8 encoding for U+00A0 "No-break space"

let length: string => int = {
  let folder = (len: int, _: int, _): int => {
    len + 1;
  };
  Uutf.String.fold_utf_8(folder, 0);
};
