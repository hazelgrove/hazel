let nbsp = "\xC2\xA0"; // UTF-8 encoding for U+00A0 "No-break space"

// NOTE: 30% faster than Camomile
let length = (s: string): int => {
  Util.StringUtil.length(s);
};
