let nbsp = "\xC2\xA0"; // UTF-8 encoding for U+00A0 "No-break space"

// NOTE: 30% faster than Camomile
let length = (s: string): int => {
  let stop = String.length(s);
  let rec distance_aux = (start: int, count: int) =>
    if (start + count >= stop) {
      stop - count;
    } else {
      let n = Char.code(String.unsafe_get(s, start + count));
      if (n < 0x80) {
        distance_aux(start + 1, count);
      } else if (n < 0xe0) {
        distance_aux(start + 1, count + 1);
      } else if (n < 0xf0) {
        distance_aux(start + 1, count + 2);
      } else {
        distance_aux(start + 1, count + 3);
      };
    };

  distance_aux(0, 0);
};
