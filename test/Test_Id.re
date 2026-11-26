open Alcotest;
open Util;

/* Id.mk builds v4 UUIDs from a counter (lo: 30 bits, hi: 28 bits) and a
   per-minter random tail. These tests pin the byte layout to the one the
   pre-#2816 Id.mk produced, which filled a scratch buffer and handed it to
   Uuidm.v4. */

let bytes_of = (id: Id.t): string => Uuidm.to_bytes(id);
let byte = (id: Id.t, i: int): int => Char.to_int(bytes_of(id).[i]);

/* RFC 4122: version in the high nibble of byte 6, variant in the top two
   bits of byte 8 (10 for RFC 4122 UUIDs) */
let version = id => byte(id, 6) lsr 4;
let variant = id => byte(id, 8) lsr 6;

/* The counter as laid out in bytes 0 to 7 */
let weighted = (digits: list((int, int))): int =>
  List.sum((module Int), digits, ~f=((d, w)) => d * w);
let lo_of = id =>
  weighted([
    (byte(id, 0), 0x1),
    (byte(id, 1), 0x100),
    (byte(id, 2), 0x10000),
    (byte(id, 3) land 0x3f, 0x1000000),
  ]);
let hi_of = id =>
  weighted([
    (byte(id, 4), 0x1),
    (byte(id, 5), 0x100),
    (byte(id, 6) land 0x0f, 0x10000),
    (byte(id, 7), 0x100000),
  ]);
let tail_of = id => String.sub(bytes_of(id), ~pos=8, ~len=8);

/* The id the pre-#2816 Id.mk minted for counter (l, h) and random bytes
   tail (bytes 8 to 15; Uuidm.v4 sets the variant bits of byte 8). */
let old_layout = (~l: int, ~h: int, ~tail: string): Id.t => {
  let scratch = Stdlib.Bytes.make(16, '\000');
  let set = (i, v) => Stdlib.Bytes.set(scratch, i, Stdlib.Char.chr(v));
  set(0, l land 0xff);
  set(1, l lsr 8 land 0xff);
  set(2, l lsr 16 land 0xff);
  set(3, l lsr 24 land 0x3f);
  set(4, h land 0xff);
  set(5, h lsr 8 land 0xff);
  set(6, h lsr 16 land 0x0f);
  set(7, h lsr 20 land 0xff);
  for (i in 8 to 15) {
    Stdlib.Bytes.set(scratch, i, tail.[i - 8]);
  };
  Uuidm.v4(scratch);
};

let lo_limit = 1 lsl 30;

let id_t = testable(Id.pp, Id.equal);

/* Mint n ids from next and check each: v4 version and variant, the old
   byte layout for the expected counter, a tail shared by all, uniqueness,
   and the to_string/of_string round trip. (l, h) is the counter of the id
   minted before the first one. */
let check_run = (~l: int, ~h: int, ~n: int, next: unit => Id.t) => {
  let first = next();
  let tail = tail_of(first);
  let seen = Hash_set.create((module String));
  let rec go = (k, l, h, id) => {
    let (l, h) = l + 1 >= lo_limit ? (0, h + 1) : (l + 1, h);
    let s = Id.to_string(id);
    check(int, "version of " ++ s, 4, version(id));
    check(int, "variant of " ++ s, 0b10, variant(id));
    check(int, "lo of " ++ s, l, lo_of(id));
    check(int, "hi of " ++ s, h, hi_of(id));
    check(id_t, "old layout of " ++ s, old_layout(~l, ~h, ~tail), id);
    check(
      id_t,
      "already v4: " ++ s,
      Uuidm.v4(Stdlib.Bytes.of_string(bytes_of(id))),
      id,
    );
    check(bool, "unique: " ++ s, false, Hash_set.mem(seen, bytes_of(id)));
    Hash_set.add(seen, bytes_of(id));
    check(option(id_t), "round trip of " ++ s, Some(id), Id.of_string(s));
    if (k < n) {
      go(k + 1, l, h, next());
    };
  };
  go(1, l, h, first);
};

let tests = (
  "Id",
  [
    test_case(
      "Id.mk mints v4 ids in the old layout, across suffix runs",
      `Quick,
      () => {
        /* other tests have minted ids already: start from wherever mk is */
        let id = Id.mk();
        check_run(~l=lo_of(id), ~h=hi_of(id), ~n=1000, Id.mk);
      },
    ),
    test_case(
      "the counter's low word wraps into its high word",
      `Quick,
      () => {
        let l = lo_limit - 300;
        /* hi bits in byte 6's low nibble and byte 7 */
        List.iter([0, 0x123456, 0x0fffff, 0xfffffff - 1], ~f=h =>
          check_run(~l, ~h, ~n=600, Id.minter(~lo=l, ~hi=h, ()))
        );
      },
    ),
    test_case(
      "minters started at the same counter mint distinct ids",
      `Quick,
      () => {
        let a = Id.minter();
        let b = Id.minter();
        let ids = List.init(512, ~f=_ => a()) @ List.init(512, ~f=_ => b());
        let distinct =
          List.dedup_and_sort(ids, ~compare=Id.compare) |> List.length;
        check(int, "distinct", 1024, distinct);
      },
    ),
  ],
);
