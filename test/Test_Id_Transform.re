open Alcotest;
open Haz3lcore;

let test_reversible_transform = () => {
  /* Test that transform_variant followed by recover_original is the identity */
  let original_id = Id.mk();
  let variant_id = Id.transform_variant(original_id);
  let recovered_id = Id.recover_original(variant_id);

  check(
    bool,
    "Transformation is reversible for random UUID",
    true,
    Id.equal(original_id, recovered_id),
  );
};

let test_known_transformations = () => {
  let test_cases = [
    (
      "6ba7b810-9dad-11d1-80b4-00c04fd430c0",
      "6ba7b810-9dad-11d1-80b4-00c04fd430c1",
    ), /* 0->1 */
    (
      "6ba7b810-9dad-11d1-80b4-00c04fd430c9",
      "6ba7b810-9dad-11d1-80b4-00c04fd430c0",
    ), /* 9->0 */
    (
      "6ba7b810-9dad-11d1-80b4-00c04fd430ca",
      "6ba7b810-9dad-11d1-80b4-00c04fd430cb",
    ), /* a->b */
    (
      "6ba7b810-9dad-11d1-80b4-00c04fd430cf",
      "6ba7b810-9dad-11d1-80b4-00c04fd430ca",
    ) /* f->a */
  ];

  List.iter(
    ((original_str, expected_str)) => {
      switch (Id.of_string(original_str), Id.of_string(expected_str)) {
      | (Some(original_id), Some(expected_id)) =>
        let variant_id = Id.transform_variant(original_id);
        check(
          bool,
          Printf.sprintf("Transform %s -> %s", original_str, expected_str),
          true,
          Id.equal(variant_id, expected_id),
        );

        /* Test reverse transformation */
        let recovered_id = Id.recover_original(variant_id);
        check(
          bool,
          Printf.sprintf("Recover %s <- %s", original_str, expected_str),
          true,
          Id.equal(recovered_id, original_id),
        );
      | _ => fail("Invalid test case UUIDs")
      }
    },
    test_cases,
  );
};

let test_multiple_rounds = () => {
  /* Test that multiple rounds of transformation work correctly */
  let original_id = Id.mk();
  let variant1 = Id.transform_variant(original_id);
  let variant2 = Id.transform_variant(variant1);
  let variant3 = Id.transform_variant(variant2);

  let recovered3 = Id.recover_original(variant3);
  let recovered2 = Id.recover_original(recovered3);
  let recovered1 = Id.recover_original(recovered2);

  check(
    bool,
    "Multiple transformation rounds are reversible",
    true,
    Id.equal(original_id, recovered1),
  );
};

let tests = (
  "Id Transform",
  [
    test_case("reversible_transform", `Quick, test_reversible_transform),
    test_case("known_transformations", `Quick, test_known_transformations),
    test_case("multiple_rounds", `Quick, test_multiple_rounds),
  ],
);
