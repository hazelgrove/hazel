;; wasm_of_ocaml counterparts of basement_stubs.js: primitives that Jane
;; Street's basement.v0.18~preview declares with C stubs but whose runtime.wat
;; does not provide. See basement_stubs.js and #1131.
(module
   (type $block (array (mut (ref eq))))

   ;; Stdlib_shim.Domain.self_index: there is one domain.
   (func (export "basement_domain_self_index")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   ;; Subatomic.Loc: a loc is the pair (block, field index), and field i of a
   ;; block lives at array index i + 1 (index 0 is the tag).
   (func $loc_block (param $loc (ref eq)) (result (ref $block))
      (ref.cast (ref $block)
         (array.get $block (ref.cast (ref $block) (local.get $loc))
            (i32.const 1))))

   (func $loc_index (param $loc (ref eq)) (result i32)
      (i32.add
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (ref.cast (ref $block) (local.get $loc))
                  (i32.const 2))))
         (i32.const 1)))

   (func (export "caml_atomic_set_stub_loc")
      (param $loc (ref eq)) (param $v (ref eq)) (result (ref eq))
      (array.set $block (call $loc_block (local.get $loc))
         (call $loc_index (local.get $loc)) (local.get $v))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_compare_exchange_stub_loc")
      (param $loc (ref eq)) (param $o (ref eq)) (param $n (ref eq))
      (result (ref eq))
      (local $b (ref $block)) (local $i i32) (local $old (ref eq))
      (local.set $b (call $loc_block (local.get $loc)))
      (local.set $i (call $loc_index (local.get $loc)))
      (local.set $old (array.get $block (local.get $b) (local.get $i)))
      (if (ref.eq (local.get $old) (local.get $o))
         (then
            (array.set $block (local.get $b) (local.get $i) (local.get $n))))
      (local.get $old))

   (func $update_int
      (param $loc (ref eq)) (param $f i32) (param $v (ref eq)) (result (ref eq))
      ;; $f: 0 add, 1 sub, 2 and, 3 or, 4 xor
      (local $b (ref $block)) (local $i i32) (local $x i32) (local $y i32)
      (local.set $b (call $loc_block (local.get $loc)))
      (local.set $i (call $loc_index (local.get $loc)))
      (local.set $x
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $b) (local.get $i)))))
      (local.set $y (i31.get_s (ref.cast (ref i31) (local.get $v))))
      (array.set $block (local.get $b) (local.get $i)
         (ref.i31
            (if (result i32) (i32.eq (local.get $f) (i32.const 0))
               (then (i32.add (local.get $x) (local.get $y)))
               (else
                  (if (result i32) (i32.eq (local.get $f) (i32.const 1))
                     (then (i32.sub (local.get $x) (local.get $y)))
                     (else
                        (if (result i32) (i32.eq (local.get $f) (i32.const 2))
                           (then (i32.and (local.get $x) (local.get $y)))
                           (else
                              (if (result i32)
                                 (i32.eq (local.get $f) (i32.const 3))
                                 (then (i32.or (local.get $x) (local.get $y)))
                                 (else
                                    (i32.xor (local.get $x)
                                       (local.get $y))))))))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_add_stub_loc")
      (param $loc (ref eq)) (param $v (ref eq)) (result (ref eq))
      (call $update_int (local.get $loc) (i32.const 0) (local.get $v)))

   (func (export "caml_atomic_sub_stub_loc")
      (param $loc (ref eq)) (param $v (ref eq)) (result (ref eq))
      (call $update_int (local.get $loc) (i32.const 1) (local.get $v)))

   (func (export "caml_atomic_land_stub_loc")
      (param $loc (ref eq)) (param $v (ref eq)) (result (ref eq))
      (call $update_int (local.get $loc) (i32.const 2) (local.get $v)))

   (func (export "caml_atomic_lor_stub_loc")
      (param $loc (ref eq)) (param $v (ref eq)) (result (ref eq))
      (call $update_int (local.get $loc) (i32.const 3) (local.get $v)))

   (func (export "caml_atomic_lxor_stub_loc")
      (param $loc (ref eq)) (param $v (ref eq)) (result (ref eq))
      (call $update_int (local.get $loc) (i32.const 4) (local.get $v)))
)
