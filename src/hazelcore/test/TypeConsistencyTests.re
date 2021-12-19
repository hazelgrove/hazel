let%test_module "finite-finite sum type consistency tests" =
  (module
   {
     let void = HTyp.Sum(Finite(TagMap.empty));

     let%test "+{} ~ +{}" = HTyp.consistent(void, void);

     let singleton = (tag, ty) =>
       HTyp.Sum(Finite(TagMap.singleton(tag, ty)));

     let of_list = bindings => HTyp.Sum(Finite(TagMap.of_list(bindings)));

     let empty_tag_hole = u => UHTag.EmptyTagHole(u);

     let const_tag = t => UHTag.Tag(NotInTagHole, t);

     let%test "+{} !~ +{?}" =
       !HTyp.consistent(void, singleton(empty_tag_hole(1), None));

     let%test "+{?} !~ +{}" =
       !HTyp.consistent(singleton(empty_tag_hole(1), None), void);

     let%test "+{?} ~ +{?}" =
       HTyp.consistent(
         singleton(empty_tag_hole(1), None),
         singleton(empty_tag_hole(2), None),
       );

     let%test "+{?} ~ +{A}" =
       HTyp.consistent(
         singleton(empty_tag_hole(1), None),
         singleton(const_tag("A"), None),
       );

     let%test "+{A} ~ +{?}" =
       HTyp.consistent(
         singleton(const_tag("A"), None),
         singleton(empty_tag_hole(1), None),
       );

     let%test "+{A} ~ +{A}" =
       HTyp.consistent(
         singleton(const_tag("A"), None),
         singleton(const_tag("A"), None),
       );

     let%test "+{A} !~ +{B}" =
       !
         HTyp.consistent(
           singleton(const_tag("A"), None),
           singleton(const_tag("B"), None),
         );

     let%test "+{A, ?} ~ +{?, B}" =
       HTyp.consistent(
         of_list([(const_tag("A"), None), (empty_tag_hole(1), None)]),
         of_list([(empty_tag_hole(2), None), (const_tag("B"), None)]),
       );

     let%test "+{A(?)} ~ +{?(Int)}" =
       HTyp.consistent(
         singleton(const_tag("A"), Some(Hole)),
         singleton(empty_tag_hole(1), Some(Int)),
       );

     let%test "+{A(?), B} !~ +{?(Int), C}" =
       !
         HTyp.consistent(
           of_list([
             (const_tag("A"), Some(Hole)),
             (const_tag("B"), None),
           ]),
           of_list([
             (empty_tag_hole(1), Some(Int)),
             (const_tag("C"), None),
           ]),
         );

     let%test "+{A(?), ?(Int), C} ~ +{C, B(?), ?(Bool)}" =
       HTyp.consistent(
         of_list([
           (const_tag("A"), Some(Hole)),
           (empty_tag_hole(1), Some(Int)),
           (const_tag("C"), None),
         ]),
         of_list([
           (const_tag("C"), None),
           (const_tag("B"), Some(Hole)),
           (empty_tag_hole(1), Some(Bool)),
         ]),
       );
   });
