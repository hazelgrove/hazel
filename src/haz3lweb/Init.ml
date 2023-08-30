let startup : PersistentData.t =
  {
    settings =
      {
        captions = true;
        secondary_icons = false;
        statics = true;
        dynamics = true;
        async_evaluation = false;
        context_inspector = false;
        instructor_mode = true;
        benchmark = false;
        mode = Examples;
      };
    scratch =
      ( 0,
        [
          ( 1418,
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(()((Grout((id \
                 1417)(shape Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( 287,
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(()((Grout((id \
                 286)(shape Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( 18902,
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(()((Grout((id \
                 18901)(shape Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( 553,
            {
              zipper =
                "((selection((focus \
                 Right)(content())))(backpack())(relatives((siblings(()((Grout((id \
                 552)(shape Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( 811,
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(()((Grout((id \
                 810)(shape Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( 4,
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(()((Grout((id \
                 0)(shape Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( 4,
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(()((Grout((id \
                 0)(shape Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( 4,
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
                 2)(content(Whitespace\" \"))))(Secondary((id \
                 3)(content(Whitespace\" \")))))((Grout((id 0)(shape \
                 Convex))))))(ancestors())))(caret Outer))";
              backup_text = "   ";
            } );
        ] );
    examples =
      ( "Introduction",
        [
          ( "Types & static errors",
            ( 25702,
              {
                zipper =
                  "((selection((focus \
                   Left)(content())))(backpack())(relatives((siblings(()((Secondary((id \
                   25611)(content(Comment\"# Internal Regression Tests: Type \
                   errors #\"))))(Secondary((id \
                   25454)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   25698)(content(Comment\"# Each line should show errors or \
                   not as indicated #\"))))(Secondary((id \
                   23733)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   23734)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   23738)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25393)(content(Whitespace\" \"))))(Tile((id \
                   25396)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   23739)(content(Whitespace\" \")))))((Secondary((id \
                   23744)(content(Whitespace\" \"))))(Tile((id \
                   23751)(label(unbound))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   23754)(content(Whitespace\" \")))))))))(Secondary((id \
                   23756)(content(Whitespace\" \"))))(Secondary((id \
                   23760)(content(Comment #err#))))(Secondary((id \
                   23761)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   23765)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   23766)(content(Whitespace\" \"))))(Tile((id \
                   23776)(label(Undefined))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   23777)(content(Whitespace\" \")))))((Secondary((id \
                   23779)(content(Whitespace\" \"))))(Tile((id \
                   23788)(label(Undefined))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   23791)(content(Whitespace\" \")))))))))(Secondary((id \
                   23793)(content(Whitespace\" \"))))(Secondary((id \
                   23801)(content(Comment\"# 2x err#\"))))(Secondary((id \
                   23802)(content(Whitespace\" \"))))(Secondary((id \
                   23803)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   23807)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   23808)(content(Whitespace\" \"))))(Tile((id \
                   23813)(label(true))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   23814)(content(Whitespace\" \")))))((Secondary((id \
                   23816)(content(Whitespace\" \"))))(Tile((id \
                   23817)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   23820)(content(Whitespace\" \")))))))))(Secondary((id \
                   23822)(content(Whitespace\" \"))))(Secondary((id \
                   23826)(content(Comment #err#))))(Secondary((id \
                   23827)(content(Whitespace\" \"))))(Secondary((id \
                   23828)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   23829)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   23833)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25453)(content(Whitespace\" \"))))(Grout((id 23838)(shape \
                   Convex)))(Secondary((id 23834)(content(Whitespace\" \
                   \")))))((Secondary((id 23839)(content(Whitespace\" \
                   \"))))(Tile((id 23842)(label(if then else))(mold((out \
                   Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                   2))(children(((Secondary((id 23843)(content(Whitespace\" \
                   \"))))(Tile((id 23848)(label(true))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   23851)(content(Whitespace\" \")))))((Secondary((id \
                   23855)(content(Whitespace\" \"))))(Tile((id \
                   23856)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   23859)(content(Whitespace\" \")))))))))(Secondary((id \
                   23863)(content(Whitespace\" \"))))(Tile((id \
                   23865)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   23868)(content(Whitespace\" \")))))))))(Secondary((id \
                   23870)(content(Whitespace\" \"))))(Secondary((id \
                   23874)(content(Comment #err#))))(Secondary((id \
                   23875)(content(Whitespace\" \"))))(Secondary((id \
                   23876)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   23880)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   23881)(content(Whitespace\" \"))))(Tile((id \
                   23883)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   23884)(content(Whitespace\" \")))))((Secondary((id \
                   23886)(content(Whitespace\" \"))))(Tile((id 23889)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   23890)(content(Whitespace\" \"))))(Tile((id \
                   23895)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   23898)(content(Whitespace\" \")))))((Secondary((id \
                   23902)(content(Whitespace\" \"))))(Tile((id \
                   23903)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   23906)(content(Whitespace\" \")))))))))(Secondary((id \
                   23910)(content(Whitespace\" \"))))(Tile((id \
                   23912)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   23915)(content(Whitespace\" \")))))))))(Secondary((id \
                   23917)(content(Whitespace\" \"))))(Secondary((id \
                   23921)(content(Comment #err#))))(Secondary((id \
                   23922)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   23926)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   23927)(content(Whitespace\" \"))))(Tile((id \
                   23929)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   23930)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   25452)(content(Whitespace\" \"))))(Grout((id 23935)(shape \
                   Convex)))(Secondary((id 23932)(content(Whitespace\" \
                   \")))))((Secondary((id 23936)(content(Whitespace\" \
                   \"))))(Tile((id 23939)(label(if then else))(mold((out \
                   Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                   2))(children(((Secondary((id 23940)(content(Whitespace\" \
                   \"))))(Tile((id 23945)(label(true))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   23948)(content(Whitespace\" \")))))((Secondary((id \
                   23952)(content(Whitespace\" \"))))(Tile((id \
                   23953)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   23956)(content(Whitespace\" \")))))))))(Secondary((id \
                   23960)(content(Whitespace\" \"))))(Tile((id \
                   23962)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   23965)(content(Whitespace\" \")))))))))(Secondary((id \
                   23967)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   23971)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   23972)(content(Whitespace\" \"))))(Tile((id \
                   23974)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   23975)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   23977)(content(Whitespace\" \"))))(Tile((id \
                   23980)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   23981)(content(Whitespace\" \")))))((Secondary((id \
                   23983)(content(Whitespace\" \"))))(Tile((id 23986)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   23987)(content(Whitespace\" \"))))(Tile((id \
                   23992)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   23995)(content(Whitespace\" \")))))((Secondary((id \
                   23999)(content(Whitespace\" \"))))(Tile((id \
                   24000)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24003)(content(Whitespace\" \")))))))))(Secondary((id \
                   24007)(content(Whitespace\" \"))))(Tile((id \
                   24009)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24012)(content(Whitespace\" \")))))))))(Secondary((id \
                   24014)(content(Whitespace\" \"))))(Secondary((id \
                   24018)(content(Comment #err#))))(Secondary((id \
                   24019)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24023)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24024)(content(Whitespace\" \"))))(Tile((id \
                   24026)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   24027)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   24029)(content(Whitespace\" \"))))(Tile((id \
                   24033)(label(Fake))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   24034)(content(Whitespace\" \")))))((Secondary((id \
                   24036)(content(Whitespace\" \"))))(Tile((id 24039)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24040)(content(Whitespace\" \"))))(Tile((id \
                   24045)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24048)(content(Whitespace\" \")))))((Secondary((id \
                   24052)(content(Whitespace\" \"))))(Tile((id \
                   24053)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24056)(content(Whitespace\" \")))))))))(Secondary((id \
                   24060)(content(Whitespace\" \"))))(Tile((id \
                   24064)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24067)(content(Whitespace\" \")))))))))(Secondary((id \
                   24069)(content(Whitespace\" \"))))(Secondary((id \
                   24073)(content(Comment #err#))))(Secondary((id \
                   24074)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24078)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24079)(content(Whitespace\" \"))))(Tile((id \
                   24081)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   24082)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   24084)(content(Whitespace\" \"))))(Tile((id \
                   24085)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   24086)(content(Whitespace\" \")))))((Secondary((id \
                   24088)(content(Whitespace\" \"))))(Tile((id 24091)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24092)(content(Whitespace\" \"))))(Tile((id \
                   24097)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24100)(content(Whitespace\" \")))))((Secondary((id \
                   24104)(content(Whitespace\" \"))))(Tile((id \
                   24105)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24108)(content(Whitespace\" \")))))))))(Secondary((id \
                   24112)(content(Whitespace\" \"))))(Tile((id \
                   24114)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24117)(content(Whitespace\" \")))))))))(Secondary((id \
                   24119)(content(Whitespace\" \"))))(Secondary((id \
                   24126)(content(Comment\"#2x err#\"))))(Secondary((id \
                   24127)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24131)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24132)(content(Whitespace\" \"))))(Tile((id \
                   24134)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   24135)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   24137)(content(Whitespace\" \"))))(Tile((id \
                   24138)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   24139)(content(Whitespace\" \")))))((Secondary((id \
                   24141)(content(Whitespace\" \"))))(Tile((id \
                   24142)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24145)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24146)(content(Whitespace\" \"))))(Tile((id \
                   24151)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24154)(content(Whitespace\" \")))))((Secondary((id \
                   24158)(content(Whitespace\" \"))))(Tile((id \
                   24159)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24162)(content(Whitespace\" \")))))))))(Secondary((id \
                   24166)(content(Whitespace\" \"))))(Tile((id \
                   24168)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   24169)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25699)(content(Whitespace\" \"))))(Grout((id 24175)(shape \
                   Convex)))(Secondary((id 24171)(content(Whitespace\" \
                   \")))))))))(Secondary((id 24177)(content(Whitespace\" \
                   \"))))(Secondary((id 24181)(content(Comment \
                   #err#))))(Secondary((id \
                   24182)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24186)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24187)(content(Whitespace\" \"))))(Tile((id \
                   24189)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   24190)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   24192)(content(Whitespace\" \"))))(Grout((id 24194)(shape \
                   Convex)))(Tile((id 24193)(label(,))(mold((out \
                   Pat)(in_())(nibs(((shape(Concave 14))(sort \
                   Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   24195)(content(Whitespace\" \"))))(Tile((id \
                   24196)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   24197)(content(Whitespace\" \")))))((Secondary((id \
                   24199)(content(Whitespace\" \"))))(Tile((id \
                   24200)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24203)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24204)(content(Whitespace\" \"))))(Tile((id \
                   24209)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24212)(content(Whitespace\" \")))))((Secondary((id \
                   24216)(content(Whitespace\" \"))))(Tile((id \
                   24217)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24220)(content(Whitespace\" \")))))))))(Secondary((id \
                   24224)(content(Whitespace\" \"))))(Tile((id \
                   24226)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   24227)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25700)(content(Whitespace\" \"))))(Grout((id 24233)(shape \
                   Convex)))(Secondary((id 24229)(content(Whitespace\" \
                   \")))))))))(Secondary((id 24235)(content(Whitespace\" \
                   \"))))(Secondary((id \
                   24236)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24240)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24241)(content(Whitespace\" \"))))(Tile((id 24243)(label([ \
                   ]))(mold((out Pat)(in_(Pat))(nibs(((shape Convex)(sort \
                   Pat))((shape Convex)(sort Pat))))))(shards(0 \
                   1))(children(((Tile((id 24244)(label(_))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   24245)(content(Whitespace\" \")))))((Secondary((id \
                   24247)(content(Whitespace\" \"))))(Tile((id 24248)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 24249)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24252)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24253)(content(Whitespace\" \"))))(Tile((id \
                   24258)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24261)(content(Whitespace\" \")))))((Secondary((id \
                   24265)(content(Whitespace\" \"))))(Tile((id \
                   24266)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24269)(content(Whitespace\" \")))))))))(Secondary((id \
                   24273)(content(Whitespace\" \"))))(Tile((id \
                   24275)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                   24278)(content(Whitespace\" \")))))))))(Secondary((id \
                   24280)(content(Whitespace\" \"))))(Secondary((id \
                   24281)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24285)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24286)(content(Whitespace\" \"))))(Tile((id 24288)(label([ \
                   ]))(mold((out Pat)(in_(Pat))(nibs(((shape Convex)(sort \
                   Pat))((shape Convex)(sort Pat))))))(shards(0 \
                   1))(children(((Tile((id 24289)(label(_))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   24290)(content(Whitespace\" \")))))((Secondary((id \
                   24292)(content(Whitespace\" \"))))(Tile((id \
                   24293)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24296)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24297)(content(Whitespace\" \"))))(Tile((id \
                   24302)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24305)(content(Whitespace\" \")))))((Secondary((id \
                   24309)(content(Whitespace\" \"))))(Tile((id \
                   24310)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24313)(content(Whitespace\" \")))))))))(Secondary((id \
                   24317)(content(Whitespace\" \"))))(Tile((id \
                   24319)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   24322)(content(Whitespace\" \")))))))))(Secondary((id \
                   24324)(content(Whitespace\" \"))))(Secondary((id \
                   24331)(content(Comment\"#2x err#\"))))(Secondary((id \
                   24332)(content(Whitespace\" \"))))(Secondary((id \
                   24333)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   24334)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25411)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Grout((id \
                   25413)(shape Convex))))))))(Tile((id \
                   24338)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24342)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24343)(content(Whitespace\" \"))))(Tile((id \
                   24348)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24351)(content(Whitespace\" \")))))((Secondary((id \
                   24355)(content(Whitespace\" \"))))(Tile((id \
                   24356)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24359)(content(Whitespace\" \")))))))))(Secondary((id \
                   24363)(content(Whitespace\" \"))))(Tile((id \
                   24365)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   24366)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24368)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24369)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   24370)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24374)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24375)(content(Whitespace\" \"))))(Tile((id \
                   24380)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24383)(content(Whitespace\" \")))))((Secondary((id \
                   24387)(content(Whitespace\" \"))))(Tile((id \
                   24388)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24391)(content(Whitespace\" \")))))))))(Secondary((id \
                   24395)(content(Whitespace\" \"))))(Tile((id \
                   24397)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   24398)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24400)(content(Whitespace\" \"))))(Secondary((id \
                   24404)(content(Comment #err#))))(Secondary((id \
                   24405)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24406)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24407)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   24408)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24412)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24413)(content(Whitespace\" \"))))(Tile((id \
                   24418)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24421)(content(Whitespace\" \")))))((Secondary((id \
                   24425)(content(Whitespace\" \"))))(Tile((id \
                   24426)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24429)(content(Whitespace\" \")))))))))(Secondary((id \
                   24433)(content(Whitespace\" \"))))(Tile((id \
                   24435)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   24436)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24438)(content(Whitespace\" \"))))(Secondary((id \
                   24442)(content(Comment #err#))))(Secondary((id \
                   24443)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24444)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24448)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   25439)(content(Whitespace\" \"))))(Grout((id 25440)(shape \
                   Convex)))(Secondary((id 25441)(content(Whitespace\" \
                   \")))))))))(Secondary((id 25442)(content(Whitespace\" \
                   \"))))(Grout((id 24457)(shape Convex))))))))(Tile((id \
                   24458)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24462)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24463)(content(Whitespace\" \"))))(Tile((id \
                   24468)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24471)(content(Whitespace\" \")))))((Secondary((id \
                   24475)(content(Whitespace\" \"))))(Tile((id \
                   24476)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24479)(content(Whitespace\" \")))))))))(Secondary((id \
                   24483)(content(Whitespace\" \"))))(Tile((id \
                   24485)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   24486)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24488)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24489)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24493)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   24494)(content(Whitespace\" \"))))(Tile((id \
                   24496)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   24497)(content(Whitespace\" \")))))))))(Secondary((id \
                   25443)(content(Whitespace\" \"))))(Grout((id 24502)(shape \
                   Convex))))))))(Tile((id 24503)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24507)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24508)(content(Whitespace\" \"))))(Tile((id \
                   24513)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24516)(content(Whitespace\" \")))))((Secondary((id \
                   24520)(content(Whitespace\" \"))))(Tile((id \
                   24521)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24524)(content(Whitespace\" \")))))))))(Secondary((id \
                   24528)(content(Whitespace\" \"))))(Tile((id \
                   24530)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   24531)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24533)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24534)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24538)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   24539)(content(Whitespace\" \"))))(Tile((id \
                   24541)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   24542)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   25444)(content(Whitespace\" \"))))(Grout((id 24548)(shape \
                   Convex)))(Secondary((id 24544)(content(Whitespace\" \
                   \")))))))))(Secondary((id 25445)(content(Whitespace\" \
                   \"))))(Grout((id 24551)(shape Convex))))))))(Tile((id \
                   24552)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24556)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24557)(content(Whitespace\" \"))))(Tile((id \
                   24562)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24565)(content(Whitespace\" \")))))((Secondary((id \
                   24569)(content(Whitespace\" \"))))(Tile((id \
                   24570)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24573)(content(Whitespace\" \")))))))))(Secondary((id \
                   24577)(content(Whitespace\" \"))))(Tile((id \
                   24579)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   24580)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24582)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24583)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24587)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   24588)(content(Whitespace\" \"))))(Tile((id \
                   24590)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   24591)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   24593)(content(Whitespace\" \"))))(Tile((id \
                   24596)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   24597)(content(Whitespace\" \")))))))))(Secondary((id \
                   25446)(content(Whitespace\" \"))))(Grout((id 24602)(shape \
                   Convex))))))))(Tile((id 24603)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24607)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24608)(content(Whitespace\" \"))))(Tile((id \
                   24613)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24616)(content(Whitespace\" \")))))((Secondary((id \
                   24620)(content(Whitespace\" \"))))(Tile((id \
                   24621)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24624)(content(Whitespace\" \")))))))))(Secondary((id \
                   24628)(content(Whitespace\" \"))))(Tile((id \
                   24630)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   24631)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24633)(content(Whitespace\" \"))))(Secondary((id \
                   24637)(content(Comment #err#))))(Secondary((id \
                   24638)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   24639)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24643)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24644)(content(Whitespace\" \"))))(Tile((id \
                   24646)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   24647)(content(Whitespace\" \")))))((Secondary((id \
                   24649)(content(Whitespace\" \"))))(Tile((id \
                   24653)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   24654)(content(Whitespace\" \"))))(Tile((id \
                   24656)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   24657)(content(Whitespace\" \")))))))))(Secondary((id \
                   24660)(content(Whitespace\" \"))))(Tile((id 24663)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24664)(content(Whitespace\" \"))))(Tile((id \
                   24669)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24672)(content(Whitespace\" \")))))((Secondary((id \
                   24676)(content(Whitespace\" \"))))(Tile((id \
                   24677)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24680)(content(Whitespace\" \")))))))))(Secondary((id \
                   24684)(content(Whitespace\" \"))))(Tile((id \
                   24686)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24689)(content(Whitespace\" \")))))))))(Secondary((id \
                   24691)(content(Whitespace\" \"))))(Secondary((id \
                   24695)(content(Comment #err#))))(Secondary((id \
                   24696)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24700)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24701)(content(Whitespace\" \"))))(Tile((id \
                   24703)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   24704)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   25417)(content(Whitespace\" \"))))(Grout((id 24709)(shape \
                   Convex)))(Secondary((id 24706)(content(Whitespace\" \
                   \")))))((Secondary((id 24710)(content(Whitespace\" \
                   \"))))(Tile((id 24714)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 24715)(content(Whitespace\" \
                   \"))))(Tile((id 24717)(label(x))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   24718)(content(Whitespace\" \")))))))))(Secondary((id \
                   24721)(content(Whitespace\" \"))))(Tile((id 24724)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24725)(content(Whitespace\" \"))))(Tile((id \
                   24730)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24733)(content(Whitespace\" \")))))((Secondary((id \
                   24737)(content(Whitespace\" \"))))(Tile((id \
                   24738)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24741)(content(Whitespace\" \")))))))))(Secondary((id \
                   24745)(content(Whitespace\" \"))))(Tile((id \
                   24747)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24750)(content(Whitespace\" \")))))))))(Secondary((id \
                   24752)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24756)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24757)(content(Whitespace\" \"))))(Tile((id \
                   24759)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   24760)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   25416)(content(Whitespace\" \"))))(Grout((id 25415)(shape \
                   Convex)))(Secondary((id 25419)(content(Whitespace\" \
                   \"))))(Tile((id 24766)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   25418)(content(Whitespace\" \"))))(Grout((id 24771)(shape \
                   Convex)))(Secondary((id 24768)(content(Whitespace\" \
                   \")))))((Secondary((id 24772)(content(Whitespace\" \
                   \"))))(Tile((id 24776)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 24777)(content(Whitespace\" \
                   \"))))(Tile((id 24779)(label(x))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   24780)(content(Whitespace\" \")))))))))(Secondary((id \
                   24783)(content(Whitespace\" \"))))(Tile((id 24786)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24787)(content(Whitespace\" \"))))(Tile((id \
                   24792)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24795)(content(Whitespace\" \")))))((Secondary((id \
                   24799)(content(Whitespace\" \"))))(Tile((id \
                   24800)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24803)(content(Whitespace\" \")))))))))(Secondary((id \
                   24807)(content(Whitespace\" \"))))(Tile((id \
                   24809)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24812)(content(Whitespace\" \")))))))))(Secondary((id \
                   24814)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24818)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24819)(content(Whitespace\" \"))))(Tile((id \
                   24821)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   24822)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   24824)(content(Whitespace\" \"))))(Grout((id 24829)(shape \
                   Convex)))(Secondary((id 25420)(content(Whitespace\" \
                   \"))))(Tile((id 24828)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   24830)(content(Whitespace\" \"))))(Tile((id \
                   24833)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   24834)(content(Whitespace\" \")))))((Secondary((id \
                   24836)(content(Whitespace\" \"))))(Tile((id \
                   24840)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   24841)(content(Whitespace\" \"))))(Tile((id \
                   24843)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   24844)(content(Whitespace\" \")))))))))(Secondary((id \
                   24847)(content(Whitespace\" \"))))(Tile((id 24850)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24851)(content(Whitespace\" \"))))(Tile((id \
                   24856)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24859)(content(Whitespace\" \")))))((Secondary((id \
                   24863)(content(Whitespace\" \"))))(Tile((id \
                   24864)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24867)(content(Whitespace\" \")))))))))(Secondary((id \
                   24871)(content(Whitespace\" \"))))(Tile((id \
                   24873)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24876)(content(Whitespace\" \")))))))))(Secondary((id \
                   24878)(content(Whitespace\" \"))))(Secondary((id \
                   24882)(content(Comment #err#))))(Secondary((id \
                   24883)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24887)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24888)(content(Whitespace\" \"))))(Tile((id \
                   24890)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   24891)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   24893)(content(Whitespace\" \"))))(Grout((id 24898)(shape \
                   Convex)))(Secondary((id 25421)(content(Whitespace\" \
                   \"))))(Tile((id 24897)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   24899)(content(Whitespace\" \"))))(Tile((id 25423)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Grout((id 25431)(shape \
                   Convex))))))))(Secondary((id 24903)(content(Whitespace\" \
                   \")))))((Secondary((id 24905)(content(Whitespace\" \
                   \"))))(Tile((id 24909)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 24910)(content(Whitespace\" \
                   \"))))(Tile((id 24912)(label(x))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   24913)(content(Whitespace\" \")))))))))(Secondary((id \
                   24916)(content(Whitespace\" \"))))(Tile((id 24919)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24920)(content(Whitespace\" \"))))(Tile((id \
                   24925)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24928)(content(Whitespace\" \")))))((Secondary((id \
                   24932)(content(Whitespace\" \"))))(Tile((id \
                   24933)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24936)(content(Whitespace\" \")))))))))(Secondary((id \
                   24940)(content(Whitespace\" \"))))(Tile((id \
                   24942)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24945)(content(Whitespace\" \")))))))))(Secondary((id \
                   24947)(content(Whitespace\" \"))))(Secondary((id \
                   24954)(content(Comment\"#2x err#\"))))(Secondary((id \
                   24955)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   24956)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25448)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Grout((id \
                   25449)(shape Convex))))))))(Tile((id \
                   24962)(label(::))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 6))(sort \
                   Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 24963)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 24964)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   24967)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   24968)(content(Whitespace\" \"))))(Tile((id \
                   24973)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24976)(content(Whitespace\" \")))))((Secondary((id \
                   24980)(content(Whitespace\" \"))))(Tile((id \
                   24981)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24984)(content(Whitespace\" \")))))))))(Secondary((id \
                   24988)(content(Whitespace\" \"))))(Tile((id \
                   24990)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                   24991)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   24993)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   24994)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   24997)(label(::))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 6))(sort \
                   Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 24998)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 24999)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   25002)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25003)(content(Whitespace\" \"))))(Tile((id \
                   25008)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25011)(content(Whitespace\" \")))))((Secondary((id \
                   25015)(content(Whitespace\" \"))))(Tile((id \
                   25016)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25019)(content(Whitespace\" \")))))))))(Secondary((id \
                   25023)(content(Whitespace\" \"))))(Tile((id \
                   25025)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                   25026)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25028)(content(Whitespace\" \"))))(Secondary((id \
                   25032)(content(Comment #err#))))(Secondary((id \
                   25033)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25034)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   25035)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   25036)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25451)(content(Whitespace\" \"))))(Tile((id \
                   25038)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   25041)(label(::))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 6))(sort \
                   Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 25042)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 25043)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   25046)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25047)(content(Whitespace\" \"))))(Tile((id \
                   25052)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25055)(content(Whitespace\" \")))))((Secondary((id \
                   25059)(content(Whitespace\" \"))))(Tile((id \
                   25060)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25063)(content(Whitespace\" \")))))))))(Secondary((id \
                   25067)(content(Whitespace\" \"))))(Tile((id \
                   25069)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                   25070)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25072)(content(Whitespace\" \"))))(Secondary((id \
                   25079)(content(Comment\"#2x err#\"))))(Secondary((id \
                   25080)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   25081)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25085)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25701)(content(Whitespace\" \"))))(Grout((id 25090)(shape \
                   Convex)))(Secondary((id 25086)(content(Whitespace\" \
                   \")))))((Secondary((id 25091)(content(Whitespace\" \
                   \"))))(Tile((id 25092)(label([ ]))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   25093)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   25094)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25096)(content(Whitespace\" \"))))(Tile((id \
                   25098)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   25099)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25101)(content(Whitespace\" \"))))(Tile((id \
                   25105)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   25108)(content(Whitespace\" \")))))))))(Secondary((id \
                   25110)(content(Whitespace\" \"))))(Secondary((id \
                   25128)(content(Comment\"#err: \
                   inconsistent#\"))))(Secondary((id \
                   25129)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25133)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25134)(content(Whitespace\" \"))))(Tile((id \
                   25136)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   25137)(content(Whitespace\" \")))))((Secondary((id \
                   25139)(content(Whitespace\" \"))))(Tile((id 25140)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 25141)(label(1))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   25142)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25144)(content(Whitespace\" \"))))(Tile((id \
                   25146)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   25147)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25149)(content(Whitespace\" \"))))(Tile((id \
                   25153)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   25156)(content(Whitespace\" \")))))))))(Secondary((id \
                   25158)(content(Whitespace\" \"))))(Secondary((id \
                   25176)(content(Comment\"#err: \
                   inconsistent#\"))))(Secondary((id \
                   25177)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25181)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25182)(content(Whitespace\" \"))))(Tile((id \
                   25184)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   25185)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   25433)(content(Whitespace\" \"))))(Grout((id 25189)(shape \
                   Convex)))(Secondary((id 25187)(content(Whitespace\" \
                   \"))))(Secondary((id 25188)(content(Whitespace\" \
                   \")))))((Secondary((id 25190)(content(Whitespace\" \
                   \"))))(Tile((id 25191)(label([ ]))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   25192)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   25193)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25195)(content(Whitespace\" \"))))(Tile((id \
                   25197)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   25198)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25200)(content(Whitespace\" \"))))(Tile((id \
                   25204)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   25207)(content(Whitespace\" \")))))))))(Secondary((id \
                   25209)(content(Whitespace\" \"))))(Secondary((id \
                   25210)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25214)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25215)(content(Whitespace\" \"))))(Tile((id \
                   25217)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   25218)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   25434)(content(Whitespace\" \"))))(Tile((id 25437)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Grout((id 25436)(shape \
                   Convex))))))))(Secondary((id 25223)(content(Whitespace\" \
                   \")))))((Secondary((id 25225)(content(Whitespace\" \
                   \"))))(Tile((id 25226)(label([ ]))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   25227)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   25228)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25230)(content(Whitespace\" \"))))(Tile((id \
                   25232)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   25233)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25235)(content(Whitespace\" \"))))(Tile((id \
                   25239)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   25242)(content(Whitespace\" \")))))))))(Secondary((id \
                   25244)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25248)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25249)(content(Whitespace\" \"))))(Tile((id \
                   25251)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   25252)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   25435)(content(Whitespace\" \"))))(Tile((id 25254)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 25257)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   25258)(content(Whitespace\" \")))))((Secondary((id \
                   25260)(content(Whitespace\" \"))))(Tile((id 25261)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 25262)(label(1))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   25263)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25265)(content(Whitespace\" \"))))(Tile((id \
                   25267)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   25268)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25270)(content(Whitespace\" \"))))(Tile((id \
                   25274)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   25277)(content(Whitespace\" \")))))))))(Secondary((id \
                   25279)(content(Whitespace\" \"))))(Secondary((id \
                   25286)(content(Comment\"#2x err#\"))))(Secondary((id \
                   25287)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   25288)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25292)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25293)(content(Whitespace\" \"))))(Tile((id \
                   25295)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   25296)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   25298)(content(Whitespace\" \"))))(Tile((id 25299)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 25302)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   25303)(content(Whitespace\" \")))))((Secondary((id \
                   25305)(content(Whitespace\" \"))))(Tile((id \
                   25306)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   25309)(label(::))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 6))(sort \
                   Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 25310)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 25311)(label(2))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   25314)(content(Whitespace\" \")))))))))(Secondary((id \
                   25316)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25320)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25321)(content(Whitespace\" \"))))(Tile((id \
                   25323)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   25324)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   25326)(content(Whitespace\" \"))))(Tile((id 25327)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 25330)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   25331)(content(Whitespace\" \")))))((Secondary((id \
                   25333)(content(Whitespace\" \"))))(Tile((id \
                   25336)(label(1.0))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   25339)(label(::))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 6))(sort \
                   Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 25340)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 25341)(label(2))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   25344)(content(Whitespace\" \")))))))))(Secondary((id \
                   25346)(content(Whitespace\" \"))))(Secondary((id \
                   25350)(content(Comment #err#))))(Secondary((id \
                   25351)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25355)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25356)(content(Whitespace\" \"))))(Tile((id \
                   25358)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   25359)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   25361)(content(Whitespace\" \"))))(Tile((id 25362)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 25365)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   25366)(content(Whitespace\" \")))))((Secondary((id \
                   25368)(content(Whitespace\" \"))))(Tile((id \
                   25369)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   25372)(label(::))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 6))(sort \
                   Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 25373)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 25376)(label(2.0))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   25379)(content(Whitespace\" \")))))))))(Secondary((id \
                   25381)(content(Whitespace\" \"))))(Secondary((id \
                   25385)(content(Comment #err#))))(Secondary((id \
                   25386)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25390)(label(\"\\\"BYE\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))(ancestors())))(caret \
                   Outer))";
                backup_text =
                  "# Internal Regression Tests: Type errors #\n\
                   # Each line should show errors or not as indicated #\n\n\
                   let _ = unbound in #err#\n\
                   let Undefined = Undefined in # 2x err# \n\
                   let true = 2 in #err# \n\n\
                   let   = if true then 1 else 1. in #err# \n\
                   let _ = if true then 1 else 1. in #err#\n\
                   let _:   = if true then 1 else 1. in\n\
                   let _: Int = if true then 1 else 1. in #err#\n\
                   let _: Fake = if true then 1 else true in #err#\n\
                   let _, _ = if true then 1 else 1. in #2x err#\n\
                   let _, _ = (if true then 1 else 1.),   in #err#\n\
                   let _:  , _ = (if true then 1 else 1.),   in \n\
                   let [_] = [(if true then 1 else 1.)] in \n\
                   let [_] = (if true then 1 else 1.) in #2x err# \n\n\
                   ( )(if true then 1 else 1.);\n\
                   1(if true then 1 else 1.); #err#\n\
                   (1)(if true then 1 else 1.); #err#\n\
                   (fun   ->  )(if true then 1 else 1.);\n\
                   (fun _ ->  )(if true then 1 else 1.);\n\
                   (fun _:   ->  )(if true then 1 else 1.);\n\
                   (fun _: Int ->  )(if true then 1 else 1.); #err#\n\n\
                   let _ = fun x -> if true then 1 else 1. in #err#\n\
                   let _:   = fun x -> if true then 1 else 1. in\n\
                   let _:   ->   = fun x -> if true then 1 else 1. in\n\
                   let _:   -> Int = fun x -> if true then 1 else 1. in #err#\n\
                   let _:   -> [ ] = fun x -> if true then 1 else 1. in #2x \
                   err#\n\n\
                   ( )::[(if true then 1 else 1.)];\n\
                   1::[(if true then 1 else 1.)]; #err#\n\
                   (1, 1)::[(if true then 1 else 1.)]; #2x err#\n\n\
                   let   = [1, 1., true] in #err: inconsistent#\n\
                   let _ = [1, 1., true] in #err: inconsistent#\n\
                   let _:    = [1, 1., true] in \n\
                   let _: [ ] = [1, 1., true] in\n\
                   let _: [Int] = [1, 1., true] in #2x err#\n\n\
                   let _: [Int] = 1::[2] in\n\
                   let _: [Int] = 1.0::[2] in #err#\n\
                   let _: [Int] = 1::[2.0] in #err#\n\
                   \"BYE\"";
              } ) );
          ( "Casting",
            ( 7009,
              {
                zipper =
                  "((selection((focus \
                   Left)(content())))(backpack())(relatives((siblings(()((Secondary((id \
                   7008)(content(Comment\"# Internal Regression Tests: \
                   Function literal casting #\"))))(Secondary((id \
                   384)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   446)(content(Comment\"# None of the below should trigger \
                   runtime exceptions #\"))))(Secondary((id \
                   284)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   285)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5)(content(Whitespace\" \"))))(Tile((id \
                   7)(label(g))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   9)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   11)(content(Whitespace\" \"))))(Grout((id 14)(shape \
                   Convex)))(Secondary((id 349)(content(Whitespace\" \
                   \"))))(Tile((id 13)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   350)(content(Whitespace\" \"))))(Grout((id 16)(shape \
                   Convex)))(Secondary((id 15)(content(Whitespace\" \
                   \")))))((Secondary((id 17)(content(Whitespace\" \
                   \"))))(Tile((id 21)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 22)(content(Whitespace\" \
                   \"))))(Tile((id 24)(label(_))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   25)(content(Whitespace\" \")))))))))(Secondary((id \
                   28)(content(Whitespace\" \"))))(Tile((id \
                   29)(label(9))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   32)(content(Whitespace\" \")))))))))(Secondary((id \
                   34)(content(Whitespace\" \"))))(Tile((id \
                   35)(label(-))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 2))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   36)(label(g))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   37)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   39)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   40)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1708)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   902)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   907)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   908)(content(Whitespace\" \"))))(Tile((id \
                   909)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   912)(content(Whitespace\" \")))))((Secondary((id \
                   916)(content(Whitespace\" \"))))(Tile((id 921)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 922)(content(Whitespace\" \
                   \"))))(Tile((id 923)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   963)(content(Whitespace\" \")))))))))(Secondary((id \
                   934)(content(Whitespace\" \"))))(Tile((id \
                   935)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   937)(content(Whitespace\" \"))))(Tile((id \
                   939)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   940)(content(Whitespace\" \"))))(Tile((id \
                   945)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   948)(content(Whitespace\" \")))))))))(Secondary((id \
                   949)(content(Whitespace\" \"))))(Tile((id \
                   950)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   952)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   957)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   958)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2884)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2889)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2890)(content(Whitespace\" \"))))(Tile((id \
                   2891)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2893)(content(Whitespace\" \")))))((Secondary((id \
                   2894)(content(Whitespace\" \"))))(Tile((id 2899)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2900)(content(Whitespace\" \
                   \"))))(Tile((id 2901)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2978)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2986)(content(Whitespace\" \"))))(Grout((id 2979)(shape \
                   Convex)))(Secondary((id 2903)(content(Whitespace\" \
                   \")))))))))(Secondary((id 2905)(content(Whitespace\" \
                   \"))))(Tile((id 2906)(label(b))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2908)(content(Whitespace\" \"))))(Tile((id \
                   2910)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2911)(content(Whitespace\" \"))))(Tile((id \
                   2916)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2919)(content(Whitespace\" \")))))))))(Secondary((id \
                   2920)(content(Whitespace\" \"))))(Tile((id \
                   2921)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2923)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2928)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   2929)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2931)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2936)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2937)(content(Whitespace\" \"))))(Tile((id \
                   2938)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2940)(content(Whitespace\" \")))))((Secondary((id \
                   2941)(content(Whitespace\" \"))))(Tile((id 2946)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2947)(content(Whitespace\" \
                   \"))))(Tile((id 2948)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2980)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2987)(content(Whitespace\" \"))))(Tile((id \
                   2985)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2950)(content(Whitespace\" \")))))))))(Secondary((id \
                   2952)(content(Whitespace\" \"))))(Tile((id \
                   2953)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2955)(content(Whitespace\" \"))))(Tile((id \
                   2957)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2958)(content(Whitespace\" \"))))(Tile((id \
                   2963)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2966)(content(Whitespace\" \")))))))))(Secondary((id \
                   2967)(content(Whitespace\" \"))))(Tile((id \
                   2968)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2970)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2975)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   2976)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   964)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   969)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   970)(content(Whitespace\" \"))))(Tile((id \
                   971)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   1012)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1067)(content(Whitespace\" \"))))(Grout((id 1013)(shape \
                   Convex)))(Secondary((id 973)(content(Whitespace\" \
                   \")))))((Secondary((id 974)(content(Whitespace\" \
                   \"))))(Tile((id 979)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 980)(content(Whitespace\" \
                   \"))))(Tile((id 981)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   983)(content(Whitespace\" \")))))))))(Secondary((id \
                   985)(content(Whitespace\" \"))))(Tile((id \
                   986)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   988)(content(Whitespace\" \"))))(Tile((id \
                   990)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   991)(content(Whitespace\" \"))))(Tile((id \
                   996)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   999)(content(Whitespace\" \")))))))))(Secondary((id \
                   1000)(content(Whitespace\" \"))))(Tile((id \
                   1001)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1003)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1008)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   1009)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1014)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1019)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1020)(content(Whitespace\" \"))))(Tile((id \
                   1021)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   1023)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1068)(content(Whitespace\" \"))))(Grout((id 1026)(shape \
                   Convex)))(Secondary((id 1024)(content(Whitespace\" \
                   \")))))((Secondary((id 1027)(content(Whitespace\" \
                   \"))))(Tile((id 1032)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1033)(content(Whitespace\" \
                   \"))))(Tile((id 1034)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   1064)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1066)(content(Whitespace\" \"))))(Grout((id 1065)(shape \
                   Convex)))(Secondary((id 1036)(content(Whitespace\" \
                   \")))))))))(Secondary((id 1038)(content(Whitespace\" \
                   \"))))(Tile((id 1039)(label(b))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1041)(content(Whitespace\" \"))))(Tile((id \
                   1043)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1044)(content(Whitespace\" \"))))(Tile((id \
                   1049)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1052)(content(Whitespace\" \")))))))))(Secondary((id \
                   1053)(content(Whitespace\" \"))))(Tile((id \
                   1054)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1056)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1061)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   1062)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   707)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   712)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   713)(content(Whitespace\" \"))))(Tile((id \
                   714)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   716)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1069)(content(Whitespace\" \"))))(Grout((id 719)(shape \
                   Convex)))(Secondary((id 717)(content(Whitespace\" \
                   \")))))((Secondary((id 720)(content(Whitespace\" \
                   \"))))(Tile((id 725)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 726)(content(Whitespace\" \
                   \"))))(Tile((id 727)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   729)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   730)(content(Whitespace\" \"))))(Tile((id \
                   735)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   736)(content(Whitespace\" \")))))))))(Secondary((id \
                   738)(content(Whitespace\" \"))))(Tile((id \
                   739)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   741)(content(Whitespace\" \"))))(Tile((id \
                   743)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   744)(content(Whitespace\" \"))))(Tile((id \
                   749)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   752)(content(Whitespace\" \")))))))))(Secondary((id \
                   753)(content(Whitespace\" \"))))(Tile((id \
                   754)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   756)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   761)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   762)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1070)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1075)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1076)(content(Whitespace\" \"))))(Tile((id \
                   1077)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   1079)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1121)(content(Whitespace\" \"))))(Grout((id 1083)(shape \
                   Convex)))(Secondary((id 1122)(content(Whitespace\" \
                   \"))))(Tile((id 1125)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1126)(content(Whitespace\" \"))))(Grout((id 1124)(shape \
                   Convex)))(Secondary((id 1080)(content(Whitespace\" \
                   \")))))((Secondary((id 1084)(content(Whitespace\" \
                   \"))))(Tile((id 1089)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1090)(content(Whitespace\" \
                   \"))))(Tile((id 1091)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1093)(content(Whitespace\" \")))))))))(Secondary((id \
                   1095)(content(Whitespace\" \"))))(Tile((id \
                   1096)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1098)(content(Whitespace\" \"))))(Tile((id \
                   1100)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1101)(content(Whitespace\" \"))))(Tile((id \
                   1106)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1109)(content(Whitespace\" \")))))))))(Secondary((id \
                   1110)(content(Whitespace\" \"))))(Tile((id \
                   1111)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1113)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1118)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   1119)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1127)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1132)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1133)(content(Whitespace\" \"))))(Tile((id \
                   1134)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   1136)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1137)(content(Whitespace\" \"))))(Grout((id 1142)(shape \
                   Convex)))(Secondary((id 1245)(content(Whitespace\" \
                   \"))))(Tile((id 1141)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1246)(content(Whitespace\" \"))))(Grout((id 1148)(shape \
                   Convex)))(Secondary((id 1143)(content(Whitespace\" \
                   \")))))((Secondary((id 1149)(content(Whitespace\" \
                   \"))))(Tile((id 1154)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1155)(content(Whitespace\" \
                   \"))))(Tile((id 1156)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   1249)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1251)(content(Whitespace\" \"))))(Grout((id 1250)(shape \
                   Convex)))(Secondary((id 1158)(content(Whitespace\" \
                   \")))))))))(Secondary((id 1160)(content(Whitespace\" \
                   \"))))(Tile((id 1161)(label(b))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1163)(content(Whitespace\" \"))))(Tile((id \
                   1165)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1166)(content(Whitespace\" \"))))(Tile((id \
                   1171)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1174)(content(Whitespace\" \")))))))))(Secondary((id \
                   1175)(content(Whitespace\" \"))))(Tile((id \
                   1176)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1178)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1183)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   1184)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1186)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1262)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1263)(content(Whitespace\" \"))))(Tile((id \
                   1264)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   1266)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1267)(content(Whitespace\" \"))))(Grout((id 1272)(shape \
                   Convex)))(Secondary((id 1325)(content(Whitespace\" \
                   \"))))(Tile((id 1271)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1326)(content(Whitespace\" \"))))(Grout((id 1276)(shape \
                   Convex)))(Secondary((id 1273)(content(Whitespace\" \
                   \")))))((Secondary((id 1277)(content(Whitespace\" \
                   \"))))(Tile((id 1282)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1283)(content(Whitespace\" \
                   \"))))(Tile((id 1284)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   1286)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1287)(content(Whitespace\" \"))))(Tile((id \
                   6953)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1292)(content(Whitespace\" \")))))))))(Secondary((id \
                   1294)(content(Whitespace\" \"))))(Tile((id \
                   1295)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1297)(content(Whitespace\" \"))))(Tile((id \
                   1299)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1300)(content(Whitespace\" \"))))(Tile((id \
                   1305)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1308)(content(Whitespace\" \")))))))))(Secondary((id \
                   1309)(content(Whitespace\" \"))))(Tile((id \
                   1310)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1312)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1317)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   1318)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1320)(content(Whitespace\" \"))))(Secondary((id \
                   1324)(content(Comment #ERR#))))(Secondary((id \
                   42)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   46)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   47)(content(Whitespace\" \"))))(Tile((id \
                   49)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   50)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   52)(content(Whitespace\" \"))))(Tile((id \
                   56)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   57)(content(Whitespace\" \"))))(Tile((id \
                   60)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   348)(content(Whitespace\" \"))))(Grout((id 63)(shape \
                   Convex)))(Secondary((id 61)(content(Whitespace\" \
                   \")))))((Secondary((id 64)(content(Whitespace\" \
                   \"))))(Tile((id 68)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 69)(content(Whitespace\" \
                   \"))))(Tile((id 71)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   74)(content(Whitespace\" \")))))))))(Secondary((id \
                   77)(content(Whitespace\" \"))))(Tile((id \
                   78)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   79)(content(Whitespace\" \"))))(Tile((id \
                   82)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   83)(content(Whitespace\" \"))))(Tile((id \
                   87)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   90)(content(Whitespace\" \")))))))))(Secondary((id \
                   92)(content(Whitespace\" \"))))(Tile((id \
                   93)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   94)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   99)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   100)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1327)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1332)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1333)(content(Whitespace\" \"))))(Tile((id \
                   1334)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   1336)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1337)(content(Whitespace\" \"))))(Tile((id \
                   1342)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1343)(content(Whitespace\" \"))))(Tile((id \
                   1345)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1453)(content(Whitespace\" \"))))(Grout((id 1349)(shape \
                   Convex)))(Secondary((id 1346)(content(Whitespace\" \
                   \")))))((Secondary((id 1350)(content(Whitespace\" \
                   \"))))(Tile((id 1355)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1356)(content(Whitespace\" \
                   \"))))(Tile((id 1357)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   1359)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1454)(content(Whitespace\" \"))))(Grout((id 1363)(shape \
                   Convex)))(Secondary((id 1360)(content(Whitespace\" \
                   \")))))))))(Secondary((id 1364)(content(Whitespace\" \
                   \"))))(Tile((id 1365)(label(b))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1367)(content(Whitespace\" \"))))(Tile((id \
                   1369)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1370)(content(Whitespace\" \"))))(Tile((id \
                   1375)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1378)(content(Whitespace\" \")))))))))(Secondary((id \
                   1379)(content(Whitespace\" \"))))(Tile((id \
                   1380)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1382)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1387)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   1388)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1390)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1395)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1396)(content(Whitespace\" \"))))(Tile((id \
                   1397)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   1399)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1400)(content(Whitespace\" \"))))(Tile((id \
                   1405)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1406)(content(Whitespace\" \"))))(Tile((id \
                   1408)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1455)(content(Whitespace\" \"))))(Grout((id 1412)(shape \
                   Convex)))(Secondary((id 1409)(content(Whitespace\" \
                   \")))))((Secondary((id 1413)(content(Whitespace\" \
                   \"))))(Tile((id 1418)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1419)(content(Whitespace\" \
                   \"))))(Tile((id 1420)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   1422)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1456)(content(Whitespace\" \"))))(Tile((id \
                   1460)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1423)(content(Whitespace\" \")))))))))(Secondary((id \
                   1427)(content(Whitespace\" \"))))(Tile((id \
                   1428)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1430)(content(Whitespace\" \"))))(Tile((id \
                   1432)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1433)(content(Whitespace\" \"))))(Tile((id \
                   1438)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1441)(content(Whitespace\" \")))))))))(Secondary((id \
                   1442)(content(Whitespace\" \"))))(Tile((id \
                   1443)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1445)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1450)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   1451)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1461)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1466)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1467)(content(Whitespace\" \"))))(Tile((id \
                   1468)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   1470)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1471)(content(Whitespace\" \"))))(Tile((id \
                   1476)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1477)(content(Whitespace\" \"))))(Tile((id \
                   1479)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1480)(content(Whitespace\" \"))))(Tile((id \
                   1485)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1486)(content(Whitespace\" \")))))((Secondary((id \
                   1487)(content(Whitespace\" \"))))(Tile((id 1492)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1493)(content(Whitespace\" \
                   \"))))(Tile((id 1494)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1496)(content(Whitespace\" \")))))))))(Secondary((id \
                   1498)(content(Whitespace\" \"))))(Tile((id \
                   1499)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1501)(content(Whitespace\" \"))))(Tile((id \
                   1503)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1504)(content(Whitespace\" \"))))(Tile((id \
                   1509)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1512)(content(Whitespace\" \")))))))))(Secondary((id \
                   1513)(content(Whitespace\" \"))))(Tile((id \
                   1514)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1516)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1521)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   1522)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   102)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   106)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   107)(content(Whitespace\" \"))))(Tile((id \
                   109)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   110)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   112)(content(Whitespace\" \"))))(Tile((id \
                   363)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   365)(content(Whitespace\" \"))))(Tile((id \
                   367)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   368)(content(Whitespace\" \"))))(Tile((id \
                   362)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   359)(content(Whitespace\" \")))))((Secondary((id \
                   125)(content(Whitespace\" \"))))(Tile((id 129)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 130)(content(Whitespace\" \
                   \"))))(Tile((id 132)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   133)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   448)(content(Whitespace\" \"))))(Grout((id 460)(shape \
                   Convex)))(Secondary((id 135)(content(Whitespace\" \
                   \")))))))))(Secondary((id 138)(content(Whitespace\" \
                   \"))))(Tile((id 139)(label(b))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   140)(content(Whitespace\" \"))))(Tile((id \
                   143)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   144)(content(Whitespace\" \"))))(Tile((id \
                   148)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   151)(content(Whitespace\" \")))))))))(Secondary((id \
                   153)(content(Whitespace\" \"))))(Tile((id \
                   154)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   155)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   160)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   161)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   163)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   225)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   226)(content(Whitespace\" \"))))(Tile((id \
                   228)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   229)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   231)(content(Whitespace\" \"))))(Tile((id \
                   235)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   354)(content(Whitespace\" \"))))(Tile((id \
                   238)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   355)(content(Whitespace\" \"))))(Tile((id \
                   242)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   357)(content(Whitespace\" \")))))((Secondary((id \
                   244)(content(Whitespace\" \"))))(Tile((id 248)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 249)(content(Whitespace\" \
                   \"))))(Tile((id 251)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   252)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   447)(content(Whitespace\" \"))))(Tile((id \
                   381)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   383)(content(Whitespace\" \")))))))))(Secondary((id \
                   260)(content(Whitespace\" \"))))(Tile((id \
                   261)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   262)(content(Whitespace\" \"))))(Tile((id \
                   265)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   266)(content(Whitespace\" \"))))(Tile((id \
                   270)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   273)(content(Whitespace\" \")))))))))(Secondary((id \
                   275)(content(Whitespace\" \"))))(Tile((id \
                   276)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   277)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   282)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   464)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1524)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1528)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1529)(content(Whitespace\" \"))))(Tile((id \
                   1531)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   1532)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1534)(content(Whitespace\" \"))))(Grout((id 1539)(shape \
                   Convex)))(Secondary((id 1709)(content(Whitespace\" \
                   \"))))(Tile((id 1538)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1710)(content(Whitespace\" \"))))(Tile((id \
                   1714)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1540)(content(Whitespace\" \")))))((Secondary((id \
                   1544)(content(Whitespace\" \"))))(Tile((id 1548)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1549)(content(Whitespace\" \
                   \"))))(Tile((id 1551)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1552)(content(Whitespace\" \")))))))))(Secondary((id \
                   1555)(content(Whitespace\" \"))))(Tile((id \
                   1556)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1557)(content(Whitespace\" \"))))(Tile((id \
                   1560)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1561)(content(Whitespace\" \"))))(Tile((id \
                   1565)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1568)(content(Whitespace\" \")))))))))(Secondary((id \
                   1570)(content(Whitespace\" \"))))(Tile((id \
                   1571)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1572)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1577)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   1578)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1580)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1584)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1585)(content(Whitespace\" \"))))(Tile((id \
                   1587)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   1588)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1590)(content(Whitespace\" \"))))(Grout((id 1595)(shape \
                   Convex)))(Secondary((id 1715)(content(Whitespace\" \
                   \"))))(Tile((id 1594)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1716)(content(Whitespace\" \"))))(Tile((id \
                   1720)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1596)(content(Whitespace\" \")))))((Secondary((id \
                   1600)(content(Whitespace\" \"))))(Tile((id 1604)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1605)(content(Whitespace\" \
                   \"))))(Tile((id 1607)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   1608)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1721)(content(Whitespace\" \"))))(Grout((id 1614)(shape \
                   Convex)))(Secondary((id 1610)(content(Whitespace\" \
                   \")))))))))(Secondary((id 1615)(content(Whitespace\" \
                   \"))))(Tile((id 1616)(label(b))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1617)(content(Whitespace\" \"))))(Tile((id \
                   1620)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1621)(content(Whitespace\" \"))))(Tile((id \
                   1625)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1628)(content(Whitespace\" \")))))))))(Secondary((id \
                   1630)(content(Whitespace\" \"))))(Tile((id \
                   1631)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1632)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1637)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   1638)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1640)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1644)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1645)(content(Whitespace\" \"))))(Tile((id \
                   1647)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   1648)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1650)(content(Whitespace\" \"))))(Grout((id 1655)(shape \
                   Convex)))(Secondary((id 1722)(content(Whitespace\" \
                   \"))))(Tile((id 1654)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1723)(content(Whitespace\" \"))))(Tile((id \
                   1727)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1656)(content(Whitespace\" \")))))((Secondary((id \
                   1660)(content(Whitespace\" \"))))(Tile((id 1664)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1665)(content(Whitespace\" \
                   \"))))(Tile((id 1667)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   1668)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1670)(content(Whitespace\" \"))))(Tile((id \
                   6954)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1674)(content(Whitespace\" \")))))))))(Secondary((id \
                   1677)(content(Whitespace\" \"))))(Tile((id \
                   1678)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1679)(content(Whitespace\" \"))))(Tile((id \
                   1682)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1683)(content(Whitespace\" \"))))(Tile((id \
                   1687)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1690)(content(Whitespace\" \")))))))))(Secondary((id \
                   1692)(content(Whitespace\" \"))))(Tile((id \
                   1693)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1694)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1699)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   1700)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1702)(content(Whitespace\" \"))))(Secondary((id \
                   1706)(content(Comment #ERR#))))(Secondary((id \
                   1728)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   1735)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1739)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1740)(content(Whitespace\" \"))))(Tile((id \
                   1742)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1743)(content(Whitespace\" \")))))((Secondary((id \
                   1745)(content(Whitespace\" \"))))(Tile((id 1749)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1750)(content(Whitespace\" \
                   \"))))(Tile((id 1752)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1753)(content(Whitespace\" \")))))))))(Secondary((id \
                   1756)(content(Whitespace\" \"))))(Tile((id \
                   1757)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1758)(content(Whitespace\" \"))))(Tile((id \
                   1761)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1762)(content(Whitespace\" \"))))(Tile((id \
                   1766)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1769)(content(Whitespace\" \")))))))))(Secondary((id \
                   1771)(content(Whitespace\" \"))))(Tile((id \
                   1772)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1773)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1778)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2735)(content(Whitespace\" \"))))(Tile((id \
                   2726)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2727)(content(Whitespace\" \"))))(Tile((id \
                   2731)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1779)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2988)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2993)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2994)(content(Whitespace\" \"))))(Tile((id \
                   2995)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2997)(content(Whitespace\" \")))))((Secondary((id \
                   2998)(content(Whitespace\" \"))))(Tile((id 3003)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3004)(content(Whitespace\" \
                   \"))))(Tile((id 3005)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3100)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3114)(content(Whitespace\" \"))))(Grout((id 3101)(shape \
                   Convex)))(Secondary((id 3007)(content(Whitespace\" \
                   \")))))))))(Secondary((id 3009)(content(Whitespace\" \
                   \"))))(Tile((id 3010)(label(b))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3012)(content(Whitespace\" \"))))(Tile((id \
                   3014)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3015)(content(Whitespace\" \"))))(Tile((id \
                   3020)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3023)(content(Whitespace\" \")))))))))(Secondary((id \
                   3024)(content(Whitespace\" \"))))(Tile((id \
                   3025)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3027)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3032)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   3033)(content(Whitespace\" \"))))(Tile((id \
                   3035)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3036)(content(Whitespace\" \"))))(Tile((id \
                   3041)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3042)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3044)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3049)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3050)(content(Whitespace\" \"))))(Tile((id \
                   3051)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3053)(content(Whitespace\" \")))))((Secondary((id \
                   3054)(content(Whitespace\" \"))))(Tile((id 3059)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3060)(content(Whitespace\" \
                   \"))))(Tile((id 3061)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3102)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3113)(content(Whitespace\" \"))))(Tile((id \
                   3112)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3063)(content(Whitespace\" \")))))))))(Secondary((id \
                   3065)(content(Whitespace\" \"))))(Tile((id \
                   3066)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3068)(content(Whitespace\" \"))))(Tile((id \
                   3070)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3071)(content(Whitespace\" \"))))(Tile((id \
                   3076)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3079)(content(Whitespace\" \")))))))))(Secondary((id \
                   3080)(content(Whitespace\" \"))))(Tile((id \
                   3081)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3083)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3088)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   3089)(content(Whitespace\" \"))))(Tile((id \
                   3091)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3092)(content(Whitespace\" \"))))(Tile((id \
                   3097)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3098)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1781)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1785)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1786)(content(Whitespace\" \"))))(Tile((id \
                   1788)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   1789)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3115)(content(Whitespace\" \"))))(Grout((id 1794)(shape \
                   Convex)))(Secondary((id 1791)(content(Whitespace\" \
                   \")))))((Secondary((id 1795)(content(Whitespace\" \
                   \"))))(Tile((id 1799)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1800)(content(Whitespace\" \
                   \"))))(Tile((id 1802)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1803)(content(Whitespace\" \")))))))))(Secondary((id \
                   1806)(content(Whitespace\" \"))))(Tile((id \
                   1807)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1808)(content(Whitespace\" \"))))(Tile((id \
                   1811)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1812)(content(Whitespace\" \"))))(Tile((id \
                   1816)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1819)(content(Whitespace\" \")))))))))(Secondary((id \
                   1821)(content(Whitespace\" \"))))(Tile((id \
                   1822)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1823)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1828)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2736)(content(Whitespace\" \"))))(Tile((id \
                   3145)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3146)(content(Whitespace\" \"))))(Tile((id \
                   3150)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1829)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1831)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1835)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1836)(content(Whitespace\" \"))))(Tile((id \
                   1838)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   1839)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3117)(content(Whitespace\" \"))))(Grout((id 1844)(shape \
                   Convex)))(Secondary((id 1841)(content(Whitespace\" \
                   \")))))((Secondary((id 1845)(content(Whitespace\" \
                   \"))))(Tile((id 1849)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1850)(content(Whitespace\" \
                   \"))))(Tile((id 1852)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   1853)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3118)(content(Whitespace\" \"))))(Grout((id 1859)(shape \
                   Convex)))(Secondary((id 1855)(content(Whitespace\" \
                   \")))))))))(Secondary((id 1860)(content(Whitespace\" \
                   \"))))(Tile((id 1861)(label(b))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1862)(content(Whitespace\" \"))))(Tile((id \
                   1865)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1866)(content(Whitespace\" \"))))(Tile((id \
                   1870)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1873)(content(Whitespace\" \")))))))))(Secondary((id \
                   1875)(content(Whitespace\" \"))))(Tile((id \
                   1876)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1877)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1882)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2745)(content(Whitespace\" \"))))(Tile((id \
                   3151)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3152)(content(Whitespace\" \"))))(Tile((id \
                   3156)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1883)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1885)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1889)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1890)(content(Whitespace\" \"))))(Tile((id \
                   1892)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   1893)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3119)(content(Whitespace\" \"))))(Grout((id 1898)(shape \
                   Convex)))(Secondary((id 1895)(content(Whitespace\" \
                   \")))))((Secondary((id 1899)(content(Whitespace\" \
                   \"))))(Tile((id 1903)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1904)(content(Whitespace\" \
                   \"))))(Tile((id 1906)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   1907)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1909)(content(Whitespace\" \"))))(Tile((id \
                   1913)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1914)(content(Whitespace\" \")))))))))(Secondary((id \
                   1917)(content(Whitespace\" \"))))(Tile((id \
                   1918)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1919)(content(Whitespace\" \"))))(Tile((id \
                   1922)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1923)(content(Whitespace\" \"))))(Tile((id \
                   1927)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1930)(content(Whitespace\" \")))))))))(Secondary((id \
                   1932)(content(Whitespace\" \"))))(Tile((id \
                   1933)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1934)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1939)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2754)(content(Whitespace\" \"))))(Tile((id \
                   3142)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3157)(content(Whitespace\" \"))))(Tile((id \
                   3161)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1940)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1942)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1946)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1947)(content(Whitespace\" \"))))(Tile((id \
                   1949)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   1950)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1952)(content(Whitespace\" \"))))(Grout((id 1957)(shape \
                   Convex)))(Secondary((id 3120)(content(Whitespace\" \
                   \"))))(Tile((id 1956)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3121)(content(Whitespace\" \"))))(Grout((id 1961)(shape \
                   Convex)))(Secondary((id 1958)(content(Whitespace\" \
                   \")))))((Secondary((id 1962)(content(Whitespace\" \
                   \"))))(Tile((id 1966)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1967)(content(Whitespace\" \
                   \"))))(Tile((id 1969)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1970)(content(Whitespace\" \")))))))))(Secondary((id \
                   1973)(content(Whitespace\" \"))))(Tile((id \
                   1974)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1975)(content(Whitespace\" \"))))(Tile((id \
                   1978)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1979)(content(Whitespace\" \"))))(Tile((id \
                   1983)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1986)(content(Whitespace\" \")))))))))(Secondary((id \
                   1988)(content(Whitespace\" \"))))(Tile((id \
                   1989)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1990)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1995)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2757)(content(Whitespace\" \"))))(Tile((id \
                   2760)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2770)(content(Whitespace\" \"))))(Tile((id \
                   2768)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1996)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1998)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2002)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2003)(content(Whitespace\" \"))))(Tile((id \
                   2005)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2006)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2008)(content(Whitespace\" \"))))(Grout((id 2013)(shape \
                   Convex)))(Secondary((id 3122)(content(Whitespace\" \
                   \"))))(Tile((id 2012)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3123)(content(Whitespace\" \"))))(Grout((id 2017)(shape \
                   Convex)))(Secondary((id 2014)(content(Whitespace\" \
                   \")))))((Secondary((id 2018)(content(Whitespace\" \
                   \"))))(Tile((id 2022)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2023)(content(Whitespace\" \
                   \"))))(Tile((id 2025)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2026)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3127)(content(Whitespace\" \"))))(Grout((id 2032)(shape \
                   Convex)))(Secondary((id 2028)(content(Whitespace\" \
                   \")))))))))(Secondary((id 2033)(content(Whitespace\" \
                   \"))))(Tile((id 2034)(label(b))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2035)(content(Whitespace\" \"))))(Tile((id \
                   2038)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2039)(content(Whitespace\" \"))))(Tile((id \
                   2043)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2046)(content(Whitespace\" \")))))))))(Secondary((id \
                   2048)(content(Whitespace\" \"))))(Tile((id \
                   2049)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2050)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2055)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2761)(content(Whitespace\" \"))))(Tile((id \
                   2764)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2771)(content(Whitespace\" \"))))(Tile((id \
                   2775)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2056)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2058)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2062)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2063)(content(Whitespace\" \"))))(Tile((id \
                   2065)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2066)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2068)(content(Whitespace\" \"))))(Grout((id 2073)(shape \
                   Convex)))(Secondary((id 3124)(content(Whitespace\" \
                   \"))))(Tile((id 2072)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3125)(content(Whitespace\" \"))))(Grout((id 2077)(shape \
                   Convex)))(Secondary((id 2074)(content(Whitespace\" \
                   \")))))((Secondary((id 2078)(content(Whitespace\" \
                   \"))))(Tile((id 2082)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2083)(content(Whitespace\" \
                   \"))))(Tile((id 2085)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2086)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2088)(content(Whitespace\" \"))))(Tile((id \
                   6955)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2092)(content(Whitespace\" \")))))))))(Secondary((id \
                   2095)(content(Whitespace\" \"))))(Tile((id \
                   2096)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2097)(content(Whitespace\" \"))))(Tile((id \
                   2100)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2101)(content(Whitespace\" \"))))(Tile((id \
                   2105)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2108)(content(Whitespace\" \")))))))))(Secondary((id \
                   2110)(content(Whitespace\" \"))))(Tile((id \
                   2111)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2112)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2117)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2777)(content(Whitespace\" \"))))(Tile((id \
                   2780)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2781)(content(Whitespace\" \"))))(Tile((id \
                   2785)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2787)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2125)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2129)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2130)(content(Whitespace\" \"))))(Tile((id \
                   2132)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2133)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2135)(content(Whitespace\" \"))))(Tile((id \
                   2139)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2140)(content(Whitespace\" \"))))(Tile((id \
                   2143)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3126)(content(Whitespace\" \"))))(Grout((id 2147)(shape \
                   Convex)))(Secondary((id 2144)(content(Whitespace\" \
                   \")))))((Secondary((id 2148)(content(Whitespace\" \
                   \"))))(Tile((id 2152)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2153)(content(Whitespace\" \
                   \"))))(Tile((id 2155)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2156)(content(Whitespace\" \")))))))))(Secondary((id \
                   2159)(content(Whitespace\" \"))))(Tile((id \
                   2160)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2161)(content(Whitespace\" \"))))(Tile((id \
                   2164)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2165)(content(Whitespace\" \"))))(Tile((id \
                   2169)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2172)(content(Whitespace\" \")))))))))(Secondary((id \
                   2174)(content(Whitespace\" \"))))(Tile((id \
                   2175)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2176)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2181)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2794)(content(Whitespace\" \"))))(Tile((id \
                   2797)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2798)(content(Whitespace\" \"))))(Tile((id \
                   2802)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2182)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2184)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2188)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2189)(content(Whitespace\" \"))))(Tile((id \
                   2191)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2192)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2194)(content(Whitespace\" \"))))(Tile((id \
                   2198)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2199)(content(Whitespace\" \"))))(Tile((id \
                   2202)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3128)(content(Whitespace\" \"))))(Grout((id 2206)(shape \
                   Convex)))(Secondary((id 2203)(content(Whitespace\" \
                   \")))))((Secondary((id 2207)(content(Whitespace\" \
                   \"))))(Tile((id 2211)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2212)(content(Whitespace\" \
                   \"))))(Tile((id 2214)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2215)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3130)(content(Whitespace\" \"))))(Grout((id 2221)(shape \
                   Convex)))(Secondary((id 2217)(content(Whitespace\" \
                   \")))))))))(Secondary((id 2222)(content(Whitespace\" \
                   \"))))(Tile((id 2223)(label(b))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2224)(content(Whitespace\" \"))))(Tile((id \
                   2227)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2228)(content(Whitespace\" \"))))(Tile((id \
                   2232)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2235)(content(Whitespace\" \")))))))))(Secondary((id \
                   2237)(content(Whitespace\" \"))))(Tile((id \
                   2238)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2239)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2244)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2809)(content(Whitespace\" \"))))(Tile((id \
                   2812)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2813)(content(Whitespace\" \"))))(Tile((id \
                   2817)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2245)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2247)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2251)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2252)(content(Whitespace\" \"))))(Tile((id \
                   2254)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2255)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2257)(content(Whitespace\" \"))))(Tile((id \
                   2261)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2262)(content(Whitespace\" \"))))(Tile((id \
                   2265)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3129)(content(Whitespace\" \"))))(Grout((id 2269)(shape \
                   Convex)))(Secondary((id 2266)(content(Whitespace\" \
                   \")))))((Secondary((id 2270)(content(Whitespace\" \
                   \"))))(Tile((id 2274)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2275)(content(Whitespace\" \
                   \"))))(Tile((id 2277)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2278)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2280)(content(Whitespace\" \"))))(Tile((id \
                   2284)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2285)(content(Whitespace\" \")))))))))(Secondary((id \
                   2288)(content(Whitespace\" \"))))(Tile((id \
                   2289)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2290)(content(Whitespace\" \"))))(Tile((id \
                   2293)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2294)(content(Whitespace\" \"))))(Tile((id \
                   2298)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2301)(content(Whitespace\" \")))))))))(Secondary((id \
                   2303)(content(Whitespace\" \"))))(Tile((id \
                   2304)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2305)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2310)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2818)(content(Whitespace\" \"))))(Tile((id \
                   2821)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2822)(content(Whitespace\" \"))))(Tile((id \
                   2826)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2311)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2313)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2317)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2318)(content(Whitespace\" \"))))(Tile((id \
                   2320)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2321)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2323)(content(Whitespace\" \"))))(Tile((id \
                   2327)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2328)(content(Whitespace\" \"))))(Tile((id \
                   2331)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2332)(content(Whitespace\" \"))))(Tile((id \
                   2336)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2337)(content(Whitespace\" \")))))((Secondary((id \
                   2339)(content(Whitespace\" \"))))(Tile((id 2343)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2344)(content(Whitespace\" \
                   \"))))(Tile((id 2346)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2347)(content(Whitespace\" \")))))))))(Secondary((id \
                   2350)(content(Whitespace\" \"))))(Tile((id \
                   2351)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2352)(content(Whitespace\" \"))))(Tile((id \
                   2355)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2356)(content(Whitespace\" \"))))(Tile((id \
                   2360)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2363)(content(Whitespace\" \")))))))))(Secondary((id \
                   2365)(content(Whitespace\" \"))))(Tile((id \
                   2366)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2367)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2372)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2827)(content(Whitespace\" \"))))(Tile((id \
                   2830)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2831)(content(Whitespace\" \"))))(Tile((id \
                   2835)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2373)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2375)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2379)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2380)(content(Whitespace\" \"))))(Tile((id \
                   2382)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2383)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2385)(content(Whitespace\" \"))))(Tile((id \
                   2389)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2390)(content(Whitespace\" \"))))(Tile((id \
                   2393)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2394)(content(Whitespace\" \"))))(Tile((id \
                   2398)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2399)(content(Whitespace\" \")))))((Secondary((id \
                   2401)(content(Whitespace\" \"))))(Tile((id 2405)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2406)(content(Whitespace\" \
                   \"))))(Tile((id 2408)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2409)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3131)(content(Whitespace\" \"))))(Grout((id 2415)(shape \
                   Convex)))(Secondary((id 2411)(content(Whitespace\" \
                   \")))))))))(Secondary((id 2416)(content(Whitespace\" \
                   \"))))(Tile((id 2417)(label(b))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2418)(content(Whitespace\" \"))))(Tile((id \
                   2421)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2422)(content(Whitespace\" \"))))(Tile((id \
                   2426)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2429)(content(Whitespace\" \")))))))))(Secondary((id \
                   2431)(content(Whitespace\" \"))))(Tile((id \
                   2432)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2433)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2438)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2836)(content(Whitespace\" \"))))(Tile((id \
                   2839)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2840)(content(Whitespace\" \"))))(Tile((id \
                   2844)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2439)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2441)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2445)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2446)(content(Whitespace\" \"))))(Tile((id \
                   2448)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2449)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2451)(content(Whitespace\" \"))))(Tile((id \
                   2455)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2456)(content(Whitespace\" \"))))(Tile((id \
                   2459)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2460)(content(Whitespace\" \"))))(Tile((id \
                   2464)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2465)(content(Whitespace\" \")))))((Secondary((id \
                   2467)(content(Whitespace\" \"))))(Tile((id 2471)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2472)(content(Whitespace\" \
                   \"))))(Tile((id 2474)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2475)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2477)(content(Whitespace\" \"))))(Tile((id \
                   2481)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2482)(content(Whitespace\" \")))))))))(Secondary((id \
                   2485)(content(Whitespace\" \"))))(Tile((id \
                   2486)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2487)(content(Whitespace\" \"))))(Tile((id \
                   2490)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2491)(content(Whitespace\" \"))))(Tile((id \
                   2495)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2498)(content(Whitespace\" \")))))))))(Secondary((id \
                   2500)(content(Whitespace\" \"))))(Tile((id \
                   2501)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2502)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2507)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2845)(content(Whitespace\" \"))))(Tile((id \
                   2848)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2849)(content(Whitespace\" \"))))(Tile((id \
                   2853)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2508)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2510)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2514)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2515)(content(Whitespace\" \"))))(Tile((id \
                   2517)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2518)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2520)(content(Whitespace\" \"))))(Grout((id 2525)(shape \
                   Convex)))(Secondary((id 3134)(content(Whitespace\" \
                   \"))))(Tile((id 2524)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2526)(content(Whitespace\" \"))))(Tile((id \
                   2530)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2531)(content(Whitespace\" \")))))((Secondary((id \
                   2533)(content(Whitespace\" \"))))(Tile((id 2537)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2538)(content(Whitespace\" \
                   \"))))(Tile((id 2540)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2541)(content(Whitespace\" \")))))))))(Secondary((id \
                   2544)(content(Whitespace\" \"))))(Tile((id \
                   2545)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2546)(content(Whitespace\" \"))))(Tile((id \
                   2549)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2550)(content(Whitespace\" \"))))(Tile((id \
                   2554)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2557)(content(Whitespace\" \")))))))))(Secondary((id \
                   2559)(content(Whitespace\" \"))))(Tile((id \
                   2560)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2561)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2566)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2854)(content(Whitespace\" \"))))(Tile((id \
                   2857)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2858)(content(Whitespace\" \"))))(Tile((id \
                   2862)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2567)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2569)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2573)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2574)(content(Whitespace\" \"))))(Tile((id \
                   2576)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2577)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2579)(content(Whitespace\" \"))))(Grout((id 2584)(shape \
                   Convex)))(Secondary((id 3133)(content(Whitespace\" \
                   \"))))(Tile((id 2583)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2585)(content(Whitespace\" \"))))(Tile((id \
                   2589)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2590)(content(Whitespace\" \")))))((Secondary((id \
                   2592)(content(Whitespace\" \"))))(Tile((id 2596)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2597)(content(Whitespace\" \
                   \"))))(Tile((id 2599)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2600)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3135)(content(Whitespace\" \"))))(Grout((id 2606)(shape \
                   Convex)))(Secondary((id 2602)(content(Whitespace\" \
                   \")))))))))(Secondary((id 2607)(content(Whitespace\" \
                   \"))))(Tile((id 2608)(label(b))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2609)(content(Whitespace\" \"))))(Tile((id \
                   2612)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2613)(content(Whitespace\" \"))))(Tile((id \
                   2617)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2620)(content(Whitespace\" \")))))))))(Secondary((id \
                   2622)(content(Whitespace\" \"))))(Tile((id \
                   2623)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2624)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2629)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2863)(content(Whitespace\" \"))))(Tile((id \
                   2866)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2867)(content(Whitespace\" \"))))(Tile((id \
                   2871)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2630)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2632)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2636)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2637)(content(Whitespace\" \"))))(Tile((id \
                   2639)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2640)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2642)(content(Whitespace\" \"))))(Grout((id 2647)(shape \
                   Convex)))(Secondary((id 3132)(content(Whitespace\" \
                   \"))))(Tile((id 2646)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2648)(content(Whitespace\" \"))))(Tile((id \
                   2652)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2653)(content(Whitespace\" \")))))((Secondary((id \
                   2655)(content(Whitespace\" \"))))(Tile((id 2659)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2660)(content(Whitespace\" \
                   \"))))(Tile((id 2662)(label(b))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2663)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2665)(content(Whitespace\" \"))))(Tile((id \
                   6956)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2669)(content(Whitespace\" \")))))))))(Secondary((id \
                   2672)(content(Whitespace\" \"))))(Tile((id \
                   2673)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2674)(content(Whitespace\" \"))))(Tile((id \
                   2677)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2678)(content(Whitespace\" \"))))(Tile((id \
                   2682)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2685)(content(Whitespace\" \")))))))))(Secondary((id \
                   2687)(content(Whitespace\" \"))))(Tile((id \
                   2688)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2689)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2694)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2872)(content(Whitespace\" \"))))(Tile((id \
                   2875)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2876)(content(Whitespace\" \"))))(Tile((id \
                   2882)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2695)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4178)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4179)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4183)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4184)(content(Whitespace\" \"))))(Tile((id \
                   4186)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4187)(content(Whitespace\" \")))))((Secondary((id \
                   4189)(content(Whitespace\" \"))))(Tile((id 4193)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 4194)(content(Whitespace\" \
                   \"))))(Tile((id 5280)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   5250)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5283)(content(Whitespace\" \"))))(Tile((id \
                   5282)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4197)(content(Whitespace\" \")))))))))(Secondary((id \
                   4200)(content(Whitespace\" \"))))(Tile((id \
                   5284)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5285)(content(Whitespace\" \"))))(Tile((id \
                   5286)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5288)(content(Whitespace\" \"))))(Tile((id \
                   5289)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4213)(content(Whitespace\" \")))))))))(Secondary((id \
                   4215)(content(Whitespace\" \"))))(Tile((id \
                   4216)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4217)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5253)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5254)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5970)(content(Whitespace\" \"))))(Tile((id \
                   5256)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   4223)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4225)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4229)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4230)(content(Whitespace\" \"))))(Tile((id \
                   4232)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4233)(content(Whitespace\" \")))))((Secondary((id \
                   4235)(content(Whitespace\" \"))))(Tile((id 4239)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 4240)(content(Whitespace\" \
                   \"))))(Tile((id 5298)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   4243)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5274)(content(Whitespace\" \"))))(Grout((id 4249)(shape \
                   Convex)))(Tile((id 5257)(label(,))(mold((out \
                   Pat)(in_())(nibs(((shape(Concave 14))(sort \
                   Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5275)(content(Whitespace\" \"))))(Tile((id \
                   5300)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4246)(content(Whitespace\" \"))))(Secondary((id \
                   4247)(content(Whitespace\" \")))))))))(Secondary((id \
                   4250)(content(Whitespace\" \"))))(Tile((id \
                   5290)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5291)(content(Whitespace\" \"))))(Tile((id \
                   5292)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5294)(content(Whitespace\" \"))))(Tile((id \
                   5295)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4263)(content(Whitespace\" \")))))))))(Secondary((id \
                   4265)(content(Whitespace\" \"))))(Tile((id \
                   4266)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4267)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5260)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5261)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5967)(content(Whitespace\" \"))))(Tile((id \
                   5263)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   4273)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4275)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4279)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4280)(content(Whitespace\" \"))))(Tile((id \
                   4282)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4283)(content(Whitespace\" \")))))((Secondary((id \
                   4285)(content(Whitespace\" \"))))(Tile((id 4289)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 4290)(content(Whitespace\" \
                   \"))))(Tile((id 5302)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   4293)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4295)(content(Whitespace\" \"))))(Tile((id \
                   5273)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5264)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5276)(content(Whitespace\" \"))))(Tile((id \
                   5304)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4300)(content(Whitespace\" \")))))))))(Secondary((id \
                   4303)(content(Whitespace\" \"))))(Tile((id \
                   5305)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5306)(content(Whitespace\" \"))))(Tile((id \
                   5307)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5309)(content(Whitespace\" \"))))(Tile((id \
                   5310)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4316)(content(Whitespace\" \")))))))))(Secondary((id \
                   4318)(content(Whitespace\" \"))))(Tile((id \
                   4319)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4320)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5267)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5268)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5966)(content(Whitespace\" \"))))(Tile((id \
                   5270)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   4326)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6414)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6419)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6420)(content(Whitespace\" \"))))(Tile((id \
                   6421)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6423)(content(Whitespace\" \")))))((Secondary((id \
                   6424)(content(Whitespace\" \"))))(Tile((id 6429)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 6430)(content(Whitespace\" \
                   \"))))(Tile((id 6471)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   6431)(label(a))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6439)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6440)(content(Whitespace\" \"))))(Tile((id \
                   6441)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   6472)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6488)(content(Whitespace\" \"))))(Tile((id \
                   6476)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   6479)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6480)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6485)(content(Whitespace\" \"))))(Grout((id 6481)(shape \
                   Convex))))))))(Secondary((id 6484)(content(Whitespace\" \
                   \")))))))))(Secondary((id 6445)(content(Whitespace\" \
                   \"))))(Tile((id 6446)(label(a))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6448)(content(Whitespace\" \"))))(Tile((id \
                   6449)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6450)(content(Whitespace\" \"))))(Tile((id \
                   6451)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6455)(content(Whitespace\" \")))))))))(Secondary((id \
                   6456)(content(Whitespace\" \"))))(Tile((id \
                   6457)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6459)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   6460)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6462)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6463)(content(Whitespace\" \"))))(Tile((id \
                   6464)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   6466)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4328)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4332)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4333)(content(Whitespace\" \"))))(Tile((id \
                   4335)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   4336)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5311)(content(Whitespace\" \"))))(Grout((id 4341)(shape \
                   Convex)))(Secondary((id 4338)(content(Whitespace\" \
                   \")))))((Secondary((id 4342)(content(Whitespace\" \
                   \"))))(Tile((id 5318)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 5319)(content(Whitespace\" \
                   \"))))(Tile((id 5320)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   5322)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5323)(content(Whitespace\" \"))))(Tile((id \
                   5324)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5326)(content(Whitespace\" \")))))))))(Secondary((id \
                   5328)(content(Whitespace\" \"))))(Tile((id \
                   5329)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5331)(content(Whitespace\" \"))))(Tile((id \
                   5332)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5333)(content(Whitespace\" \"))))(Tile((id \
                   5334)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5338)(content(Whitespace\" \")))))))))(Secondary((id \
                   5339)(content(Whitespace\" \"))))(Tile((id \
                   5340)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5342)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5343)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5345)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5965)(content(Whitespace\" \"))))(Tile((id \
                   5346)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   5348)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4378)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4382)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4383)(content(Whitespace\" \"))))(Tile((id \
                   4385)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   4386)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5312)(content(Whitespace\" \"))))(Grout((id 4391)(shape \
                   Convex)))(Secondary((id 4388)(content(Whitespace\" \
                   \")))))((Secondary((id 4392)(content(Whitespace\" \
                   \"))))(Tile((id 5354)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 5355)(content(Whitespace\" \
                   \"))))(Tile((id 5356)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   5358)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5359)(content(Whitespace\" \"))))(Grout((id 5362)(shape \
                   Convex)))(Tile((id 5361)(label(,))(mold((out \
                   Pat)(in_())(nibs(((shape(Concave 14))(sort \
                   Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5363)(content(Whitespace\" \"))))(Tile((id \
                   5364)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5366)(content(Whitespace\" \"))))(Secondary((id \
                   5367)(content(Whitespace\" \")))))))))(Secondary((id \
                   5369)(content(Whitespace\" \"))))(Tile((id \
                   5370)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5372)(content(Whitespace\" \"))))(Tile((id \
                   5373)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5374)(content(Whitespace\" \"))))(Tile((id \
                   5375)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5379)(content(Whitespace\" \")))))))))(Secondary((id \
                   5380)(content(Whitespace\" \"))))(Tile((id \
                   5381)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5383)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5384)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5386)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5964)(content(Whitespace\" \"))))(Tile((id \
                   5387)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   5389)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4432)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4436)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4437)(content(Whitespace\" \"))))(Tile((id \
                   4439)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   4440)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5313)(content(Whitespace\" \"))))(Grout((id 4445)(shape \
                   Convex)))(Secondary((id 4442)(content(Whitespace\" \
                   \")))))((Secondary((id 4446)(content(Whitespace\" \
                   \"))))(Tile((id 5395)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 5396)(content(Whitespace\" \
                   \"))))(Tile((id 5397)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   5399)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5400)(content(Whitespace\" \"))))(Tile((id \
                   5560)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5405)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5406)(content(Whitespace\" \"))))(Tile((id \
                   5407)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5409)(content(Whitespace\" \")))))))))(Secondary((id \
                   5411)(content(Whitespace\" \"))))(Tile((id \
                   5412)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5414)(content(Whitespace\" \"))))(Tile((id \
                   5415)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5416)(content(Whitespace\" \"))))(Tile((id \
                   5417)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5421)(content(Whitespace\" \")))))))))(Secondary((id \
                   5422)(content(Whitespace\" \"))))(Tile((id \
                   5423)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5425)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5426)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5428)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5963)(content(Whitespace\" \"))))(Tile((id \
                   5429)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   5431)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6489)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6494)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6495)(content(Whitespace\" \"))))(Tile((id \
                   6496)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6550)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6552)(content(Whitespace\" \"))))(Grout((id 6551)(shape \
                   Convex)))(Secondary((id 6498)(content(Whitespace\" \
                   \")))))((Secondary((id 6499)(content(Whitespace\" \
                   \"))))(Tile((id 6504)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 6505)(content(Whitespace\" \
                   \"))))(Tile((id 6506)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   6507)(label(a))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6509)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6510)(content(Whitespace\" \"))))(Tile((id \
                   6511)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   6513)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6514)(content(Whitespace\" \"))))(Tile((id \
                   6515)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   6519)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6520)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6553)(content(Whitespace\" \"))))(Grout((id 6523)(shape \
                   Convex))))))))(Secondary((id 6525)(content(Whitespace\" \
                   \")))))))))(Secondary((id 6527)(content(Whitespace\" \
                   \"))))(Tile((id 6528)(label(a))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6530)(content(Whitespace\" \"))))(Tile((id \
                   6531)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6532)(content(Whitespace\" \"))))(Tile((id \
                   6533)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6537)(content(Whitespace\" \")))))))))(Secondary((id \
                   6538)(content(Whitespace\" \"))))(Tile((id \
                   6539)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6541)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   6542)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6544)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6545)(content(Whitespace\" \"))))(Tile((id \
                   6546)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   6548)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4489)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4493)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4494)(content(Whitespace\" \"))))(Tile((id \
                   4496)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   4497)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4499)(content(Whitespace\" \"))))(Grout((id 4504)(shape \
                   Convex)))(Secondary((id 5433)(content(Whitespace\" \
                   \"))))(Tile((id 4503)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5438)(content(Whitespace\" \"))))(Grout((id 5439)(shape \
                   Convex)))(Secondary((id 4505)(content(Whitespace\" \
                   \")))))((Secondary((id 4509)(content(Whitespace\" \
                   \"))))(Tile((id 5444)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 5445)(content(Whitespace\" \
                   \"))))(Tile((id 5446)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   5448)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5449)(content(Whitespace\" \"))))(Tile((id \
                   5450)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5452)(content(Whitespace\" \")))))))))(Secondary((id \
                   5454)(content(Whitespace\" \"))))(Tile((id \
                   5455)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5457)(content(Whitespace\" \"))))(Tile((id \
                   5458)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5459)(content(Whitespace\" \"))))(Tile((id \
                   5460)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5464)(content(Whitespace\" \")))))))))(Secondary((id \
                   5465)(content(Whitespace\" \"))))(Tile((id \
                   5466)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5468)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5469)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5471)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5962)(content(Whitespace\" \"))))(Tile((id \
                   5472)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   5474)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4545)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4549)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4550)(content(Whitespace\" \"))))(Tile((id \
                   4552)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   4553)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4555)(content(Whitespace\" \"))))(Grout((id 4560)(shape \
                   Convex)))(Secondary((id 5434)(content(Whitespace\" \
                   \"))))(Tile((id 4559)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5437)(content(Whitespace\" \"))))(Grout((id 4564)(shape \
                   Convex)))(Secondary((id 4561)(content(Whitespace\" \
                   \")))))((Secondary((id 4565)(content(Whitespace\" \
                   \"))))(Tile((id 5480)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 5481)(content(Whitespace\" \
                   \"))))(Tile((id 5482)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   5484)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5485)(content(Whitespace\" \"))))(Secondary((id \
                   5486)(content(Whitespace\" \"))))(Grout((id 5488)(shape \
                   Convex)))(Tile((id 5487)(label(,))(mold((out \
                   Pat)(in_())(nibs(((shape(Concave 14))(sort \
                   Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5489)(content(Whitespace\" \"))))(Tile((id \
                   5490)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5492)(content(Whitespace\" \"))))(Secondary((id \
                   5493)(content(Whitespace\" \")))))))))(Secondary((id \
                   5495)(content(Whitespace\" \"))))(Tile((id \
                   5496)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5498)(content(Whitespace\" \"))))(Tile((id \
                   5499)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5500)(content(Whitespace\" \"))))(Tile((id \
                   5501)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5505)(content(Whitespace\" \")))))))))(Secondary((id \
                   5506)(content(Whitespace\" \"))))(Tile((id \
                   5507)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5509)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5510)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5512)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5961)(content(Whitespace\" \"))))(Tile((id \
                   5513)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   5515)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4605)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4609)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4610)(content(Whitespace\" \"))))(Tile((id \
                   4612)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   4613)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4615)(content(Whitespace\" \"))))(Grout((id 4620)(shape \
                   Convex)))(Secondary((id 5435)(content(Whitespace\" \
                   \"))))(Tile((id 4619)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5436)(content(Whitespace\" \"))))(Grout((id 4624)(shape \
                   Convex)))(Secondary((id 4621)(content(Whitespace\" \
                   \")))))((Secondary((id 4625)(content(Whitespace\" \
                   \"))))(Tile((id 5522)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 5523)(content(Whitespace\" \
                   \"))))(Tile((id 5524)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   5526)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5527)(content(Whitespace\" \"))))(Tile((id \
                   6957)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5531)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5532)(content(Whitespace\" \"))))(Tile((id \
                   5533)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5535)(content(Whitespace\" \")))))))))(Secondary((id \
                   5537)(content(Whitespace\" \"))))(Tile((id \
                   5538)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5540)(content(Whitespace\" \"))))(Tile((id \
                   5541)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5542)(content(Whitespace\" \"))))(Tile((id \
                   5543)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5547)(content(Whitespace\" \")))))))))(Secondary((id \
                   5548)(content(Whitespace\" \"))))(Tile((id \
                   5549)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5551)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5552)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5554)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5960)(content(Whitespace\" \"))))(Tile((id \
                   5555)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   5557)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6554)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6559)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6560)(content(Whitespace\" \"))))(Tile((id \
                   6561)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6615)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6617)(content(Whitespace\" \"))))(Grout((id 6616)(shape \
                   Convex)))(Secondary((id 6618)(content(Whitespace\" \
                   \"))))(Tile((id 6622)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6623)(content(Whitespace\" \"))))(Grout((id 6620)(shape \
                   Convex)))(Secondary((id 6563)(content(Whitespace\" \
                   \")))))((Secondary((id 6564)(content(Whitespace\" \
                   \"))))(Tile((id 6569)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 6570)(content(Whitespace\" \
                   \"))))(Tile((id 6571)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   6572)(label(a))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6574)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6575)(content(Whitespace\" \"))))(Tile((id \
                   6576)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   6578)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6579)(content(Whitespace\" \"))))(Tile((id \
                   6580)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   6958)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6585)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6624)(content(Whitespace\" \"))))(Grout((id 6588)(shape \
                   Convex))))))))(Secondary((id 6590)(content(Whitespace\" \
                   \")))))))))(Secondary((id 6592)(content(Whitespace\" \
                   \"))))(Tile((id 6593)(label(a))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6595)(content(Whitespace\" \"))))(Tile((id \
                   6596)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6597)(content(Whitespace\" \"))))(Tile((id \
                   6598)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6602)(content(Whitespace\" \")))))))))(Secondary((id \
                   6603)(content(Whitespace\" \"))))(Tile((id \
                   6604)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6606)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   6607)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6609)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6610)(content(Whitespace\" \"))))(Tile((id \
                   6611)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   6613)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5618)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5623)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5624)(content(Whitespace\" \"))))(Tile((id \
                   5625)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   5627)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5628)(content(Whitespace\" \"))))(Tile((id \
                   5803)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                   5805)(shape Convex)))(Tile((id 5804)(label(,))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 14))(sort \
                   Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5824)(content(Whitespace\" \"))))(Grout((id 5633)(shape \
                   Convex))))))))(Secondary((id 5821)(content(Whitespace\" \
                   \"))))(Tile((id 5632)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5822)(content(Whitespace\" \"))))(Grout((id 5637)(shape \
                   Convex)))(Secondary((id 5636)(content(Whitespace\" \
                   \")))))((Secondary((id 5638)(content(Whitespace\" \
                   \"))))(Tile((id 5643)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 5644)(content(Whitespace\" \
                   \"))))(Tile((id 5645)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   5647)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5648)(content(Whitespace\" \"))))(Tile((id \
                   5649)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5651)(content(Whitespace\" \")))))))))(Secondary((id \
                   5653)(content(Whitespace\" \"))))(Tile((id \
                   5654)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5656)(content(Whitespace\" \"))))(Tile((id \
                   5657)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5658)(content(Whitespace\" \"))))(Tile((id \
                   5659)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5663)(content(Whitespace\" \")))))))))(Secondary((id \
                   5664)(content(Whitespace\" \"))))(Tile((id \
                   5665)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5667)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5668)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5670)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5959)(content(Whitespace\" \"))))(Tile((id \
                   5671)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   5673)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5674)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5679)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5680)(content(Whitespace\" \"))))(Tile((id \
                   5681)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   5683)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5684)(content(Whitespace\" \"))))(Tile((id \
                   5806)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                   5820)(shape Convex)))(Tile((id 5808)(label(,))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 14))(sort \
                   Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5823)(content(Whitespace\" \"))))(Grout((id 5811)(shape \
                   Convex))))))))(Secondary((id 5685)(content(Whitespace\" \
                   \"))))(Tile((id 5688)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5826)(content(Whitespace\" \"))))(Grout((id 5693)(shape \
                   Convex)))(Secondary((id 5692)(content(Whitespace\" \
                   \")))))((Secondary((id 5694)(content(Whitespace\" \
                   \"))))(Tile((id 5699)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 5700)(content(Whitespace\" \
                   \"))))(Tile((id 5701)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   5703)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5704)(content(Whitespace\" \"))))(Grout((id 5708)(shape \
                   Convex)))(Tile((id 5707)(label(,))(mold((out \
                   Pat)(in_())(nibs(((shape(Concave 14))(sort \
                   Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5709)(content(Whitespace\" \"))))(Tile((id \
                   5710)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5712)(content(Whitespace\" \")))))))))(Secondary((id \
                   5715)(content(Whitespace\" \"))))(Tile((id \
                   5716)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5718)(content(Whitespace\" \"))))(Tile((id \
                   5719)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5720)(content(Whitespace\" \"))))(Tile((id \
                   5721)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5725)(content(Whitespace\" \")))))))))(Secondary((id \
                   5726)(content(Whitespace\" \"))))(Tile((id \
                   5727)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5729)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5730)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5732)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5958)(content(Whitespace\" \"))))(Tile((id \
                   5733)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   5735)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5736)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5741)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5742)(content(Whitespace\" \"))))(Tile((id \
                   5743)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   5745)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5746)(content(Whitespace\" \"))))(Tile((id \
                   5813)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                   5816)(shape Convex)))(Tile((id 5815)(label(,))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 14))(sort \
                   Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5825)(content(Whitespace\" \"))))(Grout((id 5818)(shape \
                   Convex))))))))(Secondary((id 5748)(content(Whitespace\" \
                   \"))))(Tile((id 5750)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5827)(content(Whitespace\" \"))))(Grout((id 5755)(shape \
                   Convex)))(Secondary((id 5754)(content(Whitespace\" \
                   \")))))((Secondary((id 5756)(content(Whitespace\" \
                   \"))))(Tile((id 5761)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 5762)(content(Whitespace\" \
                   \"))))(Tile((id 5763)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   5765)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5766)(content(Whitespace\" \"))))(Tile((id \
                   6959)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5770)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5771)(content(Whitespace\" \"))))(Tile((id \
                   5772)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5774)(content(Whitespace\" \")))))))))(Secondary((id \
                   5776)(content(Whitespace\" \"))))(Tile((id \
                   5777)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5779)(content(Whitespace\" \"))))(Tile((id \
                   5780)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5781)(content(Whitespace\" \"))))(Tile((id \
                   5782)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5786)(content(Whitespace\" \")))))))))(Secondary((id \
                   5787)(content(Whitespace\" \"))))(Tile((id \
                   5788)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5790)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5791)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5793)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5957)(content(Whitespace\" \"))))(Tile((id \
                   5794)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   5796)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6625)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6630)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6631)(content(Whitespace\" \"))))(Tile((id \
                   6632)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6634)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6635)(content(Whitespace\" \"))))(Tile((id \
                   6695)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                   6698)(shape Convex)))(Tile((id 6697)(label(,))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 14))(sort \
                   Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6700)(content(Whitespace\" \"))))(Grout((id 6699)(shape \
                   Convex))))))))(Secondary((id 6701)(content(Whitespace\" \
                   \"))))(Tile((id 6639)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6702)(content(Whitespace\" \"))))(Grout((id 6644)(shape \
                   Convex)))(Secondary((id 6641)(content(Whitespace\" \
                   \")))))((Secondary((id 6645)(content(Whitespace\" \
                   \"))))(Tile((id 6650)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 6651)(content(Whitespace\" \
                   \"))))(Tile((id 6652)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   6653)(label(a))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6655)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6656)(content(Whitespace\" \"))))(Tile((id \
                   6657)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   6659)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6660)(content(Whitespace\" \"))))(Tile((id \
                   6661)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   6960)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6665)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6703)(content(Whitespace\" \"))))(Grout((id 6668)(shape \
                   Convex))))))))(Secondary((id 6670)(content(Whitespace\" \
                   \")))))))))(Secondary((id 6672)(content(Whitespace\" \
                   \"))))(Tile((id 6673)(label(a))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6675)(content(Whitespace\" \"))))(Tile((id \
                   6676)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6677)(content(Whitespace\" \"))))(Tile((id \
                   6678)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6682)(content(Whitespace\" \")))))))))(Secondary((id \
                   6683)(content(Whitespace\" \"))))(Tile((id \
                   6684)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6686)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   6687)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6689)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6690)(content(Whitespace\" \"))))(Tile((id \
                   6691)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   6693)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4672)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4676)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4677)(content(Whitespace\" \"))))(Tile((id \
                   4679)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   4680)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4682)(content(Whitespace\" \"))))(Tile((id \
                   5564)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   5567)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5568)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5570)(content(Whitespace\" \"))))(Grout((id 5569)(shape \
                   Convex))))))))(Secondary((id 5571)(content(Whitespace\" \
                   \"))))(Tile((id 4690)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5572)(content(Whitespace\" \"))))(Grout((id 4694)(shape \
                   Convex)))(Secondary((id 4691)(content(Whitespace\" \
                   \")))))((Secondary((id 4695)(content(Whitespace\" \
                   \"))))(Tile((id 5832)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 5833)(content(Whitespace\" \
                   \"))))(Tile((id 5834)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   5836)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5837)(content(Whitespace\" \"))))(Tile((id \
                   5838)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5840)(content(Whitespace\" \")))))))))(Secondary((id \
                   5842)(content(Whitespace\" \"))))(Tile((id \
                   5843)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5845)(content(Whitespace\" \"))))(Tile((id \
                   5846)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5847)(content(Whitespace\" \"))))(Tile((id \
                   5848)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5852)(content(Whitespace\" \")))))))))(Secondary((id \
                   5853)(content(Whitespace\" \"))))(Tile((id \
                   5854)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5856)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5857)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5859)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5956)(content(Whitespace\" \"))))(Tile((id \
                   5860)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   5862)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4731)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4735)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4736)(content(Whitespace\" \"))))(Tile((id \
                   4738)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   5573)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5575)(content(Whitespace\" \"))))(Tile((id \
                   5576)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   5579)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5580)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5613)(content(Whitespace\" \"))))(Grout((id 5584)(shape \
                   Convex))))))))(Secondary((id 5585)(content(Whitespace\" \
                   \"))))(Tile((id 5588)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5589)(content(Whitespace\" \"))))(Grout((id 5587)(shape \
                   Convex)))(Secondary((id 5616)(content(Whitespace\" \
                   \")))))((Secondary((id 4754)(content(Whitespace\" \
                   \"))))(Tile((id 5868)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 5869)(content(Whitespace\" \
                   \"))))(Tile((id 5870)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   5872)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5873)(content(Whitespace\" \"))))(Grout((id 5878)(shape \
                   Convex)))(Tile((id 5877)(label(,))(mold((out \
                   Pat)(in_())(nibs(((shape(Concave 14))(sort \
                   Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5879)(content(Whitespace\" \"))))(Tile((id \
                   5880)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5882)(content(Whitespace\" \"))))(Secondary((id \
                   5883)(content(Whitespace\" \")))))))))(Secondary((id \
                   5885)(content(Whitespace\" \"))))(Tile((id \
                   5886)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5888)(content(Whitespace\" \"))))(Tile((id \
                   5889)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5890)(content(Whitespace\" \"))))(Tile((id \
                   5891)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5895)(content(Whitespace\" \")))))))))(Secondary((id \
                   5896)(content(Whitespace\" \"))))(Tile((id \
                   5897)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5899)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5900)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5902)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5955)(content(Whitespace\" \"))))(Tile((id \
                   5903)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   5905)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4794)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4798)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4799)(content(Whitespace\" \"))))(Tile((id \
                   4801)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   5593)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5595)(content(Whitespace\" \"))))(Tile((id \
                   5596)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   5599)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5600)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5614)(content(Whitespace\" \"))))(Grout((id 5615)(shape \
                   Convex))))))))(Secondary((id 5605)(content(Whitespace\" \
                   \"))))(Tile((id 5608)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5609)(content(Whitespace\" \"))))(Grout((id 5607)(shape \
                   Convex)))(Secondary((id 5617)(content(Whitespace\" \
                   \")))))((Secondary((id 4817)(content(Whitespace\" \
                   \"))))(Tile((id 5911)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 5912)(content(Whitespace\" \
                   \"))))(Tile((id 5913)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   5915)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5916)(content(Whitespace\" \"))))(Tile((id \
                   5953)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5920)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5921)(content(Whitespace\" \"))))(Tile((id \
                   5922)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5924)(content(Whitespace\" \")))))))))(Secondary((id \
                   5926)(content(Whitespace\" \"))))(Tile((id \
                   5927)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5929)(content(Whitespace\" \"))))(Tile((id \
                   5930)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5931)(content(Whitespace\" \"))))(Tile((id \
                   5932)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5936)(content(Whitespace\" \")))))))))(Secondary((id \
                   5937)(content(Whitespace\" \"))))(Tile((id \
                   5938)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5940)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5941)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5943)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5954)(content(Whitespace\" \"))))(Tile((id \
                   5944)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   5946)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6704)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6709)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6710)(content(Whitespace\" \"))))(Tile((id \
                   6711)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6765)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6767)(content(Whitespace\" \"))))(Tile((id \
                   6768)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   6771)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6772)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6781)(content(Whitespace\" \"))))(Grout((id 6775)(shape \
                   Convex))))))))(Secondary((id 6774)(content(Whitespace\" \
                   \"))))(Tile((id 6779)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6780)(content(Whitespace\" \"))))(Grout((id 6778)(shape \
                   Convex)))(Secondary((id 6713)(content(Whitespace\" \
                   \")))))((Secondary((id 6714)(content(Whitespace\" \
                   \"))))(Tile((id 6719)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 6720)(content(Whitespace\" \
                   \"))))(Tile((id 6721)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   6722)(label(a))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6724)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6725)(content(Whitespace\" \"))))(Tile((id \
                   6726)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   6728)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6729)(content(Whitespace\" \"))))(Tile((id \
                   6730)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   6734)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6735)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6782)(content(Whitespace\" \"))))(Grout((id 6738)(shape \
                   Convex))))))))(Secondary((id 6740)(content(Whitespace\" \
                   \")))))))))(Secondary((id 6742)(content(Whitespace\" \
                   \"))))(Tile((id 6743)(label(a))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6745)(content(Whitespace\" \"))))(Tile((id \
                   6746)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6747)(content(Whitespace\" \"))))(Tile((id \
                   6748)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6752)(content(Whitespace\" \")))))))))(Secondary((id \
                   6753)(content(Whitespace\" \"))))(Tile((id \
                   6754)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6756)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   6757)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6759)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6760)(content(Whitespace\" \"))))(Tile((id \
                   6761)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   6763)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4860)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5975)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5976)(content(Whitespace\" \"))))(Tile((id \
                   5977)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   5979)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5980)(content(Whitespace\" \"))))(Tile((id \
                   5981)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   5985)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5986)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6192)(content(Whitespace\" \"))))(Grout((id 5989)(shape \
                   Convex))))))))(Secondary((id 5991)(content(Whitespace\" \
                   \"))))(Tile((id 5993)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6180)(content(Whitespace\" \"))))(Tile((id \
                   6183)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5996)(content(Whitespace\" \")))))((Secondary((id \
                   5998)(content(Whitespace\" \"))))(Tile((id 6003)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 6004)(content(Whitespace\" \
                   \"))))(Tile((id 6005)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   6007)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6008)(content(Whitespace\" \"))))(Tile((id \
                   6009)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6011)(content(Whitespace\" \")))))))))(Secondary((id \
                   6013)(content(Whitespace\" \"))))(Tile((id \
                   6014)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6016)(content(Whitespace\" \"))))(Tile((id \
                   6017)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6018)(content(Whitespace\" \"))))(Tile((id \
                   6019)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6023)(content(Whitespace\" \")))))))))(Secondary((id \
                   6024)(content(Whitespace\" \"))))(Tile((id \
                   6025)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6027)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   6028)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6030)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6031)(content(Whitespace\" \"))))(Tile((id \
                   6032)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   6034)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6035)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6040)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6041)(content(Whitespace\" \"))))(Tile((id \
                   6042)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6044)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6045)(content(Whitespace\" \"))))(Tile((id \
                   6046)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   6050)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6051)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6193)(content(Whitespace\" \"))))(Grout((id 6054)(shape \
                   Convex))))))))(Secondary((id 6056)(content(Whitespace\" \
                   \"))))(Tile((id 6058)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6184)(content(Whitespace\" \"))))(Tile((id \
                   6187)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6061)(content(Whitespace\" \")))))((Secondary((id \
                   6063)(content(Whitespace\" \"))))(Tile((id 6068)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 6069)(content(Whitespace\" \
                   \"))))(Tile((id 6070)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   6072)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6073)(content(Whitespace\" \"))))(Grout((id 6079)(shape \
                   Convex)))(Tile((id 6078)(label(,))(mold((out \
                   Pat)(in_())(nibs(((shape(Concave 14))(sort \
                   Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6080)(content(Whitespace\" \"))))(Tile((id \
                   6081)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6083)(content(Whitespace\" \"))))(Secondary((id \
                   6084)(content(Whitespace\" \")))))))))(Secondary((id \
                   6086)(content(Whitespace\" \"))))(Tile((id \
                   6087)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6089)(content(Whitespace\" \"))))(Tile((id \
                   6090)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6091)(content(Whitespace\" \"))))(Tile((id \
                   6092)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6096)(content(Whitespace\" \")))))))))(Secondary((id \
                   6097)(content(Whitespace\" \"))))(Tile((id \
                   6098)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6100)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   6101)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6103)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6104)(content(Whitespace\" \"))))(Tile((id \
                   6105)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   6107)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6108)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6113)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6114)(content(Whitespace\" \"))))(Tile((id \
                   6115)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6117)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6118)(content(Whitespace\" \"))))(Tile((id \
                   6119)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   6123)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6124)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6194)(content(Whitespace\" \"))))(Grout((id 6127)(shape \
                   Convex))))))))(Secondary((id 6129)(content(Whitespace\" \
                   \"))))(Tile((id 6131)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6188)(content(Whitespace\" \"))))(Tile((id \
                   6191)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6134)(content(Whitespace\" \")))))((Secondary((id \
                   6136)(content(Whitespace\" \"))))(Tile((id 6141)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 6142)(content(Whitespace\" \
                   \"))))(Tile((id 6143)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   6145)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6146)(content(Whitespace\" \"))))(Tile((id \
                   6150)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6151)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6152)(content(Whitespace\" \"))))(Tile((id \
                   6153)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6155)(content(Whitespace\" \")))))))))(Secondary((id \
                   6157)(content(Whitespace\" \"))))(Tile((id \
                   6158)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6160)(content(Whitespace\" \"))))(Tile((id \
                   6161)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6162)(content(Whitespace\" \"))))(Tile((id \
                   6163)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6167)(content(Whitespace\" \")))))))))(Secondary((id \
                   6168)(content(Whitespace\" \"))))(Tile((id \
                   6169)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6171)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   6172)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6174)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6175)(content(Whitespace\" \"))))(Tile((id \
                   6176)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   6178)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6783)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6788)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6789)(content(Whitespace\" \"))))(Tile((id \
                   6790)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6792)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6793)(content(Whitespace\" \"))))(Tile((id \
                   6794)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   6798)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6799)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6862)(content(Whitespace\" \"))))(Grout((id 6802)(shape \
                   Convex))))))))(Secondary((id 6804)(content(Whitespace\" \
                   \"))))(Tile((id 6806)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6863)(content(Whitespace\" \"))))(Tile((id \
                   6867)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6809)(content(Whitespace\" \")))))((Secondary((id \
                   6811)(content(Whitespace\" \"))))(Tile((id 6816)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 6817)(content(Whitespace\" \
                   \"))))(Tile((id 6818)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   6819)(label(a))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6821)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6822)(content(Whitespace\" \"))))(Tile((id \
                   6823)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   6825)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6826)(content(Whitespace\" \"))))(Tile((id \
                   6827)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   6831)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6832)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6868)(content(Whitespace\" \"))))(Grout((id 6835)(shape \
                   Convex))))))))(Secondary((id 6837)(content(Whitespace\" \
                   \")))))))))(Secondary((id 6839)(content(Whitespace\" \
                   \"))))(Tile((id 6840)(label(a))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6842)(content(Whitespace\" \"))))(Tile((id \
                   6843)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6844)(content(Whitespace\" \"))))(Tile((id \
                   6845)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6849)(content(Whitespace\" \")))))))))(Secondary((id \
                   6850)(content(Whitespace\" \"))))(Tile((id \
                   6851)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6853)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   6854)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6856)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6857)(content(Whitespace\" \"))))(Tile((id \
                   6858)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   6860)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5057)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6198)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6199)(content(Whitespace\" \"))))(Tile((id \
                   6201)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6202)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6204)(content(Whitespace\" \"))))(Grout((id 6408)(shape \
                   Convex)))(Secondary((id 6214)(content(Whitespace\" \
                   \"))))(Tile((id 6217)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6218)(content(Whitespace\" \"))))(Tile((id \
                   6221)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6222)(content(Whitespace\" \")))))((Secondary((id \
                   6224)(content(Whitespace\" \"))))(Tile((id 6228)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 6229)(content(Whitespace\" \
                   \"))))(Tile((id 6231)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   6232)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6234)(content(Whitespace\" \"))))(Tile((id \
                   6235)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6236)(content(Whitespace\" \")))))))))(Secondary((id \
                   6239)(content(Whitespace\" \"))))(Tile((id \
                   6240)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6241)(content(Whitespace\" \"))))(Tile((id \
                   6242)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6244)(content(Whitespace\" \"))))(Tile((id \
                   6245)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6248)(content(Whitespace\" \")))))))))(Secondary((id \
                   6250)(content(Whitespace\" \"))))(Tile((id \
                   6251)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6252)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   6254)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6255)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6257)(content(Whitespace\" \"))))(Tile((id \
                   6258)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   6259)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6261)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6265)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6266)(content(Whitespace\" \"))))(Tile((id \
                   6268)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6269)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6271)(content(Whitespace\" \"))))(Grout((id 6410)(shape \
                   Convex)))(Secondary((id 6281)(content(Whitespace\" \
                   \"))))(Tile((id 6284)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6285)(content(Whitespace\" \"))))(Tile((id \
                   6288)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6289)(content(Whitespace\" \")))))((Secondary((id \
                   6291)(content(Whitespace\" \"))))(Tile((id 6295)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 6296)(content(Whitespace\" \
                   \"))))(Tile((id 6298)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   6299)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6301)(content(Whitespace\" \"))))(Secondary((id \
                   6302)(content(Whitespace\" \"))))(Grout((id 6304)(shape \
                   Convex)))(Tile((id 6303)(label(,))(mold((out \
                   Pat)(in_())(nibs(((shape(Concave 14))(sort \
                   Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6305)(content(Whitespace\" \"))))(Tile((id \
                   6306)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6307)(content(Whitespace\" \"))))(Secondary((id \
                   6308)(content(Whitespace\" \")))))))))(Secondary((id \
                   6311)(content(Whitespace\" \"))))(Tile((id \
                   6312)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6313)(content(Whitespace\" \"))))(Tile((id \
                   6314)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6316)(content(Whitespace\" \"))))(Tile((id \
                   6317)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6320)(content(Whitespace\" \")))))))))(Secondary((id \
                   6322)(content(Whitespace\" \"))))(Tile((id \
                   6323)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6324)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   6326)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6327)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6329)(content(Whitespace\" \"))))(Tile((id \
                   6330)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   6331)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6333)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6337)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6338)(content(Whitespace\" \"))))(Tile((id \
                   6340)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6341)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6343)(content(Whitespace\" \"))))(Grout((id 6413)(shape \
                   Convex)))(Secondary((id 6353)(content(Whitespace\" \
                   \"))))(Tile((id 6356)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6357)(content(Whitespace\" \"))))(Tile((id \
                   6360)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6361)(content(Whitespace\" \")))))((Secondary((id \
                   6363)(content(Whitespace\" \"))))(Tile((id 6367)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 6368)(content(Whitespace\" \
                   \"))))(Tile((id 6370)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   6371)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6373)(content(Whitespace\" \"))))(Tile((id \
                   6963)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6377)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6379)(content(Whitespace\" \"))))(Tile((id \
                   6380)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6381)(content(Whitespace\" \")))))))))(Secondary((id \
                   6384)(content(Whitespace\" \"))))(Tile((id \
                   6385)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6386)(content(Whitespace\" \"))))(Tile((id \
                   6387)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6389)(content(Whitespace\" \"))))(Tile((id \
                   6390)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6393)(content(Whitespace\" \")))))))))(Secondary((id \
                   6395)(content(Whitespace\" \"))))(Tile((id \
                   6396)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6397)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   6399)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6400)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6402)(content(Whitespace\" \"))))(Tile((id \
                   6403)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   6404)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2883)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6872)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6873)(content(Whitespace\" \"))))(Tile((id \
                   6875)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6876)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6878)(content(Whitespace\" \"))))(Secondary((id \
                   6944)(content(Whitespace\" \"))))(Grout((id 6945)(shape \
                   Convex)))(Secondary((id 6886)(content(Whitespace\" \
                   \"))))(Tile((id 6889)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6946)(content(Whitespace\" \"))))(Tile((id \
                   6949)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6890)(content(Whitespace\" \")))))((Secondary((id \
                   6894)(content(Whitespace\" \"))))(Tile((id 6898)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 6899)(content(Whitespace\" \
                   \"))))(Tile((id 6901)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   6902)(label(a))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6903)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   6905)(content(Whitespace\" \"))))(Tile((id \
                   6906)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   6907)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6909)(content(Whitespace\" \"))))(Tile((id \
                   6910)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   6964)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6913)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6951)(content(Whitespace\" \"))))(Grout((id 6917)(shape \
                   Convex))))))))(Secondary((id 6918)(content(Whitespace\" \
                   \")))))))))(Secondary((id 6921)(content(Whitespace\" \
                   \"))))(Tile((id 6922)(label(a))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6923)(content(Whitespace\" \"))))(Tile((id \
                   6924)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6926)(content(Whitespace\" \"))))(Tile((id \
                   6927)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6930)(content(Whitespace\" \")))))))))(Secondary((id \
                   6932)(content(Whitespace\" \"))))(Tile((id \
                   6933)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6934)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   6936)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   6937)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6939)(content(Whitespace\" \"))))(Tile((id \
                   6940)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   6941)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6952)(content(Whitespace\"\\226\\143\\142\"))))(Grout((id \
                   6942)(shape Convex)))(Secondary((id \
                   636)(content(Whitespace\"\\226\\143\\142\")))))))(ancestors())))(caret \
                   Outer))";
                backup_text =
                  "# Internal Regression Tests: Function literal casting #\n\
                   # None of the below should trigger runtime exceptions #\n\n\
                   let g:   ->   = fun _ -> 9 in -g(1);\n\n\
                   let f = fun b -> b && true in f(true);\n\
                   let f = fun b:   -> b && true in f(true);\n\
                   let f = fun b: Bool -> b && true in f(true);\n\
                   let f:   = fun b -> b && true in f(true);\n\
                   let f:   = fun b:   -> b && true in f(true);\n\
                   let f:   = fun b: Bool -> b && true in f(true);\n\
                   let f:   ->   = fun b -> b && true in f(true);\n\
                   let f:   ->   = fun b:   -> b && true in f(true);\n\
                   let f:   ->   = fun b: Bool -> b && true in f(true); #ERR#\n\
                   let f: Bool ->   = fun b -> b && true in f(true);\n\
                   let f: Bool ->   = fun b:   -> b && true in f(true);\n\
                   let f: Bool ->   = fun b: Bool -> b && true in f(true);\n\
                   let f: Bool -> Bool = fun b -> b && true in f(true);\n\
                   let f: Bool -> Bool = fun b:   -> b && true in f(true);\n\
                   let f: Bool -> Bool = fun b: Bool -> b && true in f(true);\n\
                   let f:   -> Bool = fun b -> b && true in f(true);\n\
                   let f:   -> Bool = fun b:   -> b && true in f(true);\n\
                   let f:   -> Bool = fun b: Bool -> b && true in f(true); \
                   #ERR#\n\n\
                   let f = fun b -> b && true in f(true) && true;\n\
                   let f = fun b:   -> b && true in f(true) && true;\n\
                   let f = fun b: Bool -> b && true in f(true) && true;\n\
                   let f:   = fun b -> b && true in f(true) && true;\n\
                   let f:   = fun b:   -> b && true in f(true) && true;\n\
                   let f:   = fun b: Bool -> b && true in f(true) && true;\n\
                   let f:   ->   = fun b -> b && true in f(true) && true;\n\
                   let f:   ->   = fun b:   -> b && true in f(true) && true;\n\
                   let f:   ->   = fun b: Bool -> b && true in f(true) && true;\n\
                   let f: Bool ->   = fun b -> b && true in f(true) && true;\n\
                   let f: Bool ->   = fun b:   -> b && true in f(true) && true;\n\
                   let f: Bool ->   = fun b: Bool -> b && true in f(true) && \
                   true;\n\
                   let f: Bool -> Bool = fun b -> b && true in f(true) && true;\n\
                   let f: Bool -> Bool = fun b:   -> b && true in f(true) && \
                   true;\n\
                   let f: Bool -> Bool = fun b: Bool -> b && true in f(true) \
                   && true;\n\
                   let f:   -> Bool = fun b -> b && true in f(true) && true;\n\
                   let f:   -> Bool = fun b:   -> b && true in f(true) && true;\n\
                   let f:   -> Bool = fun b: Bool -> b && true in f(true) && \
                   true;\n\n\
                   let f = fun a, b -> a + 1 in f(1, 2);\n\
                   let f = fun a:  , b  -> a + 1 in f(1, 2);\n\
                   let f = fun a: Int, b -> a + 1 in f(1, 2);\n\
                   let f = fun (a, b): (Int,  ) -> a + 1 in f(1, 2);\n\
                   let f:   = fun a, b -> a + 1 in f(1, 2);\n\
                   let f:   = fun a:  , b  -> a + 1 in f(1, 2);\n\
                   let f:   = fun a: Int, b -> a + 1 in f(1, 2);\n\
                   let f:   = fun (a, b): (Int,  ) -> a + 1 in f(1, 2);\n\
                   let f:   ->   = fun a, b -> a + 1 in f(1, 2);\n\
                   let f:   ->   = fun a:   , b  -> a + 1 in f(1, 2);\n\
                   let f:   ->   = fun a: Int, b -> a + 1 in f(1, 2);\n\
                   let f:   ->   = fun (a, b): (Int,  ) -> a + 1 in f(1, 2);\n\
                   let f: ( ,  ) ->   = fun a, b -> a + 1 in f(1, 2);\n\
                   let f: ( ,  ) ->   = fun a:  , b -> a + 1 in f(1, 2);\n\
                   let f: ( ,  ) ->   = fun a: Int, b -> a + 1 in f(1, 2);\n\
                   let f: ( ,  ) ->   = fun (a, b): (Int,  ) -> a + 1 in f(1, \
                   2);\n\
                   let f: (Int,  ) ->   = fun a, b -> a + 1 in f(1, 2);\n\
                   let f: (Int,  ) ->   = fun a:  , b  -> a + 1 in f(1, 2);\n\
                   let f: (Int,  ) ->   = fun a: Int, b -> a + 1 in f(1, 2);\n\
                   let f: (Int,  ) ->   = fun (a, b): (Int,  ) -> a + 1 in \
                   f(1, 2);\n\
                   let f: (Int,  ) -> Int = fun a, b -> a + 1 in f(1, 2);\n\
                   let f: (Int,  ) -> Int = fun a:  , b  -> a + 1 in f(1, 2);\n\
                   let f: (Int,  ) -> Int = fun a: Int, b -> a + 1 in f(1, 2);\n\
                   let f: (Int,  ) -> Int = fun (a, b): (Int,  ) -> a + 1 in \
                   f(1, 2);\n\
                   let f:   -> Int = fun a, b -> a + 1 in f(1, 2);\n\
                   let f:   -> Int = fun a:   , b  -> a + 1 in f(1, 2);\n\
                   let f:   -> Int = fun a: Int, b -> a + 1 in f(1, 2);\n\
                   let f:    -> Int = fun (a, b): (Int,  ) -> a + 1 in f(1, 2);\n\
                  \ \n";
              } ) );
          ( "Introduction",
            ( 16873,
              {
                zipper =
                  "((selection((focus \
                   Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
                   14750)(content(Comment\"# Welcome to Hazel! \
                   #\"))))(Secondary((id \
                   15734)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   15735)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16048)(content(Comment\"# To get started, type 2 * 3 + 4 \
                   into the hole below, #\"))))(Secondary((id \
                   15875)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16071)(content(Comment\"# stopping between each character \
                   to observe how holes #\"))))(Secondary((id \
                   15941)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16868)(content(Comment\"# appear at each step to ensure \
                   that every editor state #\"))))(Secondary((id \
                   16170)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16187)(content(Comment\"# is meaningful. \
                   #\"))))(Secondary((id \
                   15830)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16872)(content(Whitespace\"\\226\\143\\142\")))))((Grout((id \
                   16871)(shape Convex)))(Secondary((id \
                   16009)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16010)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16295)(content(Comment\"# Once you are finished, navigate \
                   the menu in the top bar #\"))))(Secondary((id \
                   16296)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16369)(content(Comment\"# to see other examples, enter \
                   Scratch mode to play with #\"))))(Secondary((id \
                   16370)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16427)(content(Comment\"# Hazel, or enter Exercises mode to \
                   do some introductory #\"))))(Secondary((id \
                   16428)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16650)(content(Comment\"# exercises. Hazel is a \
                   work-in-progress research project, #\"))))(Secondary((id \
                   16569)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16793)(content(Comment\"# so there is not much public \
                   educational material yet. #\"))))(Secondary((id \
                   16669)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16805)(content(Comment\"# Check out the research papers at \
                   hazel.org for more on #\"))))(Secondary((id \
                   16745)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16766)(content(Comment\"# how Hazel works. \
                   #\")))))))(ancestors())))(caret Outer))";
                backup_text =
                  "# Welcome to Hazel! #\n\n\
                   # To get started, type 2 * 3 + 4 into the hole below, #\n\
                   # stopping between each character to observe how holes #\n\
                   # appear at each step to ensure that every editor state #\n\
                   # is meaningful. #\n\n\
                  \ \n\n\
                   # Once you are finished, navigate the menu in the top bar #\n\
                   # to see other examples, enter Scratch mode to play with #\n\
                   # Hazel, or enter Exercises mode to do some introductory #\n\
                   # exercises. Hazel is a work-in-progress research project, #\n\
                   # so there is not much public educational material yet. #\n\
                   # Check out the research papers at hazel.org for more on #\n\
                   # how Hazel works. #";
              } ) );
          ( "ADT Dynamics",
            ( 5303,
              {
                zipper =
                  "((selection((focus \
                   Left)(content())))(backpack())(relatives((siblings(()((Secondary((id \
                   2903)(content(Comment\"# Lambda Calculus via evaluation by \
                   substitution #\"))))(Secondary((id \
                   2390)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4913)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   5001)(content(Comment\"# An Expression is a variable, \
                   function, or application #\"))))(Secondary((id \
                   2352)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   337)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   338)(content(Whitespace\" \"))))(Tile((id \
                   342)(label(Exp))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   343)(content(Whitespace\" \")))))((Secondary((id \
                   2484)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2482)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2486)(content(Whitespace\" \"))))(Tile((id \
                   352)(label(Var))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   353)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   360)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2483)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   362)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2487)(content(Whitespace\" \"))))(Tile((id \
                   2480)(label(Lam))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   371)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   378)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   379)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   381)(content(Whitespace\" \"))))(Tile((id \
                   384)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2488)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2489)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2491)(content(Whitespace\" \"))))(Tile((id \
                   2493)(label(Ap))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   2494)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   2498)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   2499)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2501)(content(Whitespace\" \"))))(Tile((id \
                   2504)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2536)(content(Whitespace\" \")))))))))(Secondary((id \
                   3522)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3848)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3887)(content(Comment\"# Syntatic Equality of Expressions \
                   #\"))))(Secondary((id \
                   3523)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3529)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3530)(content(Whitespace\" \"))))(Tile((id \
                   3542)(label(exp_equal))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3708)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3710)(content(Whitespace\" \"))))(Tile((id \
                   3713)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   3717)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   3718)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3723)(content(Whitespace\" \"))))(Tile((id \
                   3722)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   3733)(content(Whitespace\" \"))))(Tile((id \
                   3726)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3732)(content(Whitespace\" \"))))(Tile((id \
                   3829)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3731)(content(Whitespace\" \")))))((Secondary((id \
                   3545)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3550)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   3551)(content(Whitespace\" \"))))(Tile((id \
                   3569)(label(es))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4075)(content(Whitespace\" \")))))))))(Secondary((id \
                   3563)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3573)(label(case end))(mold((out \
                   Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3574)(content(Whitespace\" \
                   \"))))(Tile((id 3577)(label(es))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3579)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3580)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3581)(content(Whitespace\" \
                   \"))))(Tile((id 3585)(label(Var))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3586)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3587)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   3589)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3590)(content(Whitespace\" \"))))(Tile((id \
                   3594)(label(Var))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3595)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3598)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   3600)(content(Whitespace\" \")))))))))(Secondary((id \
                   3602)(content(Whitespace\" \"))))(Tile((id \
                   3603)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3605)(content(Whitespace\" \"))))(Tile((id \
                   3608)(label($==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3609)(content(Whitespace\" \"))))(Tile((id \
                   3610)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3619)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3620)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3621)(content(Whitespace\" \
                   \"))))(Tile((id 3625)(label(Lam))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3626)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3665)(label(x1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3629)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3630)(content(Whitespace\" \"))))(Tile((id \
                   3641)(label(e1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   3645)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3646)(content(Whitespace\" \"))))(Tile((id \
                   3650)(label(Lam))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3651)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3659)(label(x2))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3660)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3661)(content(Whitespace\" \"))))(Tile((id \
                   3664)(label(e2))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   3666)(content(Whitespace\" \")))))))))(Secondary((id \
                   3832)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3846)(content(Whitespace\" \"))))(Secondary((id \
                   3847)(content(Whitespace\" \"))))(Tile((id \
                   3671)(label(x1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3672)(content(Whitespace\" \"))))(Tile((id \
                   3679)(label($==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3680)(content(Whitespace\" \"))))(Tile((id \
                   3683)(label(x2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3684)(content(Whitespace\" \"))))(Tile((id \
                   3686)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3687)(content(Whitespace\" \"))))(Tile((id \
                   3697)(label(exp_equal))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3698)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3701)(label(e1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3702)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3703)(content(Whitespace\" \"))))(Tile((id \
                   3706)(label(e2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   3734)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3735)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3736)(content(Whitespace\" \
                   \"))))(Tile((id 3739)(label(Ap))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3740)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3743)(label(e1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3744)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3748)(content(Whitespace\" \"))))(Tile((id \
                   3747)(label(e2))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   3750)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3751)(content(Whitespace\" \"))))(Tile((id \
                   3754)(label(Ap))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3755)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3760)(label(e3))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3761)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3771)(content(Whitespace\" \"))))(Tile((id \
                   3778)(label(e4))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   3779)(content(Whitespace\" \")))))))))(Secondary((id \
                   3831)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3835)(content(Whitespace\" \"))))(Secondary((id \
                   3836)(content(Whitespace\" \"))))(Tile((id \
                   3791)(label(exp_equal))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3793)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3796)(label(e1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3797)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3798)(content(Whitespace\" \"))))(Tile((id \
                   3801)(label(e3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   3802)(content(Whitespace\" \"))))(Tile((id \
                   3804)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   9))(sort Exp))((shape(Concave 9))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3805)(content(Whitespace\" \"))))(Tile((id \
                   3815)(label(exp_equal))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3816)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3819)(label(e2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3820)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3821)(content(Whitespace\" \"))))(Tile((id \
                   3824)(label(e4))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   4465)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4466)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 4481)(content(Whitespace\" \
                   \"))))(Tile((id 4480)(label(_))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4482)(content(Whitespace\" \")))))))))(Secondary((id \
                   4471)(content(Whitespace\" \"))))(Tile((id \
                   4478)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4479)(content(Whitespace\" \")))))))))(Secondary((id \
                   3830)(content(Whitespace\" \")))))))))(Secondary((id \
                   3707)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4866)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4912)(content(Comment\"# Substitute Exp v for variable name \
                   in Exp e #\"))))(Secondary((id \
                   2506)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2511)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2512)(content(Whitespace\" \"))))(Tile((id \
                   2518)(label(subst))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2914)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2961)(content(Whitespace\" \"))))(Tile((id \
                   2916)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   2949)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   2923)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2925)(content(Whitespace\" \"))))(Tile((id \
                   2937)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   2938)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2940)(content(Whitespace\" \"))))(Tile((id \
                   2951)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2960)(content(Whitespace\" \"))))(Tile((id \
                   2954)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2956)(content(Whitespace\" \"))))(Tile((id \
                   2959)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))((Secondary((id \
                   2742)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2541)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   2542)(content(Whitespace\" \"))))(Tile((id \
                   2586)(label(v))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2553)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2596)(content(Whitespace\" \"))))(Tile((id \
                   2564)(label(name))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2574)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2576)(content(Whitespace\" \"))))(Tile((id \
                   2587)(label(e))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2753)(content(Whitespace\" \")))))))))(Secondary((id \
                   2595)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2601)(label(case end))(mold((out \
                   Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2602)(content(Whitespace\" \
                   \"))))(Tile((id 2604)(label(e))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2605)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2606)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2626)(content(Whitespace\" \
                   \"))))(Tile((id 2629)(label(Var))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2630)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   2632)(label(n))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   2633)(content(Whitespace\" \")))))))))(Secondary((id \
                   3840)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3841)(content(Whitespace\" \"))))(Secondary((id \
                   3842)(content(Whitespace\" \"))))(Tile((id \
                   2672)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2636)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2637)(content(Whitespace\" \"))))(Tile((id \
                   2639)(label(n))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2657)(content(Whitespace\" \"))))(Tile((id \
                   2651)(label($==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2652)(content(Whitespace\" \"))))(Tile((id \
                   2656)(label(name))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2674)(content(Whitespace\" \")))))((Secondary((id \
                   2663)(content(Whitespace\" \"))))(Tile((id \
                   2662)(label(v))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2664)(content(Whitespace\" \")))))))))(Secondary((id \
                   2665)(content(Whitespace\" \"))))(Tile((id \
                   2666)(label(e))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2668)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2613)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2675)(content(Whitespace\" \
                   \"))))(Tile((id 2680)(label(Lam))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2681)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   2701)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2684)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2686)(content(Whitespace\" \"))))(Tile((id \
                   2690)(label(body))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   2691)(content(Whitespace\" \")))))))))(Secondary((id \
                   3843)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3844)(content(Whitespace\" \"))))(Secondary((id \
                   3845)(content(Whitespace\" \"))))(Tile((id \
                   2704)(label(Lam))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2705)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2707)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2708)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2712)(content(Whitespace\" \"))))(Tile((id \
                   2738)(label(subst))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2718)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2720)(label(v))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2721)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2726)(label(name))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2727)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2729)(content(Whitespace\" \"))))(Tile((id \
                   2733)(label(body))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                   2620)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2621)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2760)(content(Whitespace\" \
                   \"))))(Tile((id 2762)(label(Ap))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2763)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   2766)(label(e1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2767)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2770)(label(e2))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   2771)(content(Whitespace\" \")))))))))(Secondary((id \
                   3837)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3838)(content(Whitespace\" \"))))(Secondary((id \
                   3839)(content(Whitespace\" \"))))(Tile((id \
                   2774)(label(Ap))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2775)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2781)(label(subst))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2783)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2785)(label(v))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2786)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2820)(content(Whitespace\" \"))))(Tile((id \
                   2791)(label(name))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2792)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2821)(content(Whitespace\" \"))))(Tile((id \
                   2795)(label(e1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   2796)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2798)(content(Whitespace\" \"))))(Tile((id \
                   2803)(label(subst))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2805)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2807)(label(v))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2808)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2810)(content(Whitespace\" \"))))(Tile((id \
                   2814)(label(name))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2815)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2817)(content(Whitespace\" \"))))(Tile((id \
                   2819)(label(e2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                   2822)(content(Whitespace\" \")))))))))(Secondary((id \
                   2625)(content(Whitespace\" \")))))))))(Secondary((id \
                   2521)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   5002)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   5063)(content(Comment\"# Evaluation can result in either an \
                   Exp or an Error #\"))))(Secondary((id \
                   2965)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2971)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2972)(content(Whitespace\" \"))))(Tile((id \
                   2979)(label(Result))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2980)(content(Whitespace\" \")))))((Secondary((id \
                   2981)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5077)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2983)(content(Whitespace\" \"))))(Tile((id \
                   2989)(label(Error))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   3002)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   3010)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   5065)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5066)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5068)(content(Whitespace\" \"))))(Tile((id \
                   5070)(label(Ok))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5071)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   5075)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2990)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3001)(content(Whitespace\" \")))))))))(Secondary((id \
                   3888)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3889)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3894)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3895)(content(Whitespace\" \"))))(Tile((id \
                   3908)(label(result_equal))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3910)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3912)(content(Whitespace\" \"))))(Tile((id \
                   3913)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   3919)(label(Result))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   3920)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3922)(content(Whitespace\" \"))))(Tile((id \
                   3928)(label(Result))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   3940)(content(Whitespace\" \"))))(Tile((id \
                   3932)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3934)(content(Whitespace\" \"))))(Tile((id \
                   3938)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3939)(content(Whitespace\" \")))))((Secondary((id \
                   3941)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3961)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   3962)(content(Whitespace\" \"))))(Tile((id \
                   3967)(label(rs))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3968)(content(Whitespace\" \")))))))))(Secondary((id \
                   3969)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3975)(label(case end))(mold((out \
                   Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3976)(content(Whitespace\" \
                   \"))))(Tile((id 3985)(label(rs))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3987)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3988)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3989)(content(Whitespace\" \
                   \"))))(Tile((id 3992)(label(Ok))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3993)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3996)(label(e1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   3997)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3998)(content(Whitespace\" \"))))(Tile((id \
                   4001)(label(Ok))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   4002)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   4005)(label(e2))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   4006)(content(Whitespace\" \")))))))))(Secondary((id \
                   4009)(content(Whitespace\" \"))))(Tile((id \
                   4019)(label(exp_equal))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4020)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4023)(label(e1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4024)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4025)(content(Whitespace\" \"))))(Tile((id \
                   4033)(label(e2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   4034)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4035)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 4036)(content(Whitespace\" \
                   \"))))(Tile((id 4042)(label(Error))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   4043)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   4046)(label(e1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   4047)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4048)(content(Whitespace\" \"))))(Tile((id \
                   4054)(label(Error))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   4055)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   4058)(label(e2))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   4059)(content(Whitespace\" \")))))))))(Secondary((id \
                   4061)(content(Whitespace\" \"))))(Tile((id \
                   4064)(label(e1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4065)(content(Whitespace\" \"))))(Tile((id \
                   4068)(label($==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4069)(content(Whitespace\" \"))))(Tile((id \
                   4072)(label(e2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4483)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4484)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 4486)(content(Whitespace\" \
                   \"))))(Tile((id 4487)(label(_))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4488)(content(Whitespace\" \")))))))))(Secondary((id \
                   4491)(content(Whitespace\" \"))))(Tile((id \
                   4496)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4497)(content(Whitespace\" \")))))))))(Secondary((id \
                   4074)(content(Whitespace\" \")))))))))(Secondary((id \
                   3011)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   5078)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   5117)(content(Comment\"# Evaluation by substitution \
                   #\"))))(Secondary((id \
                   2522)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2527)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2528)(content(Whitespace\" \"))))(Tile((id \
                   2533)(label(eval))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3012)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3014)(content(Whitespace\" \"))))(Tile((id \
                   3017)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3030)(content(Whitespace\" \"))))(Tile((id \
                   3021)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3023)(content(Whitespace\" \"))))(Tile((id \
                   3029)(label(Result))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3521)(content(Whitespace\" \")))))((Secondary((id \
                   3032)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3036)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   3037)(content(Whitespace\" \"))))(Tile((id \
                   3050)(label(e))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3051)(content(Whitespace\" \")))))))))(Secondary((id \
                   3054)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3059)(label(case end))(mold((out \
                   Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3060)(content(Whitespace\" \
                   \"))))(Tile((id 3062)(label(e))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3063)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3064)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3066)(content(Whitespace\" \
                   \"))))(Tile((id 3069)(label(Var))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3070)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3072)(label(n))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   3073)(content(Whitespace\" \")))))))))(Secondary((id \
                   3076)(content(Whitespace\" \"))))(Tile((id \
                   3236)(label(Error))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3237)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4203)(label(\"\\\"Free Variable\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   3110)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3111)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3113)(content(Whitespace\" \
                   \"))))(Tile((id 3116)(label(Lam))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3117)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3119)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3120)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3122)(content(Whitespace\" \"))))(Tile((id \
                   3126)(label(body))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   3127)(content(Whitespace\" \")))))))))(Secondary((id \
                   3130)(content(Whitespace\" \"))))(Tile((id \
                   3290)(label(Ok))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3291)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3289)(label(Lam))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3270)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3272)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3273)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3275)(content(Whitespace\" \"))))(Tile((id \
                   3279)(label(body))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                   3161)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3162)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3164)(content(Whitespace\" \
                   \"))))(Tile((id 3166)(label(Ap))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3167)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3170)(label(e1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3171)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3174)(label(e2))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   3175)(content(Whitespace\" \")))))))))(Secondary((id \
                   3335)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3298)(label(case end))(mold((out \
                   Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3299)(content(Whitespace\" \
                   \"))))(Tile((id 3318)(label(eval))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   3323)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3326)(label(e1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   3329)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3330)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3332)(content(Whitespace\" \
                   \"))))(Tile((id 3334)(label(Ok))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3336)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3348)(label(Lam))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3349)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3351)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3353)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3355)(content(Whitespace\" \"))))(Tile((id \
                   3359)(label(body))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                   3417)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3422)(label(case end))(mold((out \
                   Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3423)(content(Whitespace\" \
                   \"))))(Tile((id 3432)(label(eval))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   3433)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3436)(label(e2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   3437)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3438)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3440)(content(Whitespace\" \
                   \"))))(Tile((id 3445)(label(Error))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3446)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3450)(label(err))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   3451)(content(Whitespace\" \")))))))))(Secondary((id \
                   3454)(content(Whitespace\" \"))))(Tile((id \
                   3459)(label(Error))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3460)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3464)(label(err))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   3465)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3466)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3468)(content(Whitespace\" \
                   \"))))(Tile((id 3472)(label(Ok))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3473)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3477)(label(arg))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   3478)(content(Whitespace\" \")))))))))(Secondary((id \
                   3480)(content(Whitespace\" \"))))(Tile((id \
                   3511)(label(eval))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3506)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3504)(label(subst))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3486)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3490)(label(arg))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3491)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3493)(content(Whitespace\" \"))))(Tile((id \
                   3494)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3495)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3497)(content(Whitespace\" \"))))(Tile((id \
                   3501)(label(body))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                   3507)(content(Whitespace\" \")))))))))(Secondary((id \
                   3391)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5127)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 5203)(content(Whitespace\" \
                   \"))))(Tile((id 5204)(label(_))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5205)(content(Whitespace\" \")))))))))(Secondary((id \
                   5143)(content(Whitespace\" \"))))(Tile((id \
                   5148)(label(Error))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5238)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5302)(label(\"\\\"Not a Function\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   5255)(content(Whitespace\" \")))))))))(Secondary((id \
                   3513)(content(Whitespace\" \")))))))))(Secondary((id \
                   3292)(content(Whitespace\" \")))))))))(Secondary((id \
                   2472)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2097)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4080)(label(test end))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 4085)(content(Whitespace\" \
                   \"))))(Tile((id 4111)(label(result_equal))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   4112)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id \
                   4114)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4118)(label(eval))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4119)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4123)(label(Var))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4124)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4130)(label(\"\\\"yo\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                   4158)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4160)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4165)(label(Error))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4166)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4181)(label(\"\\\"Free Variable\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                   4208)(content(Whitespace\" \")))))))))(Tile((id \
                   4183)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4767)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4768)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4774)(label(test end))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 4775)(content(Whitespace\" \
                   \"))))(Tile((id 4788)(label(result_equal))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   4789)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id \
                   4790)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4795)(label(eval))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4796)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4799)(label(Ap))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4800)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4804)(label(Var))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4805)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4809)(label(\"\\\"no\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   4810)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4811)(content(Whitespace\" \"))))(Tile((id \
                   4815)(label(Lam))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4816)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4821)(label(\"\\\"bro\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   4822)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4823)(content(Whitespace\" \"))))(Tile((id \
                   4827)(label(Var))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4828)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4833)(label(\"\\\"bro\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))))))))))))(Tile((id \
                   4834)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4835)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4841)(label(Error))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4842)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4858)(label(\"\\\"Not a Function\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                   4861)(content(Whitespace\" \")))))))))(Tile((id \
                   4863)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4764)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4185)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4214)(label(test end))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 4215)(content(Whitespace\" \
                   \"))))(Tile((id 4228)(label(result_equal))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   4229)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id \
                   4231)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4235)(label(eval))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4236)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4415)(label(Lam))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4416)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4420)(label(\"\\\"yo\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   4421)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4423)(content(Whitespace\" \"))))(Tile((id \
                   4426)(label(Var))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4427)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4431)(label(\"\\\"yo\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))))))))))))(Tile((id \
                   4246)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4248)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4499)(label(Ok))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4254)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4502)(label(Lam))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4503)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4507)(label(\"\\\"yo\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   4508)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4510)(content(Whitespace\" \"))))(Tile((id \
                   4513)(label(Var))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4514)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4518)(label(\"\\\"yo\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))))))))))))(Secondary((id \
                   4272)(content(Whitespace\" \")))))))))(Tile((id \
                   4274)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4765)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4277)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4282)(label(test end))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 4283)(content(Whitespace\" \
                   \"))))(Tile((id 4296)(label(result_equal))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   4297)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id \
                   4299)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4303)(label(eval))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4522)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4525)(label(Ap))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4530)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4534)(label(Lam))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4539)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4547)(label(\"\\\"yo\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   4548)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4550)(content(Whitespace\" \"))))(Tile((id \
                   4553)(label(Var))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4554)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4558)(label(\"\\\"yo\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                   4559)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4561)(content(Whitespace\" \"))))(Tile((id \
                   4564)(label(Lam))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4565)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4586)(label(\"\\\"bro\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   4570)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4572)(content(Whitespace\" \"))))(Tile((id \
                   4575)(label(Var))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4576)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4591)(label(\"\\\"bro\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))))))))))))(Tile((id \
                   4314)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4316)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4594)(label(Ok))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4595)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4599)(label(Lam))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4600)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4605)(label(\"\\\"bro\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   4606)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4608)(content(Whitespace\" \"))))(Tile((id \
                   4611)(label(Var))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4612)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4617)(label(\"\\\"bro\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))))))))))))(Secondary((id \
                   4340)(content(Whitespace\" \
                   \"))))))))))))(ancestors())))(caret Outer))";
                backup_text =
                  "# Lambda Calculus via evaluation by substitution #\n\n\
                   # An Expression is a variable, function, or application #\n\
                   type Exp =\n\
                   + Var(String)\n\
                   + Lam(String, Exp)\n\
                   + Ap(Exp, Exp) in\n\n\
                   # Syntatic Equality of Expressions #\n\
                   let exp_equal: (Exp, Exp) -> Bool =\n\
                   fun es ->\n\
                   case es\n\
                   | Var(x), Var(y) => x $== y\n\
                   | Lam(x1, e1), Lam(x2, e2) =>\n\
                  \  x1 $== x2 && exp_equal(e1, e2)\n\
                   | Ap(e1, e2), Ap(e3, e4) =>\n\
                  \  exp_equal(e1, e3) && exp_equal(e2, e4)\n\
                   | _ => false end in\n\n\
                   # Substitute Exp v for variable name in Exp e #\n\
                   let subst: (Exp, String, Exp) -> Exp=\n\
                   fun v, name, e ->\n\
                   case e\n\
                   | Var(n) =>\n\
                  \  (if n $== name then v else e)\n\
                   | Lam(x, body) =>\n\
                  \  Lam(x, subst(v,name, body))\n\
                   | Ap(e1,e2) =>\n\
                  \  Ap(subst(v, name, e1), subst(v, name, e2)) end in\n\n\
                   # Evaluation can result in either an Exp or an Error #\n\
                   type Result =\n\
                   + Error(String)\n\
                   + Ok(Exp)\n\
                  \ in\n\n\
                   let result_equal: (Result, Result) -> Bool =\n\
                   fun rs ->\n\
                   case rs\n\
                   | Ok(e1), Ok(e2) => exp_equal(e1, e2)\n\
                   | Error(e1), Error(e2) => e1 $== e2\n\
                   | _ => false end in\n\n\
                   # Evaluation by substitution #\n\
                   let eval: Exp -> Result =\n\
                   fun e ->\n\
                   case e\n\
                   | Var(n) => Error(\"Free Variable\")\n\
                   | Lam(x, body) => Ok(Lam(x, body))\n\
                   | Ap(e1,e2) =>\n\
                   case eval(e1)\n\
                   | Ok(Lam(x, body))=>\n\
                   case eval(e2)\n\
                   | Error(err) => Error(err)\n\
                   | Ok(arg) => eval(subst(arg, x, body)) end\n\
                   | _ => Error(\"Not a Function\") end end in\n\n\
                   test result_equal(\n\
                   eval(Var(\"yo\")),\n\
                   Error(\"Free Variable\")) end;\n\n\
                   test result_equal(\n\
                   eval(Ap(Var(\"no\"), Lam(\"bro\", Var(\"bro\")))),\n\
                   Error(\"Not a Function\")) end;\n\n\
                   test result_equal(\n\
                   eval(Lam(\"yo\", Var(\"yo\"))),\n\
                   Ok(Lam(\"yo\", Var(\"yo\")))) end;\n\n\
                   test result_equal(\n\
                   eval(Ap(Lam(\"yo\", Var(\"yo\")), Lam(\"bro\", \
                   Var(\"bro\")))),\n\
                   Ok(Lam(\"bro\", Var(\"bro\")))) end";
              } ) );
          ( "ADT Statics",
            ( 29557,
              {
                zipper =
                  "((selection((focus \
                   Left)(content())))(backpack())(relatives((siblings(()((Secondary((id \
                   29474)(content(Comment\"# Internal Regression Tests: ADT \
                   Statics #\"))))(Secondary((id \
                   8112)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   29556)(content(Comment\"# All commented lines should show \
                   errors as described #\"))))(Secondary((id \
                   8175)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   29509)(content(Comment\"# No other lines should show errors \
                   #\"))))(Secondary((id \
                   6602)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   6603)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   6930)(content(Comment\"#type definitions: no \
                   errors#\"))))(Secondary((id \
                   3939)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2974)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2975)(content(Whitespace\" \"))))(Grout((id 2979)(shape \
                   Convex)))(Secondary((id 3016)(content(Whitespace\" \
                   \")))))((Secondary((id 2980)(content(Whitespace\" \
                   \"))))(Grout((id 8391)(shape Convex)))(Secondary((id \
                   2987)(content(Whitespace\" \")))))))))(Secondary((id \
                   3017)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3023)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3024)(content(Whitespace\" \"))))(Tile((id \
                   3054)(label(SingleNull))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   3039)(content(Whitespace\" \")))))((Secondary((id \
                   3041)(content(Whitespace\" \"))))(Tile((id \
                   3042)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   3048)(label(One))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3049)(content(Whitespace\" \")))))))))(Secondary((id \
                   2861)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2867)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2868)(content(Whitespace\" \"))))(Tile((id \
                   3059)(label(Single))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2871)(content(Whitespace\" \")))))((Secondary((id \
                   2873)(content(Whitespace\" \"))))(Tile((id \
                   2874)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   2876)(label(F))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   2878)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   2942)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2883)(content(Whitespace\" \")))))))))(Secondary((id \
                   2885)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2046)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2047)(content(Whitespace\" \"))))(Tile((id \
                   3185)(label(GoodSum))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2050)(content(Whitespace\" \")))))((Secondary((id \
                   2052)(content(Whitespace\" \"))))(Tile((id \
                   2065)(label(A))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2080)(content(Whitespace\" \"))))(Tile((id \
                   2068)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2069)(content(Whitespace\" \"))))(Tile((id \
                   2070)(label(B))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2072)(content(Whitespace\" \"))))(Tile((id \
                   2073)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2074)(content(Whitespace\" \"))))(Tile((id \
                   2375)(label(C))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   3186)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   3201)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2077)(content(Whitespace\" \")))))))))(Secondary((id \
                   6117)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6123)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6124)(content(Whitespace\" \"))))(Tile((id \
                   6132)(label(Partial))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   6133)(content(Whitespace\" \")))))((Secondary((id \
                   6135)(content(Whitespace\" \"))))(Tile((id \
                   6138)(label(Ok))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6139)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                   6141)(shape Convex))))))))(Secondary((id \
                   6143)(content(Whitespace\" \"))))(Tile((id \
                   6172)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6181)(content(Whitespace\" \"))))(Grout((id 6180)(shape \
                   Convex)))(Secondary((id 6173)(content(Whitespace\" \
                   \")))))))))(Secondary((id \
                   3316)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2495)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2496)(content(Whitespace\" \"))))(Tile((id \
                   3477)(label(DoubleAlias))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2499)(content(Whitespace\" \")))))((Secondary((id \
                   2501)(content(Whitespace\" \"))))(Tile((id \
                   3460)(label(GoodSum))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2506)(content(Whitespace\" \")))))))))(Secondary((id \
                   1428)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1434)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1435)(content(Whitespace\" \"))))(Tile((id \
                   8239)(label(VerticalLeading))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   1438)(content(Whitespace\" \")))))((Secondary((id \
                   8305)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   8307)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   8308)(content(Whitespace\" \"))))(Tile((id \
                   8306)(label(A))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1446)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1447)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1448)(content(Whitespace\" \"))))(Tile((id \
                   1449)(label(B))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   1451)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   3631)(label(GoodSum))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   1456)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1457)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1458)(content(Whitespace\" \"))))(Tile((id \
                   1459)(label(C))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   1461)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   1466)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   1468)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   1473)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   1476)(content(Whitespace\" \"))))(Secondary((id \
                   1474)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   3687)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3951)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   6938)(content(Comment\"#incorrect or incomplete type \
                   definitions#\"))))(Secondary((id \
                   3860)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3866)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3867)(content(Whitespace\" \"))))(Tile((id \
                   3879)(label(badTypeName))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3880)(content(Whitespace\" \")))))((Secondary((id \
                   3882)(content(Whitespace\" \"))))(Grout((id 7903)(shape \
                   Convex)))(Secondary((id 3898)(content(Whitespace\" \
                   \")))))))))(Secondary((id 6939)(content(Whitespace\" \
                   \"))))(Secondary((id 7915)(content(Comment\"#err: invalid \
                   type name#\"))))(Secondary((id \
                   3900)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3906)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3907)(content(Whitespace\" \"))))(Tile((id \
                   3908)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Grout((id \
                   3911)(shape Convex)))(Tile((id 3910)(label(,))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 14))(sort \
                   Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4907)(content(Whitespace\" \"))))(Grout((id 3914)(shape \
                   Convex))))))))(Secondary((id 3916)(content(Whitespace\" \
                   \")))))((Secondary((id 3918)(content(Whitespace\" \
                   \"))))(Grout((id 6260)(shape Convex)))(Secondary((id \
                   3936)(content(Whitespace\" \")))))))))(Secondary((id \
                   6974)(content(Whitespace\" \"))))(Secondary((id \
                   7938)(content(Comment\"#err: invalid type \
                   name#\"))))(Secondary((id \
                   7939)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   7945)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   7946)(content(Whitespace\" \"))))(Grout((id 7995)(shape \
                   Convex)))(Secondary((id 7959)(content(Whitespace\" \
                   \")))))((Secondary((id 7961)(content(Whitespace\" \
                   \"))))(Tile((id 7974)(label(badTypeToken))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   7977)(content(Whitespace\" \")))))))))(Secondary((id \
                   7979)(content(Whitespace\" \"))))(Secondary((id \
                   8008)(content(Comment\"#err: invalid type \
                   token#\"))))(Secondary((id \
                   6205)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6211)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6212)(content(Whitespace\" \"))))(Tile((id \
                   6253)(label(NotASum))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   6221)(content(Whitespace\" \")))))((Secondary((id \
                   6223)(content(Whitespace\" \"))))(Tile((id \
                   6232)(label(NotInSum))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6233)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   6238)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   6241)(content(Whitespace\" \")))))))))(Secondary((id \
                   6995)(content(Whitespace\" \"))))(Secondary((id \
                   8363)(content(Comment\"#err: cons not in \
                   sum#\"))))(Secondary((id \
                   8309)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   8315)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   8316)(content(Whitespace\" \"))))(Tile((id \
                   8371)(label(Bool))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   8321)(content(Whitespace\" \")))))((Secondary((id \
                   8330)(content(Whitespace\" \"))))(Grout((id 8329)(shape \
                   Convex)))(Secondary((id 8323)(content(Whitespace\" \
                   \"))))(Secondary((id 8324)(content(Whitespace\" \
                   \")))))))))(Secondary((id 8328)(content(Whitespace\" \
                   \"))))(Secondary((id 8361)(content(Comment\"#err: shadows \
                   base type#\"))))(Secondary((id \
                   3688)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3694)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3695)(content(Whitespace\" \"))))(Tile((id \
                   3701)(label(Dupes))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   3702)(content(Whitespace\" \")))))((Secondary((id \
                   8009)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   7772)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   8012)(content(Whitespace\" \"))))(Tile((id \
                   3708)(label(Guy))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   3709)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   3714)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   8052)(content(Whitespace\" \"))))(Secondary((id \
                   8059)(content(Comment\"#no err#\"))))(Secondary((id \
                   8010)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3723)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3724)(content(Whitespace\" \"))))(Tile((id \
                   3728)(label(Guy))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   3729)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   3733)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   8033)(content(Whitespace\" \"))))(Secondary((id \
                   8051)(content(Comment\"#err: already \
                   used#\"))))(Secondary((id \
                   8011)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6271)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6273)(content(Whitespace\" \"))))(Tile((id \
                   6276)(label(Guy))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6277)(content(Whitespace\" \")))))))))(Secondary((id \
                   7023)(content(Whitespace\" \"))))(Secondary((id \
                   8032)(content(Comment\"#err: already \
                   used#\"))))(Secondary((id \
                   3800)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3807)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3808)(content(Whitespace\" \"))))(Tile((id \
                   3816)(label(BadCons))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   3817)(content(Whitespace\" \")))))((Secondary((id \
                   7701)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   7770)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   7771)(content(Whitespace\" \"))))(Tile((id \
                   7738)(label(Um))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   7739)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   7746)(label(Unbound))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   7747)(content(Whitespace\" \"))))(Secondary((id \
                   7769)(content(Comment\"#err: unbound type \
                   var#\"))))(Secondary((id \
                   7524)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   7532)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   7533)(content(Whitespace\" \"))))(Tile((id \
                   7531)(label(invalid))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   7534)(content(Whitespace\" \"))))(Secondary((id \
                   7547)(content(Comment\"#err: invalid#\"))))(Secondary((id \
                   7517)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   7522)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   7523)(content(Whitespace\" \"))))(Tile((id \
                   7521)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   7548)(content(Whitespace\" \"))))(Secondary((id \
                   7578)(content(Comment\"#err: expected cons found \
                   type#\"))))(Secondary((id \
                   7498)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   7497)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   7501)(content(Whitespace\" \"))))(Tile((id \
                   3823)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   3824)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   3828)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   7602)(content(Whitespace\" \"))))(Secondary((id \
                   7632)(content(Comment\"#err: expected cons found \
                   type#\"))))(Secondary((id \
                   7499)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3830)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3831)(content(Whitespace\" \"))))(Tile((id \
                   3832)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                   3834)(shape Convex))))))))(Tile((id \
                   3836)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   3840)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   7633)(content(Whitespace\" \"))))(Secondary((id \
                   7663)(content(Comment\"#err: expected cons found \
                   type#\"))))(Secondary((id \
                   7500)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3842)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3843)(content(Whitespace\" \"))))(Tile((id \
                   3844)(label(A))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   3846)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   6020)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Tile((id \
                   3849)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   3853)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   3856)(content(Whitespace\" \")))))))))(Secondary((id \
                   3859)(content(Whitespace\" \"))))(Secondary((id \
                   7700)(content(Comment\"#err: expected cons found \
                   app#\"))))(Secondary((id \
                   1563)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3564)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   8704)(content(Comment\"#sums in compound aliases dont add \
                   ctrs to scope#\"))))(Secondary((id \
                   8930)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   8985)(content(Comment\"#but compound alias types should \
                   propagate analytically#\"))))(Secondary((id \
                   3512)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3518)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3519)(content(Whitespace\" \"))))(Tile((id \
                   9369)(label(CompoundAlias))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   3531)(content(Whitespace\" \")))))((Secondary((id \
                   3533)(content(Whitespace\" \"))))(Tile((id \
                   3534)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   3538)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   3539)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3540)(content(Whitespace\" \"))))(Tile((id \
                   3550)(label(Anonymous))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3551)(content(Whitespace\" \"))))(Tile((id \
                   3552)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3553)(content(Whitespace\" \"))))(Tile((id \
                   3557)(label(Sum))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   3560)(content(Whitespace\" \")))))))))(Secondary((id \
                   3562)(content(Whitespace\" \"))))(Secondary((id \
                   1485)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2243)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2244)(content(Whitespace\" \"))))(Tile((id \
                   9126)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2250)(content(Whitespace\" \")))))((Secondary((id \
                   2262)(content(Whitespace\" \"))))(Tile((id \
                   3503)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   8493)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3507)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6011)(content(Whitespace\" \"))))(Tile((id \
                   3511)(label(Sum))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2252)(content(Whitespace\" \")))))))))(Secondary((id \
                   6892)(content(Whitespace\" \"))))(Secondary((id \
                   7774)(content(Comment\"#err: not \
                   defined#\"))))(Secondary((id \
                   8564)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   8569)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   8570)(content(Whitespace\" \"))))(Tile((id \
                   9128)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   8573)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   9070)(content(Whitespace\" \"))))(Tile((id \
                   9383)(label(CompoundAlias))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   8586)(content(Whitespace\" \")))))((Secondary((id \
                   8587)(content(Whitespace\" \"))))(Tile((id \
                   8588)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   8589)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   8591)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   8592)(content(Whitespace\" \"))))(Tile((id \
                   8596)(label(Sum))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   8599)(content(Whitespace\" \")))))))))(Secondary((id \
                   8987)(content(Whitespace\" \"))))(Secondary((id \
                   8996)(content(Comment\"#no error#\"))))(Secondary((id \
                   8805)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   8811)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   8812)(content(Whitespace\" \"))))(Tile((id \
                   9254)(label(Yorp))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   8818)(content(Whitespace\" \")))))((Secondary((id \
                   8819)(content(Whitespace\" \"))))(Tile((id \
                   8823)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   8824)(content(Whitespace\" \"))))(Tile((id \
                   8826)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   8827)(content(Whitespace\" \"))))(Tile((id \
                   8836)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   9229)(label(Inside))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   8830)(content(Whitespace\" \"))))(Tile((id \
                   8831)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   8832)(content(Whitespace\" \"))))(Tile((id \
                   9235)(label(Ouside))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   8835)(content(Whitespace\" \")))))))))(Secondary((id \
                   8837)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   8842)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   8843)(content(Whitespace\" \"))))(Tile((id \
                   9129)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   8847)(content(Whitespace\" \")))))((Secondary((id \
                   8853)(content(Whitespace\" \"))))(Tile((id 8858)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 8859)(content(Whitespace\" \
                   \"))))(Tile((id 8860)(label(_))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   8882)(content(Whitespace\" \")))))))))(Secondary((id \
                   8884)(content(Whitespace\" \"))))(Tile((id \
                   9241)(label(Inside))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   8889)(content(Whitespace\" \")))))))))(Secondary((id \
                   9032)(content(Whitespace\" \"))))(Secondary((id \
                   9049)(content(Comment\"#err: not \
                   defined#\"))))(Secondary((id \
                   8494)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   9001)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   9002)(content(Whitespace\" \"))))(Tile((id \
                   9130)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   9006)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   9071)(content(Whitespace\" \"))))(Tile((id \
                   9253)(label(Yorp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   9010)(content(Whitespace\" \")))))((Secondary((id \
                   9011)(content(Whitespace\" \"))))(Tile((id 9016)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 9017)(content(Whitespace\" \
                   \"))))(Tile((id 9018)(label(_))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   9020)(content(Whitespace\" \")))))))))(Secondary((id \
                   9022)(content(Whitespace\" \"))))(Tile((id \
                   9248)(label(Inside))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   9029)(content(Whitespace\" \")))))))))(Secondary((id \
                   9051)(content(Whitespace\" \"))))(Secondary((id \
                   9060)(content(Comment\"#no error#\"))))(Secondary((id \
                   9073)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   9079)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   9080)(content(Whitespace\" \"))))(Tile((id \
                   9094)(label(Gargs))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   9095)(content(Whitespace\" \")))))((Secondary((id \
                   9096)(content(Whitespace\" \"))))(Tile((id 9097)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 9199)(label(BigGuy))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   9102)(content(Whitespace\" \"))))(Tile((id \
                   9103)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   9104)(content(Whitespace\" \"))))(Tile((id \
                   9211)(label(Small))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   9111)(content(Whitespace\" \")))))))))(Secondary((id \
                   9072)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   9116)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   9117)(content(Whitespace\" \"))))(Tile((id \
                   9131)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   9123)(content(Whitespace\" \")))))((Secondary((id \
                   9132)(content(Whitespace\" \"))))(Tile((id \
                   9206)(label(BigGuy))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   9137)(content(Whitespace\" \")))))))))(Secondary((id \
                   9178)(content(Whitespace\" \"))))(Secondary((id \
                   9195)(content(Comment\"#err: not \
                   defined#\"))))(Secondary((id \
                   9138)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   9143)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   9144)(content(Whitespace\" \"))))(Tile((id \
                   9147)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   9149)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   9150)(content(Whitespace\" \"))))(Tile((id \
                   9156)(label(Gargs))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   9157)(content(Whitespace\" \")))))((Secondary((id \
                   9158)(content(Whitespace\" \"))))(Tile((id 9159)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 9217)(label(BigGuy))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   9166)(content(Whitespace\" \")))))))))(Secondary((id \
                   9167)(content(Whitespace\" \"))))(Secondary((id \
                   9176)(content(Comment\"#no error#\"))))(Secondary((id \
                   9255)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   9260)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   9261)(content(Whitespace\" \"))))(Tile((id \
                   9262)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   9264)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   9265)(content(Whitespace\" \"))))(Tile((id \
                   9275)(label(Gargs))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   9271)(content(Whitespace\" \")))))((Secondary((id \
                   9276)(content(Whitespace\" \"))))(Tile((id \
                   9283)(label(BigGuy))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   9284)(content(Whitespace\" \"))))(Tile((id \
                   9286)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   6))(sort Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   9287)(content(Whitespace\" \"))))(Tile((id 9338)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 9344)(label(BigGuy))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   9291)(content(Whitespace\" \")))))))))(Secondary((id \
                   9292)(content(Whitespace\" \"))))(Secondary((id \
                   9354)(content(Comment\"#no error#\"))))(Secondary((id \
                   9293)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4008)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   8078)(content(Comment\"#unbound tyvars treated as \
                   unknown-typehole#\"))))(Secondary((id \
                   4053)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4058)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4059)(content(Whitespace\" \"))))(Tile((id \
                   4060)(label(a))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   4062)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   4066)(label(Bad))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4067)(content(Whitespace\" \")))))((Secondary((id \
                   4069)(content(Whitespace\" \"))))(Tile((id \
                   8085)(label(0))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4075)(content(Whitespace\" \")))))))))(Secondary((id \
                   4077)(content(Whitespace\" \"))))(Tile((id \
                   4078)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   8079)(content(Whitespace\" \"))))(Tile((id \
                   8082)(label(==))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   8))(sort Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   8083)(content(Whitespace\" \"))))(Tile((id \
                   8086)(label(0))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4080)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6758)(content(Whitespace\" \"))))(Secondary((id \
                   7776)(content(Comment\"#err: not bound#\"))))(Secondary((id \
                   6021)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   6022)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   8093)(content(Comment\"#non-sum-types cant be \
                   recursive#\"))))(Secondary((id \
                   6070)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6076)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6077)(content(Whitespace\" \"))))(Tile((id \
                   6081)(label(Lol))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   6082)(content(Whitespace\" \")))))((Secondary((id \
                   6084)(content(Whitespace\" \"))))(Tile((id \
                   6088)(label(Lol))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6091)(content(Whitespace\" \")))))))))(Secondary((id \
                   6736)(content(Whitespace\" \"))))(Secondary((id \
                   7778)(content(Comment\"#err: not bound#\"))))(Secondary((id \
                   6466)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   6703)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   6805)(content(Comment\"#no errors: analytic \
                   shadowing#\"))))(Secondary((id \
                   6806)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6812)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6813)(content(Whitespace\" \"))))(Tile((id \
                   6819)(label(Tork1))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   6820)(content(Whitespace\" \")))))((Secondary((id \
                   6822)(content(Whitespace\" \"))))(Tile((id \
                   6823)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6829)(label(Blob))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6832)(content(Whitespace\" \")))))))))(Secondary((id \
                   6834)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6840)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6841)(content(Whitespace\" \"))))(Tile((id \
                   6847)(label(Tork2))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   6848)(content(Whitespace\" \")))))((Secondary((id \
                   6850)(content(Whitespace\" \"))))(Tile((id \
                   6851)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6857)(label(Blob))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6860)(content(Whitespace\" \")))))))))(Secondary((id \
                   6862)(content(Whitespace\" \"))))(Secondary((id \
                   6863)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   6868)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   6869)(content(Whitespace\" \"))))(Tile((id \
                   6870)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   6872)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6878)(label(Tork1))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   6879)(content(Whitespace\" \")))))((Secondary((id \
                   6881)(content(Whitespace\" \"))))(Tile((id \
                   6886)(label(Blob))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   6889)(content(Whitespace\" \")))))))))(Secondary((id \
                   6467)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3994)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   5074)(content(Comment\"#exp tests: \
                   happy#\"))))(Secondary((id \
                   4344)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4350)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4351)(content(Whitespace\" \"))))(Tile((id \
                   5397)(label(YoDawg))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   4354)(content(Whitespace\" \")))))((Secondary((id \
                   4356)(content(Whitespace\" \"))))(Secondary((id \
                   4357)(content(Whitespace\" \"))))(Tile((id \
                   4952)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   4360)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   4434)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   6501)(content(Whitespace\" \"))))(Tile((id \
                   6507)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   8413)(content(Whitespace\" \"))))(Tile((id \
                   6502)(label(Bo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   6503)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   6506)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Tile((id \
                   4367)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4368)(content(Whitespace\" \"))))(Tile((id \
                   4980)(label(Dawg))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   4371)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   4438)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   4378)(content(Whitespace\" \")))))))))(Secondary((id \
                   4388)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4393)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4394)(content(Whitespace\" \"))))(Tile((id \
                   4728)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4397)(content(Whitespace\" \")))))((Secondary((id \
                   4399)(content(Whitespace\" \"))))(Tile((id \
                   4954)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4402)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4403)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   4407)(content(Whitespace\" \")))))))))(Secondary((id \
                   4595)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4600)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4601)(content(Whitespace\" \"))))(Tile((id \
                   4730)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5016)(content(Whitespace\" \"))))(Tile((id \
                   4604)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5013)(content(Whitespace\" \"))))(Tile((id \
                   6116)(label(YoDawg))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4607)(content(Whitespace\" \")))))((Secondary((id \
                   4609)(content(Whitespace\" \"))))(Tile((id \
                   4956)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4612)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4613)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   4617)(content(Whitespace\" \")))))))))(Secondary((id \
                   4619)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4624)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4625)(content(Whitespace\" \"))))(Tile((id \
                   4732)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5017)(content(Whitespace\" \"))))(Tile((id \
                   4628)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5012)(content(Whitespace\" \"))))(Tile((id \
                   4630)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5007)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   4633)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   4638)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   4639)(content(Whitespace\" \")))))((Secondary((id \
                   4641)(content(Whitespace\" \"))))(Tile((id \
                   5009)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4644)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4649)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   4652)(content(Whitespace\" \")))))))))(Secondary((id \
                   4656)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4661)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4662)(content(Whitespace\" \"))))(Tile((id \
                   4734)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5018)(content(Whitespace\" \"))))(Tile((id \
                   4665)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5014)(content(Whitespace\" \"))))(Tile((id \
                   4667)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   5005)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5010)(content(Whitespace\" \"))))(Tile((id \
                   4670)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5011)(content(Whitespace\" \"))))(Tile((id \
                   4984)(label(Dawg))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   4673)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4674)(content(Whitespace\" \"))))(Tile((id \
                   4678)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   4679)(content(Whitespace\" \")))))((Secondary((id \
                   4681)(content(Whitespace\" \"))))(Tile((id \
                   4682)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4989)(label(Dawg))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4685)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4686)(label(5))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   4690)(content(Whitespace\" \")))))))))(Secondary((id \
                   4692)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4697)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4698)(content(Whitespace\" \"))))(Tile((id \
                   4736)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5019)(content(Whitespace\" \"))))(Tile((id \
                   4701)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5015)(content(Whitespace\" \"))))(Tile((id \
                   4713)(label(DoubleAlias))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4714)(content(Whitespace\" \")))))((Secondary((id \
                   4716)(content(Whitespace\" \"))))(Tile((id \
                   4717)(label(C))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4719)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4720)(label(4))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   4724)(content(Whitespace\" \")))))))))(Secondary((id \
                   4782)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4474)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4851)(content(Comment\"#exp tests: \
                   errors#\"))))(Secondary((id \
                   4783)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4788)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4789)(content(Whitespace\" \"))))(Tile((id \
                   4837)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4792)(content(Whitespace\" \")))))((Secondary((id \
                   4794)(content(Whitespace\" \"))))(Tile((id \
                   4795)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4797)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4798)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   4802)(content(Whitespace\" \")))))))))(Secondary((id \
                   7107)(content(Whitespace\" \"))))(Secondary((id \
                   7780)(content(Comment\"#err: incons with \
                   arrow#\"))))(Secondary((id \
                   4804)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4809)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4810)(content(Whitespace\" \"))))(Tile((id \
                   4835)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4813)(content(Whitespace\" \")))))((Secondary((id \
                   4815)(content(Whitespace\" \"))))(Tile((id \
                   4825)(label(Undefined))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4826)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4827)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   4831)(content(Whitespace\" \")))))))))(Secondary((id \
                   7135)(content(Whitespace\" \"))))(Secondary((id \
                   7782)(content(Comment\"#err: cons \
                   undefined#\"))))(Secondary((id \
                   4737)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4479)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4480)(content(Whitespace\" \"))))(Tile((id \
                   4739)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4483)(content(Whitespace\" \")))))((Secondary((id \
                   4485)(content(Whitespace\" \"))))(Tile((id \
                   4486)(label(B))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4488)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4493)(label(\"\\\"lol\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   4496)(content(Whitespace\" \")))))))))(Secondary((id \
                   7158)(content(Whitespace\" \"))))(Secondary((id \
                   7794)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   4444)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4546)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4547)(content(Whitespace\" \"))))(Tile((id \
                   4741)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5020)(content(Whitespace\" \"))))(Tile((id \
                   4550)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4551)(content(Whitespace\" \"))))(Tile((id \
                   4552)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5000)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   4555)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   4560)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   4561)(content(Whitespace\" \")))))((Secondary((id \
                   4563)(content(Whitespace\" \"))))(Tile((id \
                   4958)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4574)(content(Whitespace\" \")))))))))(Secondary((id \
                   7183)(content(Whitespace\" \"))))(Secondary((id \
                   7793)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   4410)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1756)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1757)(content(Whitespace\" \"))))(Tile((id \
                   4743)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5021)(content(Whitespace\" \"))))(Tile((id \
                   1760)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1761)(content(Whitespace\" \"))))(Tile((id \
                   1762)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5003)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1765)(content(Whitespace\" \")))))((Secondary((id \
                   1767)(content(Whitespace\" \"))))(Tile((id \
                   4960)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1770)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4582)(label(\"\\\"lol\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   1778)(content(Whitespace\" \")))))))))(Secondary((id \
                   1780)(content(Whitespace\" \"))))(Secondary((id \
                   7792)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   1713)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1718)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1719)(content(Whitespace\" \"))))(Tile((id \
                   4745)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5022)(content(Whitespace\" \"))))(Tile((id \
                   1722)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1723)(content(Whitespace\" \"))))(Tile((id \
                   4590)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   4593)(label(One))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1735)(content(Whitespace\" \")))))((Secondary((id \
                   1737)(content(Whitespace\" \"))))(Tile((id \
                   4962)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1740)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4594)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   1748)(content(Whitespace\" \")))))))))(Secondary((id \
                   7220)(content(Whitespace\" \"))))(Secondary((id \
                   7791)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   5051)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   5052)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   7272)(content(Comment\"#pat tests: happy (but refutable \
                   patterns so weird)#\"))))(Secondary((id \
                   5239)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5244)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5245)(content(Whitespace\" \"))))(Tile((id \
                   6526)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5249)(content(Whitespace\" \")))))((Secondary((id \
                   5251)(content(Whitespace\" \"))))(Tile((id \
                   6509)(label(Bo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5257)(content(Whitespace\" \")))))))))(Secondary((id \
                   5286)(content(Whitespace\" \"))))(Secondary((id \
                   5307)(content(Comment\"#kind of a weird \
                   edge#\"))))(Secondary((id \
                   5075)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5080)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5081)(content(Whitespace\" \"))))(Tile((id \
                   5139)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   5140)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   5341)(label(1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   5084)(content(Whitespace\" \")))))((Secondary((id \
                   5154)(content(Whitespace\" \"))))(Tile((id \
                   5159)(label(Dawg))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5149)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5164)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   5086)(content(Whitespace\" \")))))))))(Secondary((id \
                   5428)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5433)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5434)(content(Whitespace\" \"))))(Tile((id \
                   5437)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   5438)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   5757)(label(1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   5441)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5442)(content(Whitespace\" \"))))(Tile((id \
                   5449)(label(YoDawg))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5450)(content(Whitespace\" \")))))((Secondary((id \
                   5460)(content(Whitespace\" \"))))(Tile((id \
                   5466)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5467)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5469)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   5452)(content(Whitespace\" \")))))))))(Secondary((id \
                   5708)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5713)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5714)(content(Whitespace\" \"))))(Tile((id \
                   5717)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   5718)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   5755)(label(1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   5721)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5722)(content(Whitespace\" \"))))(Tile((id \
                   5753)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5747)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5748)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   5752)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   5730)(content(Whitespace\" \")))))((Secondary((id \
                   5732)(content(Whitespace\" \"))))(Tile((id \
                   5735)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5736)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5737)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   5741)(content(Whitespace\" \")))))))))(Secondary((id \
                   5743)(content(Whitespace\" \"))))(Secondary((id \
                   2700)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5092)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5093)(content(Whitespace\" \"))))(Tile((id \
                   5167)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   5171)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5180)(content(Whitespace\" \"))))(Tile((id \
                   5173)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5175)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5096)(content(Whitespace\" \")))))((Secondary((id \
                   5177)(content(Whitespace\" \"))))(Tile((id \
                   5179)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5098)(content(Whitespace\" \")))))))))(Secondary((id \
                   5103)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   5120)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   5208)(content(Comment\"#pat tests: \
                   errors#\"))))(Secondary((id \
                   5522)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5527)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5528)(content(Whitespace\" \"))))(Tile((id \
                   5590)(label(2))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   5555)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   5594)(label(1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   5540)(content(Whitespace\" \")))))((Secondary((id \
                   5542)(content(Whitespace\" \"))))(Tile((id \
                   5596)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5551)(content(Whitespace\" \")))))))))(Secondary((id \
                   7273)(content(Whitespace\" \"))))(Secondary((id \
                   7801)(content(Comment\"#err: incons with \
                   arrow#\"))))(Secondary((id \
                   5559)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5564)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5565)(content(Whitespace\" \"))))(Tile((id \
                   6317)(label(NotDefined))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   5568)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   5592)(label(1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   5571)(content(Whitespace\" \")))))((Secondary((id \
                   5573)(content(Whitespace\" \"))))(Tile((id \
                   5598)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5578)(content(Whitespace\" \")))))))))(Secondary((id \
                   5580)(content(Whitespace\" \"))))(Secondary((id \
                   7803)(content(Comment\"#err: cons \
                   undefined#\"))))(Secondary((id \
                   5209)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5264)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5265)(content(Whitespace\" \"))))(Tile((id \
                   5268)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   5269)(content(Whitespace\" \")))))((Secondary((id \
                   5271)(content(Whitespace\" \"))))(Tile((id \
                   5285)(label(Dawg))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5277)(content(Whitespace\" \")))))))))(Secondary((id \
                   5279)(content(Whitespace\" \"))))(Secondary((id \
                   7805)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   5329)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5346)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5347)(content(Whitespace\" \"))))(Tile((id \
                   5350)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   5351)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   5377)(label(true))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   5354)(content(Whitespace\" \")))))((Secondary((id \
                   5356)(content(Whitespace\" \"))))(Tile((id \
                   5361)(label(Dawg))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5362)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5367)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   5370)(content(Whitespace\" \")))))))))(Secondary((id \
                   7339)(content(Whitespace\" \"))))(Secondary((id \
                   7807)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   5470)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5475)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5476)(content(Whitespace\" \"))))(Tile((id \
                   5479)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   5505)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5758)(content(Whitespace\" \"))))(Tile((id \
                   5512)(label(YoDawg))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5486)(content(Whitespace\" \")))))((Secondary((id \
                   5488)(content(Whitespace\" \"))))(Tile((id \
                   5518)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5519)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5521)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   5502)(content(Whitespace\" \")))))))))(Secondary((id \
                   7358)(content(Whitespace\" \"))))(Secondary((id \
                   7809)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   5599)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5604)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5605)(content(Whitespace\" \"))))(Tile((id \
                   5608)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   5663)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   5665)(label(1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   5609)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5759)(content(Whitespace\" \"))))(Tile((id \
                   5666)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5669)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5617)(content(Whitespace\" \")))))((Secondary((id \
                   5619)(content(Whitespace\" \"))))(Tile((id \
                   5622)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   5628)(content(Whitespace\" \")))))))))(Secondary((id \
                   5630)(content(Whitespace\" \"))))(Secondary((id \
                   7811)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   5631)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   5636)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   5637)(content(Whitespace\" \"))))(Tile((id \
                   5640)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   5687)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   5703)(label(1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   5671)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   5760)(content(Whitespace\" \"))))(Tile((id \
                   5673)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5675)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   5676)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   5697)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   5649)(content(Whitespace\" \")))))((Secondary((id \
                   5651)(content(Whitespace\" \"))))(Tile((id \
                   5654)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   5655)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   5702)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   5660)(content(Whitespace\" \")))))))))(Secondary((id \
                   7395)(content(Whitespace\" \"))))(Secondary((id \
                   7813)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   6564)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   7846)(label(\"\\\"Thats all, folks\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2231)(content(Whitespace\"\\226\\143\\142\")))))))(ancestors())))(caret \
                   Outer))";
                backup_text =
                  "# Internal Regression Tests: ADT Statics #\n\
                   # All commented lines should show errors as described #\n\
                   # No other lines should show errors #\n\n\
                   #type definitions: no errors#\n\
                   type   =   in\n\
                   type SingleNull = +One in\n\
                   type Single = +F(Int) in\n\
                   type GoodSum = A + B + C(Int) in\n\
                   type Partial = Ok( ) +   in\n\
                   type DoubleAlias = GoodSum in\n\
                   type VerticalLeading =\n\
                   + A\n\
                   + B(GoodSum)\n\
                   + C(Bool->Bool) \n\
                   in\n\n\
                   #incorrect or incomplete type definitions#\n\
                   type badTypeName =   in #err: invalid type name#\n\
                   type ( ,  ) =   in #err: invalid type name#\n\
                   type   = badTypeToken in #err: invalid type token#\n\
                   type NotASum = NotInSum(Bool) in #err: cons not in sum#\n\
                   type Bool =    in #err: shadows base type#\n\
                   type Dupes =\n\
                   + Guy(Bool) #no err#\n\
                   + Guy(Int) #err: already used#\n\
                   + Guy in #err: already used#\n\
                   type BadCons =\n\
                   + Um(Unbound) #err: unbound type var#\n\
                   + invalid #err: invalid#\n\
                   + Bool #err: expected cons found type#\n\
                   + Int(Int) #err: expected cons found type#\n\
                   + ( )(Int) #err: expected cons found type#\n\
                   + A(Bool)(Int) in #err: expected cons found app#\n\n\
                   #sums in compound aliases dont add ctrs to scope#\n\
                   #but compound alias types should propagate analytically#\n\
                   type CompoundAlias = (Int, Anonymous + Sum) in \n\
                   let _ = (1, Sum) in #err: not defined#\n\
                   let _: CompoundAlias = (1, Sum) in #no error#\n\
                   type Yorp = Int -> (Inside + Ouside) in\n\
                   let _ = fun _ -> Inside in #err: not defined#\n\
                   let _: Yorp = fun _ -> Inside in #no error#\n\
                   type Gargs = [BigGuy + Small] in\n\
                   let _ = BigGuy in #err: not defined#\n\
                   let _: Gargs = [BigGuy] in #no error#\n\
                   let _: Gargs = BigGuy :: [BigGuy] in #no error#\n\n\
                   #unbound tyvars treated as unknown-typehole#\n\
                   let a:Bad = 0 in a == 0; #err: not bound#\n\n\
                   #non-sum-types cant be recursive#\n\
                   type Lol = Lol in #err: not bound#\n\n\
                   #no errors: analytic shadowing#\n\
                   type Tork1 = +Blob in\n\
                   type Tork2 = +Blob in \n\
                   let x:Tork1 = Blob in\n\n\
                   #exp tests: happy#\n\
                   type YoDawg =  Yo(Int) + Bo(Int)+ Dawg(Bool) in\n\
                   let _ = Yo(1) in\n\
                   let _ : YoDawg = Yo(2) in\n\
                   let _ : +Yo(Bool) = Yo(true) in\n\
                   let _ : (Yo + Dawg, Int) = (Dawg,5) in\n\
                   let _ : DoubleAlias = C(4) in\n\n\
                   #exp tests: errors#\n\
                   let _ = 2(1) in #err: incons with arrow#\n\
                   let _ = Undefined(1) in #err: cons undefined#\n\
                   let _ = B(\"lol\") in #err: type incons#\n\
                   let _ : +Yo(Bool) = Yo in #err: type incons#\n\
                   let _ : +Yo = Yo(\"lol\") in #err: type incons#\n\
                   let _ : +One = Yo(1) in #err: type incons#\n\n\
                   #pat tests: happy (but refutable patterns so weird)#\n\
                   let Yo = Bo in #kind of a weird edge#\n\
                   let Yo(1) = Dawg(true) in\n\
                   let Yo(1): YoDawg = Yo(1) in\n\
                   let Yo(1): +Yo(Int) = Yo(1) in \n\
                   let Yo: +Yo = Yo in\n\n\
                   #pat tests: errors#\n\
                   let 2(1) = 3 in #err: incons with arrow#\n\
                   let NotDefined(1) = 3 in #err: cons undefined#\n\
                   let Yo = Dawg in #err: type incons#\n\
                   let Yo(true) = Dawg(true) in #err: type incons#\n\
                   let Yo: YoDawg = Yo(1) in #err: type incons#\n\
                   let Yo(1): +Yo = Yo in #err: type incons#\n\
                   let Yo(1): +Yo(Bool) = Yo(true) in #err: type incons#\n\
                   \"Thats all, folks\"\n";
              } ) );
          ( "Polymorphism",
            ( 4819,
              {
                zipper =
                  "((selection((focus \
                   Left)(content())))(backpack())(relatives((siblings(((Tile((id \
                   4778)(label(ex1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4779)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4781)(content(Whitespace\" \"))))(Tile((id \
                   4788)(label(ex2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4789)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4791)(content(Whitespace\" \")))))((Tile((id \
                   4794)(label(ex3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4795)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4797)(content(Whitespace\" \"))))(Tile((id \
                   4800)(label(ex4))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   4801)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4803)(content(Whitespace\" \"))))(Tile((id \
                   4806)(label(ex5))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))(ancestors((((id \
                   4808)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards((0)(1)))(children(()())))(((Secondary((id \
                   1497)(content(Comment\"# Polymorphism #\"))))(Secondary((id \
                   1465)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   1544)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   1675)(content(Comment\"# We can take types as parameters to \
                   type functions, #\"))))(Secondary((id \
                   1676)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   1722)(content(Comment\"# and use them in annoatations in \
                   the body: #\"))))(Secondary((id \
                   1466)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1502)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1503)(content(Whitespace\" \"))))(Tile((id \
                   1506)(label(id))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1507)(content(Whitespace\" \")))))((Secondary((id \
                   1508)(content(Whitespace\" \"))))(Tile((id \
                   1516)(label(typfun ->))(mold((out \
                   Exp)(in_(TPat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1517)(content(Whitespace\" \
                   \"))))(Tile((id 1518)(label(A))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   1520)(content(Whitespace\" \")))))))))(Secondary((id \
                   1522)(content(Whitespace\" \"))))(Tile((id 1527)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1528)(content(Whitespace\" \
                   \"))))(Tile((id 1529)(label(x))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1531)(content(Whitespace\" \"))))(Tile((id \
                   1532)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1533)(content(Whitespace\" \"))))(Tile((id \
                   1534)(label(A))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1536)(content(Whitespace\" \")))))))))(Secondary((id \
                   1538)(content(Whitespace\" \"))))(Tile((id \
                   1539)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1543)(content(Whitespace\" \")))))))))(Secondary((id \
                   1467)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   1723)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   1761)(content(Comment\"# Such functions are applied like \
                   so: #\"))))(Secondary((id \
                   1468)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4680)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4681)(content(Whitespace\" \"))))(Tile((id \
                   4686)(label(ex1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4688)(content(Whitespace\" \")))))((Secondary((id \
                   4689)(content(Whitespace\" \"))))(Tile((id \
                   4685)(label(id))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 1766)(label(@< \
                   >))(mold((out Exp)(in_(Typ))(nibs(((shape(Concave 1))(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 1770)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Tile((id \
                   2088)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2090)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   4694)(content(Whitespace\" \")))))))))(Secondary((id \
                   2023)(content(Whitespace\" \"))))(Secondary((id \
                   2085)(content(Comment\"# 1 #\"))))(Secondary((id \
                   1469)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   1470)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   1878)(content(Comment\"# We can annotate the type of a type \
                   function with a forall. #\"))))(Secondary((id \
                   4690)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1884)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1885)(content(Whitespace\" \"))))(Tile((id \
                   1891)(label(const))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1892)(content(Whitespace\" \"))))(Tile((id \
                   1893)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1894)(content(Whitespace\" \"))))(Tile((id \
                   1902)(label(forall .))(mold((out \
                   Typ)(in_(TPat))(nibs(((shape Convex)(sort \
                   Typ))((shape(Concave 13))(sort Typ))))))(shards(0 \
                   1))(children(((Secondary((id 1903)(content(Whitespace\" \
                   \"))))(Tile((id 1904)(label(A))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   1914)(content(Whitespace\" \")))))))))(Secondary((id \
                   1915)(content(Whitespace\" \"))))(Tile((id \
                   1929)(label(forall .))(mold((out \
                   Typ)(in_(TPat))(nibs(((shape Convex)(sort \
                   Typ))((shape(Concave 13))(sort Typ))))))(shards(0 \
                   1))(children(((Secondary((id 1930)(content(Whitespace\" \
                   \"))))(Tile((id 1931)(label(B))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   1933)(content(Whitespace\" \")))))))))(Secondary((id \
                   1934)(content(Whitespace\" \"))))(Tile((id \
                   1935)(label(A))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1937)(content(Whitespace\" \"))))(Tile((id \
                   1939)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1940)(content(Whitespace\" \"))))(Tile((id \
                   1941)(label(B))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1943)(content(Whitespace\" \"))))(Tile((id \
                   1945)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1946)(content(Whitespace\" \"))))(Tile((id \
                   1947)(label(A))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1948)(content(Whitespace\" \")))))((Secondary((id \
                   1953)(content(Whitespace\" \"))))(Secondary((id \
                   2008)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1985)(label(typfun ->))(mold((out \
                   Exp)(in_(TPat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1986)(content(Whitespace\" \
                   \"))))(Tile((id 1989)(label(A))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   1991)(content(Whitespace\" \")))))))))(Secondary((id \
                   1993)(content(Whitespace\" \"))))(Tile((id \
                   2001)(label(typfun ->))(mold((out \
                   Exp)(in_(TPat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2002)(content(Whitespace\" \
                   \"))))(Tile((id 2003)(label(B))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2005)(content(Whitespace\" \")))))))))(Secondary((id \
                   2007)(content(Whitespace\" \"))))(Tile((id 1957)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1958)(content(Whitespace\" \
                   \"))))(Tile((id 1960)(label(x))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1961)(content(Whitespace\" \")))))))))(Secondary((id \
                   1964)(content(Whitespace\" \"))))(Tile((id 1968)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 1969)(content(Whitespace\" \
                   \"))))(Tile((id 1971)(label(y))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1972)(content(Whitespace\" \")))))))))(Secondary((id \
                   1975)(content(Whitespace\" \"))))(Tile((id \
                   1976)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1977)(content(Whitespace\" \")))))))))(Secondary((id \
                   1879)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4700)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4701)(content(Whitespace\" \"))))(Tile((id \
                   4706)(label(ex2))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4708)(content(Whitespace\" \")))))((Secondary((id \
                   4709)(content(Whitespace\" \"))))(Tile((id \
                   4705)(label(const))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 2025)(label(@< \
                   >))(mold((out Exp)(in_(Typ))(nibs(((shape(Concave 1))(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 2033)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Tile((id \
                   2049)(label(@< >))(mold((out \
                   Exp)(in_(Typ))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2056)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Tile((id \
                   2057)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2062)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   2064)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2077)(label(\"\\\"Hello World\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   4713)(content(Whitespace\" \")))))))))(Secondary((id \
                   2079)(content(Whitespace\" \"))))(Secondary((id \
                   2083)(content(Comment\"# 2 #\"))))(Secondary((id \
                   2009)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2091)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2158)(content(Comment\"# We can go beyond rank 1 \
                   polymorphism: #\"))))(Secondary((id \
                   2092)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2164)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2165)(content(Whitespace\" \"))))(Tile((id \
                   2187)(label(apply_both))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2188)(content(Whitespace\" \"))))(Tile((id \
                   2191)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2218)(content(Whitespace\" \"))))(Tile((id \
                   2227)(label(forall .))(mold((out \
                   Typ)(in_(TPat))(nibs(((shape Convex)(sort \
                   Typ))((shape(Concave 13))(sort Typ))))))(shards(0 \
                   1))(children(((Secondary((id 2228)(content(Whitespace\" \
                   \"))))(Tile((id 2229)(label(A))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2231)(content(Whitespace\" \")))))))))(Secondary((id \
                   2232)(content(Whitespace\" \"))))(Tile((id \
                   2240)(label(forall .))(mold((out \
                   Typ)(in_(TPat))(nibs(((shape Convex)(sort \
                   Typ))((shape(Concave 13))(sort Typ))))))(shards(0 \
                   1))(children(((Secondary((id 2241)(content(Whitespace\" \
                   \"))))(Tile((id 2242)(label(B))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2244)(content(Whitespace\" \")))))))))(Secondary((id \
                   2245)(content(Whitespace\" \"))))(Tile((id \
                   2274)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   2282)(label(forall .))(mold((out \
                   Typ)(in_(TPat))(nibs(((shape Convex)(sort \
                   Typ))((shape(Concave 13))(sort Typ))))))(shards(0 \
                   1))(children(((Secondary((id 2283)(content(Whitespace\" \
                   \"))))(Tile((id 2284)(label(D))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2286)(content(Whitespace\" \")))))))))(Secondary((id \
                   2287)(content(Whitespace\" \"))))(Tile((id \
                   2288)(label(D))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2306)(content(Whitespace\" \"))))(Tile((id \
                   2304)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2316)(content(Whitespace\" \"))))(Tile((id \
                   2317)(label(D))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2324)(content(Whitespace\" \"))))(Tile((id \
                   2321)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2323)(content(Whitespace\" \"))))(Tile((id \
                   2327)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   2329)(label(A))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2330)(content(Whitespace\" \"))))(Tile((id \
                   2331)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2333)(content(Whitespace\" \"))))(Tile((id \
                   2334)(label(B))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2341)(content(Whitespace\" \"))))(Tile((id \
                   2338)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2345)(content(Whitespace\" \"))))(Tile((id \
                   2346)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   2347)(label(A))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2348)(content(Whitespace\" \"))))(Tile((id \
                   2349)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2351)(content(Whitespace\" \"))))(Tile((id \
                   2352)(label(B))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2353)(content(Whitespace\" \")))))((Secondary((id \
                   2355)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2377)(label(typfun ->))(mold((out \
                   Exp)(in_(TPat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2378)(content(Whitespace\" \
                   \"))))(Tile((id 2380)(label(A))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2381)(content(Whitespace\" \")))))))))(Secondary((id \
                   2384)(content(Whitespace\" \"))))(Tile((id \
                   2391)(label(typfun ->))(mold((out \
                   Exp)(in_(TPat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2392)(content(Whitespace\" \
                   \"))))(Tile((id 2394)(label(B))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2395)(content(Whitespace\" \")))))))))(Secondary((id \
                   2402)(content(Whitespace\" \"))))(Tile((id 2408)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2409)(content(Whitespace\" \
                   \"))))(Tile((id 2411)(label(f))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2412)(content(Whitespace\" \")))))))))(Secondary((id \
                   2415)(content(Whitespace\" \"))))(Tile((id 2419)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2420)(content(Whitespace\" \
                   \"))))(Tile((id 2422)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   2423)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2424)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2426)(content(Whitespace\" \"))))(Tile((id \
                   2427)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   2428)(content(Whitespace\" \")))))))))(Secondary((id \
                   2431)(content(Whitespace\" \"))))(Tile((id \
                   2432)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2433)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 2452)(label(@< \
                   >))(mold((out Exp)(in_(Typ))(nibs(((shape(Concave 1))(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 2453)(label(A))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Tile((id \
                   2440)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2442)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   2443)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2445)(content(Whitespace\" \"))))(Tile((id \
                   2446)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 2455)(label(@< \
                   >))(mold((out Exp)(in_(Typ))(nibs(((shape(Concave 1))(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 2456)(label(B))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Tile((id \
                   2447)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2449)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                   2450)(content(Whitespace\" \")))))))))(Secondary((id \
                   2308)(content(Whitespace\" \"))))(Secondary((id \
                   2313)(content(Whitespace\" \"))))(Secondary((id \
                   2159)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4719)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4720)(content(Whitespace\" \"))))(Tile((id \
                   4725)(label(ex3))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4727)(content(Whitespace\" \")))))((Secondary((id \
                   4728)(content(Whitespace\" \"))))(Tile((id \
                   4724)(label(apply_both))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 2476)(label(@< \
                   >))(mold((out Exp)(in_(Typ))(nibs(((shape(Concave 1))(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 2481)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Tile((id \
                   2483)(label(@< >))(mold((out \
                   Exp)(in_(Typ))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2490)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Tile((id \
                   2491)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2494)(label(id))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   2495)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2501)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2503)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2504)(content(Whitespace\" \"))))(Tile((id \
                   2517)(label(\"\\\"Hello World\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   4732)(content(Whitespace\" \")))))))))(Secondary((id \
                   2526)(content(Whitespace\" \"))))(Secondary((id \
                   2548)(content(Comment\"# (3, \\\"Hello World\\\") \
                   #\"))))(Secondary((id 2521)(content(Whitespace\" \
                   \"))))(Secondary((id \
                   2457)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2549)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2650)(content(Comment\"# Finally, here is a more in-depth, \
                   yet applicable example: polymorphic map \
                   #\"))))(Secondary((id \
                   2550)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2932)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2933)(content(Whitespace\" \"))))(Tile((id \
                   2943)(label(emptylist))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2944)(content(Whitespace\" \"))))(Tile((id \
                   2945)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2946)(content(Whitespace\" \"))))(Tile((id \
                   2954)(label(forall .))(mold((out \
                   Typ)(in_(TPat))(nibs(((shape Convex)(sort \
                   Typ))((shape(Concave 13))(sort Typ))))))(shards(0 \
                   1))(children(((Secondary((id 2955)(content(Whitespace\" \
                   \"))))(Tile((id 2956)(label(A))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2958)(content(Whitespace\" \")))))))))(Secondary((id \
                   2959)(content(Whitespace\" \"))))(Tile((id 2960)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 2961)(label(A))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2963)(content(Whitespace\" \")))))((Secondary((id \
                   2964)(content(Whitespace\" \"))))(Tile((id \
                   2972)(label(typfun ->))(mold((out \
                   Exp)(in_(TPat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2973)(content(Whitespace\" \
                   \"))))(Tile((id 2974)(label(A))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2976)(content(Whitespace\" \")))))))))(Secondary((id \
                   2978)(content(Whitespace\" \"))))(Tile((id \
                   2980)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2984)(content(Whitespace\" \")))))))))(Secondary((id \
                   2986)(content(Whitespace\" \"))))(Secondary((id \
                   3009)(content(Comment\"# polymorphic constant \
                   #\"))))(Secondary((id \
                   2927)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2656)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2657)(content(Whitespace\" \"))))(Tile((id \
                   2661)(label(map))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2662)(content(Whitespace\" \"))))(Tile((id \
                   2663)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2664)(content(Whitespace\" \"))))(Tile((id \
                   2672)(label(forall .))(mold((out \
                   Typ)(in_(TPat))(nibs(((shape Convex)(sort \
                   Typ))((shape(Concave 13))(sort Typ))))))(shards(0 \
                   1))(children(((Secondary((id 2673)(content(Whitespace\" \
                   \"))))(Tile((id 2674)(label(A))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2676)(content(Whitespace\" \")))))))))(Secondary((id \
                   2677)(content(Whitespace\" \"))))(Tile((id \
                   2685)(label(forall .))(mold((out \
                   Typ)(in_(TPat))(nibs(((shape Convex)(sort \
                   Typ))((shape(Concave 13))(sort Typ))))))(shards(0 \
                   1))(children(((Secondary((id 2686)(content(Whitespace\" \
                   \"))))(Tile((id 2687)(label(B))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2689)(content(Whitespace\" \")))))))))(Secondary((id \
                   2690)(content(Whitespace\" \"))))(Tile((id \
                   2691)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   2692)(label(A))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2701)(content(Whitespace\" \"))))(Tile((id \
                   2697)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2698)(content(Whitespace\" \"))))(Tile((id \
                   2699)(label(B))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2702)(content(Whitespace\" \"))))(Tile((id \
                   2704)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2705)(content(Whitespace\" \"))))(Tile((id \
                   2706)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   2707)(label([ ]))(mold((out Typ)(in_(Typ))(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 2708)(label(A))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2719)(content(Whitespace\" \"))))(Tile((id \
                   2713)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2714)(content(Whitespace\" \"))))(Tile((id 2715)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 2716)(label(B))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                   2720)(content(Whitespace\" \")))))((Secondary((id \
                   2721)(content(Whitespace\" \"))))(Secondary((id \
                   2722)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2730)(label(typfun ->))(mold((out \
                   Exp)(in_(TPat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2731)(content(Whitespace\" \
                   \"))))(Tile((id 2732)(label(A))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2734)(content(Whitespace\" \")))))))))(Secondary((id \
                   2736)(content(Whitespace\" \"))))(Tile((id \
                   2744)(label(typfun ->))(mold((out \
                   Exp)(in_(TPat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2745)(content(Whitespace\" \
                   \"))))(Tile((id 2746)(label(B))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2748)(content(Whitespace\" \")))))))))(Secondary((id \
                   2755)(content(Whitespace\" \"))))(Tile((id 2759)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2760)(content(Whitespace\" \
                   \"))))(Tile((id 2762)(label(f))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2763)(content(Whitespace\" \"))))(Tile((id \
                   2764)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2766)(content(Whitespace\" \"))))(Tile((id \
                   2775)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   2776)(label(A))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2788)(content(Whitespace\" \"))))(Tile((id \
                   2780)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2782)(content(Whitespace\" \"))))(Tile((id \
                   2783)(label(B))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2784)(content(Whitespace\" \")))))))))(Secondary((id \
                   2787)(content(Whitespace\" \"))))(Tile((id 2792)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2793)(content(Whitespace\" \
                   \"))))(Tile((id 2795)(label(l))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2797)(content(Whitespace\" \"))))(Tile((id \
                   2798)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2800)(content(Whitespace\" \"))))(Tile((id 2801)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 2802)(label(A))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2803)(content(Whitespace\" \")))))))))(Secondary((id \
                   2807)(content(Whitespace\" \"))))(Secondary((id \
                   4473)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2826)(label(case end))(mold((out \
                   Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2827)(content(Whitespace\" \
                   \"))))(Tile((id 2829)(label(l))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2838)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2839)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2841)(content(Whitespace\" \
                   \"))))(Tile((id 2842)(label(h))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2843)(content(Whitespace\" \"))))(Tile((id \
                   2846)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   6))(sort Pat))((shape(Concave 6))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2847)(content(Whitespace\" \"))))(Tile((id \
                   2848)(label(t))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2849)(content(Whitespace\" \")))))))))(Secondary((id \
                   2852)(content(Whitespace\" \"))))(Tile((id \
                   2863)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2864)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2866)(label(h))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2867)(content(Whitespace\" \"))))(Tile((id \
                   2870)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   6))(sort Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2871)(content(Whitespace\" \"))))(Tile((id \
                   2874)(label(map))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 2878)(label(@< \
                   >))(mold((out Exp)(in_(Typ))(nibs(((shape(Concave 1))(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 2879)(label(A))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Tile((id \
                   2882)(label(@< >))(mold((out \
                   Exp)(in_(Typ))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2883)(label(B))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Tile((id \
                   2884)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2886)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   2887)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2889)(label(t))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2890)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2891)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2893)(content(Whitespace\" \
                   \"))))(Tile((id 2925)(label(_))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2926)(content(Whitespace\" \")))))))))(Secondary((id \
                   2899)(content(Whitespace\" \"))))(Tile((id \
                   3020)(label(emptylist))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 3023)(label(@< \
                   >))(mold((out Exp)(in_(Typ))(nibs(((shape(Concave 1))(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 3024)(label(B))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2836)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   2837)(content(Whitespace\" \")))))))))(Secondary((id \
                   2651)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4740)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4741)(content(Whitespace\" \"))))(Tile((id \
                   4746)(label(ex4))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4748)(content(Whitespace\" \")))))((Secondary((id \
                   4749)(content(Whitespace\" \"))))(Tile((id \
                   4745)(label(map))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 3033)(label(@< \
                   >))(mold((out Exp)(in_(Typ))(nibs(((shape(Concave 1))(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 3037)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Tile((id \
                   3039)(label(@< >))(mold((out \
                   Exp)(in_(Typ))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3046)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Tile((id \
                   3051)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3068)(label(string_of_int))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   3073)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3080)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 3082)(label(1))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   3086)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3089)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3090)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3092)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                   4753)(content(Whitespace\" \")))))))))(Secondary((id \
                   3101)(content(Whitespace\" \"))))(Secondary((id \
                   3125)(content(Comment\"# [\\\"1\\\", \\\"2\\\", \\\"3\\\"] \
                   #\"))))(Secondary((id \
                   3126)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3128)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3129)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3147)(content(Comment\"# Recursive types \
                   #\"))))(Secondary((id \
                   3148)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3149)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3266)(content(Comment\"# We can express types that are the \
                   least fixed point of #\"))))(Secondary((id \
                   3267)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3319)(content(Comment\"# some type function with the rec \
                   keyword. #\"))))(Secondary((id \
                   1472)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   681)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   682)(content(Whitespace\" \"))))(Tile((id \
                   970)(label(MyList))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   975)(content(Whitespace\" \")))))((Secondary((id \
                   3332)(content(Whitespace\" \"))))(Tile((id 891)(label(rec \
                   .))(mold((out Typ)(in_(TPat))(nibs(((shape Convex)(sort \
                   Typ))((shape(Concave 13))(sort Typ))))))(shards(0 \
                   1))(children(((Secondary((id 989)(content(Whitespace\" \
                   \"))))(Tile((id 4015)(label(A))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children()))))))))(Secondary((id \
                   4013)(content(Whitespace\" \"))))(Tile((id \
                   1075)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   894)(label(Nil))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   851)(content(Whitespace\" \"))))(Tile((id \
                   849)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   852)(content(Whitespace\" \"))))(Tile((id \
                   856)(label(Cons))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   868)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   4021)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   873)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   875)(content(Whitespace\" \"))))(Tile((id \
                   4017)(label(A))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                   885)(content(Whitespace\" \")))))))))(Secondary((id \
                   3562)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4217)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4356)(content(Comment\"# Hazel does not (yet) support \
                   higher-kinded or existential types, #\"))))(Secondary((id \
                   4287)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4357)(content(Comment\"# So we cannot implement our own \
                   polymorphic lists. #\"))))(Secondary((id \
                   4218)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3782)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3899)(content(Comment\"# Now anything that returns an \
                   element of the least fixed point matches MyList. \
                   #\"))))(Secondary((id \
                   3900)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3905)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3906)(content(Whitespace\" \"))))(Tile((id \
                   3907)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3909)(content(Whitespace\" \"))))(Tile((id \
                   3910)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3911)(content(Whitespace\" \"))))(Tile((id \
                   3918)(label(MyList))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3979)(content(Whitespace\" \")))))((Secondary((id \
                   3920)(content(Whitespace\" \"))))(Tile((id \
                   3995)(label(Cons))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3926)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3927)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3929)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3930)(content(Whitespace\" \"))))(Tile((id \
                   3935)(label(Cons))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3936)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3937)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3939)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3940)(content(Whitespace\" \"))))(Tile((id \
                   3945)(label(Cons))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3946)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3947)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3949)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3950)(content(Whitespace\" \"))))(Tile((id \
                   3954)(label(Nil))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                   3957)(content(Whitespace\" \")))))))))(Secondary((id \
                   3783)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3563)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4216)(content(Comment\"# Note that if the sum is the top \
                   level operator, #\"))))(Secondary((id \
                   3616)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3701)(content(Comment\"# type aliases are implicitly least \
                   fixed points on their own name: #\"))))(Secondary((id \
                   990)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1002)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1011)(content(Whitespace\" \"))))(Tile((id \
                   1091)(label(MyList2))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   1141)(content(Whitespace\" \")))))((Secondary((id \
                   3513)(content(Whitespace\" \"))))(Tile((id \
                   3507)(label(Nil))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3378)(content(Whitespace\" \"))))(Tile((id \
                   3379)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3381)(content(Whitespace\" \"))))(Tile((id \
                   3385)(label(Cons))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   3386)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   3705)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   3389)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3391)(content(Whitespace\" \"))))(Tile((id \
                   3398)(label(MyList2))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   3399)(content(Whitespace\" \")))))))))(Secondary((id \
                   3706)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3715)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3716)(content(Whitespace\" \"))))(Tile((id \
                   4079)(label(Broken))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   3730)(content(Whitespace\" \")))))((Secondary((id \
                   3740)(content(Whitespace\" \"))))(Tile((id \
                   4111)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4122)(content(Whitespace\" \"))))(Tile((id \
                   4121)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3744)(content(Whitespace\" \"))))(Tile((id \
                   4120)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   4134)(label(HasInt))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   4135)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   4138)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   4062)(content(Whitespace\" \"))))(Tile((id \
                   4063)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4065)(content(Whitespace\" \"))))(Tile((id \
                   4149)(label(HasMore))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   4098)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   4154)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   4155)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4156)(content(Whitespace\" \"))))(Tile((id \
                   4153)(label(Broken))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                   4106)(content(Whitespace\" \")))))))))(Secondary((id \
                   3779)(content(Whitespace\" \"))))(Secondary((id \
                   3780)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3781)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   907)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4566)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4567)(content(Whitespace\" \"))))(Tile((id \
                   4585)(label(list_of_mylist))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   921)(content(Whitespace\" \"))))(Tile((id \
                   4410)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4412)(content(Whitespace\" \"))))(Tile((id \
                   4435)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   4418)(label(MyList))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4448)(content(Whitespace\" \"))))(Tile((id \
                   4439)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4441)(content(Whitespace\" \"))))(Tile((id 4603)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 4444)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                   4604)(content(Whitespace\" \")))))((Secondary((id \
                   4449)(content(Whitespace\" \"))))(Tile((id 4453)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 4454)(content(Whitespace\" \
                   \"))))(Tile((id 4458)(label(myl))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4459)(content(Whitespace\" \"))))(Tile((id \
                   4460)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4462)(content(Whitespace\" \"))))(Tile((id \
                   4468)(label(MyList))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   4469)(content(Whitespace\" \")))))))))(Secondary((id \
                   4472)(content(Whitespace\" \"))))(Secondary((id \
                   4479)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4484)(label(case end))(mold((out \
                   Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 4485)(content(Whitespace\" \
                   \"))))(Tile((id 4489)(label(myl))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4490)(content(Whitespace\" \"))))(Secondary((id \
                   4491)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4492)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 4494)(content(Whitespace\" \
                   \"))))(Tile((id 4497)(label(Nil))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4498)(content(Whitespace\" \")))))))))(Secondary((id \
                   4506)(content(Whitespace\" \"))))(Tile((id \
                   4508)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4509)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4510)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 4512)(content(Whitespace\" \
                   \"))))(Tile((id 4516)(label(Cons))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   4517)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   4519)(label(h))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   4520)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4522)(content(Whitespace\" \"))))(Tile((id \
                   4525)(label(t))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   4526)(content(Whitespace\" \")))))))))(Secondary((id \
                   4529)(content(Whitespace\" \"))))(Tile((id \
                   4530)(label(h))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4531)(content(Whitespace\" \"))))(Tile((id \
                   4534)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   6))(sort Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   4535)(content(Whitespace\" \"))))(Tile((id \
                   4549)(label(list_of_mylist))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   4550)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4552)(label(t))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   4560)(content(Whitespace\" \"))))(Secondary((id \
                   4554)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   4601)(content(Whitespace\" \")))))))))(Secondary((id \
                   4606)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   4759)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   4760)(content(Whitespace\" \"))))(Tile((id \
                   4765)(label(ex5))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   4767)(content(Whitespace\" \")))))((Secondary((id \
                   4768)(content(Whitespace\" \"))))(Tile((id \
                   4764)(label(list_of_mylist))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   4621)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   4623)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   4771)(content(Whitespace\" \")))))))))(Secondary((id \
                   4773)(content(Whitespace\" \"))))(Secondary((id \
                   4640)(content(Comment\"# [1, 2, 3] #\"))))(Secondary((id \
                   4643)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4644)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4645)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   4673)(content(Comment\"# All output from examples: \
                   #\"))))(Secondary((id \
                   4674)(content(Whitespace\"\\226\\143\\142\")))))()))))))(caret \
                   Outer))";
                backup_text =
                  "# Polymorphism #\n\n\
                   # We can take types as parameters to type functions, #\n\
                   # and use them in annoatations in the body: #\n\
                   let id = typfun A -> fun x : A -> x in\n\n\
                   # Such functions are applied like so: #\n\
                   let ex1 = id@<Int>(1) in # 1 #\n\n\
                   # We can annotate the type of a type function with a \
                   forall. #\n\
                   let const : forall A . forall B . A -> B -> A = \n\
                   typfun A -> typfun B -> fun x -> fun y -> x in\n\
                   let ex2 = const@<Int>@<String>(2)(\"Hello World\") in # 2 #\n\n\
                   # We can go beyond rank 1 polymorphism: #\n\
                   let apply_both : forall A . forall B . (forall D . D -> D) \
                   -> (A , B) -> (A , B) =\n\
                   typfun A -> typfun B -> fun f -> fun (x, y) -> (f@<A>(x), \
                   f@<B>(y)) in  \n\
                   let ex3 = apply_both@<Int>@<String>(id)(3, \"Hello World\") \
                   in # (3, \"Hello World\") # \n\n\
                   # Finally, here is a more in-depth, yet applicable example: \
                   polymorphic map #\n\
                   let emptylist : forall A . [A] = typfun A -> [] in # \
                   polymorphic constant #\n\
                   let map : forall A . forall B . (A -> B) -> ([A] -> [B]) = \n\
                   typfun A -> typfun B -> fun f : (A -> B) -> fun l : [A] -> \n\
                   case l\n\
                   | h :: t => f(h) :: map@<A>@<B>(f)(t)\n\
                   | _ => emptylist@<B>\n\
                   end in\n\
                   let ex4 = map@<Int>@<String>(string_of_int)([1,2,3]) in # \
                   [\"1\", \"2\", \"3\"] #\n\n\n\
                   # Recursive types #\n\n\
                   # We can express types that are the least fixed point of #\n\
                   # some type function with the rec keyword. #\n\
                   type MyList = rec A. (Nil + Cons(Int, A)) in\n\n\
                   # Hazel does not (yet) support higher-kinded or existential \
                   types, #\n\
                   # So we cannot implement our own polymorphic lists. #\n\n\
                   # Now anything that returns an element of the least fixed \
                   point matches MyList. #\n\
                   let x : MyList = Cons(1, Cons(2, Cons(3, Nil))) in\n\n\
                   # Note that if the sum is the top level operator, #\n\
                   # type aliases are implicitly least fixed points on their \
                   own name: #\n\
                   type MyList2 = Nil + Cons(Int, MyList2) in\n\
                   type Broken = Int -> (HasInt(Int) + HasMore(Int, Broken)) \
                   in \n\n\n\
                   let list_of_mylist : (MyList -> [Int]) = fun myl : MyList -> \n\
                   case myl \n\
                   | Nil => []\n\
                   | Cons(h, t) => h :: list_of_mylist(t) \n\
                   end in\n\
                   let ex5 = list_of_mylist(x) in # [1, 2, 3] #\n\n\n\
                   # All output from examples: #\n\
                   (ex1, ex2, ex3, ex4, ex5)";
              } ) );
          ( "Basic Reference",
            ( 13633,
              {
                zipper =
                  "((selection((focus \
                   Left)(content())))(backpack())(relatives((siblings(()((Secondary((id \
                   2915)(content(Comment\"# Hazel Language Quick Reference \
                   #\"))))(Secondary((id \
                   3020)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3021)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3150)(content(Comment\"# Empty holes stand for missing \
                   expressions, patterns, or types #\"))))(Secondary((id \
                   3030)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3035)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3036)(content(Whitespace\" \"))))(Tile((id \
                   3047)(label(empty_hole))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3048)(content(Whitespace\" \")))))((Secondary((id \
                   3052)(content(Whitespace\" \"))))(Grout((id 3051)(shape \
                   Convex)))(Secondary((id 3050)(content(Whitespace\" \
                   \")))))))))(Secondary((id \
                   3151)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3152)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2840)(content(Comment\"# Integers #\"))))(Secondary((id \
                   2829)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1498)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1499)(content(Whitespace\" \"))))(Tile((id \
                   1508)(label(int_lits))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1509)(content(Whitespace\" \"))))(Tile((id \
                   1510)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1512)(content(Whitespace\" \"))))(Tile((id \
                   1515)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1516)(content(Whitespace\" \")))))((Secondary((id \
                   1519)(content(Whitespace\" \"))))(Tile((id \
                   1520)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1523)(content(Whitespace\" \")))))))))(Secondary((id \
                   1525)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1530)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1531)(content(Whitespace\" \"))))(Tile((id \
                   1540)(label(negation))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1541)(content(Whitespace\" \")))))((Secondary((id \
                   1544)(content(Whitespace\" \"))))(Tile((id \
                   1545)(label(-))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 2))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1546)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1549)(content(Whitespace\" \")))))))))(Secondary((id \
                   1551)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1556)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1557)(content(Whitespace\" \"))))(Tile((id \
                   1568)(label(arithmetic))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1569)(content(Whitespace\" \")))))((Secondary((id \
                   1572)(content(Whitespace\" \"))))(Tile((id \
                   1573)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1574)(label(*))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   4))(sort Exp))((shape(Concave 4))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1576)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1577)(content(Whitespace\" \"))))(Tile((id \
                   1578)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1580)(content(Whitespace\" \"))))(Tile((id \
                   1581)(label(8))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1582)(label(/))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   4))(sort Exp))((shape(Concave 4))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1584)(label(4))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1587)(content(Whitespace\" \")))))))))(Secondary((id \
                   1589)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1594)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1595)(content(Whitespace\" \"))))(Tile((id \
                   1610)(label(int_comparison))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1611)(content(Whitespace\" \")))))((Secondary((id \
                   1614)(content(Whitespace\" \"))))(Tile((id \
                   1615)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1617)(label(10))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1618)(content(Whitespace\" \"))))(Tile((id \
                   1621)(label(==))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   8))(sort Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1622)(content(Whitespace\" \"))))(Tile((id \
                   1624)(label(10))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1625)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1627)(content(Whitespace\" \"))))(Tile((id \
                   1628)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1629)(content(Whitespace\" \"))))(Tile((id \
                   1630)(label(<))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1632)(content(Whitespace\" \"))))(Tile((id \
                   1633)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1634)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1636)(content(Whitespace\" \"))))(Tile((id \
                   1637)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1638)(content(Whitespace\" \"))))(Tile((id \
                   1641)(label(<=))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   8))(sort Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1642)(content(Whitespace\" \"))))(Tile((id \
                   1643)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1644)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1646)(content(Whitespace\" \"))))(Tile((id \
                   1647)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1648)(content(Whitespace\" \"))))(Tile((id \
                   1649)(label(>))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1651)(content(Whitespace\" \"))))(Tile((id \
                   1652)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1653)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1655)(content(Whitespace\" \"))))(Tile((id \
                   1656)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1657)(content(Whitespace\" \"))))(Tile((id \
                   1660)(label(>=))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   8))(sort Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1661)(content(Whitespace\" \"))))(Tile((id \
                   1662)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   1665)(content(Whitespace\" \")))))))))(Secondary((id \
                   3353)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2841)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2866)(content(Comment\"# Floating Point Numbers \
                   #\"))))(Secondary((id \
                   1669)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1673)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1674)(content(Whitespace\" \"))))(Tile((id \
                   1685)(label(float_lits))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1686)(content(Whitespace\" \"))))(Tile((id \
                   1687)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1689)(content(Whitespace\" \"))))(Tile((id \
                   1694)(label(Float))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1695)(content(Whitespace\" \")))))((Secondary((id \
                   1698)(content(Whitespace\" \"))))(Tile((id \
                   1701)(label(1.5))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1704)(content(Whitespace\" \")))))))))(Secondary((id \
                   1706)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1711)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1712)(content(Whitespace\" \"))))(Tile((id \
                   1724)(label(float_artih))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1725)(content(Whitespace\" \")))))((Secondary((id \
                   1728)(content(Whitespace\" \"))))(Tile((id \
                   1730)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1731)(content(Whitespace\" \"))))(Tile((id \
                   1734)(label(*.))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   4))(sort Exp))((shape(Concave 4))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1735)(content(Whitespace\" \"))))(Tile((id \
                   1737)(label(2.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1738)(content(Whitespace\" \"))))(Tile((id \
                   1741)(label(+.))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1742)(content(Whitespace\" \"))))(Tile((id \
                   1744)(label(8.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1745)(content(Whitespace\" \"))))(Tile((id \
                   1748)(label(/.))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   4))(sort Exp))((shape(Concave 4))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1749)(content(Whitespace\" \"))))(Tile((id \
                   1751)(label(4.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1754)(content(Whitespace\" \")))))))))(Secondary((id \
                   1756)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1761)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1762)(content(Whitespace\" \"))))(Tile((id \
                   1779)(label(float_comparison))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1780)(content(Whitespace\" \")))))((Secondary((id \
                   1783)(content(Whitespace\" \"))))(Tile((id \
                   1784)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1787)(label(10.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1788)(content(Whitespace\" \"))))(Tile((id \
                   1792)(label(==.))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1793)(content(Whitespace\" \"))))(Tile((id \
                   1796)(label(10.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1797)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1799)(content(Whitespace\" \"))))(Tile((id \
                   1801)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1802)(content(Whitespace\" \"))))(Tile((id \
                   1805)(label(<.))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1806)(content(Whitespace\" \"))))(Tile((id \
                   1808)(label(2.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1809)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1811)(content(Whitespace\" \"))))(Tile((id \
                   1813)(label(2.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1814)(content(Whitespace\" \"))))(Tile((id \
                   1818)(label(<=.))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1819)(content(Whitespace\" \"))))(Tile((id \
                   1821)(label(3.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1822)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1824)(content(Whitespace\" \"))))(Tile((id \
                   1826)(label(3.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1827)(content(Whitespace\" \"))))(Tile((id \
                   1830)(label(>.))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1831)(content(Whitespace\" \"))))(Tile((id \
                   1833)(label(2.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1834)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1836)(content(Whitespace\" \"))))(Tile((id \
                   1838)(label(2.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1839)(content(Whitespace\" \"))))(Tile((id \
                   1843)(label(>=.))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1844)(content(Whitespace\" \"))))(Tile((id \
                   13632)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   1849)(content(Whitespace\" \")))))))))(Secondary((id \
                   1851)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2867)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2878)(content(Comment\"# Booleans #\"))))(Secondary((id \
                   1853)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1857)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1858)(content(Whitespace\" \"))))(Tile((id \
                   1867)(label(booleans))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1868)(content(Whitespace\" \"))))(Tile((id \
                   1869)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1871)(content(Whitespace\" \"))))(Tile((id \
                   1872)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   1876)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   1877)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   1879)(content(Whitespace\" \"))))(Tile((id \
                   1883)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   1884)(content(Whitespace\" \")))))((Secondary((id \
                   1887)(content(Whitespace\" \"))))(Tile((id \
                   1888)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1892)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1893)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1895)(content(Whitespace\" \"))))(Tile((id \
                   1900)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   1903)(content(Whitespace\" \")))))))))(Secondary((id \
                   1905)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1910)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1911)(content(Whitespace\" \"))))(Tile((id \
                   1924)(label(conditionals))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1925)(content(Whitespace\" \")))))((Secondary((id \
                   1928)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1932)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1933)(content(Whitespace\" \"))))(Tile((id \
                   1935)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   1936)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   1937)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   1939)(content(Whitespace\" \"))))(Tile((id \
                   1940)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   1941)(content(Whitespace\" \")))))((Secondary((id \
                   1944)(content(Whitespace\" \"))))(Tile((id \
                   1945)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   1946)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1947)(content(Whitespace\" \"))))(Tile((id \
                   1948)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1950)(content(Whitespace\" \"))))(Tile((id \
                   1951)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   1952)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1954)(content(Whitespace\" \"))))(Tile((id \
                   1955)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1956)(content(Whitespace\" \"))))(Tile((id \
                   1957)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1959)(content(Whitespace\" \"))))(Tile((id \
                   1960)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   1963)(content(Whitespace\" \")))))))))(Secondary((id \
                   1965)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   1969)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   1970)(content(Whitespace\" \"))))(Tile((id \
                   1972)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1973)(content(Whitespace\" \"))))(Tile((id \
                   1974)(label(>))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1976)(content(Whitespace\" \"))))(Tile((id \
                   1977)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1980)(content(Whitespace\" \")))))((Secondary((id \
                   1984)(content(Whitespace\" \"))))(Tile((id \
                   1986)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   1991)(content(Whitespace\" \"))))(Secondary((id \
                   1987)(content(Whitespace\" \"))))(Secondary((id \
                   1988)(content(Whitespace\" \"))))(Secondary((id \
                   1989)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   1995)(content(Whitespace\" \"))))(Tile((id \
                   1997)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2002)(content(Whitespace\" \"))))(Secondary((id \
                   1998)(content(Whitespace\" \"))))(Secondary((id \
                   1999)(content(Whitespace\" \"))))(Secondary((id \
                   2000)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   2004)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2916)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2925)(content(Comment\"# Tuples #\"))))(Secondary((id \
                   2006)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2010)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2011)(content(Whitespace\" \"))))(Tile((id \
                   2018)(label(tuples))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2019)(content(Whitespace\" \"))))(Tile((id \
                   2020)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2022)(content(Whitespace\" \"))))(Tile((id \
                   2023)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   2026)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   2027)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2029)(content(Whitespace\" \"))))(Tile((id \
                   2033)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   2034)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2036)(content(Whitespace\" \"))))(Tile((id \
                   2037)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   2041)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   2042)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2044)(content(Whitespace\" \"))))(Tile((id \
                   2047)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                   2048)(content(Whitespace\" \")))))((Secondary((id \
                   2051)(content(Whitespace\" \"))))(Tile((id \
                   2052)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2053)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2054)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2056)(content(Whitespace\" \"))))(Tile((id \
                   2060)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2061)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2063)(content(Whitespace\" \"))))(Tile((id \
                   2064)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2069)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2070)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2072)(content(Whitespace\" \"))))(Tile((id \
                   2073)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                   2076)(content(Whitespace\" \")))))))))(Secondary((id \
                   2078)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2083)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2084)(content(Whitespace\" \"))))(Tile((id \
                   2086)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   2087)(label(a))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2088)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2090)(content(Whitespace\" \"))))(Tile((id \
                   2091)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2092)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2094)(content(Whitespace\" \"))))(Tile((id \
                   2095)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   2096)(label(c))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2097)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2099)(content(Whitespace\" \"))))(Tile((id \
                   2100)(label(d))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))))))))))))(Secondary((id \
                   2101)(content(Whitespace\" \")))))((Secondary((id \
                   2104)(content(Whitespace\" \"))))(Tile((id \
                   2110)(label(tuples))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2113)(content(Whitespace\" \")))))))))(Secondary((id \
                   2115)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2926)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2938)(content(Comment\"# Functions #\"))))(Secondary((id \
                   2117)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2121)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2122)(content(Whitespace\" \"))))(Tile((id \
                   2124)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2125)(content(Whitespace\" \"))))(Tile((id \
                   2126)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2128)(content(Whitespace\" \"))))(Tile((id \
                   2129)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   2132)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   2133)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2135)(content(Whitespace\" \"))))(Tile((id \
                   2138)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   2139)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2141)(content(Whitespace\" \"))))(Tile((id \
                   2144)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2145)(content(Whitespace\" \"))))(Tile((id \
                   2148)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2149)(content(Whitespace\" \"))))(Tile((id \
                   2152)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2153)(content(Whitespace\" \")))))((Secondary((id \
                   2156)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2160)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   2161)(content(Whitespace\" \"))))(Tile((id \
                   2163)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   2164)(label(m))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2165)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2167)(content(Whitespace\" \"))))(Tile((id \
                   2168)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2169)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2171)(content(Whitespace\" \"))))(Tile((id \
                   2172)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   2173)(content(Whitespace\" \")))))))))(Secondary((id \
                   2177)(content(Whitespace\" \"))))(Tile((id \
                   2178)(label(m))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2179)(content(Whitespace\" \"))))(Tile((id \
                   2180)(label(*))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   4))(sort Exp))((shape(Concave 4))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2182)(content(Whitespace\" \"))))(Tile((id \
                   2183)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2184)(content(Whitespace\" \"))))(Tile((id \
                   2185)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2187)(content(Whitespace\" \"))))(Tile((id \
                   2188)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2193)(content(Whitespace\" \"))))(Secondary((id \
                   2189)(content(Whitespace\" \"))))(Secondary((id \
                   2190)(content(Whitespace\" \"))))(Secondary((id \
                   2191)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   2195)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2939)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2998)(content(Comment\"# Recursive Functions (arrow type \
                   annotation required) #\"))))(Secondary((id \
                   2197)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2201)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2202)(content(Whitespace\" \"))))(Tile((id \
                   2221)(label(double_recursively))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2222)(content(Whitespace\" \"))))(Tile((id \
                   2223)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2225)(content(Whitespace\" \"))))(Tile((id \
                   2228)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2229)(content(Whitespace\" \"))))(Tile((id \
                   2232)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2233)(content(Whitespace\" \"))))(Tile((id \
                   2236)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2237)(content(Whitespace\" \")))))((Secondary((id \
                   2240)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2244)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   2245)(content(Whitespace\" \"))))(Tile((id \
                   2247)(label(n))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2248)(content(Whitespace\" \")))))))))(Secondary((id \
                   2252)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2255)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2256)(content(Whitespace\" \"))))(Tile((id \
                   2258)(label(n))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2259)(content(Whitespace\" \"))))(Tile((id \
                   2262)(label(==))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   8))(sort Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2263)(content(Whitespace\" \"))))(Tile((id \
                   2264)(label(0))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2267)(content(Whitespace\" \")))))((Secondary((id \
                   2271)(content(Whitespace\" \"))))(Tile((id \
                   2273)(label(0))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2278)(content(Whitespace\" \"))))(Secondary((id \
                   2274)(content(Whitespace\" \"))))(Secondary((id \
                   2275)(content(Whitespace\" \"))))(Secondary((id \
                   2276)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   2282)(content(Whitespace\" \"))))(Tile((id \
                   2301)(label(double_recursively))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   2302)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2304)(label(n))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2305)(content(Whitespace\" \"))))(Tile((id \
                   2306)(label(-))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2308)(content(Whitespace\" \"))))(Tile((id \
                   2309)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2310)(content(Whitespace\" \"))))(Tile((id \
                   2311)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2313)(content(Whitespace\" \"))))(Tile((id \
                   2314)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2319)(content(Whitespace\" \"))))(Secondary((id \
                   2315)(content(Whitespace\" \"))))(Secondary((id \
                   2316)(content(Whitespace\" \"))))(Secondary((id \
                   2317)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   2321)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2999)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3007)(content(Comment\"# Lists #\"))))(Secondary((id \
                   2323)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2327)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2328)(content(Whitespace\" \"))))(Tile((id \
                   2339)(label(empty_list))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2340)(content(Whitespace\" \"))))(Tile((id \
                   2341)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2343)(content(Whitespace\" \"))))(Tile((id 2344)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 2347)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2348)(content(Whitespace\" \")))))((Secondary((id \
                   2351)(content(Whitespace\" \"))))(Tile((id \
                   2354)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2357)(content(Whitespace\" \")))))))))(Secondary((id \
                   2359)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2364)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2365)(content(Whitespace\" \"))))(Tile((id \
                   2380)(label(non_empty_list))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2381)(content(Whitespace\" \"))))(Tile((id \
                   2382)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2384)(content(Whitespace\" \"))))(Tile((id 2385)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 2388)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2389)(content(Whitespace\" \")))))((Secondary((id \
                   2392)(content(Whitespace\" \"))))(Tile((id \
                   2393)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2396)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   6))(sort Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2397)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2400)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   6))(sort Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2401)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2404)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   6))(sort Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2407)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2410)(content(Whitespace\" \")))))))))(Secondary((id \
                   2412)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2417)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2418)(content(Whitespace\" \"))))(Tile((id \
                   2432)(label(list_literals))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2433)(content(Whitespace\" \"))))(Tile((id \
                   2434)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2436)(content(Whitespace\" \"))))(Tile((id 2437)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 2440)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2441)(content(Whitespace\" \")))))((Secondary((id \
                   2444)(content(Whitespace\" \"))))(Tile((id 2445)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 2446)(label(1))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   2447)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2449)(content(Whitespace\" \"))))(Tile((id \
                   2450)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2451)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2453)(content(Whitespace\" \"))))(Tile((id \
                   2454)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2457)(content(Whitespace\" \")))))))))(Secondary((id \
                   2459)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2464)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2465)(content(Whitespace\" \"))))(Tile((id \
                   2472)(label(length))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2473)(content(Whitespace\" \"))))(Tile((id \
                   2474)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2476)(content(Whitespace\" \"))))(Tile((id 2477)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 2480)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2481)(content(Whitespace\" \"))))(Tile((id \
                   2484)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2485)(content(Whitespace\" \"))))(Tile((id \
                   2488)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2489)(content(Whitespace\" \")))))((Secondary((id \
                   2492)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2496)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   2497)(content(Whitespace\" \"))))(Tile((id \
                   2500)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2501)(content(Whitespace\" \")))))))))(Secondary((id \
                   2505)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2510)(label(case end))(mold((out \
                   Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2511)(content(Whitespace\" \
                   \"))))(Tile((id 2514)(label(xs))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2515)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2516)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2518)(content(Whitespace\" \
                   \"))))(Tile((id 2521)(label([]))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2522)(content(Whitespace\" \")))))))))(Secondary((id \
                   2525)(content(Whitespace\" \"))))(Tile((id \
                   2526)(label(0))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2527)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2528)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2530)(content(Whitespace\" \
                   \"))))(Tile((id 2532)(label(hd))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2535)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   6))(sort Pat))((shape(Concave 6))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2537)(label(tl))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2538)(content(Whitespace\" \")))))))))(Secondary((id \
                   2541)(content(Whitespace\" \"))))(Tile((id \
                   2542)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2543)(content(Whitespace\" \"))))(Tile((id \
                   2544)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2546)(content(Whitespace\" \"))))(Tile((id \
                   2552)(label(length))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2553)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2556)(label(tl))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2561)(content(Whitespace\" \"))))(Secondary((id \
                   2557)(content(Whitespace\" \"))))(Secondary((id \
                   2558)(content(Whitespace\" \"))))(Secondary((id \
                   2559)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   2568)(content(Whitespace\" \"))))(Secondary((id \
                   2564)(content(Whitespace\" \"))))(Secondary((id \
                   2565)(content(Whitespace\" \"))))(Secondary((id \
                   2566)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   2570)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2575)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2576)(content(Whitespace\" \"))))(Tile((id \
                   2602)(label(has_at_least_two_elements))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2603)(content(Whitespace\" \"))))(Tile((id \
                   2604)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2606)(content(Whitespace\" \"))))(Tile((id 2607)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 2610)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2611)(content(Whitespace\" \"))))(Tile((id \
                   2614)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2615)(content(Whitespace\" \"))))(Tile((id \
                   2619)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2620)(content(Whitespace\" \")))))((Secondary((id \
                   2623)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2627)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   2628)(content(Whitespace\" \"))))(Tile((id \
                   2631)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2632)(content(Whitespace\" \")))))))))(Secondary((id \
                   2636)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2641)(label(case end))(mold((out \
                   Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2642)(content(Whitespace\" \
                   \"))))(Tile((id 2645)(label(xs))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2646)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2647)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2649)(content(Whitespace\" \
                   \"))))(Tile((id 2652)(label([]))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2653)(content(Whitespace\" \")))))))))(Secondary((id \
                   2656)(content(Whitespace\" \"))))(Tile((id \
                   2661)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2662)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2663)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2665)(content(Whitespace\" \
                   \"))))(Tile((id 2667)(label(hd))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2670)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   6))(sort Pat))((shape(Concave 6))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2673)(label([]))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2674)(content(Whitespace\" \")))))))))(Secondary((id \
                   2677)(content(Whitespace\" \"))))(Tile((id \
                   2682)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2683)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2684)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2686)(content(Whitespace\" \
                   \"))))(Tile((id 2687)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2690)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   6))(sort Pat))((shape(Concave 6))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2691)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2694)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   6))(sort Pat))((shape(Concave 6))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2697)(label([]))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2698)(content(Whitespace\" \")))))))))(Secondary((id \
                   2701)(content(Whitespace\" \"))))(Tile((id \
                   2705)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2710)(content(Whitespace\" \"))))(Secondary((id \
                   2706)(content(Whitespace\" \"))))(Secondary((id \
                   2707)(content(Whitespace\" \"))))(Secondary((id \
                   2708)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   2717)(content(Whitespace\" \"))))(Secondary((id \
                   2713)(content(Whitespace\" \"))))(Secondary((id \
                   2714)(content(Whitespace\" \"))))(Secondary((id \
                   2715)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   2719)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3008)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3018)(content(Comment\"# Strings #\"))))(Secondary((id \
                   2722)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2726)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2727)(content(Whitespace\" \"))))(Tile((id \
                   2739)(label(string_lits))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2740)(content(Whitespace\" \")))))((Secondary((id \
                   2743)(content(Whitespace\" \"))))(Tile((id \
                   2757)(label(\"\\\"Hello, world!\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2760)(content(Whitespace\" \")))))))))(Secondary((id \
                   2762)(content(Whitespace\" \"))))(Secondary((id \
                   2764)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2768)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2769)(content(Whitespace\" \"))))(Tile((id \
                   2785)(label(string_equality))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2786)(content(Whitespace\" \")))))((Secondary((id \
                   2789)(content(Whitespace\" \"))))(Tile((id \
                   2800)(label(string_lits))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2801)(content(Whitespace\" \"))))(Tile((id \
                   2805)(label($==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2806)(content(Whitespace\" \"))))(Tile((id \
                   2820)(label(\"\\\"Hello, world!\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2823)(content(Whitespace\" \")))))))))(Secondary((id \
                   2825)(content(Whitespace\" \"))))(Secondary((id \
                   3019)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3409)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3466)(content(Comment\"# Non-empty holes are the red dotted \
                   boxes around errors #\"))))(Secondary((id \
                   3467)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3519)(content(Comment\"# (you can still run programs with \
                   non-empty holes) #\"))))(Secondary((id \
                   3520)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3524)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3525)(content(Whitespace\" \"))))(Tile((id \
                   3540)(label(non_empty_hole))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3541)(content(Whitespace\" \"))))(Tile((id \
                   3542)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3544)(content(Whitespace\" \"))))(Tile((id \
                   3547)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3548)(content(Whitespace\" \")))))((Secondary((id \
                   3551)(content(Whitespace\" \"))))(Tile((id \
                   3555)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3558)(content(Whitespace\" \")))))))))(Secondary((id \
                   3560)(content(Whitespace\" \"))))(Secondary((id \
                   3562)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3563)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3564)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3565)(content(Whitespace\" \"))))(Tile((id \
                   3566)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3568)(content(Whitespace\" \"))))(Tile((id \
                   3569)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2827)(content(Whitespace\"\\226\\143\\142\")))))))(ancestors())))(caret \
                   Outer))";
                backup_text =
                  "# Hazel Language Quick Reference #\n\n\
                   # Empty holes stand for missing expressions, patterns, or \
                   types #\n\
                   let empty_hole =   in\n\n\
                   # Integers #\n\
                   let int_lits : Int = 1 in\n\
                   let negation = -1 in\n\
                   let arithmetic = 1*2 + 8/4 in\n\
                   let int_comparison = (10 == 10, 1 < 2, 2 <= 3, 3 > 2, 2 >= \
                   1) in\n\n\
                   # Floating Point Numbers #\n\
                   let float_lits : Float = 1.5 in\n\
                   let float_artih = 1. *. 2. +. 8. /. 4. in\n\
                   let float_comparison = (10. ==. 10., 1. <. 2., 2. <=. 3., \
                   3. >. 2., 2. >=. 1.) in\n\n\
                   # Booleans #\n\
                   let booleans : (Bool, Bool) = (true, false) in\n\
                   let conditionals =\n\
                   let (x, y) = (2 + 2, 3 + 3) in\n\
                   if y > x then 1   \n\
                   else 2   \n\
                   in\n\n\
                   # Tuples #\n\
                   let tuples : (Int, Bool, (Bool, Int)) = (1, true, (false, \
                   3)) in\n\
                   let (a, b, (c, d)) = tuples in\n\n\
                   # Functions #\n\
                   let y : (Int, Int, Int) -> Int =\n\
                   fun (m, x, b) -> m * x + b   \n\
                   in\n\n\
                   # Recursive Functions (arrow type annotation required) #\n\
                   let double_recursively : Int -> Int =\n\
                   fun n ->\n\
                   if n == 0 then 0   \n\
                   else double_recursively(n - 1) + 2   \n\
                   in\n\n\
                   # Lists #\n\
                   let empty_list : [Int] = [] in\n\
                   let non_empty_list : [Int] = 1::2::3::[] in\n\
                   let list_literals : [Int] = [1, 2, 3] in\n\
                   let length : [Int] -> Int =\n\
                   fun xs ->\n\
                   case xs\n\
                   | [] => 0\n\
                   | hd::tl => 1 + length(tl)   \n\
                   end   \n\
                   in\n\
                   let has_at_least_two_elements : [Int] -> Bool =\n\
                   fun xs ->\n\
                   case xs\n\
                   | [] => false\n\
                   | hd::[] => false\n\
                   | a::b::[] => true   \n\
                   end   \n\
                   in\n\n\
                   # Strings #\n\
                   let string_lits = \"Hello, world!\" in \n\
                   let string_equality = string_lits $== \"Hello, world!\" in \n\n\
                   # Non-empty holes are the red dotted boxes around errors #\n\
                   # (you can still run programs with non-empty holes) #\n\
                   let non_empty_hole : Int = true in \n\n\
                   2 + 2\n";
              } ) );
        ] );
  }
