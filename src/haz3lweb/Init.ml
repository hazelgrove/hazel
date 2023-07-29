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
        mode = Scratch;
      };
    scratch =
      ( 7,
        [
          ( 33,
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Grout((id \
                 32)(shape Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( 5194,
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Tile((id \
                 30)(label(type = in))(mold((out Exp)(in_(TPat \
                 Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 31)(content(Whitespace\" \"))))(Tile((id \
                 38)(label(Option))(mold((out TPat)(in_())(nibs(((shape \
                 Convex)(sort TPat))((shape Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 39)(content(Whitespace\" \")))))((Secondary((id \
                 41)(content(Whitespace\" \"))))(Tile((id \
                 45)(label(None))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 46)(content(Whitespace\" \"))))(Tile((id \
                 47)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 49)(content(Whitespace\" \"))))(Tile((id \
                 53)(label(Some))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 54)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 56)(label(?))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 59)(content(Whitespace\" \")))))))))(Secondary((id \
                 4643)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 4644)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4649)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 4650)(content(Whitespace\" \"))))(Tile((id \
                 4654)(label(fst))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4655)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4656)(content(Whitespace\" \"))))(Tile((id \
                 4657)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 4658)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 4660)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4661)(content(Whitespace\" \"))))(Tile((id \
                 4662)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 4664)(content(Whitespace\" \"))))(Tile((id \
                 4666)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4667)(content(Whitespace\" \"))))(Tile((id \
                 4668)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4670)(content(Whitespace\" \")))))((Secondary((id \
                 4671)(content(Whitespace\" \"))))(Tile((id 4676)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 4677)(content(Whitespace\" \
                 \"))))(Tile((id 4678)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4680)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4681)(content(Whitespace\" \"))))(Tile((id \
                 4682)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4684)(content(Whitespace\" \")))))))))(Secondary((id \
                 4686)(content(Whitespace\" \"))))(Tile((id \
                 4687)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4691)(content(Whitespace\" \")))))))))(Secondary((id \
                 4692)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4697)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 4698)(content(Whitespace\" \"))))(Tile((id \
                 4702)(label(snd))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4703)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4704)(content(Whitespace\" \"))))(Tile((id \
                 4705)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 4706)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 4708)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4709)(content(Whitespace\" \"))))(Tile((id \
                 4710)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 4712)(content(Whitespace\" \"))))(Tile((id \
                 4714)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4715)(content(Whitespace\" \"))))(Tile((id \
                 4716)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4718)(content(Whitespace\" \")))))((Secondary((id \
                 4719)(content(Whitespace\" \"))))(Tile((id 4724)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 4725)(content(Whitespace\" \
                 \"))))(Tile((id 4726)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4728)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4729)(content(Whitespace\" \"))))(Tile((id \
                 4730)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4732)(content(Whitespace\" \")))))))))(Secondary((id \
                 4734)(content(Whitespace\" \"))))(Tile((id \
                 4735)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4739)(content(Whitespace\" \")))))))))(Secondary((id \
                 4464)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 4465)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4470)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 4471)(content(Whitespace\" \"))))(Tile((id \
                 4475)(label(not))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4476)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4477)(content(Whitespace\" \"))))(Tile((id \
                 4482)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4483)(content(Whitespace\" \"))))(Tile((id \
                 4485)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4486)(content(Whitespace\" \"))))(Tile((id \
                 4491)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4492)(content(Whitespace\" \")))))((Secondary((id \
                 4493)(content(Whitespace\" \"))))(Tile((id 4498)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 4499)(content(Whitespace\" \
                 \"))))(Tile((id 4500)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 4502)(content(Whitespace\" \")))))))))(Secondary((id \
                 4504)(content(Whitespace\" \"))))(Tile((id \
                 4505)(label(!))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4506)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4510)(content(Whitespace\" \")))))))))(Secondary((id \
                 4642)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 4511)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4516)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 4517)(content(Whitespace\" \"))))(Tile((id \
                 4525)(label(bool_eq))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4526)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4527)(content(Whitespace\" \"))))(Tile((id \
                 4528)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 4533)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 4534)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4535)(content(Whitespace\" \"))))(Tile((id \
                 4540)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 4541)(content(Whitespace\" \"))))(Tile((id \
                 4543)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4544)(content(Whitespace\" \"))))(Tile((id \
                 4549)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4550)(content(Whitespace\" \")))))((Secondary((id \
                 4551)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4556)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 4557)(content(Whitespace\" \"))))(Tile((id \
                 4559)(label(a))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4561)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4562)(content(Whitespace\" \"))))(Tile((id \
                 4563)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4565)(content(Whitespace\" \")))))))))(Secondary((id \
                 4567)(content(Whitespace\" \"))))(Tile((id \
                 4569)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4571)(content(Whitespace\" \"))))(Tile((id \
                 4573)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 9))(sort Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4574)(content(Whitespace\" \"))))(Tile((id \
                 4575)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4577)(content(Whitespace\" \"))))(Tile((id \
                 4579)(label(\"\\\\/\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4580)(content(Whitespace\" \"))))(Tile((id \
                 4582)(label(!))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4583)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4585)(content(Whitespace\" \"))))(Tile((id \
                 4587)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 9))(sort Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4588)(content(Whitespace\" \"))))(Tile((id \
                 4589)(label(!))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4590)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4595)(content(Whitespace\" \")))))))))(Secondary((id \
                 2577)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 4741)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4746)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 4747)(content(Whitespace\" \"))))(Tile((id \
                 4757)(label(List.cons))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4758)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4759)(content(Whitespace\" \"))))(Tile((id \
                 4760)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 4761)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 4763)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4764)(content(Whitespace\" \"))))(Tile((id 4765)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 4766)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 4768)(content(Whitespace\" \"))))(Tile((id \
                 4770)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4771)(content(Whitespace\" \"))))(Tile((id 4772)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 4773)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 4775)(content(Whitespace\" \")))))((Secondary((id \
                 4776)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4781)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 4782)(content(Whitespace\" \"))))(Tile((id \
                 4858)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4785)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4786)(content(Whitespace\" \"))))(Tile((id \
                 4860)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4790)(content(Whitespace\" \")))))))))(Secondary((id \
                 4792)(content(Whitespace\" \"))))(Tile((id \
                 4862)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4796)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 6))(sort Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4864)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4802)(content(Whitespace\" \")))))))))(Secondary((id \
                 4804)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 3105)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3110)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3111)(content(Whitespace\" \"))))(Tile((id \
                 3123)(label(List.length))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3124)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3125)(content(Whitespace\" \"))))(Tile((id 3126)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3127)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 3129)(content(Whitespace\" \"))))(Tile((id \
                 3131)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3132)(content(Whitespace\" \"))))(Tile((id \
                 3136)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3137)(content(Whitespace\" \")))))((Secondary((id \
                 3138)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3143)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 3144)(content(Whitespace\" \"))))(Tile((id \
                 4866)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3150)(content(Whitespace\" \")))))))))(Secondary((id \
                 3152)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3158)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3159)(content(Whitespace\" \
                 \"))))(Tile((id 4869)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3165)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3166)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3167)(content(Whitespace\" \
                 \"))))(Tile((id 3169)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3171)(content(Whitespace\" \")))))))))(Secondary((id \
                 3173)(content(Whitespace\" \"))))(Tile((id \
                 3174)(label(0))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3176)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3177)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3178)(content(Whitespace\" \
                 \"))))(Tile((id 3179)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3182)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4871)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3186)(content(Whitespace\" \")))))))))(Secondary((id \
                 3188)(content(Whitespace\" \"))))(Tile((id \
                 3189)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3191)(content(Whitespace\" \"))))(Tile((id \
                 3192)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 5))(sort Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3193)(content(Whitespace\" \"))))(Tile((id \
                 3205)(label(List.length))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3206)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4873)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3213)(content(Whitespace\" \")))))))))(Secondary((id \
                 3217)(content(Whitespace\" \")))))))))(Secondary((id \
                 4805)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2857)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2862)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 2863)(content(Whitespace\" \"))))(Tile((id \
                 2871)(label(List.hd))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 2872)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2873)(content(Whitespace\" \"))))(Tile((id 2874)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2875)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 2877)(content(Whitespace\" \"))))(Tile((id \
                 2879)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2880)(content(Whitespace\" \"))))(Tile((id \
                 2881)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2883)(content(Whitespace\" \")))))((Secondary((id \
                 2884)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2889)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 2890)(content(Whitespace\" \"))))(Tile((id \
                 2891)(label(l))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2893)(content(Whitespace\" \")))))))))(Secondary((id \
                 2895)(content(Whitespace\" \"))))(Secondary((id \
                 2896)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2902)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 2903)(content(Whitespace\" \
                 \"))))(Tile((id 2904)(label(l))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2906)(content(Whitespace\" \"))))(Secondary((id \
                 2907)(content(Whitespace\" \"))))(Secondary((id \
                 2908)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2909)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 2910)(content(Whitespace\" \
                 \"))))(Tile((id 2912)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 2914)(content(Whitespace\" \")))))))))(Secondary((id \
                 2916)(content(Whitespace\" \"))))(Tile((id \
                 2917)(label(?))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2919)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2920)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 2921)(content(Whitespace\" \
                 \"))))(Tile((id 4874)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2926)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4876)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2930)(content(Whitespace\" \")))))))))(Secondary((id \
                 2932)(content(Whitespace\" \"))))(Tile((id \
                 4888)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4889)(content(Whitespace\" \")))))))))(Secondary((id \
                 2943)(content(Whitespace\" \")))))))))(Secondary((id \
                 3037)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2944)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2949)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 2950)(content(Whitespace\" \"))))(Tile((id \
                 2958)(label(List.tl))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 2959)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2960)(content(Whitespace\" \"))))(Tile((id 2961)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2962)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 2964)(content(Whitespace\" \"))))(Tile((id \
                 2966)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2967)(content(Whitespace\" \"))))(Tile((id 2968)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2969)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 2971)(content(Whitespace\" \")))))((Secondary((id \
                 2972)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2977)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 2978)(content(Whitespace\" \"))))(Tile((id \
                 2979)(label(l))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2981)(content(Whitespace\" \")))))))))(Secondary((id \
                 2983)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2989)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 2990)(content(Whitespace\" \
                 \"))))(Tile((id 2991)(label(l))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2993)(content(Whitespace\" \"))))(Secondary((id \
                 2994)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2995)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 2996)(content(Whitespace\" \
                 \"))))(Tile((id 2998)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3000)(content(Whitespace\" \")))))))))(Secondary((id \
                 3002)(content(Whitespace\" \"))))(Tile((id \
                 3039)(label(?))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3006)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3007)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3008)(content(Whitespace\" \
                 \"))))(Tile((id 4879)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3013)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4881)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3017)(content(Whitespace\" \")))))))))(Secondary((id \
                 3019)(content(Whitespace\" \"))))(Tile((id \
                 4883)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3027)(content(Whitespace\" \")))))))))(Secondary((id \
                 3031)(content(Whitespace\" \")))))))))(Secondary((id \
                 5074)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 5075)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5081)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 5082)(content(Whitespace\" \"))))(Tile((id \
                 5098)(label(List.is_empty))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5172)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 5175)(content(Whitespace\" \"))))(Tile((id 5176)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 5178)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 5179)(content(Whitespace\" \"))))(Tile((id \
                 5183)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 5184)(content(Whitespace\" \"))))(Tile((id \
                 5191)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 5193)(content(Whitespace\" \")))))((Secondary((id \
                 5102)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5109)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 5110)(content(Whitespace\" \"))))(Tile((id \
                 5113)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 5114)(content(Whitespace\" \")))))))))(Secondary((id \
                 5116)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5125)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 5126)(content(Whitespace\" \
                 \"))))(Tile((id 5129)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5131)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5132)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 5134)(content(Whitespace\" \
                 \"))))(Tile((id 5136)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 5137)(content(Whitespace\" \")))))))))(Secondary((id \
                 5139)(content(Whitespace\" \"))))(Tile((id \
                 5148)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5149)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5150)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 5152)(content(Whitespace\" \
                 \"))))(Tile((id 5153)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5168)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 5170)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 5171)(content(Whitespace\" \")))))))))(Secondary((id \
                 5157)(content(Whitespace\" \"))))(Tile((id \
                 5162)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5163)(content(Whitespace\" \")))))))))(Secondary((id \
                 5164)(content(Whitespace\" \")))))))))(Secondary((id \
                 3220)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 3221)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3226)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3227)(content(Whitespace\" \"))))(Tile((id \
                 3236)(label(List.nth))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3237)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3238)(content(Whitespace\" \"))))(Tile((id \
                 3239)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 3240)(label([ ]))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3241)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 3243)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3244)(content(Whitespace\" \"))))(Tile((id \
                 3248)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 3249)(content(Whitespace\" \"))))(Tile((id \
                 3251)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3252)(content(Whitespace\" \"))))(Tile((id \
                 3253)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3255)(content(Whitespace\" \")))))((Secondary((id \
                 3258)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3263)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 3264)(content(Whitespace\" \"))))(Tile((id \
                 3267)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3268)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3269)(content(Whitespace\" \"))))(Tile((id \
                 3270)(label(n))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3272)(content(Whitespace\" \")))))))))(Secondary((id \
                 3274)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3280)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3281)(content(Whitespace\" \
                 \"))))(Tile((id 3284)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3285)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3286)(content(Whitespace\" \"))))(Tile((id \
                 3287)(label(n))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3289)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3290)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3291)(content(Whitespace\" \
                 \"))))(Tile((id 4890)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3296)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3297)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3299)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3300)(content(Whitespace\" \"))))(Tile((id \
                 3301)(label(0))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3303)(content(Whitespace\" \")))))))))(Secondary((id \
                 3305)(content(Whitespace\" \"))))(Tile((id \
                 4891)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3309)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3310)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3311)(content(Whitespace\" \
                 \"))))(Tile((id 3312)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3315)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4893)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3319)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3320)(content(Whitespace\" \"))))(Tile((id \
                 3321)(label(n))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3323)(content(Whitespace\" \")))))))))(Secondary((id \
                 3325)(content(Whitespace\" \"))))(Tile((id \
                 3334)(label(List.nth))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3335)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4896)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3339)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3340)(content(Whitespace\" \"))))(Tile((id \
                 3341)(label(n))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3343)(content(Whitespace\" \"))))(Tile((id \
                 3344)(label(-))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 5))(sort Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3345)(content(Whitespace\" \"))))(Tile((id \
                 3346)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3348)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3349)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3350)(content(Whitespace\" \
                 \"))))(Tile((id 3352)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3354)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3355)(content(Whitespace\" \"))))(Tile((id \
                 3356)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3358)(content(Whitespace\" \")))))))))(Secondary((id \
                 3360)(content(Whitespace\" \"))))(Tile((id \
                 3361)(label(?))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3366)(content(Whitespace\" \")))))))))(Secondary((id \
                 3370)(content(Whitespace\" \")))))))))(Secondary((id \
                 3371)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3377)(label(test end))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 4635)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3387)(label(List.nth))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3388)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3389)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 3390)(label(7))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3392)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3393)(content(Whitespace\" \"))))(Tile((id \
                 3394)(label(8))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3396)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3397)(content(Whitespace\" \"))))(Tile((id \
                 3398)(label(9))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 3400)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3401)(content(Whitespace\" \"))))(Tile((id \
                 3402)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3417)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3406)(label(==))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 8))(sort Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3407)(content(Whitespace\" \"))))(Tile((id \
                 3408)(label(9))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4640)(content(Whitespace\" \")))))))))(Tile((id \
                 3414)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3419)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 3420)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3425)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3426)(content(Whitespace\" \"))))(Tile((id \
                 3435)(label(List.rev))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3436)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3437)(content(Whitespace\" \"))))(Tile((id 3438)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3439)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 3441)(content(Whitespace\" \"))))(Tile((id \
                 3443)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3444)(content(Whitespace\" \"))))(Tile((id 3445)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3446)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 3448)(content(Whitespace\" \")))))((Secondary((id \
                 3449)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3454)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 3455)(content(Whitespace\" \"))))(Tile((id \
                 3456)(label(l))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3458)(content(Whitespace\" \")))))))))(Secondary((id \
                 3460)(content(Whitespace\" \"))))(Secondary((id \
                 3461)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3466)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3467)(content(Whitespace\" \"))))(Tile((id \
                 3606)(label(go))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3474)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3475)(content(Whitespace\" \"))))(Tile((id \
                 3476)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 3477)(label([ ]))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3478)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 3480)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3481)(content(Whitespace\" \"))))(Tile((id 3482)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3483)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 3485)(content(Whitespace\" \"))))(Tile((id \
                 3487)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3488)(content(Whitespace\" \"))))(Tile((id 3489)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3490)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 3492)(content(Whitespace\" \")))))((Secondary((id \
                 3493)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3498)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 3499)(content(Whitespace\" \"))))(Tile((id \
                 3502)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3503)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3504)(content(Whitespace\" \"))))(Tile((id \
                 3508)(label(acc))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3509)(content(Whitespace\" \")))))))))(Secondary((id \
                 3511)(content(Whitespace\" \"))))(Secondary((id \
                 3512)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3518)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3519)(content(Whitespace\" \
                 \"))))(Tile((id 3522)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3523)(content(Whitespace\" \"))))(Secondary((id \
                 3524)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3525)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3526)(content(Whitespace\" \
                 \"))))(Tile((id 3528)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3530)(content(Whitespace\" \")))))))))(Secondary((id \
                 3532)(content(Whitespace\" \"))))(Tile((id \
                 3536)(label(acc))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3537)(content(Whitespace\" \"))))(Secondary((id \
                 3538)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3539)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3540)(content(Whitespace\" \
                 \"))))(Tile((id 4897)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3545)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4899)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3549)(content(Whitespace\" \")))))))))(Secondary((id \
                 3551)(content(Whitespace\" \"))))(Tile((id \
                 3609)(label(go))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3558)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4901)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3562)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3563)(content(Whitespace\" \"))))(Tile((id \
                 4902)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3568)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 6))(sort Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3572)(label(acc))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 4637)(content(Whitespace\" \")))))))))(Secondary((id \
                 3582)(content(Whitespace\" \")))))))))(Secondary((id \
                 4806)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3626)(label(go))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3615)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3617)(label(l))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3618)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3620)(content(Whitespace\" \"))))(Tile((id \
                 3622)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 4638)(content(Whitespace\" \")))))))))(Secondary((id \
                 4639)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 3629)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3634)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3635)(content(Whitespace\" \"))))(Tile((id \
                 3645)(label(List.init))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3646)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3647)(content(Whitespace\" \"))))(Tile((id \
                 3648)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 3652)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 3653)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3654)(content(Whitespace\" \"))))(Tile((id \
                 3658)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4911)(content(Whitespace\" \"))))(Tile((id \
                 3660)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4912)(content(Whitespace\" \"))))(Tile((id \
                 3661)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 3663)(content(Whitespace\" \"))))(Tile((id \
                 3665)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3666)(content(Whitespace\" \"))))(Tile((id 3667)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3668)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 3670)(content(Whitespace\" \")))))((Secondary((id \
                 3671)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3676)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 3677)(content(Whitespace\" \"))))(Tile((id \
                 3681)(label(len))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3682)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3683)(content(Whitespace\" \"))))(Tile((id \
                 3684)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3686)(content(Whitespace\" \")))))))))(Secondary((id \
                 3689)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3694)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3695)(content(Whitespace\" \"))))(Tile((id \
                 3822)(label(go))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3702)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3703)(content(Whitespace\" \"))))(Tile((id \
                 4914)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 4936)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 4916)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4938)(content(Whitespace\" \"))))(Tile((id 4939)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3704)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 3706)(content(Whitespace\" \"))))(Tile((id \
                 3708)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3709)(content(Whitespace\" \"))))(Tile((id 4913)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3710)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 3712)(content(Whitespace\" \")))))((Secondary((id \
                 3713)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3718)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 3719)(content(Whitespace\" \"))))(Tile((id \
                 4943)(label(idx))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3722)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3723)(content(Whitespace\" \"))))(Tile((id \
                 4904)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3729)(content(Whitespace\" \")))))))))(Secondary((id \
                 3732)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3736)(label(if then else))(mold((out Exp)(in_(Exp \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3737)(content(Whitespace\" \"))))(Tile((id \
                 4948)(label(idx))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4963)(content(Whitespace\" \"))))(Tile((id \
                 3741)(label(<))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 5))(sort Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3742)(content(Whitespace\" \"))))(Tile((id \
                 3746)(label(len))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3747)(content(Whitespace\"\\226\\143\\142\")))))((Secondary((id \
                 3752)(content(Whitespace\" \"))))(Tile((id \
                 3839)(label(go))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3759)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4953)(label(idx))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3763)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 5))(sort Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3764)(content(Whitespace\" \"))))(Tile((id \
                 3765)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3767)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3768)(content(Whitespace\" \"))))(Tile((id \
                 4907)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3774)(content(Whitespace\" \"))))(Tile((id \
                 3775)(label(@))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 7))(sort Exp))((shape(Concave 7))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3776)(content(Whitespace\" \"))))(Tile((id 3777)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 3778)(label(f))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4956)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4962)(label(idx))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 3786)(content(Whitespace\" \"))))(Secondary((id \
                 3783)(content(Whitespace\" \"))))(Secondary((id \
                 3784)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 3789)(content(Whitespace\" \"))))(Tile((id \
                 4910)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3799)(content(Whitespace\" \")))))))))(Secondary((id \
                 3840)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3837)(label(go))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3826)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3828)(label(0))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3829)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3831)(content(Whitespace\" \"))))(Tile((id \
                 3833)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 4807)(content(Whitespace\" \")))))))))(Secondary((id \
                 3600)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 127)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 131)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 132)(content(Whitespace\" \"))))(Tile((id \
                 143)(label(List.equal))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 145)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 147)(content(Whitespace\" \"))))(Tile((id \
                 148)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 4329)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 4367)(content(Whitespace\" \"))))(Tile((id \
                 4333)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4368)(content(Whitespace\" \"))))(Tile((id \
                 4339)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 4340)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4341)(content(Whitespace\" \"))))(Tile((id 149)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 150)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 151)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 153)(content(Whitespace\" \"))))(Tile((id 154)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 155)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 2638)(content(Whitespace\" \"))))(Tile((id \
                 158)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2639)(content(Whitespace\" \"))))(Tile((id \
                 162)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4342)(content(Whitespace\" \")))))((Secondary((id \
                 164)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 168)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 169)(content(Whitespace\" \"))))(Tile((id \
                 4345)(label(p))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4346)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4347)(content(Whitespace\" \"))))(Tile((id \
                 4348)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4349)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4351)(content(Whitespace\" \"))))(Tile((id \
                 4353)(label(ys))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4845)(content(Whitespace\" \")))))))))(Secondary((id \
                 177)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 182)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 183)(content(Whitespace\" \
                 \"))))(Tile((id 4356)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4357)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4361)(content(Whitespace\" \"))))(Tile((id \
                 4360)(label(ys))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 188)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 189)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 191)(content(Whitespace\" \
                 \"))))(Tile((id 193)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 194)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 196)(content(Whitespace\" \"))))(Tile((id \
                 198)(label([]))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 199)(content(Whitespace\" \")))))))))(Secondary((id \
                 202)(content(Whitespace\" \"))))(Tile((id \
                 206)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 207)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 208)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 210)(content(Whitespace\" \
                 \"))))(Tile((id 4964)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 214)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4966)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 217)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 219)(content(Whitespace\" \"))))(Tile((id \
                 4974)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 223)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4976)(label(ys))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 226)(content(Whitespace\" \")))))))))(Secondary((id \
                 229)(content(Whitespace\" \"))))(Tile((id \
                 4371)(label(p))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4372)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4970)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4376)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4377)(content(Whitespace\" \"))))(Tile((id \
                 4978)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 4378)(content(Whitespace\" \"))))(Tile((id \
                 240)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 9))(sort Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 241)(content(Whitespace\" \"))))(Tile((id \
                 251)(label(List.equal))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 252)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4364)(label(p))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4365)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4366)(content(Whitespace\" \"))))(Tile((id \
                 4972)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 256)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4973)(content(Whitespace\" \"))))(Tile((id \
                 4980)(label(ys))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 260)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 261)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 263)(content(Whitespace\" \
                 \"))))(Tile((id 264)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 265)(content(Whitespace\" \")))))))))(Secondary((id \
                 268)(content(Whitespace\" \"))))(Tile((id \
                 273)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 279)(content(Whitespace\" \")))))))))(Secondary((id \
                 286)(content(Whitespace\" \")))))))))(Secondary((id \
                 288)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 289)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 293)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 294)(content(Whitespace\" \"))))(Tile((id \
                 309)(label(List.fold_left))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 310)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 312)(content(Whitespace\" \"))))(Tile((id \
                 313)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 314)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 315)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 316)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 318)(content(Whitespace\" \"))))(Tile((id \
                 319)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Tile((id \
                 322)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 323)(content(Whitespace\" \"))))(Tile((id \
                 324)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 325)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 327)(content(Whitespace\" \"))))(Tile((id \
                 328)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 329)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 331)(content(Whitespace\" \"))))(Tile((id 332)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 333)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Tile((id \
                 336)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2637)(content(Whitespace\" \"))))(Tile((id \
                 2690)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 338)(content(Whitespace\" \"))))(Secondary((id \
                 339)(content(Whitespace\" \"))))(Secondary((id \
                 340)(content(Whitespace\" \")))))((Secondary((id \
                 342)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 346)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 347)(content(Whitespace\" \"))))(Tile((id \
                 349)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 350)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 352)(content(Whitespace\" \"))))(Tile((id \
                 355)(label(acc))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 356)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 358)(content(Whitespace\" \"))))(Tile((id \
                 360)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 361)(content(Whitespace\" \")))))))))(Secondary((id \
                 364)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 369)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 370)(content(Whitespace\" \
                 \"))))(Tile((id 373)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 374)(content(Whitespace\" \"))))(Secondary((id \
                 375)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 376)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 378)(content(Whitespace\" \
                 \"))))(Tile((id 380)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 381)(content(Whitespace\" \")))))))))(Secondary((id \
                 384)(content(Whitespace\" \"))))(Tile((id \
                 387)(label(acc))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 388)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 389)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 391)(content(Whitespace\" \
                 \"))))(Tile((id 393)(label(hd))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 396)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 398)(label(tl))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 399)(content(Whitespace\" \")))))))))(Secondary((id \
                 402)(content(Whitespace\" \"))))(Tile((id \
                 416)(label(List.fold_left))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 417)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 419)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 420)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 422)(content(Whitespace\" \"))))(Tile((id \
                 423)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 424)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 428)(label(acc))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 429)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 431)(content(Whitespace\" \"))))(Tile((id \
                 433)(label(hd))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 434)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 436)(content(Whitespace\" \"))))(Tile((id \
                 438)(label(tl))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 444)(content(Whitespace\" \")))))))))(Secondary((id \
                 451)(content(Whitespace\" \")))))))))(Secondary((id \
                 2850)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 458)(label(test end))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 2845)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 475)(label(List.fold_left))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 476)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 481)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 482)(content(Whitespace\" \"))))(Tile((id \
                 484)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 485)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2691)(content(Whitespace\" \"))))(Tile((id \
                 489)(label(acc))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2834)(content(Whitespace\" \")))))))))(Secondary((id \
                 2835)(content(Whitespace\" \"))))(Tile((id \
                 492)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2838)(content(Whitespace\" \"))))(Tile((id \
                 493)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 5))(sort Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2839)(content(Whitespace\" \"))))(Tile((id \
                 497)(label(acc))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 498)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2693)(content(Whitespace\" \"))))(Tile((id \
                 502)(label(666))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 503)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2696)(content(Whitespace\" \"))))(Tile((id \
                 506)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 2844)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 510)(label(==))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 8))(sort Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 511)(content(Whitespace\" \"))))(Tile((id \
                 514)(label(666))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4808)(content(Whitespace\" \")))))))))(Tile((id \
                 519)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 10))(sort Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 521)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 526)(label(test end))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 2847)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 543)(label(List.fold_left))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 544)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 549)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 550)(content(Whitespace\" \"))))(Tile((id \
                 552)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 553)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2692)(content(Whitespace\" \"))))(Tile((id \
                 557)(label(acc))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2836)(content(Whitespace\" \")))))))))(Secondary((id \
                 2837)(content(Whitespace\" \"))))(Tile((id \
                 560)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2840)(content(Whitespace\" \"))))(Tile((id \
                 561)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 5))(sort Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2841)(content(Whitespace\" \"))))(Tile((id \
                 565)(label(acc))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 566)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2694)(content(Whitespace\" \"))))(Tile((id \
                 568)(label(0))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 569)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2695)(content(Whitespace\" \"))))(Tile((id 571)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 572)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 573)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2843)(content(Whitespace\" \"))))(Tile((id \
                 575)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 576)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2842)(content(Whitespace\" \"))))(Tile((id \
                 578)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 2848)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 582)(label(==))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 8))(sort Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 583)(content(Whitespace\" \"))))(Tile((id \
                 584)(label(6))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4809)(content(Whitespace\" \")))))))))(Tile((id \
                 589)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 10))(sort Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 591)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 592)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 596)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 597)(content(Whitespace\" \"))))(Tile((id \
                 613)(label(List.fold_right))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 614)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 616)(content(Whitespace\" \"))))(Tile((id \
                 617)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 618)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 2697)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 622)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2699)(content(Whitespace\" \"))))(Tile((id \
                 2698)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Tile((id \
                 629)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 630)(content(Whitespace\" \"))))(Tile((id \
                 2700)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 633)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2701)(content(Whitespace\" \"))))(Tile((id 635)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2702)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 640)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 643)(content(Whitespace\" \"))))(Tile((id \
                 2703)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Tile((id \
                 647)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2705)(content(Whitespace\" \"))))(Tile((id \
                 2704)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 649)(content(Whitespace\" \")))))((Secondary((id \
                 653)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 657)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 658)(content(Whitespace\" \"))))(Tile((id \
                 660)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 661)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 663)(content(Whitespace\" \"))))(Tile((id \
                 665)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 666)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 668)(content(Whitespace\" \"))))(Tile((id \
                 671)(label(acc))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 672)(content(Whitespace\" \")))))))))(Secondary((id \
                 675)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 680)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 681)(content(Whitespace\" \
                 \"))))(Tile((id 684)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 685)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 686)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 688)(content(Whitespace\" \
                 \"))))(Tile((id 690)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 691)(content(Whitespace\" \")))))))))(Secondary((id \
                 694)(content(Whitespace\" \"))))(Tile((id \
                 697)(label(acc))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 698)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 699)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 701)(content(Whitespace\" \
                 \"))))(Tile((id 703)(label(hd))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 706)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 708)(label(tl))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 709)(content(Whitespace\" \")))))))))(Secondary((id \
                 712)(content(Whitespace\" \"))))(Tile((id \
                 713)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 714)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 717)(label(hd))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 718)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 720)(content(Whitespace\" \"))))(Tile((id \
                 735)(label(List.fold_right))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 736)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 738)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 739)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 741)(content(Whitespace\" \"))))(Tile((id \
                 743)(label(tl))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 744)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 746)(content(Whitespace\" \"))))(Tile((id \
                 749)(label(acc))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 4811)(content(Whitespace\" \")))))))))(Secondary((id \
                 4810)(content(Whitespace\" \")))))))))(Secondary((id \
                 765)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 891)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 895)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 896)(content(Whitespace\" \"))))(Tile((id \
                 912)(label(List.fold_left2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 913)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 915)(content(Whitespace\" \"))))(Tile((id \
                 916)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 917)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 2721)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 920)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 922)(content(Whitespace\" \"))))(Tile((id \
                 2722)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 924)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2724)(content(Whitespace\" \"))))(Tile((id \
                 2723)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 2720)(content(Whitespace\" \"))))(Tile((id \
                 931)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 932)(content(Whitespace\" \"))))(Tile((id \
                 2719)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 934)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 936)(content(Whitespace\" \"))))(Tile((id \
                 2716)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 938)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2718)(content(Whitespace\" \"))))(Tile((id 940)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2715)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 945)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 947)(content(Whitespace\" \"))))(Tile((id 948)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2714)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 2749)(content(Whitespace\" \"))))(Tile((id \
                 955)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2750)(content(Whitespace\" \"))))(Tile((id 956)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2713)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 961)(content(Whitespace\" \")))))((Secondary((id \
                 963)(content(Whitespace\" \"))))(Secondary((id \
                 964)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 968)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 969)(content(Whitespace\" \"))))(Tile((id \
                 971)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 972)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 974)(content(Whitespace\" \"))))(Tile((id \
                 977)(label(acc))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 978)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 980)(content(Whitespace\" \"))))(Tile((id \
                 5051)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 983)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 985)(content(Whitespace\" \"))))(Tile((id \
                 5061)(label(ys))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 988)(content(Whitespace\" \")))))))))(Secondary((id \
                 991)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 996)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 997)(content(Whitespace\" \
                 \"))))(Tile((id 5054)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1001)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1003)(content(Whitespace\" \"))))(Tile((id \
                 5064)(label(ys))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1006)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1007)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 1009)(content(Whitespace\" \
                 \"))))(Tile((id 1011)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1012)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4832)(content(Whitespace\" \"))))(Tile((id \
                 1015)(label([]))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1016)(content(Whitespace\" \")))))))))(Secondary((id \
                 1019)(content(Whitespace\" \"))))(Tile((id \
                 1022)(label(acc))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1023)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1024)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 1026)(content(Whitespace\" \
                 \"))))(Tile((id 5070)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1034)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 5056)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1040)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1042)(content(Whitespace\" \"))))(Tile((id \
                 5072)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1050)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 5066)(label(ys))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1056)(content(Whitespace\" \")))))))))(Secondary((id \
                 4831)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1074)(label(List.fold_left2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1075)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1077)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1078)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1080)(content(Whitespace\" \"))))(Tile((id \
                 1081)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1082)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1086)(label(acc))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1087)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1089)(content(Whitespace\" \"))))(Tile((id \
                 5071)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1095)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1097)(content(Whitespace\" \"))))(Tile((id \
                 5073)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 1103)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1105)(content(Whitespace\" \"))))(Tile((id \
                 5059)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1111)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1113)(content(Whitespace\" \"))))(Tile((id \
                 5069)(label(ys))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 2726)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2727)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 2729)(content(Whitespace\" \
                 \"))))(Tile((id 2730)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 2731)(content(Whitespace\" \")))))))))(Secondary((id \
                 2740)(content(Whitespace\" \"))))(Tile((id \
                 2741)(label(?))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4813)(content(Whitespace\" \")))))))))(Secondary((id \
                 4812)(content(Whitespace\" \")))))))))(Secondary((id \
                 1146)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1147)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1151)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1152)(content(Whitespace\" \"))))(Tile((id \
                 1169)(label(List.fold_right2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1170)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1172)(content(Whitespace\" \"))))(Tile((id \
                 1173)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 1174)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 2789)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 1177)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2802)(content(Whitespace\" \"))))(Tile((id \
                 2790)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 1182)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2803)(content(Whitespace\" \"))))(Tile((id \
                 2792)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 2801)(content(Whitespace\" \"))))(Tile((id \
                 1190)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1191)(content(Whitespace\" \"))))(Tile((id \
                 2793)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 1194)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1196)(content(Whitespace\" \"))))(Tile((id 1197)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2794)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 1203)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1205)(content(Whitespace\" \"))))(Tile((id 1206)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2795)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 1212)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2800)(content(Whitespace\" \"))))(Tile((id \
                 2796)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 2799)(content(Whitespace\" \"))))(Tile((id \
                 1220)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2798)(content(Whitespace\" \"))))(Tile((id 1221)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2797)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 1226)(content(Whitespace\" \")))))((Secondary((id \
                 1228)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1232)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 1233)(content(Whitespace\" \"))))(Tile((id \
                 1235)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1236)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1238)(content(Whitespace\" \"))))(Tile((id \
                 1241)(label(acc))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1242)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1244)(content(Whitespace\" \"))))(Tile((id \
                 2805)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1247)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1249)(content(Whitespace\" \"))))(Tile((id \
                 2809)(label(ys))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1252)(content(Whitespace\" \")))))))))(Secondary((id \
                 1255)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1260)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 1261)(content(Whitespace\" \
                 \"))))(Tile((id 2807)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1265)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1267)(content(Whitespace\" \"))))(Tile((id \
                 2811)(label(ys))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1270)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1271)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 1273)(content(Whitespace\" \
                 \"))))(Tile((id 1275)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1276)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3627)(content(Whitespace\" \"))))(Tile((id \
                 1279)(label([]))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1280)(content(Whitespace\" \")))))))))(Secondary((id \
                 1283)(content(Whitespace\" \"))))(Tile((id \
                 1286)(label(acc))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1287)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1288)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 1290)(content(Whitespace\" \
                 \"))))(Tile((id 5031)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1298)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 5037)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1304)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1306)(content(Whitespace\" \"))))(Tile((id \
                 5040)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1314)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 5043)(label(ys))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1320)(content(Whitespace\" \")))))))))(Secondary((id \
                 5047)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1324)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1325)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5033)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1332)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1334)(content(Whitespace\" \"))))(Tile((id \
                 5041)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1340)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2778)(content(Whitespace\" \"))))(Tile((id \
                 1357)(label(List.fold_right2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1358)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1360)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1361)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1363)(content(Whitespace\" \"))))(Tile((id \
                 5039)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1369)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1371)(content(Whitespace\" \"))))(Tile((id \
                 5045)(label(ys))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1377)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1379)(content(Whitespace\" \"))))(Tile((id \
                 1382)(label(acc))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 1388)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2779)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 2781)(content(Whitespace\" \
                 \"))))(Tile((id 2782)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 2783)(content(Whitespace\" \")))))))))(Secondary((id \
                 2787)(content(Whitespace\" \"))))(Tile((id \
                 2788)(label(?))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4815)(content(Whitespace\" \")))))))))(Secondary((id \
                 4814)(content(Whitespace\" \")))))))))(Secondary((id \
                 3842)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 3843)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3848)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3849)(content(Whitespace\" \"))))(Tile((id \
                 3858)(label(List.map))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3859)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3860)(content(Whitespace\" \"))))(Tile((id \
                 3861)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 3862)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3864)(content(Whitespace\" \"))))(Tile((id \
                 3866)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3867)(content(Whitespace\" \"))))(Tile((id \
                 3868)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 3870)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3871)(content(Whitespace\" \"))))(Tile((id 3872)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3873)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 3875)(content(Whitespace\" \"))))(Tile((id \
                 3877)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3878)(content(Whitespace\" \"))))(Tile((id \
                 3879)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3881)(content(Whitespace\" \")))))((Secondary((id \
                 3882)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3887)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 3888)(content(Whitespace\" \"))))(Tile((id \
                 3889)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3891)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3892)(content(Whitespace\" \"))))(Tile((id \
                 3895)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3896)(content(Whitespace\" \")))))))))(Secondary((id \
                 3898)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3914)(label(List.fold_right))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3915)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3920)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 3921)(content(Whitespace\" \"))))(Tile((id \
                 3922)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3924)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3925)(content(Whitespace\" \"))))(Tile((id \
                 3929)(label(acc))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3930)(content(Whitespace\" \")))))))))(Secondary((id \
                 3932)(content(Whitespace\" \"))))(Tile((id \
                 3933)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3935)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3936)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 3939)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 6))(sort Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3943)(label(acc))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3944)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3945)(content(Whitespace\" \"))))(Tile((id \
                 3948)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3949)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3950)(content(Whitespace\" \"))))(Tile((id \
                 3952)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3956)(content(Whitespace\" \")))))))))(Secondary((id \
                 1412)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1413)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1417)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1418)(content(Whitespace\" \"))))(Tile((id \
                 1428)(label(List.map2))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1429)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1431)(content(Whitespace\" \"))))(Tile((id \
                 1432)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 1433)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 2681)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 1436)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 2682)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 1441)(content(Whitespace\" \"))))(Tile((id \
                 1444)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1445)(content(Whitespace\" \"))))(Tile((id \
                 2684)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 1448)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1450)(content(Whitespace\" \"))))(Tile((id 1451)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2685)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 1456)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1458)(content(Whitespace\" \"))))(Tile((id 1459)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2686)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 1464)(content(Whitespace\" \"))))(Tile((id \
                 1467)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1468)(content(Whitespace\" \"))))(Tile((id 1469)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2687)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 1474)(content(Whitespace\" \")))))((Secondary((id \
                 1476)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1480)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 1481)(content(Whitespace\" \"))))(Tile((id \
                 1484)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1485)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1487)(content(Whitespace\" \"))))(Tile((id \
                 1489)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1490)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1492)(content(Whitespace\" \"))))(Tile((id \
                 1494)(label(ys))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1495)(content(Whitespace\" \")))))))))(Secondary((id \
                 1498)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1513)(label(List.fold_left2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1514)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 4817)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1519)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 1520)(content(Whitespace\" \"))))(Tile((id \
                 2752)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1523)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1525)(content(Whitespace\" \"))))(Tile((id \
                 2754)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1527)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1529)(content(Whitespace\" \"))))(Tile((id \
                 1532)(label(acc))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1533)(content(Whitespace\" \")))))))))(Secondary((id \
                 1536)(content(Whitespace\" \"))))(Tile((id \
                 1537)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1538)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2756)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1541)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2759)(content(Whitespace\" \"))))(Tile((id \
                 2758)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 1546)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 6))(sort Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1549)(label(acc))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1550)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1552)(content(Whitespace\" \"))))(Tile((id \
                 1554)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1555)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1557)(content(Whitespace\" \"))))(Tile((id \
                 1559)(label(ys))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1560)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1562)(content(Whitespace\" \"))))(Tile((id \
                 1564)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 4816)(content(Whitespace\" \")))))))))(Secondary((id \
                 4142)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 4143)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4148)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 4149)(content(Whitespace\" \"))))(Tile((id \
                 4161)(label(List.filter))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4162)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4163)(content(Whitespace\" \"))))(Tile((id \
                 4164)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 4165)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4167)(content(Whitespace\" \"))))(Tile((id \
                 4169)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4170)(content(Whitespace\" \"))))(Tile((id \
                 4175)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 4176)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4177)(content(Whitespace\" \"))))(Tile((id 4178)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 4179)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 4181)(content(Whitespace\" \"))))(Tile((id \
                 4183)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4184)(content(Whitespace\" \"))))(Tile((id 4185)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 4186)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 4188)(content(Whitespace\" \")))))((Secondary((id \
                 4189)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4194)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 4195)(content(Whitespace\" \"))))(Tile((id \
                 4196)(label(p))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4198)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4199)(content(Whitespace\" \"))))(Tile((id \
                 4202)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4203)(content(Whitespace\" \")))))))))(Secondary((id \
                 4205)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4211)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 4212)(content(Whitespace\" \
                 \"))))(Tile((id 4215)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4216)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4217)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 4218)(content(Whitespace\" \
                 \"))))(Tile((id 4220)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 4222)(content(Whitespace\" \")))))))))(Secondary((id \
                 4224)(content(Whitespace\" \"))))(Tile((id \
                 4226)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4228)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4229)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 4230)(content(Whitespace\" \
                 \"))))(Tile((id 4981)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4235)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4985)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4239)(content(Whitespace\" \")))))))))(Secondary((id \
                 5030)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4996)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 4997)(content(Whitespace\" \"))))(Tile((id \
                 5000)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 5001)(content(Whitespace\" \")))))((Secondary((id \
                 5002)(content(Whitespace\" \"))))(Tile((id \
                 5014)(label(List.filter))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 5015)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5016)(label(p))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 5018)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5019)(content(Whitespace\" \"))))(Tile((id \
                 5022)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 5023)(content(Whitespace\" \")))))))))(Secondary((id \
                 4241)(content(Whitespace\" \"))))(Secondary((id \
                 4242)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4246)(label(if then else))(mold((out Exp)(in_(Exp \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 4247)(content(Whitespace\" \"))))(Tile((id \
                 4248)(label(p))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4250)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4982)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 4257)(content(Whitespace\" \")))))((Secondary((id \
                 4260)(content(Whitespace\" \"))))(Tile((id \
                 4983)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4264)(content(Whitespace\" \"))))(Tile((id \
                 4266)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 6))(sort Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4267)(content(Whitespace\" \"))))(Tile((id \
                 5026)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4289)(content(Whitespace\" \")))))))))(Secondary((id \
                 4297)(content(Whitespace\" \"))))(Tile((id \
                 5028)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4321)(content(Whitespace\" \")))))))))(Secondary((id \
                 4327)(content(Whitespace\" \")))))))))(Secondary((id \
                 2566)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2567)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2575)(label(%EXPORT))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))(ancestors())))(caret \
                 Outer))";
              backup_text =
                "type Option = None + Some(?) in\n\n\
                 let fst: (?, ?) -> ? = fun a, b -> a in\n\
                 let snd: (?, ?) -> ? = fun a, b -> b in\n\n\
                 let not: Bool -> Bool = fun b -> !b in\n\n\
                 let bool_eq: (Bool, Bool) -> Bool =\n\
                 fun a, b -> a && b \\/ !a && !b in\n\n\
                 let List.cons: (?, [?]) -> [?] =\n\
                 fun x, xs -> x::xs in\n\n\
                 let List.length: [?] -> Int =\n\
                 fun xs ->\n\
                 case xs\n\
                 | [] => 0\n\
                 | _::xs => 1 + List.length(xs) end in\n\n\
                 let List.hd: [?] -> ? =\n\
                 fun l -> \n\
                 case l  \n\
                 | [] => ?\n\
                 | x::xs => x end in\n\n\
                 let List.tl: [?] -> [?] =\n\
                 fun l ->\n\
                 case l \n\
                 | [] => ?\n\
                 | x::xs => xs end in\n\n\
                 let List.is_empty: [?] -> Bool =\n\
                 fun xs ->\n\
                 case xs\n\
                 | [] => true\n\
                 | _::_ => false end in\n\n\
                 let List.nth: ([?], Int) -> ? =\n\
                 fun xs, n ->\n\
                 case xs, n\n\
                 | x::_, 0 => x\n\
                 | _::xs, n => List.nth(xs, n - 1)\n\
                 | [], _ => ? end in\n\
                 test\n\
                 List.nth([7, 8, 9], 2)\n\
                 == 9 end;\n\n\
                 let List.rev: [?] -> [?] =\n\
                 fun l -> \n\
                 let go: ([?], [?]) -> [?] =\n\
                 fun xs, acc -> \n\
                 case xs \n\
                 | [] => acc \n\
                 | x::xs => go(xs, x::acc) end in\n\
                 go(l, []) in\n\n\
                 let List.init: (Int, Int -> ?) -> [?] =\n\
                 fun len, f ->\n\
                 let go: (Int, [?]) -> [?] =\n\
                 fun idx, xs ->\n\
                 if idx < len\n\
                 then go(idx+ 1, xs @ [f(idx)])  \n\
                 else xs in\n\
                 go(0, []) in\n\n\
                 let List.equal: (? -> Bool, [?], [?]) -> Bool =\n\
                 fun p, xs, ys ->\n\
                 case xs, ys\n\
                 | [], [] => true\n\
                 | x::xs, y::ys => p(x, y) && List.equal(p, xs, ys)\n\
                 | _ => false end in\n\n\
                 let List.fold_left: ((?, ?)-> ?, ?, [?])-> ?   =\n\
                 fun f, acc, xs ->\n\
                 case xs \n\
                 | [] => acc\n\
                 | hd::tl => List.fold_left(f, f(acc, hd), tl) end in\n\
                 test\n\
                 List.fold_left(fun x, acc -> x + acc, 666, [])\n\
                 == 666 end;\n\
                 test\n\
                 List.fold_left(fun x, acc -> x + acc, 0, [1, 2, 3])\n\
                 == 6 end;\n\n\
                 let List.fold_right: ((?, ?)-> ?, [?], ?)-> ? =\n\
                 fun f, xs, acc ->\n\
                 case xs\n\
                 | [] => acc\n\
                 | hd::tl => f(hd, List.fold_right(f, tl, acc)) end in\n\n\
                 let List.fold_left2: ((?, ?, ?) -> ?, ?, [?], [?]) -> [?] = \n\
                 fun f, acc, xs, ys ->\n\
                 case xs, ys\n\
                 | [], [] => acc\n\
                 | x::xs, y::ys =>\n\
                 List.fold_left2(f, f(acc, x, y), xs, ys)\n\
                 | _ => ? end in\n\n\
                 let List.fold_right2: ((?, ?, ?) -> ?, [?], [?], ?) -> [?] =\n\
                 fun f, acc, xs, ys ->\n\
                 case xs, ys\n\
                 | [], [] => acc\n\
                 | x::xs, y::ys =>\n\
                 f(x, y, List.fold_right2(f, xs, ys, acc))\n\
                 | _ => ? end in\n\n\
                 let List.map: (? -> ?, [?]) -> ? =\n\
                 fun f, xs ->\n\
                 List.fold_right(fun x, acc -> f(x)::acc, xs, []) in\n\n\
                 let List.map2: ((?,?) -> ?, [?], [?]) -> [?] =\n\
                 fun f, xs, ys ->\n\
                 List.fold_left2(\n\
                 fun x, y, acc -> f(x, y)::acc, xs, ys, []) in\n\n\
                 let List.filter: (? -> Bool, [?]) -> [?] =\n\
                 fun p, xs ->\n\
                 case xs\n\
                 | [] => []\n\
                 | x::xs =>\n\
                 let xs = List.filter(p, xs) in \n\
                 if p(x) then x :: xs else xs end in\n\n\
                 %EXPORT";
            } );
          ( 4024,
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Tile((id \
                 2695)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 2696)(content(Whitespace\" \"))))(Tile((id \
                 2708)(label(List.append))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 2709)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4017)(content(Whitespace\" \"))))(Tile((id \
                 2710)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 2711)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 2712)(label([ ]))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2713)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 2715)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2716)(content(Whitespace\" \"))))(Tile((id 2717)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2718)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 2720)(content(Whitespace\" \"))))(Tile((id \
                 2722)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2723)(content(Whitespace\" \"))))(Tile((id 2724)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2725)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 2727)(content(Whitespace\" \")))))((Secondary((id \
                 2728)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2733)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 2734)(content(Whitespace\" \"))))(Tile((id \
                 2737)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 2738)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2739)(content(Whitespace\" \"))))(Tile((id \
                 2742)(label(ys))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2743)(content(Whitespace\" \")))))))))(Secondary((id \
                 2745)(content(Whitespace\" \"))))(Tile((id \
                 2761)(label(List.fold_right))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2762)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2772)(label(List.cons))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2773)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2774)(content(Whitespace\" \"))))(Tile((id \
                 2777)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2778)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2779)(content(Whitespace\" \"))))(Tile((id \
                 2782)(label(ys))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 2785)(content(Whitespace\" \")))))))))(Secondary((id \
                 2786)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2787)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2792)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 2793)(content(Whitespace\" \"))))(Tile((id \
                 2805)(label(List.concat))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 2806)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2807)(content(Whitespace\" \"))))(Tile((id 2808)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2809)(label([ ]))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 2810)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 2812)(content(Whitespace\" \"))))(Tile((id \
                 2814)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2815)(content(Whitespace\" \"))))(Tile((id 2816)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2817)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 2819)(content(Whitespace\" \")))))((Secondary((id \
                 2820)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2825)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 2826)(content(Whitespace\" \"))))(Tile((id \
                 2830)(label(xss))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2831)(content(Whitespace\" \")))))))))(Secondary((id \
                 2833)(content(Whitespace\" \"))))(Tile((id \
                 2849)(label(List.fold_right))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2850)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2862)(label(List.append))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2863)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2864)(content(Whitespace\" \"))))(Tile((id \
                 2868)(label(xss))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2869)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2870)(content(Whitespace\" \"))))(Tile((id \
                 2872)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 2876)(content(Whitespace\" \"))))(Secondary((id \
                 4018)(content(Whitespace\" \")))))))))(Secondary((id \
                 2877)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2878)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2883)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 2884)(content(Whitespace\" \"))))(Tile((id \
                 2897)(label(List.flatten))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2898)(content(Whitespace\" \")))))((Secondary((id \
                 2899)(content(Whitespace\" \"))))(Tile((id \
                 2911)(label(List.concat))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2914)(content(Whitespace\" \")))))))))(Secondary((id \
                 2915)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2916)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2921)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 2922)(content(Whitespace\" \"))))(Tile((id \
                 2932)(label(List.mapi))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 2933)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3284)(content(Whitespace\" \"))))(Tile((id \
                 2934)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 2935)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 2939)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 2940)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2941)(content(Whitespace\" \"))))(Tile((id \
                 2942)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 2944)(content(Whitespace\" \"))))(Tile((id \
                 2946)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2947)(content(Whitespace\" \"))))(Tile((id \
                 2948)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 2950)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2951)(content(Whitespace\" \"))))(Tile((id 2952)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2953)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 2955)(content(Whitespace\" \"))))(Tile((id \
                 2957)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2958)(content(Whitespace\" \"))))(Tile((id 2959)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 2960)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 2962)(content(Whitespace\" \")))))((Secondary((id \
                 2963)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2968)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 2969)(content(Whitespace\" \"))))(Tile((id \
                 2970)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 2972)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2973)(content(Whitespace\" \"))))(Tile((id \
                 2976)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2977)(content(Whitespace\" \")))))))))(Secondary((id \
                 2979)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2984)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 2985)(content(Whitespace\" \"))))(Tile((id \
                 2988)(label(go))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 2989)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2990)(content(Whitespace\" \"))))(Tile((id \
                 2991)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2993)(content(Whitespace\" \"))))(Tile((id \
                 2995)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2996)(content(Whitespace\" \"))))(Tile((id \
                 2997)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2999)(content(Whitespace\" \")))))((Secondary((id \
                 3000)(content(Whitespace\" \"))))(Tile((id 3005)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3006)(content(Whitespace\" \
                 \"))))(Tile((id 3010)(label(idx))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3011)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3012)(content(Whitespace\" \"))))(Tile((id \
                 3015)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3016)(content(Whitespace\" \")))))))))(Secondary((id \
                 3018)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3024)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3025)(content(Whitespace\" \
                 \"))))(Tile((id 3028)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3029)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3030)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3031)(content(Whitespace\" \
                 \"))))(Tile((id 3033)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3035)(content(Whitespace\" \")))))))))(Secondary((id \
                 3037)(content(Whitespace\" \"))))(Tile((id \
                 3039)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3041)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3042)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3043)(content(Whitespace\" \
                 \"))))(Tile((id 3046)(label(hd))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3048)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 6))(sort Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3051)(label(tl))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3052)(content(Whitespace\" \")))))))))(Secondary((id \
                 3054)(content(Whitespace\" \"))))(Tile((id \
                 3055)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3057)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3061)(label(idx))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3062)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3063)(content(Whitespace\" \"))))(Tile((id \
                 3066)(label(hd))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 3068)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 6))(sort Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3071)(label(go))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3072)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3076)(label(idx))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3077)(content(Whitespace\" \"))))(Tile((id \
                 3078)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 5))(sort Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3079)(content(Whitespace\" \"))))(Tile((id \
                 3080)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3082)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3083)(content(Whitespace\" \"))))(Tile((id \
                 3086)(label(tl))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 4023)(content(Whitespace\" \")))))))))(Secondary((id \
                 3097)(content(Whitespace\" \")))))))))(Secondary((id \
                 3098)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3101)(label(go))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3102)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3103)(label(0))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3105)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3106)(content(Whitespace\" \"))))(Tile((id \
                 3109)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3115)(content(Whitespace\" \")))))))))(Secondary((id \
                 3116)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 3117)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3122)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3123)(content(Whitespace\" \"))))(Tile((id \
                 3136)(label(List.filteri))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3137)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3138)(content(Whitespace\" \"))))(Tile((id \
                 3139)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 3140)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 3144)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 3145)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3146)(content(Whitespace\" \"))))(Tile((id \
                 3147)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 3149)(content(Whitespace\" \"))))(Tile((id \
                 3151)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3152)(content(Whitespace\" \"))))(Tile((id \
                 3157)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 3158)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3159)(content(Whitespace\" \"))))(Tile((id 3160)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3161)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 3163)(content(Whitespace\" \"))))(Tile((id \
                 3165)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3166)(content(Whitespace\" \"))))(Tile((id 3167)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3168)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 3170)(content(Whitespace\" \")))))((Secondary((id \
                 3171)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3176)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 3177)(content(Whitespace\" \"))))(Tile((id \
                 3178)(label(f))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3180)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3181)(content(Whitespace\" \"))))(Tile((id \
                 3184)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3185)(content(Whitespace\" \")))))))))(Secondary((id \
                 3187)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3199)(label(List.concat))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3200)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3211)(label(List.mapi))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3212)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 4020)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3217)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 3218)(content(Whitespace\" \"))))(Tile((id \
                 3219)(label(i))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3221)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3222)(content(Whitespace\" \"))))(Tile((id \
                 3223)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3225)(content(Whitespace\" \")))))))))(Secondary((id \
                 3227)(content(Whitespace\" \"))))(Tile((id 3231)(label(if \
                 then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 12))(sort \
                 Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3232)(content(Whitespace\" \"))))(Tile((id \
                 3233)(label(f))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3235)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3236)(label(i))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3238)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3239)(content(Whitespace\" \"))))(Tile((id \
                 3240)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3244)(content(Whitespace\" \")))))((Secondary((id \
                 3247)(content(Whitespace\" \"))))(Tile((id 3248)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 3249)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3253)(content(Whitespace\" \")))))))))(Secondary((id \
                 3256)(content(Whitespace\" \"))))(Tile((id \
                 3258)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3260)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3261)(content(Whitespace\" \"))))(Tile((id \
                 3264)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 3267)(content(Whitespace\" \")))))))))(Secondary((id \
                 3715)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 3716)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3721)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3722)(content(Whitespace\" \"))))(Tile((id \
                 3734)(label(List.exists))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3735)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3736)(content(Whitespace\" \"))))(Tile((id \
                 3737)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 3738)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3740)(content(Whitespace\" \"))))(Tile((id \
                 3742)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3743)(content(Whitespace\" \"))))(Tile((id \
                 3748)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 3749)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3750)(content(Whitespace\" \"))))(Tile((id 3751)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3752)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 3754)(content(Whitespace\" \"))))(Tile((id \
                 3756)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3757)(content(Whitespace\" \"))))(Tile((id \
                 3762)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3763)(content(Whitespace\" \")))))((Secondary((id \
                 3764)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3769)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 3770)(content(Whitespace\" \"))))(Tile((id \
                 3771)(label(p))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3773)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3774)(content(Whitespace\" \"))))(Tile((id \
                 3777)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3778)(content(Whitespace\" \")))))))))(Secondary((id \
                 3780)(content(Whitespace\" \"))))(Tile((id \
                 3792)(label(List.exists))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3793)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4022)(label(p))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3810)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3811)(content(Whitespace\" \"))))(Tile((id \
                 3814)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3818)(content(Whitespace\" \")))))))))(Secondary((id \
                 4016)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 3819)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3824)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3825)(content(Whitespace\" \"))))(Tile((id \
                 3838)(label(List.for_all))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3839)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3840)(content(Whitespace\" \"))))(Tile((id \
                 3841)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 3842)(label(?))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3844)(content(Whitespace\" \"))))(Tile((id \
                 3846)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3847)(content(Whitespace\" \"))))(Tile((id \
                 3852)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 3853)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3854)(content(Whitespace\" \"))))(Tile((id 3855)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3856)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 3858)(content(Whitespace\" \"))))(Tile((id \
                 3860)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3861)(content(Whitespace\" \"))))(Tile((id \
                 3866)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3867)(content(Whitespace\" \")))))((Secondary((id \
                 3868)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3873)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 3874)(content(Whitespace\" \"))))(Tile((id \
                 3875)(label(p))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3877)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3878)(content(Whitespace\" \"))))(Tile((id \
                 3881)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3882)(content(Whitespace\" \")))))))))(Secondary((id \
                 3884)(content(Whitespace\" \"))))(Tile((id \
                 3888)(label(not))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3889)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3901)(label(List.exists))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3902)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3907)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 3908)(content(Whitespace\" \"))))(Tile((id \
                 3909)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3911)(content(Whitespace\" \")))))))))(Secondary((id \
                 3913)(content(Whitespace\" \"))))(Tile((id \
                 3917)(label(not))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3918)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3919)(label(p))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3921)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3922)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 3924)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3925)(content(Whitespace\" \"))))(Tile((id \
                 3928)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 3931)(content(Whitespace\" \")))))))))(Secondary((id \
                 3933)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 3934)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3939)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3940)(content(Whitespace\" \"))))(Tile((id \
                 3949)(label(List.mem))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3950)(content(Whitespace\" \")))))((Secondary((id \
                 3951)(content(Whitespace\" \"))))(Tile((id 3956)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3957)(content(Whitespace\" \
                 \"))))(Tile((id 3960)(label(eq))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3961)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3962)(content(Whitespace\" \"))))(Tile((id \
                 3965)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3966)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3967)(content(Whitespace\" \"))))(Tile((id \
                 3968)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3970)(content(Whitespace\" \")))))))))(Secondary((id \
                 3972)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3984)(label(List.exists))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3985)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3990)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 3991)(content(Whitespace\" \"))))(Tile((id \
                 3992)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3994)(content(Whitespace\" \")))))))))(Secondary((id \
                 3996)(content(Whitespace\" \"))))(Tile((id \
                 3999)(label(eq))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4000)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4001)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4003)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4004)(content(Whitespace\" \"))))(Tile((id \
                 4005)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 4007)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4008)(content(Whitespace\" \"))))(Tile((id \
                 4011)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 4014)(content(Whitespace\" \")))))))))(Secondary((id \
                 1833)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1834)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1840)(content(Comment #TODO:#))))(Secondary((id \
                 1491)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1497)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1498)(content(Whitespace\" \"))))(Tile((id \
                 1514)(label(List.filter_map))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 1515)(content(Whitespace\" \")))))((Secondary((id \
                 1560)(content(Whitespace\" \"))))(Tile((id \
                 1559)(label(?))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1517)(content(Whitespace\" \")))))))))(Secondary((id \
                 1490)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1522)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1523)(content(Whitespace\" \"))))(Tile((id \
                 1558)(label(List.concat_map))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 1540)(content(Whitespace\" \")))))((Secondary((id \
                 1561)(content(Whitespace\" \"))))(Tile((id \
                 1562)(label(?))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1542)(content(Whitespace\" \")))))))))(Secondary((id \
                 3612)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1647)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1648)(content(Whitespace\" \"))))(Tile((id \
                 1737)(label(List.for_all2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 1665)(content(Whitespace\" \")))))((Secondary((id \
                 1666)(content(Whitespace\" \"))))(Tile((id \
                 1667)(label(?))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1671)(content(Whitespace\" \")))))))))(Secondary((id \
                 1673)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1678)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1679)(content(Whitespace\" \"))))(Tile((id \
                 1754)(label(List.exists2))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1696)(content(Whitespace\" \")))))((Secondary((id \
                 1697)(content(Whitespace\" \"))))(Tile((id \
                 1698)(label(?))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1702)(content(Whitespace\" \")))))))))(Secondary((id \
                 1755)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1785)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1786)(content(Whitespace\" \"))))(Tile((id \
                 1798)(label(List.find))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1799)(content(Whitespace\" \")))))((Secondary((id \
                 1801)(content(Whitespace\" \"))))(Tile((id \
                 1802)(label(?))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1803)(content(Whitespace\" \")))))))))(Secondary((id \
                 1804)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1810)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1811)(content(Whitespace\" \"))))(Tile((id \
                 1826)(label(List.partition))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 1827)(content(Whitespace\" \")))))((Secondary((id \
                 1830)(content(Whitespace\" \"))))(Tile((id \
                 1829)(label(?))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1831)(content(Whitespace\" \")))))))))(Secondary((id \
                 1932)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1940)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1941)(content(Whitespace\" \"))))(Tile((id \
                 1952)(label(List.split))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1953)(content(Whitespace\" \")))))((Secondary((id \
                 1980)(content(Whitespace\" \"))))(Tile((id \
                 1981)(label(?))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1982)(content(Whitespace\" \")))))))))(Secondary((id \
                 1955)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1963)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1964)(content(Whitespace\" \"))))(Tile((id \
                 1977)(label(List.combine))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1978)(content(Whitespace\" \")))))((Secondary((id \
                 1983)(content(Whitespace\" \"))))(Tile((id \
                 1984)(label(?))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1985)(content(Whitespace\" \")))))))))(Secondary((id \
                 1867)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1873)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1874)(content(Whitespace\" \"))))(Tile((id \
                 1885)(label(List.merge))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1886)(content(Whitespace\" \")))))((Secondary((id \
                 1889)(content(Whitespace\" \"))))(Tile((id \
                 1888)(label(?))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1890)(content(Whitespace\" \")))))))))(Secondary((id \
                 1891)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1897)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1898)(content(Whitespace\" \"))))(Tile((id \
                 1908)(label(List.sort))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3269)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3272)(content(Whitespace\" \"))))(Tile((id 3273)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3274)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 3275)(content(Whitespace\" \"))))(Tile((id \
                 3279)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3280)(content(Whitespace\" \"))))(Tile((id 3281)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 3282)(label(?))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 3283)(content(Whitespace\" \")))))((Secondary((id \
                 1930)(content(Whitespace\" \"))))(Tile((id \
                 1929)(label(?))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1931)(content(Whitespace\" \")))))))))(Secondary((id \
                 1841)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1842)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1866)(content(Comment\"#TODO: Association \
                 Lists#\"))))(Secondary((id \
                 1832)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 307)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 315)(label(%EXPORT))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))(ancestors())))(caret \
                 Outer))";
              backup_text =
                "let List.append: (([?], [?]) -> [?]) =\n\
                 fun xs, ys -> List.fold_right(List.cons, xs, ys) in\n\n\
                 let List.concat: [[?]] -> [?] =\n\
                 fun xss -> List.fold_right(List.append, xss, [])  in\n\n\
                 let List.flatten = List.concat in\n\n\
                 let List.mapi: ((Int, ?) -> ?, [?]) -> [?] =\n\
                 fun f, xs ->\n\
                 let go: ? -> ? = fun idx, xs ->\n\
                 case xs\n\
                 | [] => []\n\
                 | hd::tl => f(idx, hd)::go(idx + 1, tl) end in\n\
                 go(0, xs) in\n\n\
                 let List.filteri: ((Int, ?) -> Bool, [?]) -> [?] =\n\
                 fun f, xs ->\n\
                 List.concat(List.mapi(\n\
                 fun i, x -> if f(i, x) then [x] else [], xs)) in\n\n\
                 let List.exists: (? -> Bool, [?]) -> Bool =\n\
                 fun p, xs -> List.exists(p, xs) in\n\n\
                 let List.for_all: (? -> Bool, [?]) -> Bool =\n\
                 fun p, xs -> not(List.exists(fun x -> not(p(x)), xs)) in\n\n\
                 let List.mem = fun eq, xs, y ->\n\
                 List.exists(fun x -> eq(x, y), xs) in\n\n\
                 #TODO:#\n\
                 let List.filter_map = ? in\n\
                 let List.concat_map = ? in\n\
                 let List.for_all2 = ? in\n\
                 let List.exists2 = ? in\n\
                 let List.find = ? in\n\
                 let List.partition = ? in\n\
                 let List.split = ? in\n\
                 let List.combine = ? in\n\
                 let List.merge = ? in\n\
                 let List.sort: [?] -> [?] = ? in\n\n\
                 #TODO: Association Lists#\n\n\
                 %EXPORT";
            } );
          ( 2402,
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Secondary((id \
                 2179)(content(Comment\"# A todo has a description and a \
                 status #\"))))(Secondary((id \
                 2139)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 504)(label(type = in))(mold((out Exp)(in_(TPat \
                 Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 505)(content(Whitespace\" \"))))(Tile((id \
                 510)(label(Todo))(mold((out TPat)(in_())(nibs(((shape \
                 Convex)(sort TPat))((shape Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 511)(content(Whitespace\" \")))))((Secondary((id \
                 512)(content(Whitespace\" \"))))(Tile((id \
                 513)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 535)(label(String))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 536)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1958)(content(Whitespace\" \"))))(Tile((id \
                 551)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 555)(content(Whitespace\" \")))))))))(Secondary((id \
                 2180)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2181)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2242)(content(Comment\"# A description input buffer and a \
                 todo list #\"))))(Secondary((id \
                 1568)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 587)(label(type = in))(mold((out Exp)(in_(TPat \
                 Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 588)(content(Whitespace\" \"))))(Tile((id \
                 594)(label(Model))(mold((out TPat)(in_())(nibs(((shape \
                 Convex)(sort TPat))((shape Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 595)(content(Whitespace\" \")))))((Secondary((id \
                 596)(content(Whitespace\" \"))))(Tile((id \
                 597)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 613)(label(String))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 614)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1967)(content(Whitespace\" \"))))(Tile((id 1959)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 1965)(label(Todo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 633)(content(Whitespace\" \")))))))))(Secondary((id \
                 2243)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1569)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 673)(label(type = in))(mold((out Exp)(in_(TPat \
                 Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 674)(content(Whitespace\" \"))))(Tile((id \
                 681)(label(Action))(mold((out TPat)(in_())(nibs(((shape \
                 Convex)(sort TPat))((shape Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 682)(content(Whitespace\" \")))))((Secondary((id \
                 683)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 684)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 685)(content(Whitespace\" \"))))(Tile((id \
                 693)(label(AddTodo))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 694)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 695)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 696)(content(Whitespace\" \"))))(Tile((id \
                 707)(label(RemoveTodo))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 708)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 712)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 713)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 714)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 715)(content(Whitespace\" \"))))(Tile((id \
                 726)(label(ToggleTodo))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 727)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 731)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 732)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 733)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 734)(content(Whitespace\" \"))))(Tile((id \
                 2258)(label(UpdateBuffer))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 747)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 754)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 759)(content(Whitespace\" \")))))))))(Secondary((id \
                 2247)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1570)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 766)(label(type = in))(mold((out Exp)(in_(TPat \
                 Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 767)(content(Whitespace\" \"))))(Tile((id \
                 774)(label(Update))(mold((out TPat)(in_())(nibs(((shape \
                 Convex)(sort TPat))((shape Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 775)(content(Whitespace\" \")))))((Secondary((id \
                 776)(content(Whitespace\" \"))))(Tile((id \
                 777)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 783)(label(Model))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 784)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 785)(content(Whitespace\" \"))))(Tile((id \
                 792)(label(Action))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 793)(content(Whitespace\" \"))))(Tile((id \
                 795)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 796)(content(Whitespace\" \"))))(Tile((id \
                 802)(label(Model))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 805)(content(Whitespace\" \")))))))))(Secondary((id \
                 1423)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1258)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1678)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1679)(content(Whitespace\" \"))))(Tile((id \
                 1687)(label(Todo.eq))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1688)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1689)(content(Whitespace\" \"))))(Tile((id \
                 1690)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 1695)(label(Todo))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 1696)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1697)(content(Whitespace\" \"))))(Tile((id \
                 1702)(label(Todo))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 1703)(content(Whitespace\" \"))))(Tile((id \
                 1705)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1706)(content(Whitespace\" \"))))(Tile((id \
                 1711)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1712)(content(Whitespace\" \")))))((Secondary((id \
                 1713)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1718)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 1719)(content(Whitespace\" \"))))(Tile((id \
                 1720)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 1969)(label(d1))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1724)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1725)(content(Whitespace\" \"))))(Tile((id \
                 2260)(label(s1))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Tile((id \
                 1729)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1730)(content(Whitespace\" \"))))(Tile((id \
                 1731)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 1971)(label(d2))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1735)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1736)(content(Whitespace\" \"))))(Tile((id \
                 2262)(label(s2))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 1740)(content(Whitespace\" \")))))))))(Secondary((id \
                 1742)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1973)(label(d1))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1746)(content(Whitespace\" \"))))(Tile((id \
                 1749)(label($==))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 8))(sort Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1750)(content(Whitespace\" \"))))(Tile((id \
                 1975)(label(d2))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1754)(content(Whitespace\" \"))))(Tile((id \
                 1756)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 9))(sort Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1757)(content(Whitespace\" \"))))(Tile((id \
                 1765)(label(bool_eq))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1766)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2264)(label(s1))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1770)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1771)(content(Whitespace\" \"))))(Tile((id \
                 2266)(label(s2))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 1777)(content(Whitespace\" \")))))))))(Secondary((id \
                 1778)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1779)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1784)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1785)(content(Whitespace\" \"))))(Tile((id \
                 1794)(label(Model.eq))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1795)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1796)(content(Whitespace\" \"))))(Tile((id \
                 1797)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 1803)(label(Model))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 1804)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1805)(content(Whitespace\" \"))))(Tile((id \
                 1811)(label(Model))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 1812)(content(Whitespace\" \"))))(Tile((id \
                 1814)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1815)(content(Whitespace\" \"))))(Tile((id \
                 1820)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1821)(content(Whitespace\" \")))))((Secondary((id \
                 2064)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1827)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 1828)(content(Whitespace\" \"))))(Tile((id \
                 1829)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 2269)(label(b1))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1833)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1834)(content(Whitespace\" \"))))(Tile((id \
                 2282)(label(ts1))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Tile((id \
                 1842)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1843)(content(Whitespace\" \"))))(Tile((id \
                 1844)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 2272)(label(b2))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1848)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1849)(content(Whitespace\" \"))))(Tile((id \
                 2318)(label(ts2))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 1976)(content(Whitespace\" \")))))))))(Secondary((id \
                 1858)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2301)(label(b1))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2302)(content(Whitespace\" \"))))(Tile((id \
                 2305)(label($==))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 8))(sort Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2306)(content(Whitespace\" \"))))(Tile((id \
                 2309)(label(b2))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2311)(content(Whitespace\" \"))))(Tile((id \
                 2313)(label(&&))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 9))(sort Exp))((shape(Concave 9))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2314)(content(Whitespace\" \"))))(Tile((id \
                 2086)(label(List.equal))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2087)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2096)(label(Todo.eq))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2097)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2098)(content(Whitespace\" \"))))(Tile((id \
                 2290)(label(ts1))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2106)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2107)(content(Whitespace\" \"))))(Tile((id \
                 2319)(label(ts2))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 2297)(content(Whitespace\" \")))))))))(Secondary((id \
                 2361)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2362)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2367)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 2368)(content(Whitespace\" \"))))(Tile((id \
                 2379)(label(Model.init))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 2380)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2381)(content(Whitespace\" \"))))(Tile((id \
                 2387)(label(Model))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 2388)(content(Whitespace\" \")))))((Secondary((id \
                 2389)(content(Whitespace\" \"))))(Tile((id \
                 2390)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2391)(label(\"\\\"\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2393)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2394)(content(Whitespace\" \"))))(Tile((id \
                 2396)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 2400)(content(Whitespace\" \")))))))))(Secondary((id \
                 2317)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 489)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 497)(label(%EXPORT))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))(ancestors())))(caret \
                 Outer))";
              backup_text =
                "# A todo has a description and a status #\n\
                 type Todo = (String, Bool) in\n\n\
                 # A description input buffer and a todo list #\n\
                 type Model = (String, [Todo]) in\n\n\
                 type Action =\n\
                 + AddTodo\n\
                 + RemoveTodo(Int)\n\
                 + ToggleTodo(Int)\n\
                 + UpdateBuffer(String) in\n\n\
                 type Update = (Model, Action) -> Model in\n\n\
                 let Todo.eq: (Todo, Todo) -> Bool =\n\
                 fun (d1, s1), (d2, s2) ->\n\
                 d1 $== d2 && bool_eq(s1, s2) in\n\n\
                 let Model.eq: (Model, Model) -> Bool =\n\
                 fun (b1, ts1), (b2, ts2) ->\n\
                 b1 $== b2 && List.equal(Todo.eq, ts1, ts2) in\n\n\
                 let Model.init: Model = (\"\", []) in\n\n\
                 %EXPORT";
            } );
          ( 1046,
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Tile((id \
                 558)(label(type = in))(mold((out Exp)(in_(TPat \
                 Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 559)(content(Whitespace\" \"))))(Tile((id \
                 569)(label(StyleAttr))(mold((out TPat)(in_())(nibs(((shape \
                 Convex)(sort TPat))((shape Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 570)(content(Whitespace\" \")))))((Secondary((id \
                 571)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 572)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 573)(content(Whitespace\" \"))))(Tile((id \
                 584)(label(AlignItems))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 585)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 592)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 593)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 594)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 595)(content(Whitespace\" \"))))(Tile((id \
                 611)(label(BackgroundColor))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 612)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 619)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 620)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 621)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 622)(content(Whitespace\" \"))))(Tile((id \
                 629)(label(Border))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 630)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 637)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 638)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 639)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 640)(content(Whitespace\" \"))))(Tile((id \
                 653)(label(BorderRadius))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 654)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 661)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 662)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 663)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 664)(content(Whitespace\" \"))))(Tile((id \
                 674)(label(BoxShadow))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 675)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 682)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 683)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 684)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 685)(content(Whitespace\" \"))))(Tile((id \
                 691)(label(Color))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 692)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 699)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 700)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 701)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 702)(content(Whitespace\" \"))))(Tile((id \
                 709)(label(Cursor))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 710)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 717)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 718)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 719)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 720)(content(Whitespace\" \"))))(Tile((id \
                 728)(label(Display))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 729)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 736)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 737)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 738)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 739)(content(Whitespace\" \"))))(Tile((id \
                 753)(label(FlexDirection))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 754)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 761)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 762)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 763)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 764)(content(Whitespace\" \"))))(Tile((id \
                 775)(label(FontFamily))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 776)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 783)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 784)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 785)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 786)(content(Whitespace\" \"))))(Tile((id \
                 795)(label(FontSize))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 796)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 803)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 804)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 805)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 806)(content(Whitespace\" \"))))(Tile((id \
                 816)(label(FontStyle))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 817)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 824)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 825)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 826)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 827)(content(Whitespace\" \"))))(Tile((id \
                 831)(label(Gap))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 832)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 839)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 840)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 841)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 842)(content(Whitespace\" \"))))(Tile((id \
                 849)(label(Height))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 850)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 857)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 858)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 859)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 860)(content(Whitespace\" \"))))(Tile((id \
                 875)(label(JustifyContent))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 876)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 883)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 884)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 885)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 886)(content(Whitespace\" \"))))(Tile((id \
                 893)(label(Margin))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 894)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 901)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 902)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 903)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 904)(content(Whitespace\" \"))))(Tile((id \
                 912)(label(Opacity))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 913)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 920)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 921)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 922)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 923)(content(Whitespace\" \"))))(Tile((id \
                 931)(label(Outline))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 932)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 939)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 940)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 941)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 942)(content(Whitespace\" \"))))(Tile((id \
                 951)(label(Overflow))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 952)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 959)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 960)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 961)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 962)(content(Whitespace\" \"))))(Tile((id \
                 970)(label(Padding))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 971)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 978)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 979)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 980)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 981)(content(Whitespace\" \"))))(Tile((id \
                 990)(label(Position))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 991)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 998)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 999)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1000)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1001)(content(Whitespace\" \"))))(Tile((id \
                 1007)(label(Width))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 1008)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 1015)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 1016)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1017)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1018)(content(Whitespace\" \"))))(Tile((id \
                 1019)(label(S))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 1021)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 1028)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 1029)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1030)(content(Whitespace\" \"))))(Tile((id \
                 1037)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 1041)(content(Whitespace\" \"))))(Secondary((id \
                 1038)(content(Whitespace\" \"))))(Secondary((id \
                 1039)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 1043)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5)(label(type = in))(mold((out Exp)(in_(TPat \
                 Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 6)(content(Whitespace\" \"))))(Tile((id \
                 11)(label(Attr))(mold((out TPat)(in_())(nibs(((shape \
                 Convex)(sort TPat))((shape Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 12)(content(Whitespace\" \")))))((Secondary((id \
                 14)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 15)(label(+))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 16)(content(Whitespace\" \"))))(Tile((id \
                 23)(label(OnClick))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 24)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 27)(label(\"()\"))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 28)(content(Whitespace\" \"))))(Tile((id \
                 31)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 32)(content(Whitespace\" \"))))(Tile((id \
                 38)(label(Action))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 39)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 40)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 42)(content(Whitespace\" \"))))(Tile((id \
                 53)(label(OnMouseDown))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 54)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 57)(label(\"()\"))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 58)(content(Whitespace\" \"))))(Tile((id \
                 61)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 62)(content(Whitespace\" \"))))(Tile((id \
                 68)(label(Action))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 69)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 70)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 72)(content(Whitespace\" \"))))(Tile((id \
                 79)(label(OnInput))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 80)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 87)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 88)(content(Whitespace\" \"))))(Tile((id \
                 91)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 92)(content(Whitespace\" \"))))(Tile((id \
                 98)(label(Action))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 99)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 100)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 102)(content(Whitespace\" \"))))(Tile((id \
                 108)(label(Create))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 109)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 116)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 117)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 119)(content(Whitespace\" \"))))(Tile((id \
                 125)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 126)(content(Whitespace\" \"))))(Secondary((id \
                 127)(content(Whitespace\" \"))))(Secondary((id \
                 128)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 129)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 131)(content(Whitespace\" \"))))(Tile((id \
                 136)(label(Style))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 137)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 139)(label([ ]))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 148)(label(StyleAttr))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 153)(content(Whitespace\" \"))))(Secondary((id \
                 149)(content(Whitespace\" \"))))(Secondary((id \
                 150)(content(Whitespace\" \"))))(Secondary((id \
                 151)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 155)(content(Whitespace\" \"))))(Secondary((id \
                 156)(content(Whitespace\" \"))))(Secondary((id \
                 157)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 162)(label(type = in))(mold((out Exp)(in_(TPat \
                 Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 163)(content(Whitespace\" \"))))(Tile((id \
                 168)(label(Node))(mold((out TPat)(in_())(nibs(((shape \
                 Convex)(sort TPat))((shape Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 169)(content(Whitespace\" \")))))((Secondary((id \
                 171)(content(Whitespace\" \"))))(Secondary((id \
                 172)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 173)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 174)(content(Whitespace\" \"))))(Tile((id \
                 177)(label(Div))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 178)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 180)(label([ ]))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 184)(label(Attr))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 185)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 187)(content(Whitespace\" \"))))(Tile((id 188)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 192)(label(Node))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 193)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 194)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 196)(content(Whitespace\" \"))))(Tile((id \
                 200)(label(Text))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 201)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 208)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 209)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 210)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 212)(content(Whitespace\" \"))))(Tile((id \
                 218)(label(Button))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 219)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 221)(label([ ]))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 225)(label(Attr))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 226)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id 228)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 232)(label(Node))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 233)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 234)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 236)(content(Whitespace\" \"))))(Tile((id \
                 244)(label(Checkbox))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 245)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 247)(label([ ]))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 251)(label(Attr))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 252)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id 254)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 258)(label(Node))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 259)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 260)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 262)(content(Whitespace\" \"))))(Tile((id \
                 272)(label(ColorInput))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 273)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 275)(label([ ]))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 279)(label(Attr))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 280)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id 282)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 286)(label(Node))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 287)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 288)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 290)(content(Whitespace\" \"))))(Tile((id \
                 299)(label(DateInput))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 300)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 302)(label([ ]))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 306)(label(Attr))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 307)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id 309)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 313)(label(Node))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 314)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 315)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 317)(content(Whitespace\" \"))))(Tile((id \
                 328)(label(NumberInput))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 329)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 331)(label([ ]))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 335)(label(Attr))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 336)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id 338)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 342)(label(Node))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 343)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 344)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 346)(content(Whitespace\" \"))))(Tile((id \
                 351)(label(Radio))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 352)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 354)(label([ ]))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 358)(label(Attr))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 359)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id 361)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 365)(label(Node))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 366)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 367)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 369)(content(Whitespace\" \"))))(Tile((id \
                 374)(label(Range))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 375)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 377)(label([ ]))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 381)(label(Attr))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 382)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id 384)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 388)(label(Node))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 389)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 390)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 392)(content(Whitespace\" \"))))(Tile((id \
                 401)(label(TextInput))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 402)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 404)(label([ ]))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 408)(label(Attr))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 409)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id 411)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 415)(label(Node))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 416)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 417)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 10))(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 419)(content(Whitespace\" \"))))(Tile((id \
                 428)(label(TimeInput))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 429)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 431)(label([ ]))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 435)(label(Attr))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 436)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id 438)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 442)(label(Node))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 447)(content(Whitespace\" \"))))(Secondary((id \
                 443)(content(Whitespace\" \"))))(Secondary((id \
                 444)(content(Whitespace\" \"))))(Secondary((id \
                 445)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 449)(content(Whitespace\" \"))))(Secondary((id \
                 450)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 455)(label(type = in))(mold((out Exp)(in_(TPat \
                 Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 456)(content(Whitespace\" \"))))(Tile((id \
                 461)(label(View))(mold((out TPat)(in_())(nibs(((shape \
                 Convex)(sort TPat))((shape Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 462)(content(Whitespace\" \")))))((Secondary((id \
                 464)(content(Whitespace\" \"))))(Tile((id \
                 469)(label(Model))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 470)(content(Whitespace\" \"))))(Tile((id \
                 473)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 474)(content(Whitespace\" \"))))(Tile((id \
                 478)(label(Node))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 481)(content(Whitespace\" \")))))))))(Secondary((id \
                 483)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 488)(label(type = in))(mold((out Exp)(in_(TPat \
                 Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 489)(content(Whitespace\" \"))))(Tile((id \
                 496)(label(Render))(mold((out TPat)(in_())(nibs(((shape \
                 Convex)(sort TPat))((shape Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 497)(content(Whitespace\" \")))))((Secondary((id \
                 1044)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 500)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1045)(content(Whitespace\" \"))))(Tile((id \
                 506)(label(Render))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 507)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 514)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 515)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 517)(content(Whitespace\" \"))))(Tile((id \
                 522)(label(Model))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 523)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 525)(content(Whitespace\" \"))))(Tile((id \
                 529)(label(View))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 530)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 532)(content(Whitespace\" \"))))(Tile((id \
                 538)(label(Update))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 541)(content(Whitespace\" \")))))))))(Secondary((id \
                 543)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 551)(label(%EXPORT))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))(ancestors())))(caret \
                 Outer))";
              backup_text =
                "type StyleAttr =\n\
                 + AlignItems(String)\n\
                 + BackgroundColor(String)\n\
                 + Border(String)\n\
                 + BorderRadius(String)\n\
                 + BoxShadow(String)\n\
                 + Color(String)\n\
                 + Cursor(String)\n\
                 + Display(String)\n\
                 + FlexDirection(String)\n\
                 + FontFamily(String)\n\
                 + FontSize(String)\n\
                 + FontStyle(String)\n\
                 + Gap(String)\n\
                 + Height(String)\n\
                 + JustifyContent(String)\n\
                 + Margin(String)\n\
                 + Opacity(String)\n\
                 + Outline(String)\n\
                 + Overflow(String)\n\
                 + Padding(String)\n\
                 + Position(String)\n\
                 + Width(String)\n\
                 + S(String, String)  \n\
                 in\n\
                 type Attr =\n\
                 + OnClick(() -> Action)\n\
                 + OnMouseDown(() -> Action)\n\
                 + OnInput(String -> Action)\n\
                 + Create(String, String)  \n\
                 + Style([StyleAttr])   \n\
                 in  \n\
                 type Node = \n\
                 + Div([Attr], [Node])\n\
                 + Text(String)\n\
                 + Button([Attr],[Node])\n\
                 + Checkbox([Attr],[Node])\n\
                 + ColorInput([Attr],[Node])\n\
                 + DateInput([Attr],[Node])\n\
                 + NumberInput([Attr],[Node])\n\
                 + Radio([Attr],[Node])\n\
                 + Range([Attr],[Node])\n\
                 + TextInput([Attr],[Node])\n\
                 + TimeInput([Attr],[Node])   \n\
                 in \n\
                 type View = Model -> Node in\n\
                 type Render =\n\
                 + Render(String, Model, View, Update) in\n\
                 %EXPORT";
            } );
          ( 4585,
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Tile((id \
                 385)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 386)(content(Whitespace\" \"))))(Tile((id \
                 393)(label(update))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 394)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 396)(content(Whitespace\" \"))))(Tile((id \
                 397)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 402)(label(Model))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 403)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 405)(content(Whitespace\" \"))))(Tile((id \
                 411)(label(Action))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 412)(content(Whitespace\" \"))))(Tile((id \
                 415)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 416)(content(Whitespace\" \"))))(Tile((id \
                 421)(label(Model))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 422)(content(Whitespace\" \")))))((Secondary((id \
                 3855)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3860)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3861)(content(Whitespace\" \"))))(Tile((id \
                 4382)(label(add))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3871)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3872)(content(Whitespace\" \"))))(Tile((id \
                 4442)(label(Model))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3889)(content(Whitespace\" \"))))(Tile((id \
                 3891)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3892)(content(Whitespace\" \"))))(Tile((id 4467)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 4473)(label(Todo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 4475)(content(Whitespace\" \")))))((Secondary((id \
                 3900)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3905)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 3906)(content(Whitespace\" \"))))(Tile((id \
                 3907)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 3919)(label(description))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3920)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3921)(content(Whitespace\" \"))))(Tile((id \
                 3927)(label(todos))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 3928)(content(Whitespace\" \")))))))))(Secondary((id \
                 4532)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4542)(label(if then else))(mold((out Exp)(in_(Exp \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 4543)(content(Whitespace\" \"))))(Tile((id \
                 4554)(label(description))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4556)(content(Whitespace\" \"))))(Tile((id \
                 4566)(label($==))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 8))(sort Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4567)(content(Whitespace\" \"))))(Tile((id \
                 4568)(label(\"\\\"\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4571)(content(Whitespace\" \"))))(Secondary((id \
                 4569)(content(Whitespace\"\\226\\143\\142\")))))((Secondary((id \
                 4576)(content(Whitespace\" \"))))(Tile((id \
                 4582)(label(todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4583)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 4584)(content(Whitespace\" \"))))(Tile((id \
                 3931)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3943)(label(description))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3944)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3945)(content(Whitespace\" \"))))(Tile((id \
                 3951)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3952)(content(Whitespace\" \"))))(Tile((id \
                 3954)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 6))(sort Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3955)(content(Whitespace\" \"))))(Tile((id \
                 3961)(label(todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3964)(content(Whitespace\" \")))))))))(Secondary((id \
                 4260)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4265)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 4266)(content(Whitespace\" \"))))(Tile((id \
                 4399)(label(remove))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4279)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4280)(content(Whitespace\" \"))))(Tile((id \
                 4281)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 4285)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 4286)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4287)(content(Whitespace\" \"))))(Tile((id 4476)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 4481)(label(Todo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 4294)(content(Whitespace\" \"))))(Tile((id \
                 4296)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 4297)(content(Whitespace\" \"))))(Tile((id 4483)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 4488)(label(Todo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))((Secondary((id \
                 4305)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4310)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 4311)(content(Whitespace\" \"))))(Tile((id \
                 4312)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 4457)(label(index))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4317)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4318)(content(Whitespace\" \"))))(Tile((id \
                 4324)(label(todos))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 4325)(content(Whitespace\" \")))))))))(Secondary((id \
                 4327)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4340)(label(List.filteri))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4341)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4346)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 4347)(content(Whitespace\" \"))))(Tile((id \
                 4348)(label(i))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4350)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4351)(content(Whitespace\" \"))))(Tile((id \
                 4352)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4354)(content(Whitespace\" \")))))))))(Secondary((id \
                 4356)(content(Whitespace\" \"))))(Tile((id \
                 4357)(label(i))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4465)(content(Whitespace\" \"))))(Tile((id \
                 4361)(label(!=))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 5))(sort Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4362)(content(Whitespace\" \"))))(Tile((id \
                 4463)(label(index))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4367)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4368)(content(Whitespace\" \"))))(Tile((id \
                 4374)(label(todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 4377)(content(Whitespace\" \")))))))))(Secondary((id \
                 3965)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3970)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3971)(content(Whitespace\" \"))))(Tile((id \
                 3978)(label(toggle))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3979)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3980)(content(Whitespace\" \"))))(Tile((id \
                 3981)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 3985)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 3986)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3987)(content(Whitespace\" \"))))(Tile((id 4490)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 4495)(label(Todo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 3994)(content(Whitespace\" \"))))(Tile((id \
                 3996)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3997)(content(Whitespace\" \"))))(Tile((id 4497)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 4502)(label(Todo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))((Secondary((id \
                 4005)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4010)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 4011)(content(Whitespace\" \"))))(Tile((id \
                 4012)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 4018)(label(index))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4019)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4020)(content(Whitespace\" \"))))(Tile((id \
                 4026)(label(todos))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 4027)(content(Whitespace\" \")))))))))(Secondary((id \
                 4029)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4039)(label(List.mapi))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4040)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 4041)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4046)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 4047)(content(Whitespace\" \"))))(Tile((id \
                 4048)(label(i))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4050)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4051)(content(Whitespace\" \"))))(Tile((id \
                 4052)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 4064)(label(description))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4065)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4066)(content(Whitespace\" \"))))(Tile((id \
                 4071)(label(done))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 4466)(content(Whitespace\" \")))))))))(Secondary((id \
                 4436)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4074)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4086)(label(description))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4087)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4088)(content(Whitespace\" \"))))(Tile((id 4092)(label(if \
                 then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 12))(sort \
                 Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 4093)(content(Whitespace\" \"))))(Tile((id \
                 4094)(label(i))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4096)(content(Whitespace\" \"))))(Tile((id \
                 4098)(label(==))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 8))(sort Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4099)(content(Whitespace\" \"))))(Tile((id \
                 4105)(label(index))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4108)(content(Whitespace\" \")))))((Secondary((id \
                 4111)(content(Whitespace\" \"))))(Tile((id \
                 4112)(label(!))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4117)(label(done))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4120)(content(Whitespace\" \")))))))))(Secondary((id \
                 4123)(content(Whitespace\" \"))))(Tile((id \
                 4128)(label(done))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 4129)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4130)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4136)(label(todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 4139)(content(Whitespace\" \")))))))))(Secondary((id \
                 424)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 428)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 429)(content(Whitespace\" \"))))(Tile((id \
                 431)(label(\"(\"\")\"))(mold((out Pat)(in_(Pat))(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort Pat))))))(shards(0 \
                 1))(children(((Tile((id 432)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 437)(label(input))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 438)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 440)(content(Whitespace\" \"))))(Tile((id \
                 446)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 447)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 449)(content(Whitespace\" \"))))(Tile((id \
                 454)(label(todos))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 455)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 457)(content(Whitespace\" \"))))(Tile((id 4504)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 4509)(label(Todo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Tile((id \
                 463)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 465)(content(Whitespace\" \"))))(Tile((id \
                 471)(label(action))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 472)(content(Whitespace\" \")))))))))(Secondary((id \
                 475)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1629)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 1630)(content(Whitespace\" \
                 \"))))(Tile((id 1637)(label(action))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1638)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1639)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 1641)(content(Whitespace\" \
                 \"))))(Tile((id 1648)(label(AddTodo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 1649)(content(Whitespace\" \")))))))))(Secondary((id \
                 1652)(content(Whitespace\" \"))))(Tile((id \
                 1653)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2555)(label(\"\\\"\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1659)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1661)(content(Whitespace\" \"))))(Tile((id \
                 4389)(label(add))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1670)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1676)(label(input))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1677)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1679)(content(Whitespace\" \"))))(Tile((id \
                 1684)(label(todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 1685)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1686)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 1688)(content(Whitespace\" \
                 \"))))(Tile((id 1698)(label(ToggleTodo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1699)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 1703)(label(idx))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 1704)(content(Whitespace\" \")))))))))(Secondary((id \
                 1707)(content(Whitespace\" \"))))(Tile((id \
                 1708)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1713)(label(input))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1714)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1716)(content(Whitespace\" \"))))(Tile((id \
                 1722)(label(toggle))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1723)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1727)(label(idx))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1728)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1730)(content(Whitespace\" \"))))(Tile((id \
                 1735)(label(todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 1736)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1737)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 1739)(content(Whitespace\" \
                 \"))))(Tile((id 1749)(label(RemoveTodo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1750)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 1754)(label(idx))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 1755)(content(Whitespace\" \")))))))))(Secondary((id \
                 1758)(content(Whitespace\" \"))))(Tile((id \
                 1759)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1764)(label(input))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1765)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1767)(content(Whitespace\" \"))))(Tile((id \
                 4409)(label(remove))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1779)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1783)(label(idx))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1784)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1786)(content(Whitespace\" \"))))(Tile((id \
                 1791)(label(todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 1792)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1793)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 1795)(content(Whitespace\" \
                 \"))))(Tile((id 4528)(label(UpdateBuffer))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1807)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 4421)(label(description))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 1818)(content(Whitespace\" \")))))))))(Secondary((id \
                 1821)(content(Whitespace\" \"))))(Tile((id \
                 1822)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4433)(label(description))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1832)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1834)(content(Whitespace\" \"))))(Tile((id \
                 1839)(label(todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 1842)(content(Whitespace\" \"))))(Secondary((id \
                 1840)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 3788)(content(Whitespace\" \")))))))))(Secondary((id \
                 4529)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2383)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3786)(label(%EXPORT))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))(ancestors())))(caret \
                 Outer))";
              backup_text =
                "let update: (Model, Action) -> Model =\n\
                 let add: Model -> [Todo] =\n\
                 fun (description, todos) ->\n\
                 if description $== \"\" \n\
                 then todos\n\
                 else (description, false) :: todos in\n\
                 let remove: (Int, [Todo]) -> [Todo]=\n\
                 fun (index, todos) ->\n\
                 List.filteri(fun i, _ -> i != index, todos) in\n\
                 let toggle: (Int, [Todo]) -> [Todo]=\n\
                 fun (index, todos) ->\n\
                 List.mapi(\n\
                 fun i, (description, done) ->\n\
                 (description, if i == index then !done else done),\n\
                 todos) in\n\
                 fun ((input: String, todos: [Todo]), action) ->\n\
                 case action\n\
                 | AddTodo => (\"\", add(input, todos))\n\
                 | ToggleTodo(idx) => (input, toggle(idx, todos))\n\
                 | RemoveTodo(idx) => (input, remove(idx, todos))\n\
                 | UpdateBuffer(description) => (description, todos) \n\
                 end in\n\n\
                 %EXPORT";
            } );
          ( 2283,
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Tile((id \
                 4)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 16))(sort \
                 Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 5)(content(Whitespace\" \"))))(Tile((id \
                 2108)(label(todo_card))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 15)(content(Whitespace\" \"))))(Tile((id \
                 16)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 18)(content(Whitespace\" \"))))(Tile((id \
                 19)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 22)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 23)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 14))(sort Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 25)(content(Whitespace\" \"))))(Tile((id \
                 29)(label(Todo))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 30)(content(Whitespace\" \"))))(Tile((id \
                 33)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 34)(content(Whitespace\" \"))))(Tile((id \
                 38)(label(Node))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 39)(content(Whitespace\" \")))))((Secondary((id \
                 90)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 94)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 95)(content(Whitespace\" \"))))(Tile((id \
                 97)(label(\"(\"\")\"))(mold((out Pat)(in_(Pat))(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort Pat))))))(shards(0 \
                 1))(children(((Tile((id 100)(label(idx))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 101)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 103)(content(Whitespace\" \"))))(Tile((id \
                 104)(label(\"(\"\")\"))(mold((out Pat)(in_(Pat))(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort Pat))))))(shards(0 \
                 1))(children(((Tile((id 109)(label(descr))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 110)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1140)(content(Whitespace\" \"))))(Tile((id \
                 117)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 118)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 120)(content(Whitespace\" \"))))(Tile((id \
                 126)(label(status))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 127)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1141)(content(Whitespace\" \"))))(Tile((id \
                 132)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 133)(content(Whitespace\" \")))))))))(Secondary((id \
                 136)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 139)(label(Div))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 140)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 142)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 143)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 148)(label(Style))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 149)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 151)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 152)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 159)(label(Display))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 160)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 166)(label(\"\\\"flex\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 167)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 169)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 172)(label(Gap))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 173)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 178)(label(\"\\\"1em\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Tile((id \
                 179)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 181)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 188)(label(OnClick))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 189)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 194)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 195)(content(Whitespace\" \"))))(Tile((id \
                 198)(label(\"()\"))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 199)(content(Whitespace\" \")))))))))(Secondary((id \
                 202)(content(Whitespace\" \"))))(Tile((id \
                 212)(label(ToggleTodo))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 213)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 217)(label(idx))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Tile((id \
                 219)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2128)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 221)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 222)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 230)(label(Checkbox))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 231)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 234)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 241)(label(OnClick))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 242)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 247)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 248)(content(Whitespace\" \"))))(Tile((id \
                 251)(label(\"()\"))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 252)(content(Whitespace\" \")))))))))(Secondary((id \
                 255)(content(Whitespace\" \"))))(Tile((id \
                 265)(label(RemoveTodo))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 266)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 270)(label(idx))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Tile((id \
                 271)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2130)(content(Whitespace\" \"))))(Tile((id \
                 275)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 276)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 278)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 281)(label(Div))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 282)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 285)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 286)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1837)(content(Whitespace\" \"))))(Tile((id 288)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 292)(label(Text))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 293)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 299)(label(descr))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Tile((id \
                 300)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 302)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2123)(label(Text))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2125)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 305)(label(if then else))(mold((out Exp)(in_(Exp \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 306)(content(Whitespace\" \"))))(Tile((id \
                 313)(label(status))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 317)(content(Whitespace\" \")))))((Secondary((id \
                 321)(content(Whitespace\" \"))))(Tile((id \
                 337)(label(\"\\\"Completed\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 338)(content(Whitespace\" \")))))))))(Secondary((id \
                 347)(content(Whitespace\" \"))))(Tile((id \
                 361)(label(\"\\\"Pending\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 366)(content(Whitespace\" \")))))))))(Secondary((id \
                 2133)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1839)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1844)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1845)(content(Whitespace\" \"))))(Tile((id \
                 2151)(label(todos_deck))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1851)(content(Whitespace\" \")))))((Secondary((id \
                 1852)(content(Whitespace\" \"))))(Tile((id 1857)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 1858)(content(Whitespace\" \
                 \"))))(Tile((id 1864)(label(todos))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 1865)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1866)(content(Whitespace\" \"))))(Tile((id 1867)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 1872)(label(Todo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 1873)(content(Whitespace\" \")))))))))(Secondary((id \
                 1875)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1879)(label(if then else))(mold((out Exp)(in_(Exp \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1880)(content(Whitespace\" \"))))(Tile((id \
                 1881)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2227)(label(List.is_empty))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1893)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1909)(label(todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 1921)(content(Whitespace\" \"))))(Secondary((id \
                 1915)(content(Whitespace\" \"))))(Secondary((id \
                 1916)(content(Whitespace\" \"))))(Secondary((id \
                 1917)(content(Whitespace\" \"))))(Secondary((id \
                 1918)(content(Whitespace\" \"))))(Secondary((id \
                 1919)(content(Whitespace\"\\226\\143\\142\")))))((Secondary((id \
                 1924)(content(Whitespace\" \"))))(Tile((id \
                 1929)(label(Text))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1930)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1948)(label(\"\\\"You're caught up\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 1955)(content(Whitespace\" \"))))(Secondary((id \
                 1949)(content(Whitespace\" \"))))(Secondary((id \
                 1950)(content(Whitespace\" \"))))(Secondary((id \
                 1951)(content(Whitespace\" \"))))(Secondary((id \
                 1952)(content(Whitespace\" \"))))(Secondary((id \
                 1953)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 1958)(content(Whitespace\" \"))))(Tile((id \
                 1962)(label(Div))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1963)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 1964)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1965)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 1972)(label(Create))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1973)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1980)(label(\"\\\"class\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1981)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2134)(content(Whitespace\" \"))))(Tile((id \
                 1988)(label(\"\\\"todos\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 1989)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1990)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1991)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 1996)(label(Text))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1997)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2005)(label(\"\\\"todos:\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 2137)(content(Whitespace\" \"))))(Tile((id \
                 2007)(label(@))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 7))(sort Exp))((shape(Concave 7))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2136)(content(Whitespace\" \"))))(Tile((id \
                 2067)(label(List.mapi))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2068)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2117)(label(todo_card))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2078)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2080)(content(Whitespace\" \"))))(Tile((id \
                 2085)(label(todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 2028)(content(Whitespace\" \")))))))))(Secondary((id \
                 2207)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1160)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1166)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1167)(content(Whitespace\" \"))))(Tile((id \
                 1356)(label(add_button))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1764)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1767)(content(Whitespace\" \"))))(Tile((id \
                 1772)(label(Node))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1774)(content(Whitespace\" \")))))((Secondary((id \
                 1174)(content(Whitespace\" \"))))(Tile((id \
                 1180)(label(Div))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1181)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1182)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 1183)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1189)(label(Style))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1190)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1191)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 1192)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1200)(label(Display))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1201)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1207)(label(\"\\\"flex\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 1208)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1209)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1224)(label(JustifyContent))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1225)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1233)(label(\"\\\"center\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 1234)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1235)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1251)(label(BackgroundColor))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1252)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1258)(label(\"\\\"#986\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 1259)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1260)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1273)(label(BorderRadius))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1274)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1281)(label(\"\\\"0.3em\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 1282)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1283)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1290)(label(Cursor))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1291)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1300)(label(\"\\\"pointer\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Tile((id \
                 1302)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1303)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1311)(label(OnClick))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1312)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1317)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 1318)(content(Whitespace\" \"))))(Tile((id \
                 1320)(label(\"()\"))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 1322)(content(Whitespace\" \")))))))))(Secondary((id \
                 1324)(content(Whitespace\" \"))))(Tile((id \
                 1332)(label(AddTodo))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 1334)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2185)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1336)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 1341)(label(Text))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1342)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2282)(label(\"\\\"Add Todo\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 1480)(content(Whitespace\" \")))))))))(Secondary((id \
                 2190)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1381)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1389)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 1390)(content(Whitespace\" \"))))(Tile((id \
                 2196)(label(buffer))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 1753)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1756)(content(Whitespace\" \"))))(Tile((id \
                 1761)(label(Node))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1763)(content(Whitespace\" \")))))((Secondary((id \
                 2232)(content(Whitespace\" \"))))(Tile((id \
                 1412)(label(Div))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1413)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 1414)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1416)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1418)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1419)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1420)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 1430)(label(TextInput))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1431)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1433)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 1442)(label(OnInput))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1443)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2260)(label(UpdateBuffer))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 1470)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1481)(content(Whitespace\" \"))))(Tile((id \
                 1473)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 1479)(content(Whitespace\" \")))))))))(Secondary((id \
                 1491)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 451)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 455)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 456)(content(Whitespace\" \"))))(Tile((id \
                 461)(label(view))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 462)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 464)(content(Whitespace\" \"))))(Tile((id \
                 469)(label(Model))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 470)(content(Whitespace\" \"))))(Tile((id \
                 473)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                 6))(sort Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 474)(content(Whitespace\" \"))))(Tile((id \
                 478)(label(Node))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 479)(content(Whitespace\" \")))))((Secondary((id \
                 481)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 485)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                 Convex)(sort Exp))((shape(Concave 13))(sort \
                 Exp))))))(shards(0 1))(children(((Secondary((id \
                 486)(content(Whitespace\" \"))))(Tile((id \
                 492)(label(input))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 493)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1752)(content(Whitespace\" \"))))(Tile((id \
                 500)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 501)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 14))(sort Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 503)(content(Whitespace\" \"))))(Tile((id \
                 508)(label(todos))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 509)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1751)(content(Whitespace\" \"))))(Tile((id 511)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id 515)(label(Todo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 1775)(content(Whitespace\" \")))))))))(Secondary((id \
                 518)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 521)(label(Div))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 522)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 524)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 525)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 530)(label(Style))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 531)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 534)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 535)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 550)(label(BackgroundColor))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 551)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 557)(label(\"\\\"#543\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 558)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 560)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 572)(label(BorderRadius))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 573)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 580)(label(\"\\\"0.3em\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 581)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 583)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 588)(label(Color))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 589)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 596)(label(\"\\\"white\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 597)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 599)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 606)(label(Display))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 607)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 613)(label(\"\\\"flex\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 614)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 616)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 629)(label(FlexDirection))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 630)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 638)(label(\"\\\"column\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 639)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 641)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 644)(label(Gap))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 645)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 652)(label(\"\\\"0.5em\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 653)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 655)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 662)(label(Padding))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 663)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 670)(label(\"\\\"0.3em\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))))))))))))(Tile((id \
                 673)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 675)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 676)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 677)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 681)(label(Text))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 682)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2269)(label(\"\\\"Hazel Todos\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 696)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 698)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2204)(label(buffer))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 766)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 768)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1366)(label(add_button))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 939)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 941)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2163)(label(todos_deck))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1721)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1731)(label(todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 2206)(content(Whitespace\" \")))))))))(Secondary((id \
                 1084)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2205)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1090)(label(Render))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1091)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2247)(label(\"\\\"todo_app\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1099)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1101)(content(Whitespace\" \"))))(Tile((id \
                 1147)(label(Model.init))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1106)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1108)(content(Whitespace\" \"))))(Tile((id \
                 1112)(label(view))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1113)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1115)(content(Whitespace\" \"))))(Tile((id \
                 1121)(label(update))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))(ancestors())))(caret \
                 Outer))";
              backup_text =
                "let todo_card : (Int, Todo) -> Node =\n\
                 fun (idx, (descr: String, status: Bool)) ->\n\
                 Div([\n\
                 Style([\n\
                 Display(\"flex\"),\n\
                 Gap(\"1em\")]),\n\
                 OnClick(fun () -> ToggleTodo(idx))],\n\
                 [\n\
                 Checkbox([OnClick(fun () -> RemoveTodo(idx))], []),\n\
                 Div([], [Text(descr)]),\n\
                 Text(if status then \"Completed\" else \"Pending\")]) in\n\n\
                 let todos_deck = fun todos: [Todo] ->\n\
                 if (List.is_empty(todos))     \n\
                 then Text(\"You're caught up\")     \n\
                 else Div(\n\
                 [Create(\"class\", \"todos\")],\n\
                 [Text(\"todos:\")] @ List.mapi(todo_card, todos)) in\n\n\
                 let add_button: Node = Div([\n\
                 Style([\n\
                 Display(\"flex\"),\n\
                 JustifyContent(\"center\"),\n\
                 BackgroundColor(\"#986\"),\n\
                 BorderRadius(\"0.3em\"),\n\
                 Cursor(\"pointer\")]),\n\
                 OnClick(fun () -> AddTodo)],\n\
                 [Text(\"Add Todo\")]) in\n\n\
                 let buffer: Node = Div(\n\
                 [],\n\
                 [TextInput([OnInput(UpdateBuffer)], [])]) in\n\n\
                 let view: Model -> Node =\n\
                 fun input: String, todos: [Todo] ->\n\
                 Div(\n\
                 [Style([\n\
                 BackgroundColor(\"#543\"),\n\
                 BorderRadius(\"0.3em\"),\n\
                 Color(\"white\"),\n\
                 Display(\"flex\"),\n\
                 FlexDirection(\"column\"),\n\
                 Gap(\"0.5em\"),\n\
                 Padding(\"0.3em\")])],\n\
                 [\n\
                 Text(\"Hazel Todos\"),\n\
                 buffer,\n\
                 add_button,\n\
                 todos_deck(todos)]) in\n\n\
                 Render(\"todo_app\", Model.init, view, update)";
            } );
          ( 4536,
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Secondary((id \
                 710)(content(Comment\"# update tests #\"))))(Secondary((id \
                 3145)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 3999)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4004)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 4005)(content(Whitespace\" \"))))(Tile((id \
                 4008)(label(eq))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4009)(content(Whitespace\" \")))))((Secondary((id \
                 4010)(content(Whitespace\" \"))))(Tile((id \
                 4019)(label(Model.eq))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4022)(content(Whitespace\" \")))))))))(Secondary((id \
                 3146)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3154)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3155)(content(Whitespace\" \"))))(Tile((id \
                 3165)(label(num_todos))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3166)(content(Whitespace\" \")))))((Secondary((id \
                 3167)(content(Whitespace\" \"))))(Tile((id 3174)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3197)(content(Whitespace\" \
                 \"))))(Tile((id 3198)(label(m))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3203)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                 11))(sort Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 3211)(label(Model))(mold((out Typ)(in_())(nibs(((shape \
                 Convex)(sort Typ))((shape Convex)(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3213)(content(Whitespace\" \")))))))))(Secondary((id \
                 3177)(content(Whitespace\" \"))))(Tile((id \
                 3189)(label(List.length))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3190)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3194)(label(snd))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3200)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3202)(label(m))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 3214)(content(Whitespace\" \")))))))))(Secondary((id \
                 3367)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 713)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 719)(label(test end))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 842)(content(Whitespace\" \
                 \"))))(Secondary((id 4196)(content(Whitespace\" \
                 \"))))(Secondary((id 875)(content(Comment\"# Add adds \
                 #\"))))(Secondary((id \
                 833)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 3365)(content(Whitespace\" \"))))(Secondary((id \
                 3366)(content(Whitespace\" \"))))(Tile((id \
                 3223)(label(num_todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 730)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 738)(label(update))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 739)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4285)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4339)(label(\"\\\"Breath\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4291)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4293)(content(Whitespace\" \"))))(Tile((id \
                 4295)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 751)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 752)(content(Whitespace\" \"))))(Tile((id \
                 760)(label(AddTodo))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 3364)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1776)(label(>))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 5))(sort Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1778)(content(Whitespace\" \"))))(Tile((id \
                 3233)(label(num_todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1795)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4297)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4347)(label(\"\\\"Breath\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4303)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4305)(content(Whitespace\" \"))))(Tile((id \
                 4307)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 1826)(content(Whitespace\" \")))))))))(Tile((id \
                 788)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 10))(sort Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4024)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 695)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 89)(label(test end))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 840)(content(Whitespace\" \
                 \"))))(Secondary((id 4197)(content(Whitespace\" \
                 \"))))(Secondary((id 4328)(content(Comment\"# Add uses name, \
                 initial status set #\"))))(Secondary((id \
                 903)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3397)(label(eq))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 100)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 102)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 108)(label(update))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 109)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4259)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4355)(label(\"\\\"Breath\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4272)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4284)(content(Whitespace\" \"))))(Tile((id \
                 4275)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 121)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 123)(content(Whitespace\" \"))))(Tile((id \
                 130)(label(AddTodo))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 131)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 133)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 134)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 135)(label(\"\\\"\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 136)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 138)(content(Whitespace\" \"))))(Tile((id 139)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 140)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4363)(label(\"\\\"Breath\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 145)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 147)(content(Whitespace\" \"))))(Tile((id \
                 152)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))))))))))))(Secondary((id \
                 156)(content(Whitespace\" \")))))))))(Tile((id \
                 158)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 10))(sort Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4025)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 160)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 165)(label(test end))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 166)(content(Whitespace\" \
                 \"))))(Secondary((id 4198)(content(Whitespace\" \
                 \"))))(Secondary((id 3986)(content(Comment\"# Add nonempty \
                 (too impl spec? test add + remove eqs)#\"))))(Secondary((id \
                 904)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3400)(label(eq))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 176)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 178)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 184)(label(update))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 185)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 187)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 4374)(label(\"\\\"Chop \
                 wood\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 192)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 194)(content(Whitespace\" \"))))(Tile((id 2237)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 2239)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4386)(label(\"\\\"Carry water\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2242)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2244)(content(Whitespace\" \"))))(Tile((id \
                 2249)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Tile((id \
                 209)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 211)(content(Whitespace\" \"))))(Tile((id \
                 218)(label(AddTodo))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 219)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 221)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 222)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 2234)(label(\"\\\"\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 224)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 226)(content(Whitespace\" \"))))(Tile((id 2360)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 2362)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4407)(label(\"\\\"Chop wood\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2365)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2367)(content(Whitespace\" \"))))(Tile((id \
                 2372)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 2373)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2375)(content(Whitespace\" \"))))(Tile((id \
                 2376)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4420)(label(\"\\\"Carry water\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2379)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2381)(content(Whitespace\" \"))))(Tile((id \
                 2386)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))))))))))))(Secondary((id \
                 2903)(content(Whitespace\" \")))))))))(Tile((id \
                 261)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 10))(sort Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4026)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 3420)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3426)(label(test end))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 3427)(content(Whitespace\" \
                 \"))))(Secondary((id 3939)(content(Whitespace\" \
                 \"))))(Secondary((id 3984)(content(Comment\"# add then remove \
                 doesn't change todos #\"))))(Secondary((id \
                 3832)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3838)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3839)(content(Whitespace\" \"))))(Tile((id \
                 3858)(label(todos))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3851)(content(Whitespace\" \")))))((Secondary((id \
                 3887)(content(Whitespace\" \"))))(Tile((id 3873)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 3875)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4490)(label(\"\\\"Breath\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3878)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3880)(content(Whitespace\" \"))))(Tile((id \
                 3885)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 3888)(content(Whitespace\" \")))))))))(Secondary((id \
                 3481)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3484)(label(eq))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3485)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 3486)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3700)(label(update))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3701)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3699)(label(update))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3494)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3901)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4535)(label(\"\\\"Remove this\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3906)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3908)(content(Whitespace\" \"))))(Tile((id \
                 3913)(label(todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 3514)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3515)(content(Whitespace\" \"))))(Tile((id \
                 3523)(label(AddTodo))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 3702)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3704)(content(Whitespace\" \"))))(Tile((id \
                 3718)(label(RemoveTodo))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3780)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3782)(label(0))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 3524)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3525)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3927)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3929)(label(\"\\\"\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3930)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3932)(content(Whitespace\" \"))))(Tile((id \
                 3937)(label(todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 3560)(content(Whitespace\" \")))))))))(Tile((id \
                 3562)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4027)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1261)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1267)(label(test end))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 1268)(content(Whitespace\" \
                 \"))))(Secondary((id 4199)(content(Whitespace\" \
                 \"))))(Secondary((id 1532)(content(Comment\"# Toggle \
                 preserves length #\"))))(Secondary((id \
                 2904)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2909)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 2910)(content(Whitespace\" \"))))(Tile((id \
                 2916)(label(model))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2917)(content(Whitespace\" \")))))((Secondary((id \
                 2918)(content(Whitespace\" \"))))(Tile((id \
                 2919)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2920)(label(\"\\\"\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2922)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2923)(content(Whitespace\" \"))))(Tile((id 2924)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 2925)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2928)(label(\"\\\"1\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2929)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2930)(content(Whitespace\" \"))))(Tile((id \
                 2936)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 2937)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2938)(content(Whitespace\" \"))))(Tile((id \
                 2939)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2942)(label(\"\\\"2\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2943)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2944)(content(Whitespace\" \"))))(Tile((id \
                 2950)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 2953)(content(Whitespace\" \")))))))))(Secondary((id \
                 1278)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 3416)(content(Whitespace\" \"))))(Secondary((id \
                 3417)(content(Whitespace\" \"))))(Secondary((id \
                 3418)(content(Whitespace\" \"))))(Tile((id \
                 3243)(label(num_todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1297)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1438)(label(update))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1298)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3020)(label(model))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1512)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1531)(content(Whitespace\" \"))))(Tile((id \
                 1526)(label(ToggleTodo))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1527)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1529)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 1346)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1428)(label(==))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 8))(sort Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1421)(content(Whitespace\" \"))))(Tile((id \
                 3253)(label(num_todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2972)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3027)(label(model))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3014)(content(Whitespace\" \")))))))))(Tile((id \
                 1384)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4028)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1565)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1571)(label(test end))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 1572)(content(Whitespace\" \
                 \"))))(Secondary((id 4200)(content(Whitespace\" \
                 \"))))(Secondary((id 1601)(content(Comment\"# Toggle toggles \
                 right index #\"))))(Secondary((id \
                 1602)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3403)(label(eq))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1612)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 1613)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1620)(label(update))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1621)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1622)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1623)(label(\"\\\"\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1625)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1626)(content(Whitespace\" \"))))(Tile((id 1627)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 1628)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4436)(label(\"\\\"Chop\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1632)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1633)(content(Whitespace\" \"))))(Tile((id \
                 1639)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 1640)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1641)(content(Whitespace\" \"))))(Tile((id \
                 1642)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4464)(label(\"\\\"Carry\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1646)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1647)(content(Whitespace\" \"))))(Tile((id \
                 1652)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Tile((id \
                 1653)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1654)(content(Whitespace\" \"))))(Tile((id \
                 1665)(label(ToggleTodo))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1666)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1667)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 1669)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1670)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1671)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1672)(label(\"\\\"\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1674)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1675)(content(Whitespace\" \"))))(Tile((id 1676)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 1677)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4441)(label(\"\\\"Chop\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1681)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1682)(content(Whitespace\" \"))))(Tile((id \
                 1688)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 1689)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1690)(content(Whitespace\" \"))))(Tile((id \
                 1691)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4470)(label(\"\\\"Carry\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1695)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1696)(content(Whitespace\" \"))))(Tile((id \
                 1702)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))))))))))))(Secondary((id \
                 1706)(content(Whitespace\" \")))))))))(Tile((id \
                 1708)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4029)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2662)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2668)(label(test end))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 2669)(content(Whitespace\" \
                 \"))))(Secondary((id 4201)(content(Whitespace\" \
                 \"))))(Secondary((id 2692)(content(Comment\"# Toggle out of \
                 bounds #\"))))(Secondary((id \
                 2827)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2834)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 2835)(content(Whitespace\" \"))))(Tile((id \
                 2889)(label(model))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 2840)(content(Whitespace\" \")))))((Secondary((id \
                 2841)(content(Whitespace\" \"))))(Tile((id \
                 2879)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2880)(label(\"\\\"\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2882)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2883)(content(Whitespace\" \"))))(Tile((id 2842)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 2843)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4447)(label(\"\\\"Chop\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2847)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2848)(content(Whitespace\" \"))))(Tile((id \
                 2854)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 2855)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2856)(content(Whitespace\" \"))))(Tile((id \
                 2857)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4477)(label(\"\\\"Carry\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2861)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2862)(content(Whitespace\" \"))))(Tile((id \
                 2868)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 2870)(content(Whitespace\" \")))))))))(Secondary((id \
                 2693)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3406)(label(eq))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2703)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 3997)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2711)(label(update))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2712)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2894)(label(model))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2745)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2746)(content(Whitespace\" \"))))(Tile((id \
                 2757)(label(ToggleTodo))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2758)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2759)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 2761)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3998)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2900)(label(model))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 2797)(content(Whitespace\" \")))))))))(Tile((id \
                 2799)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4030)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 972)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 978)(label(test end))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 979)(content(Whitespace\" \
                 \"))))(Secondary((id 4202)(content(Whitespace\" \
                 \"))))(Secondary((id 1539)(content(Comment\"# Remove removes \
                 #\"))))(Secondary((id \
                 3098)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3106)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3107)(content(Whitespace\" \"))))(Tile((id \
                 3113)(label(model))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3114)(content(Whitespace\" \")))))((Secondary((id \
                 3115)(content(Whitespace\" \"))))(Tile((id \
                 3116)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3117)(label(\"\\\"\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3119)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3120)(content(Whitespace\" \"))))(Tile((id 3121)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 3122)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3125)(label(\"\\\"1\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3126)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3127)(content(Whitespace\" \"))))(Tile((id \
                 3133)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 3135)(content(Whitespace\" \")))))))))(Secondary((id \
                 989)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 3987)(content(Whitespace\" \"))))(Secondary((id \
                 3988)(content(Whitespace\" \"))))(Tile((id \
                 3265)(label(num_todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1207)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1157)(label(update))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1158)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3097)(label(model))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1190)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1192)(content(Whitespace\" \"))))(Tile((id \
                 1202)(label(RemoveTodo))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1203)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1205)(label(0))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 1227)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1228)(label(<))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 5))(sort Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1230)(content(Whitespace\" \"))))(Tile((id \
                 3275)(label(num_todos))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2601)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3144)(label(model))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 2659)(content(Whitespace\" \")))))))))(Tile((id \
                 1081)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4031)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 377)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 382)(label(test end))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 383)(content(Whitespace\" \
                 \"))))(Secondary((id 4203)(content(Whitespace\" \
                 \"))))(Secondary((id 1537)(content(Comment\"# Remove removes \
                 right index #\"))))(Secondary((id \
                 916)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3409)(label(eq))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 393)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 395)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 401)(label(update))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 402)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 404)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 405)(label(\"\\\"\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 406)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 408)(content(Whitespace\" \"))))(Tile((id 2500)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 2502)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2504)(label(\"\\\"1\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2505)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2507)(content(Whitespace\" \"))))(Tile((id \
                 2512)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 2513)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2515)(content(Whitespace\" \"))))(Tile((id \
                 2516)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2518)(label(\"\\\"2\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2519)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2521)(content(Whitespace\" \"))))(Tile((id \
                 2526)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Tile((id \
                 434)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 436)(content(Whitespace\" \"))))(Tile((id \
                 446)(label(RemoveTodo))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 447)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 449)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 450)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 452)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 453)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 454)(label(\"\\\"\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 455)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 457)(content(Whitespace\" \"))))(Tile((id 458)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 459)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 461)(label(\"\\\"1\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 462)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 464)(content(Whitespace\" \"))))(Tile((id \
                 469)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))))))))))))(Secondary((id \
                 473)(content(Whitespace\" \")))))))))(Tile((id \
                 475)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 10))(sort Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4032)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 477)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 482)(label(test end))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 927)(content(Whitespace\" \
                 \"))))(Secondary((id 4204)(content(Whitespace\" \
                 \"))))(Secondary((id 2826)(content(Comment\"# Remove out of \
                 bounds #\"))))(Secondary((id \
                 926)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3032)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 3033)(content(Whitespace\" \"))))(Tile((id \
                 3039)(label(model))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 3040)(content(Whitespace\" \")))))((Secondary((id \
                 3041)(content(Whitespace\" \"))))(Tile((id \
                 3042)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3043)(label(\"\\\"\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3045)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3046)(content(Whitespace\" \"))))(Tile((id 3047)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 3048)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3051)(label(\"\\\"1\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3052)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3053)(content(Whitespace\" \"))))(Tile((id \
                 3059)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 3076)(content(Whitespace\" \")))))))))(Secondary((id \
                 3078)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3412)(label(eq))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 493)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 3995)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 501)(label(update))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 502)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3084)(label(model))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 534)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 536)(content(Whitespace\" \"))))(Tile((id \
                 546)(label(RemoveTodo))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 547)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 549)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 550)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3996)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3090)(label(model))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 586)(content(Whitespace\" \")))))))))(Tile((id \
                 588)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 10))(sort Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4033)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 590)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 595)(label(test end))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 596)(content(Whitespace\" \
                 \"))))(Secondary((id 4205)(content(Whitespace\" \
                 \"))))(Secondary((id 964)(content(Comment\"# Update Input \
                 #\"))))(Secondary((id \
                 949)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3415)(label(eq))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 606)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 608)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 614)(label(update))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 615)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 617)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 618)(label(\"\\\"\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 619)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 621)(content(Whitespace\" \"))))(Tile((id \
                 2356)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 636)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 638)(content(Whitespace\" \"))))(Tile((id \
                 4166)(label(UpdateBuffer))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 650)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4332)(label(\"\\\"Breath\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 659)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 661)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 662)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 669)(label(\"\\\"Breath\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 670)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 672)(content(Whitespace\" \"))))(Tile((id \
                 2357)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 690)(content(Whitespace\" \")))))))))(Tile((id \
                 4167)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4034)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 4035)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4041)(label(test end))(mold((out Exp)(in_(Exp))(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id 4042)(content(Whitespace\" \
                 \"))))(Secondary((id 4206)(content(Whitespace\" \
                 \"))))(Secondary((id 4219)(content(Comment\"# Don't add blank \
                 description #\"))))(Secondary((id \
                 4067)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4071)(label(let = in))(mold((out Exp)(in_(Pat \
                 Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                 4072)(content(Whitespace\" \"))))(Tile((id \
                 4078)(label(model))(mold((out Pat)(in_())(nibs(((shape \
                 Convex)(sort Pat))((shape Convex)(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4079)(content(Whitespace\" \")))))((Secondary((id \
                 4081)(content(Whitespace\" \"))))(Tile((id \
                 4082)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4083)(label(\"\\\"\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4084)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4086)(content(Whitespace\" \"))))(Tile((id 4087)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id 4088)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4090)(label(\"\\\"1\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4091)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4093)(content(Whitespace\" \"))))(Tile((id \
                 4098)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 4101)(content(Whitespace\" \")))))))))(Secondary((id \
                 4103)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4105)(label(eq))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4106)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 4108)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4114)(label(update))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4115)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4121)(label(model))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4122)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4124)(content(Whitespace\" \"))))(Tile((id \
                 4193)(label(AddTodo))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Tile((id \
                 4138)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                 14))(sort Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4140)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4145)(label(model))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 4148)(content(Whitespace\" \
                 \"))))))))))))(ancestors())))(caret Outer))";
              backup_text =
                "# update tests #\n\n\
                 let eq = Model.eq in\n\
                 let num_todos = fun m:Model -> List.length(snd(m)) in\n\n\
                 test  # Add adds #\n\
                \  num_todos(update((\"Breath\", []), AddTodo))\n\
                 > num_todos((\"Breath\", [])) end;\n\n\
                 test  # Add uses name, initial status set #\n\
                 eq(\n\
                 update((\"Breath\", []), AddTodo),\n\
                 (\"\", [(\"Breath\", false)])) end;\n\n\
                 test  # Add nonempty (too impl spec? test add + remove eqs)#\n\
                 eq(\n\
                 update((\"Chop wood\", [(\"Carry water\", false)]), AddTodo),\n\
                 (\"\", [(\"Chop wood\", false), (\"Carry water\", false)])) \
                 end;\n\n\
                 test  # add then remove doesn't change todos #\n\
                 let todos = [(\"Breath\", false)] in\n\
                 eq(\n\
                 update(update((\"Remove this\", todos), AddTodo), \
                 RemoveTodo(0)),\n\
                 (\"\", todos)) end;\n\n\
                 test  # Toggle preserves length #\n\
                 let model = (\"\", [(\"1\", false), (\"2\", false)]) in\n\
                \   num_todos(update(model, ToggleTodo(1)))\n\
                 == num_todos(model) end;\n\n\
                 test  # Toggle toggles right index #\n\
                 eq(\n\
                 update((\"\", [(\"Chop\", false), (\"Carry\", true)]), \
                 ToggleTodo(1)),\n\
                 (\"\", [(\"Chop\", false), (\"Carry\", false)])) end;\n\n\
                 test  # Toggle out of bounds #\n\
                 let model = (\"\", [(\"Chop\", false), (\"Carry\", false)]) in\n\
                 eq(\n\
                 update(model, ToggleTodo(2)),\n\
                 model) end;\n\n\
                 test  # Remove removes #\n\
                 let model = (\"\", [(\"1\", false)]) in\n\
                \  num_todos(update(model, RemoveTodo(0)))\n\
                 < num_todos(model) end;\n\n\
                 test  # Remove removes right index #\n\
                 eq(\n\
                 update((\"\", [(\"1\", false), (\"2\", false)]), \
                 RemoveTodo(1)),\n\
                 (\"\", [(\"1\", false)])) end;\n\n\
                 test  # Remove out of bounds #\n\
                 let model = (\"\", [(\"1\", false)]) in\n\
                 eq(\n\
                 update(model, RemoveTodo(2)),\n\
                 model) end;\n\n\
                 test  # Update Input #\n\
                 eq(\n\
                 update((\"\", []), UpdateBuffer(\"Breath\")),\n\
                 (\"Breath\", [])) end;\n\n\
                 test  # Don't add blank description #\n\
                 let model = (\"\", [(\"1\", false)]) in\n\
                 eq(\n\
                 update(model, AddTodo),\n\
                 model) end";
            } );
          ( 33,
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Grout((id \
                 32)(shape Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( 33,
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Grout((id \
                 32)(shape Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( 33,
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Grout((id \
                 32)(shape Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
          ( 33,
            {
              zipper =
                "((selection((focus Left)(content())(mode \
                 Normal)))(backpack())(relatives((siblings(()((Grout((id \
                 32)(shape Convex))))))(ancestors())))(caret Outer))";
              backup_text = " ";
            } );
        ] );
    examples =
      ( "Introduction",
        [
          ( "Introduction",
            ( 16422,
              {
                zipper =
                  "((selection((focus Left)(content())(mode \
                   Normal)))(backpack())(relatives((siblings(((Secondary((id \
                   15753)(content(Comment\"# Welcome to Hazel! \
                   #\"))))(Secondary((id \
                   15754)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   15755)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   15820)(content(Comment\"# This is a program cell, which \
                   consists of a structured editor  #\"))))(Secondary((id \
                   15821)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   15886)(content(Comment\"# at the top and its evaluated \
                   result at the bottom. Right now,  #\"))))(Secondary((id \
                   15887)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   15952)(content(Comment\"# that result has a question mark, \
                   as the program is incomplete! #\"))))(Secondary((id \
                   15953)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   15954)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   15958)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   15959)(content(Whitespace\" \"))))(Tile((id \
                   15973)(label(your_function))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   15974)(content(Whitespace\" \")))))((Secondary((id \
                   15976)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16028)(content(Comment\"# Fill the hole below to see how \
                   the result changes #\"))))(Secondary((id \
                   16029)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   16033)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   16034)(content(Whitespace\" \"))))(Tile((id \
                   16044)(label(parameter))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   16045)(content(Whitespace\" \")))))))))(Grout((id \
                   16055)(shape Convex)))(Secondary((id \
                   16049)(content(Whitespace\" \"))))(Secondary((id \
                   16050)(content(Whitespace\" \"))))(Secondary((id \
                   16051)(content(Whitespace\" \"))))(Secondary((id \
                   16052)(content(Whitespace\" \"))))(Secondary((id \
                   16053)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   16057)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16058)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16123)(content(Comment\"# Here in Examples Mode, you can \
                   use the upper left dropdown to  #\"))))(Secondary((id \
                   16124)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16189)(content(Comment\"# browse Hazel language and editor \
                   features references.          #\"))))(Secondary((id \
                   16190)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16191)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16256)(content(Comment\"# Select Scratch Mode from the \
                   upper left dialog to access blank #\"))))(Secondary((id \
                   16257)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16322)(content(Comment\"# cells where you can write code; \
                   use the arrows to navigate.    #\"))))(Secondary((id \
                   16323)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16324)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16389)(content(Comment\"# Select Exercise for a small \
                   functional programming tutorial.   #\"))))(Secondary((id \
                   16390)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   16391)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   16404)(label(your_function))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   16405)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   16415)(label(\"\\\"argument\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   16416)(content(Whitespace\" \"))))(Tile((id \
                   16417)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   16419)(content(Whitespace\" \"))))(Tile((id \
                   16420)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   16421)(content(Whitespace\"\\226\\143\\142\")))))()))(ancestors())))(caret \
                   Outer))";
                backup_text =
                  "# Welcome to Hazel! #\n\n\
                   # This is a program cell, which consists of a structured \
                   editor  #\n\
                   # at the top and its evaluated result at the bottom. Right \
                   now,  #\n\
                   # that result has a question mark, as the program is \
                   incomplete! #\n\n\
                   let your_function =\n\
                   # Fill the hole below to see how the result changes #\n\
                   fun parameter ->     \n\
                   in\n\n\
                   # Here in Examples Mode, you can use the upper left \
                   dropdown to  #\n\
                   # browse Hazel language and editor features \
                   references.          #\n\n\
                   # Select Scratch Mode from the upper left dialog to access \
                   blank #\n\
                   # cells where you can write code; use the arrows to \
                   navigate.    #\n\n\
                   # Select Exercise for a small functional programming \
                   tutorial.   #\n\n\
                   your_function(\"argument\") + 1\n";
              } ) );
          ( "Basic Reference",
            ( 15306,
              {
                zipper =
                  "((selection((focus Left)(content())(mode \
                   Normal)))(backpack())(relatives((siblings(((Secondary((id \
                   13620)(content(Comment\"# Hazel Language Quick Reference \
                   #\"))))(Secondary((id \
                   13621)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   13622)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   13686)(content(Comment\"# Empty holes stand for missing \
                   expressions, patterns, or types #\"))))(Secondary((id \
                   13687)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   13691)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   13692)(content(Whitespace\" \"))))(Tile((id \
                   13703)(label(empty_hole))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   13704)(content(Whitespace\" \")))))((Grout((id 13710)(shape \
                   Convex)))(Secondary((id 13707)(content(Whitespace\" \
                   \"))))(Secondary((id 13708)(content(Whitespace\" \
                   \")))))))))(Secondary((id \
                   13712)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   13713)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   13724)(content(Comment\"# Integers #\"))))(Secondary((id \
                   13725)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   13729)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   13730)(content(Whitespace\" \"))))(Tile((id \
                   13739)(label(int_lits))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   13740)(content(Whitespace\" \"))))(Tile((id \
                   13741)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   13743)(content(Whitespace\" \"))))(Tile((id \
                   13746)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   13747)(content(Whitespace\" \")))))((Secondary((id \
                   13749)(content(Whitespace\" \"))))(Tile((id \
                   13750)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13753)(content(Whitespace\" \")))))))))(Secondary((id \
                   13755)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   13759)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   13760)(content(Whitespace\" \"))))(Tile((id \
                   13769)(label(negation))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   13770)(content(Whitespace\" \")))))((Secondary((id \
                   13772)(content(Whitespace\" \"))))(Tile((id \
                   13773)(label(-))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 2))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   13774)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13777)(content(Whitespace\" \")))))))))(Secondary((id \
                   13779)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   13783)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   13784)(content(Whitespace\" \"))))(Tile((id \
                   13795)(label(arithmetic))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   13796)(content(Whitespace\" \")))))((Secondary((id \
                   13798)(content(Whitespace\" \"))))(Tile((id \
                   13799)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   13800)(label(*))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   4))(sort Exp))((shape(Concave 4))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   13802)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13803)(content(Whitespace\" \"))))(Tile((id \
                   13804)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13806)(content(Whitespace\" \"))))(Tile((id \
                   13807)(label(8))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   13808)(label(/))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   4))(sort Exp))((shape(Concave 4))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   13810)(label(4))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13813)(content(Whitespace\" \")))))))))(Secondary((id \
                   13815)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   13819)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   13820)(content(Whitespace\" \"))))(Tile((id \
                   13835)(label(int_comparison))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   13836)(content(Whitespace\" \")))))((Secondary((id \
                   13838)(content(Whitespace\" \"))))(Tile((id \
                   13839)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   13841)(label(10))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13842)(content(Whitespace\" \"))))(Tile((id \
                   13845)(label(==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13846)(content(Whitespace\" \"))))(Tile((id \
                   13848)(label(10))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   13849)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13851)(content(Whitespace\" \"))))(Tile((id \
                   13852)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13853)(content(Whitespace\" \"))))(Tile((id \
                   13854)(label(<))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13856)(content(Whitespace\" \"))))(Tile((id \
                   13857)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   13858)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13860)(content(Whitespace\" \"))))(Tile((id \
                   13861)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13862)(content(Whitespace\" \"))))(Tile((id \
                   13865)(label(<=))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13866)(content(Whitespace\" \"))))(Tile((id \
                   13867)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   13868)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13870)(content(Whitespace\" \"))))(Tile((id \
                   13871)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13872)(content(Whitespace\" \"))))(Tile((id \
                   13873)(label(>))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13875)(content(Whitespace\" \"))))(Tile((id \
                   13876)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   13877)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13879)(content(Whitespace\" \"))))(Tile((id \
                   13880)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13881)(content(Whitespace\" \"))))(Tile((id \
                   13884)(label(>=))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13885)(content(Whitespace\" \"))))(Tile((id \
                   13886)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   13889)(content(Whitespace\" \")))))))))(Secondary((id \
                   13891)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   13892)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   13917)(content(Comment\"# Floating Point Numbers \
                   #\"))))(Secondary((id \
                   13918)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   13922)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   13923)(content(Whitespace\" \"))))(Tile((id \
                   13934)(label(float_lits))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   13935)(content(Whitespace\" \"))))(Tile((id \
                   13936)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   13938)(content(Whitespace\" \"))))(Tile((id \
                   13943)(label(Float))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   13944)(content(Whitespace\" \")))))((Secondary((id \
                   13946)(content(Whitespace\" \"))))(Tile((id \
                   13949)(label(1.5))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13952)(content(Whitespace\" \")))))))))(Secondary((id \
                   13954)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   13958)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   13959)(content(Whitespace\" \"))))(Tile((id \
                   13971)(label(float_artih))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   13972)(content(Whitespace\" \")))))((Secondary((id \
                   13974)(content(Whitespace\" \"))))(Tile((id \
                   13976)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13977)(content(Whitespace\" \"))))(Tile((id \
                   13980)(label(*.))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 4))(sort \
                   Exp))((shape(Concave 4))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13981)(content(Whitespace\" \"))))(Tile((id \
                   13983)(label(2.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13984)(content(Whitespace\" \"))))(Tile((id \
                   13987)(label(+.))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 5))(sort \
                   Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13988)(content(Whitespace\" \"))))(Tile((id \
                   13990)(label(8.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13991)(content(Whitespace\" \"))))(Tile((id \
                   13994)(label(/.))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 4))(sort \
                   Exp))((shape(Concave 4))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   13995)(content(Whitespace\" \"))))(Tile((id \
                   13997)(label(4.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14000)(content(Whitespace\" \")))))))))(Secondary((id \
                   14002)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14006)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   14007)(content(Whitespace\" \"))))(Tile((id \
                   14024)(label(float_comparison))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14025)(content(Whitespace\" \")))))((Secondary((id \
                   14027)(content(Whitespace\" \"))))(Tile((id \
                   14028)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   14031)(label(10.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14032)(content(Whitespace\" \"))))(Tile((id \
                   14036)(label(==.))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14037)(content(Whitespace\" \"))))(Tile((id \
                   14040)(label(10.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14041)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14043)(content(Whitespace\" \"))))(Tile((id \
                   14045)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14046)(content(Whitespace\" \"))))(Tile((id \
                   14049)(label(<.))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 5))(sort \
                   Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14050)(content(Whitespace\" \"))))(Tile((id \
                   14052)(label(2.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14053)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14055)(content(Whitespace\" \"))))(Tile((id \
                   14057)(label(2.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14058)(content(Whitespace\" \"))))(Tile((id \
                   14062)(label(<=.))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14063)(content(Whitespace\" \"))))(Tile((id \
                   14065)(label(3.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14066)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14068)(content(Whitespace\" \"))))(Tile((id \
                   14070)(label(3.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14071)(content(Whitespace\" \"))))(Tile((id \
                   14074)(label(>.))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 5))(sort \
                   Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14075)(content(Whitespace\" \"))))(Tile((id \
                   14077)(label(2.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14078)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14080)(content(Whitespace\" \"))))(Tile((id \
                   14082)(label(2.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14083)(content(Whitespace\" \"))))(Tile((id \
                   14087)(label(>=.))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14088)(content(Whitespace\" \"))))(Tile((id \
                   14090)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   14093)(content(Whitespace\" \")))))))))(Secondary((id \
                   14095)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   14096)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   14107)(content(Comment\"# Booleans #\"))))(Secondary((id \
                   14108)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14112)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   14113)(content(Whitespace\" \"))))(Tile((id \
                   14122)(label(booleans))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14123)(content(Whitespace\" \"))))(Tile((id \
                   14124)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14126)(content(Whitespace\" \"))))(Tile((id \
                   14127)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   14131)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   14132)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14134)(content(Whitespace\" \"))))(Tile((id \
                   14138)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   14139)(content(Whitespace\" \")))))((Secondary((id \
                   14141)(content(Whitespace\" \"))))(Tile((id \
                   14142)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   14146)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14147)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14149)(content(Whitespace\" \"))))(Tile((id \
                   14154)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   14157)(content(Whitespace\" \")))))))))(Secondary((id \
                   14159)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14163)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   14164)(content(Whitespace\" \"))))(Tile((id \
                   14177)(label(conditionals))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14178)(content(Whitespace\" \")))))((Secondary((id \
                   14180)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14184)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   14185)(content(Whitespace\" \"))))(Tile((id \
                   14187)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   14188)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   14189)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14191)(content(Whitespace\" \"))))(Tile((id \
                   14192)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   14193)(content(Whitespace\" \")))))((Secondary((id \
                   14195)(content(Whitespace\" \"))))(Tile((id \
                   14196)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   14197)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14198)(content(Whitespace\" \"))))(Tile((id \
                   14199)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14201)(content(Whitespace\" \"))))(Tile((id \
                   14202)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14203)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14205)(content(Whitespace\" \"))))(Tile((id \
                   14206)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14207)(content(Whitespace\" \"))))(Tile((id \
                   14208)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14210)(content(Whitespace\" \"))))(Tile((id \
                   14211)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   14214)(content(Whitespace\" \")))))))))(Secondary((id \
                   14216)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14219)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   14220)(content(Whitespace\" \"))))(Tile((id \
                   14222)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14223)(content(Whitespace\" \"))))(Tile((id \
                   14224)(label(>))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14226)(content(Whitespace\" \"))))(Tile((id \
                   14227)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14230)(content(Whitespace\" \")))))((Secondary((id \
                   14234)(content(Whitespace\" \"))))(Tile((id \
                   14235)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14241)(content(Whitespace\" \"))))(Secondary((id \
                   14236)(content(Whitespace\" \"))))(Secondary((id \
                   14237)(content(Whitespace\" \"))))(Secondary((id \
                   14238)(content(Whitespace\" \"))))(Secondary((id \
                   14239)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   14245)(content(Whitespace\" \"))))(Tile((id \
                   14246)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14252)(content(Whitespace\" \"))))(Secondary((id \
                   14247)(content(Whitespace\" \"))))(Secondary((id \
                   14248)(content(Whitespace\" \"))))(Secondary((id \
                   14249)(content(Whitespace\" \"))))(Secondary((id \
                   14250)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   14254)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   14255)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   14264)(content(Comment\"# Tuples #\"))))(Secondary((id \
                   14265)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14269)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   14270)(content(Whitespace\" \"))))(Tile((id \
                   14277)(label(tuples))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14278)(content(Whitespace\" \"))))(Tile((id \
                   14279)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14281)(content(Whitespace\" \"))))(Tile((id \
                   14282)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   14285)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   14286)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14288)(content(Whitespace\" \"))))(Tile((id \
                   14292)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   14293)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14295)(content(Whitespace\" \"))))(Tile((id \
                   14296)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   14300)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   14301)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14303)(content(Whitespace\" \"))))(Tile((id \
                   14306)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                   14307)(content(Whitespace\" \")))))((Secondary((id \
                   14309)(content(Whitespace\" \"))))(Tile((id \
                   14310)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   14311)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14312)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14314)(content(Whitespace\" \"))))(Tile((id \
                   14318)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14319)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14321)(content(Whitespace\" \"))))(Tile((id \
                   14322)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   14327)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14328)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14330)(content(Whitespace\" \"))))(Tile((id \
                   14331)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                   14334)(content(Whitespace\" \")))))))))(Secondary((id \
                   14336)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14340)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   14341)(content(Whitespace\" \"))))(Tile((id \
                   14343)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   14344)(label(a))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   14345)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14347)(content(Whitespace\" \"))))(Tile((id \
                   14348)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   14349)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14351)(content(Whitespace\" \"))))(Tile((id \
                   14352)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   14353)(label(c))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   14354)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14356)(content(Whitespace\" \"))))(Tile((id \
                   14357)(label(d))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))))))))))))(Secondary((id \
                   14358)(content(Whitespace\" \")))))((Secondary((id \
                   14360)(content(Whitespace\" \"))))(Tile((id \
                   14366)(label(tuples))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14369)(content(Whitespace\" \")))))))))(Secondary((id \
                   14371)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   14372)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   14384)(content(Comment\"# Functions #\"))))(Secondary((id \
                   14385)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14389)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   14390)(content(Whitespace\" \"))))(Tile((id \
                   14392)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14393)(content(Whitespace\" \"))))(Tile((id \
                   14394)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14396)(content(Whitespace\" \"))))(Tile((id \
                   14397)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   14400)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   14401)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14403)(content(Whitespace\" \"))))(Tile((id \
                   14406)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   14407)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14409)(content(Whitespace\" \"))))(Tile((id \
                   14412)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   14413)(content(Whitespace\" \"))))(Tile((id \
                   14416)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14417)(content(Whitespace\" \"))))(Tile((id \
                   14420)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14421)(content(Whitespace\" \")))))((Secondary((id \
                   14423)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14427)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   14428)(content(Whitespace\" \"))))(Tile((id \
                   14430)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   14431)(label(m))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   14432)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14434)(content(Whitespace\" \"))))(Tile((id \
                   14435)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   14436)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14438)(content(Whitespace\" \"))))(Tile((id \
                   14439)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   14440)(content(Whitespace\" \")))))))))(Secondary((id \
                   14443)(content(Whitespace\" \"))))(Tile((id \
                   14444)(label(m))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14445)(content(Whitespace\" \"))))(Tile((id \
                   14446)(label(*))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   4))(sort Exp))((shape(Concave 4))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14448)(content(Whitespace\" \"))))(Tile((id \
                   14449)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14450)(content(Whitespace\" \"))))(Tile((id \
                   14451)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14453)(content(Whitespace\" \"))))(Tile((id \
                   14454)(label(b))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14460)(content(Whitespace\" \"))))(Secondary((id \
                   14455)(content(Whitespace\" \"))))(Secondary((id \
                   14456)(content(Whitespace\" \"))))(Secondary((id \
                   14457)(content(Whitespace\" \"))))(Secondary((id \
                   14458)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   14462)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   14463)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   14518)(content(Comment\"# Recursive Functions (arrow type \
                   annotation required) #\"))))(Secondary((id \
                   14519)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14523)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   14524)(content(Whitespace\" \"))))(Tile((id \
                   14543)(label(double_recursively))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14544)(content(Whitespace\" \"))))(Tile((id \
                   14545)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14547)(content(Whitespace\" \"))))(Tile((id \
                   14550)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14551)(content(Whitespace\" \"))))(Tile((id \
                   14554)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14555)(content(Whitespace\" \"))))(Tile((id \
                   14558)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14559)(content(Whitespace\" \")))))((Secondary((id \
                   14561)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14565)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   14566)(content(Whitespace\" \"))))(Tile((id \
                   14568)(label(n))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14569)(content(Whitespace\" \")))))))))(Secondary((id \
                   14572)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14575)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   14576)(content(Whitespace\" \"))))(Tile((id \
                   14578)(label(n))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14579)(content(Whitespace\" \"))))(Tile((id \
                   14582)(label(==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14583)(content(Whitespace\" \"))))(Tile((id \
                   14584)(label(0))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14587)(content(Whitespace\" \")))))((Secondary((id \
                   14591)(content(Whitespace\" \"))))(Tile((id \
                   14592)(label(0))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14598)(content(Whitespace\" \"))))(Secondary((id \
                   14593)(content(Whitespace\" \"))))(Secondary((id \
                   14594)(content(Whitespace\" \"))))(Secondary((id \
                   14595)(content(Whitespace\" \"))))(Secondary((id \
                   14596)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   14602)(content(Whitespace\" \"))))(Tile((id \
                   14620)(label(double_recursively))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   14621)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   14623)(label(n))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14624)(content(Whitespace\" \"))))(Tile((id \
                   14625)(label(-))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14627)(content(Whitespace\" \"))))(Tile((id \
                   14628)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   14629)(content(Whitespace\" \"))))(Tile((id \
                   14630)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14632)(content(Whitespace\" \"))))(Tile((id \
                   14633)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14639)(content(Whitespace\" \"))))(Secondary((id \
                   14634)(content(Whitespace\" \"))))(Secondary((id \
                   14635)(content(Whitespace\" \"))))(Secondary((id \
                   14636)(content(Whitespace\" \"))))(Secondary((id \
                   14637)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   14641)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   14642)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   14650)(content(Comment\"# Lists #\"))))(Secondary((id \
                   14651)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14655)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   14656)(content(Whitespace\" \"))))(Tile((id \
                   14667)(label(empty_list))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14668)(content(Whitespace\" \"))))(Tile((id \
                   14669)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14671)(content(Whitespace\" \"))))(Tile((id 14672)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 14675)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   14676)(content(Whitespace\" \")))))((Secondary((id \
                   14678)(content(Whitespace\" \"))))(Tile((id \
                   14680)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14683)(content(Whitespace\" \")))))))))(Secondary((id \
                   14685)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14689)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   14690)(content(Whitespace\" \"))))(Tile((id \
                   14705)(label(non_empty_list))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14706)(content(Whitespace\" \"))))(Tile((id \
                   14707)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14709)(content(Whitespace\" \"))))(Tile((id 14710)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 14713)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   14714)(content(Whitespace\" \")))))((Secondary((id \
                   14716)(content(Whitespace\" \"))))(Tile((id \
                   14717)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14720)(label(::))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 6))(sort \
                   Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14721)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14724)(label(::))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 6))(sort \
                   Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14725)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14728)(label(::))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 6))(sort \
                   Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14730)(label([]))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14733)(content(Whitespace\" \")))))))))(Secondary((id \
                   14735)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14739)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   14740)(content(Whitespace\" \"))))(Tile((id \
                   14754)(label(list_literals))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14755)(content(Whitespace\" \"))))(Tile((id \
                   14756)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14758)(content(Whitespace\" \"))))(Tile((id 14759)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 14762)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   14763)(content(Whitespace\" \")))))((Secondary((id \
                   14765)(content(Whitespace\" \"))))(Tile((id 14766)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 14767)(label(1))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   14768)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14770)(content(Whitespace\" \"))))(Tile((id \
                   14771)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14772)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14774)(content(Whitespace\" \"))))(Tile((id \
                   14775)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   14778)(content(Whitespace\" \")))))))))(Secondary((id \
                   14780)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14784)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   14785)(content(Whitespace\" \"))))(Tile((id \
                   14792)(label(length))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14793)(content(Whitespace\" \"))))(Tile((id \
                   14794)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14796)(content(Whitespace\" \"))))(Tile((id 14797)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 14800)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   14801)(content(Whitespace\" \"))))(Tile((id \
                   14804)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14805)(content(Whitespace\" \"))))(Tile((id \
                   14808)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14809)(content(Whitespace\" \")))))((Secondary((id \
                   14811)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14815)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   14816)(content(Whitespace\" \"))))(Tile((id \
                   14819)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14820)(content(Whitespace\" \")))))))))(Secondary((id \
                   14823)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14828)(label(case end))(mold((out \
                   Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 14829)(content(Whitespace\" \
                   \"))))(Tile((id 14832)(label(xs))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14833)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14834)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 14836)(content(Whitespace\" \
                   \"))))(Tile((id 14838)(label([]))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14839)(content(Whitespace\" \")))))))))(Secondary((id \
                   14842)(content(Whitespace\" \"))))(Tile((id \
                   14843)(label(0))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14844)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14845)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 14847)(content(Whitespace\" \
                   \"))))(Tile((id 14849)(label(hd))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   14852)(label(::))(mold((out \
                   Pat)(in_())(nibs(((shape(Concave 6))(sort \
                   Pat))((shape(Concave 6))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   14854)(label(tl))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14855)(content(Whitespace\" \")))))))))(Secondary((id \
                   14858)(content(Whitespace\" \"))))(Tile((id \
                   14859)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14860)(content(Whitespace\" \"))))(Tile((id \
                   14861)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14863)(content(Whitespace\" \"))))(Tile((id \
                   14869)(label(length))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   14870)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   14873)(label(tl))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   14879)(content(Whitespace\" \"))))(Secondary((id \
                   14874)(content(Whitespace\" \"))))(Secondary((id \
                   14875)(content(Whitespace\" \"))))(Secondary((id \
                   14876)(content(Whitespace\" \"))))(Secondary((id \
                   14877)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   14886)(content(Whitespace\" \"))))(Secondary((id \
                   14881)(content(Whitespace\" \"))))(Secondary((id \
                   14882)(content(Whitespace\" \"))))(Secondary((id \
                   14883)(content(Whitespace\" \"))))(Secondary((id \
                   14884)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   14888)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14892)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   14893)(content(Whitespace\" \"))))(Tile((id \
                   14919)(label(has_at_least_two_elements))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14920)(content(Whitespace\" \"))))(Tile((id \
                   14921)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14923)(content(Whitespace\" \"))))(Tile((id 14924)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 14927)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   14928)(content(Whitespace\" \"))))(Tile((id \
                   14931)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14932)(content(Whitespace\" \"))))(Tile((id \
                   14936)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   14937)(content(Whitespace\" \")))))((Secondary((id \
                   14939)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14943)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   14944)(content(Whitespace\" \"))))(Tile((id \
                   14947)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14948)(content(Whitespace\" \")))))))))(Secondary((id \
                   14951)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14956)(label(case end))(mold((out \
                   Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 14957)(content(Whitespace\" \
                   \"))))(Tile((id 14960)(label(xs))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14961)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14962)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 14964)(content(Whitespace\" \
                   \"))))(Tile((id 14966)(label([]))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14967)(content(Whitespace\" \")))))))))(Secondary((id \
                   14970)(content(Whitespace\" \"))))(Tile((id \
                   14975)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14976)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14977)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 14979)(content(Whitespace\" \
                   \"))))(Tile((id 14981)(label(hd))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   14984)(label(::))(mold((out \
                   Pat)(in_())(nibs(((shape(Concave 6))(sort \
                   Pat))((shape(Concave 6))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   14986)(label([]))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   14987)(content(Whitespace\" \")))))))))(Secondary((id \
                   14990)(content(Whitespace\" \"))))(Tile((id \
                   14995)(label(false))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   14996)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   14997)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 14999)(content(Whitespace\" \
                   \"))))(Tile((id 15000)(label(a))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   15003)(label(::))(mold((out \
                   Pat)(in_())(nibs(((shape(Concave 6))(sort \
                   Pat))((shape(Concave 6))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   15004)(label(b))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   15007)(label(::))(mold((out \
                   Pat)(in_())(nibs(((shape(Concave 6))(sort \
                   Pat))((shape(Concave 6))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   15009)(label([]))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   15010)(content(Whitespace\" \")))))))))(Secondary((id \
                   15013)(content(Whitespace\" \"))))(Tile((id \
                   15017)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   15023)(content(Whitespace\" \"))))(Secondary((id \
                   15018)(content(Whitespace\" \"))))(Secondary((id \
                   15019)(content(Whitespace\" \"))))(Secondary((id \
                   15020)(content(Whitespace\" \"))))(Secondary((id \
                   15021)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   15030)(content(Whitespace\" \"))))(Secondary((id \
                   15025)(content(Whitespace\" \"))))(Secondary((id \
                   15026)(content(Whitespace\" \"))))(Secondary((id \
                   15027)(content(Whitespace\" \"))))(Secondary((id \
                   15028)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   15032)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   15033)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   15043)(content(Comment\"# Strings #\"))))(Secondary((id \
                   15044)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   15048)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   15049)(content(Whitespace\" \"))))(Tile((id \
                   15061)(label(string_lits))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   15062)(content(Whitespace\" \")))))((Secondary((id \
                   15064)(content(Whitespace\" \"))))(Tile((id \
                   15078)(label(\"\\\"Hello, world!\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   15081)(content(Whitespace\" \")))))))))(Secondary((id \
                   15083)(content(Whitespace\" \"))))(Secondary((id \
                   15084)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   15088)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   15089)(content(Whitespace\" \"))))(Tile((id \
                   15105)(label(string_equality))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   15106)(content(Whitespace\" \")))))((Secondary((id \
                   15108)(content(Whitespace\" \"))))(Tile((id \
                   15119)(label(string_lits))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   15120)(content(Whitespace\" \"))))(Tile((id \
                   15124)(label($==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   15125)(content(Whitespace\" \"))))(Tile((id \
                   15139)(label(\"\\\"Hello, world!\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   15142)(content(Whitespace\" \")))))))))(Secondary((id \
                   15144)(content(Whitespace\" \"))))(Secondary((id \
                   15145)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   15146)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   15203)(content(Comment\"# Non-empty holes are the red \
                   dotted boxes around errors #\"))))(Secondary((id \
                   15204)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   15256)(content(Comment\"# (you can still run programs with \
                   non-empty holes) #\"))))(Secondary((id \
                   15257)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   15261)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   15262)(content(Whitespace\" \"))))(Tile((id \
                   15277)(label(non_empty_hole))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   15278)(content(Whitespace\" \"))))(Tile((id \
                   15279)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   15281)(content(Whitespace\" \"))))(Tile((id \
                   15284)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   15285)(content(Whitespace\" \")))))((Secondary((id \
                   15287)(content(Whitespace\" \"))))(Tile((id \
                   15291)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   15294)(content(Whitespace\" \")))))))))(Secondary((id \
                   15296)(content(Whitespace\" \"))))(Secondary((id \
                   15297)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   15298)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   15299)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   15300)(content(Whitespace\" \"))))(Tile((id \
                   15301)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   15303)(content(Whitespace\" \"))))(Tile((id \
                   15304)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   15305)(content(Whitespace\"\\226\\143\\142\")))))()))(ancestors())))(caret \
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
                   if y > x then 1    \n\
                   else 2    \n\
                   in\n\n\
                   # Tuples #\n\
                   let tuples : (Int, Bool, (Bool, Int)) = (1, true, (false, \
                   3)) in\n\
                   let (a, b, (c, d)) = tuples in\n\n\
                   # Functions #\n\
                   let y : (Int, Int, Int) -> Int =\n\
                   fun (m, x, b) -> m * x + b    \n\
                   in\n\n\
                   # Recursive Functions (arrow type annotation required) #\n\
                   let double_recursively : Int -> Int =\n\
                   fun n ->\n\
                   if n == 0 then 0    \n\
                   else double_recursively(n - 1) + 2    \n\
                   in\n\n\
                   # Lists #\n\
                   let empty_list : [Int] = [] in\n\
                   let non_empty_list : [Int] = 1::2::3::[] in\n\
                   let list_literals : [Int] = [1, 2, 3] in\n\
                   let length : [Int] -> Int =\n\
                   fun xs ->\n\
                   case xs\n\
                   | [] => 0\n\
                   | hd::tl => 1 + length(tl)    \n\
                   end    \n\
                   in\n\
                   let has_at_least_two_elements : [Int] -> Bool =\n\
                   fun xs ->\n\
                   case xs\n\
                   | [] => false\n\
                   | hd::[] => false\n\
                   | a::b::[] => true    \n\
                   end    \n\
                   in\n\n\
                   # Strings #\n\
                   let string_lits = \"Hello, world!\" in \n\
                   let string_equality = string_lits $== \"Hello, world!\" in \n\n\
                   # Non-empty holes are the red dotted boxes around errors #\n\
                   # (you can still run programs with non-empty holes) #\n\
                   let non_empty_hole : Int = true in \n\n\
                   2 + 2\n";
              } ) );
          ( "Types & errors",
            ( 27152,
              {
                zipper =
                  "((selection((focus Left)(content())(mode \
                   Normal)))(backpack())(relatives((siblings(((Secondary((id \
                   25483)(content(Comment\"#Types and type error \
                   examples#\"))))(Secondary((id \
                   25484)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   25485)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25489)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25490)(content(Whitespace\" \"))))(Tile((id \
                   25492)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   25493)(content(Whitespace\" \")))))((Secondary((id \
                   25495)(content(Whitespace\" \"))))(Tile((id \
                   25502)(label(unbound))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25505)(content(Whitespace\" \")))))))))(Secondary((id \
                   25507)(content(Whitespace\" \"))))(Secondary((id \
                   25511)(content(Comment #err#))))(Secondary((id \
                   25512)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25516)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25517)(content(Whitespace\" \"))))(Tile((id \
                   25527)(label(Undefined))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   25528)(content(Whitespace\" \")))))((Secondary((id \
                   25530)(content(Whitespace\" \"))))(Tile((id \
                   25539)(label(Undefined))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25542)(content(Whitespace\" \")))))))))(Secondary((id \
                   25544)(content(Whitespace\" \"))))(Secondary((id \
                   25552)(content(Comment\"# 2x err#\"))))(Secondary((id \
                   25553)(content(Whitespace\" \"))))(Secondary((id \
                   25554)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25558)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25559)(content(Whitespace\" \"))))(Tile((id \
                   25564)(label(true))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   25565)(content(Whitespace\" \")))))((Secondary((id \
                   25567)(content(Whitespace\" \"))))(Tile((id \
                   25568)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25571)(content(Whitespace\" \")))))))))(Secondary((id \
                   25573)(content(Whitespace\" \"))))(Secondary((id \
                   25577)(content(Comment #err#))))(Secondary((id \
                   25578)(content(Whitespace\" \"))))(Secondary((id \
                   25579)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   25580)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25584)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Grout((id \
                   25589)(shape Convex)))(Secondary((id \
                   25587)(content(Whitespace\" \"))))(Secondary((id \
                   25588)(content(Whitespace\" \")))))((Secondary((id \
                   25590)(content(Whitespace\" \"))))(Tile((id 25593)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25594)(content(Whitespace\" \"))))(Tile((id \
                   25599)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25602)(content(Whitespace\" \")))))((Secondary((id \
                   25606)(content(Whitespace\" \"))))(Tile((id \
                   25607)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25610)(content(Whitespace\" \")))))))))(Secondary((id \
                   25614)(content(Whitespace\" \"))))(Tile((id \
                   25616)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25619)(content(Whitespace\" \")))))))))(Secondary((id \
                   25621)(content(Whitespace\" \"))))(Secondary((id \
                   25625)(content(Comment #err#))))(Secondary((id \
                   25626)(content(Whitespace\" \"))))(Secondary((id \
                   25627)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25631)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25632)(content(Whitespace\" \"))))(Tile((id \
                   25634)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   25635)(content(Whitespace\" \")))))((Secondary((id \
                   25637)(content(Whitespace\" \"))))(Tile((id 25640)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25641)(content(Whitespace\" \"))))(Tile((id \
                   25646)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25649)(content(Whitespace\" \")))))((Secondary((id \
                   25653)(content(Whitespace\" \"))))(Tile((id \
                   25654)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25657)(content(Whitespace\" \")))))))))(Secondary((id \
                   25661)(content(Whitespace\" \"))))(Tile((id \
                   25663)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25666)(content(Whitespace\" \")))))))))(Secondary((id \
                   25668)(content(Whitespace\" \"))))(Secondary((id \
                   25672)(content(Comment #err#))))(Secondary((id \
                   25673)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25677)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25678)(content(Whitespace\" \"))))(Tile((id \
                   25680)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   25681)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Grout((id 25686)(shape \
                   Convex)))(Secondary((id 25684)(content(Whitespace\" \
                   \"))))(Secondary((id 25685)(content(Whitespace\" \
                   \")))))((Secondary((id 25687)(content(Whitespace\" \
                   \"))))(Tile((id 25690)(label(if then else))(mold((out \
                   Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                   2))(children(((Secondary((id 25691)(content(Whitespace\" \
                   \"))))(Tile((id 25696)(label(true))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25699)(content(Whitespace\" \")))))((Secondary((id \
                   25703)(content(Whitespace\" \"))))(Tile((id \
                   25704)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25707)(content(Whitespace\" \")))))))))(Secondary((id \
                   25711)(content(Whitespace\" \"))))(Tile((id \
                   25713)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25716)(content(Whitespace\" \")))))))))(Secondary((id \
                   25718)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25722)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25723)(content(Whitespace\" \"))))(Tile((id \
                   25725)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   25726)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   25728)(content(Whitespace\" \"))))(Tile((id \
                   25731)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   25732)(content(Whitespace\" \")))))((Secondary((id \
                   25734)(content(Whitespace\" \"))))(Tile((id 25737)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25738)(content(Whitespace\" \"))))(Tile((id \
                   25743)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25746)(content(Whitespace\" \")))))((Secondary((id \
                   25750)(content(Whitespace\" \"))))(Tile((id \
                   25751)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25754)(content(Whitespace\" \")))))))))(Secondary((id \
                   25758)(content(Whitespace\" \"))))(Tile((id \
                   25760)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25763)(content(Whitespace\" \")))))))))(Secondary((id \
                   25765)(content(Whitespace\" \"))))(Secondary((id \
                   25769)(content(Comment #err#))))(Secondary((id \
                   25770)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25774)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25775)(content(Whitespace\" \"))))(Tile((id \
                   25777)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   25778)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   25780)(content(Whitespace\" \"))))(Tile((id \
                   25784)(label(Fake))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   25785)(content(Whitespace\" \")))))((Secondary((id \
                   25787)(content(Whitespace\" \"))))(Tile((id 25790)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25791)(content(Whitespace\" \"))))(Tile((id \
                   25796)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25799)(content(Whitespace\" \")))))((Secondary((id \
                   25803)(content(Whitespace\" \"))))(Tile((id \
                   25804)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25807)(content(Whitespace\" \")))))))))(Secondary((id \
                   25811)(content(Whitespace\" \"))))(Tile((id \
                   25815)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25818)(content(Whitespace\" \")))))))))(Secondary((id \
                   25820)(content(Whitespace\" \"))))(Secondary((id \
                   25824)(content(Comment #err#))))(Secondary((id \
                   25825)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25829)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25830)(content(Whitespace\" \"))))(Tile((id \
                   25832)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   25833)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   25835)(content(Whitespace\" \"))))(Tile((id \
                   25836)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   25837)(content(Whitespace\" \")))))((Secondary((id \
                   25839)(content(Whitespace\" \"))))(Tile((id 25842)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25843)(content(Whitespace\" \"))))(Tile((id \
                   25848)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25851)(content(Whitespace\" \")))))((Secondary((id \
                   25855)(content(Whitespace\" \"))))(Tile((id \
                   25856)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25859)(content(Whitespace\" \")))))))))(Secondary((id \
                   25863)(content(Whitespace\" \"))))(Tile((id \
                   25865)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25868)(content(Whitespace\" \")))))))))(Secondary((id \
                   25870)(content(Whitespace\" \"))))(Secondary((id \
                   25877)(content(Comment\"#2x err#\"))))(Secondary((id \
                   25878)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25882)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25883)(content(Whitespace\" \"))))(Tile((id \
                   25885)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   25886)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   25888)(content(Whitespace\" \"))))(Tile((id \
                   25889)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   25890)(content(Whitespace\" \")))))((Secondary((id \
                   25892)(content(Whitespace\" \"))))(Tile((id \
                   25893)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   25896)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25897)(content(Whitespace\" \"))))(Tile((id \
                   25902)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25905)(content(Whitespace\" \")))))((Secondary((id \
                   25909)(content(Whitespace\" \"))))(Tile((id \
                   25910)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25913)(content(Whitespace\" \")))))))))(Secondary((id \
                   25917)(content(Whitespace\" \"))))(Tile((id \
                   25919)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   25920)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Grout((id 25927)(shape \
                   Convex)))(Secondary((id 25923)(content(Whitespace\" \
                   \"))))(Secondary((id 25924)(content(Whitespace\" \
                   \"))))(Secondary((id 25925)(content(Whitespace\" \
                   \")))))))))(Secondary((id 25929)(content(Whitespace\" \
                   \"))))(Secondary((id 25933)(content(Comment \
                   #err#))))(Secondary((id \
                   25934)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25938)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25939)(content(Whitespace\" \"))))(Tile((id \
                   25941)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   25942)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   25944)(content(Whitespace\" \"))))(Grout((id 25947)(shape \
                   Convex)))(Tile((id 25946)(label(,))(mold((out \
                   Pat)(in_())(nibs(((shape(Concave 14))(sort \
                   Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   25948)(content(Whitespace\" \"))))(Tile((id \
                   25949)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   25950)(content(Whitespace\" \")))))((Secondary((id \
                   25952)(content(Whitespace\" \"))))(Tile((id \
                   25953)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   25956)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25957)(content(Whitespace\" \"))))(Tile((id \
                   25962)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25965)(content(Whitespace\" \")))))((Secondary((id \
                   25969)(content(Whitespace\" \"))))(Tile((id \
                   25970)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   25973)(content(Whitespace\" \")))))))))(Secondary((id \
                   25977)(content(Whitespace\" \"))))(Tile((id \
                   25979)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   25980)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Grout((id 25987)(shape \
                   Convex)))(Secondary((id 25983)(content(Whitespace\" \
                   \"))))(Secondary((id 25984)(content(Whitespace\" \
                   \"))))(Secondary((id 25985)(content(Whitespace\" \
                   \")))))))))(Secondary((id 25989)(content(Whitespace\" \
                   \"))))(Secondary((id \
                   25990)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   25994)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   25995)(content(Whitespace\" \"))))(Tile((id 25997)(label([ \
                   ]))(mold((out Pat)(in_(Pat))(nibs(((shape Convex)(sort \
                   Pat))((shape Convex)(sort Pat))))))(shards(0 \
                   1))(children(((Tile((id 25998)(label(_))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   25999)(content(Whitespace\" \")))))((Secondary((id \
                   26001)(content(Whitespace\" \"))))(Tile((id 26002)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 26003)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26006)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26007)(content(Whitespace\" \"))))(Tile((id \
                   26012)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26015)(content(Whitespace\" \")))))((Secondary((id \
                   26019)(content(Whitespace\" \"))))(Tile((id \
                   26020)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26023)(content(Whitespace\" \")))))))))(Secondary((id \
                   26027)(content(Whitespace\" \"))))(Tile((id \
                   26029)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                   26032)(content(Whitespace\" \")))))))))(Secondary((id \
                   26034)(content(Whitespace\" \"))))(Secondary((id \
                   26035)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26039)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26040)(content(Whitespace\" \"))))(Tile((id 26042)(label([ \
                   ]))(mold((out Pat)(in_(Pat))(nibs(((shape Convex)(sort \
                   Pat))((shape Convex)(sort Pat))))))(shards(0 \
                   1))(children(((Tile((id 26043)(label(_))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   26044)(content(Whitespace\" \")))))((Secondary((id \
                   26046)(content(Whitespace\" \"))))(Tile((id \
                   26047)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26050)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26051)(content(Whitespace\" \"))))(Tile((id \
                   26056)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26059)(content(Whitespace\" \")))))((Secondary((id \
                   26063)(content(Whitespace\" \"))))(Tile((id \
                   26064)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26067)(content(Whitespace\" \")))))))))(Secondary((id \
                   26071)(content(Whitespace\" \"))))(Tile((id \
                   26073)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   26076)(content(Whitespace\" \")))))))))(Secondary((id \
                   26078)(content(Whitespace\" \"))))(Secondary((id \
                   26085)(content(Comment\"#2x err#\"))))(Secondary((id \
                   26086)(content(Whitespace\" \"))))(Secondary((id \
                   26087)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   26088)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26089)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Grout((id \
                   26092)(shape Convex)))(Secondary((id \
                   26091)(content(Whitespace\" \")))))))))(Tile((id \
                   26093)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26097)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26098)(content(Whitespace\" \"))))(Tile((id \
                   26103)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26106)(content(Whitespace\" \")))))((Secondary((id \
                   26110)(content(Whitespace\" \"))))(Tile((id \
                   26111)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26114)(content(Whitespace\" \")))))))))(Secondary((id \
                   26118)(content(Whitespace\" \"))))(Tile((id \
                   26120)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   26121)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26123)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26124)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   26125)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26129)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26130)(content(Whitespace\" \"))))(Tile((id \
                   26135)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26138)(content(Whitespace\" \")))))((Secondary((id \
                   26142)(content(Whitespace\" \"))))(Tile((id \
                   26143)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26146)(content(Whitespace\" \")))))))))(Secondary((id \
                   26150)(content(Whitespace\" \"))))(Tile((id \
                   26152)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   26153)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26155)(content(Whitespace\" \"))))(Secondary((id \
                   26159)(content(Comment #err#))))(Secondary((id \
                   26160)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26161)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26162)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   26163)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26167)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26168)(content(Whitespace\" \"))))(Tile((id \
                   26173)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26176)(content(Whitespace\" \")))))((Secondary((id \
                   26180)(content(Whitespace\" \"))))(Tile((id \
                   26181)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26184)(content(Whitespace\" \")))))))))(Secondary((id \
                   26188)(content(Whitespace\" \"))))(Tile((id \
                   26190)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   26191)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26193)(content(Whitespace\" \"))))(Secondary((id \
                   26197)(content(Comment #err#))))(Secondary((id \
                   26198)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26199)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26203)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Grout((id 26209)(shape \
                   Convex)))(Secondary((id 26206)(content(Whitespace\" \
                   \"))))(Secondary((id 26207)(content(Whitespace\" \
                   \")))))))))(Grout((id 26212)(shape Convex)))(Secondary((id \
                   26211)(content(Whitespace\" \")))))))))(Tile((id \
                   26213)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26217)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26218)(content(Whitespace\" \"))))(Tile((id \
                   26223)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26226)(content(Whitespace\" \")))))((Secondary((id \
                   26230)(content(Whitespace\" \"))))(Tile((id \
                   26231)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26234)(content(Whitespace\" \")))))))))(Secondary((id \
                   26238)(content(Whitespace\" \"))))(Tile((id \
                   26240)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   26241)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26243)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26244)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26248)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   26249)(content(Whitespace\" \"))))(Tile((id \
                   26251)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   26252)(content(Whitespace\" \")))))))))(Grout((id \
                   26257)(shape Convex)))(Secondary((id \
                   26256)(content(Whitespace\" \")))))))))(Tile((id \
                   26258)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26262)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26263)(content(Whitespace\" \"))))(Tile((id \
                   26268)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26271)(content(Whitespace\" \")))))((Secondary((id \
                   26275)(content(Whitespace\" \"))))(Tile((id \
                   26276)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26279)(content(Whitespace\" \")))))))))(Secondary((id \
                   26283)(content(Whitespace\" \"))))(Tile((id \
                   26285)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   26286)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26288)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26289)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26293)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   26294)(content(Whitespace\" \"))))(Tile((id \
                   26296)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   26297)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Grout((id 26303)(shape \
                   Convex)))(Secondary((id 26300)(content(Whitespace\" \
                   \"))))(Secondary((id 26301)(content(Whitespace\" \
                   \")))))))))(Grout((id 26306)(shape Convex)))(Secondary((id \
                   26305)(content(Whitespace\" \")))))))))(Tile((id \
                   26307)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26311)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26312)(content(Whitespace\" \"))))(Tile((id \
                   26317)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26320)(content(Whitespace\" \")))))((Secondary((id \
                   26324)(content(Whitespace\" \"))))(Tile((id \
                   26325)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26328)(content(Whitespace\" \")))))))))(Secondary((id \
                   26332)(content(Whitespace\" \"))))(Tile((id \
                   26334)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   26335)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26337)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26338)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26342)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   26343)(content(Whitespace\" \"))))(Tile((id \
                   26345)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   26346)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   26348)(content(Whitespace\" \"))))(Tile((id \
                   26351)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   26352)(content(Whitespace\" \")))))))))(Grout((id \
                   26357)(shape Convex)))(Secondary((id \
                   26356)(content(Whitespace\" \")))))))))(Tile((id \
                   26358)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26362)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26363)(content(Whitespace\" \"))))(Tile((id \
                   26368)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26371)(content(Whitespace\" \")))))((Secondary((id \
                   26375)(content(Whitespace\" \"))))(Tile((id \
                   26376)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26379)(content(Whitespace\" \")))))))))(Secondary((id \
                   26383)(content(Whitespace\" \"))))(Tile((id \
                   26385)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   26386)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26388)(content(Whitespace\" \"))))(Secondary((id \
                   26392)(content(Comment #err#))))(Secondary((id \
                   26393)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   26394)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26398)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26399)(content(Whitespace\" \"))))(Tile((id \
                   26401)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   26402)(content(Whitespace\" \")))))((Secondary((id \
                   26404)(content(Whitespace\" \"))))(Tile((id \
                   26408)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   26409)(content(Whitespace\" \"))))(Tile((id \
                   26411)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   26412)(content(Whitespace\" \")))))))))(Secondary((id \
                   26415)(content(Whitespace\" \"))))(Tile((id 26418)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26419)(content(Whitespace\" \"))))(Tile((id \
                   26424)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26427)(content(Whitespace\" \")))))((Secondary((id \
                   26431)(content(Whitespace\" \"))))(Tile((id \
                   26432)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26435)(content(Whitespace\" \")))))))))(Secondary((id \
                   26439)(content(Whitespace\" \"))))(Tile((id \
                   26441)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26444)(content(Whitespace\" \")))))))))(Secondary((id \
                   26446)(content(Whitespace\" \"))))(Secondary((id \
                   26450)(content(Comment #err#))))(Secondary((id \
                   26451)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26455)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26456)(content(Whitespace\" \"))))(Tile((id \
                   26458)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   26459)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Grout((id 26464)(shape \
                   Convex)))(Secondary((id 26462)(content(Whitespace\" \
                   \"))))(Secondary((id 26463)(content(Whitespace\" \
                   \")))))((Secondary((id 26465)(content(Whitespace\" \
                   \"))))(Tile((id 26469)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 26470)(content(Whitespace\" \
                   \"))))(Tile((id 26472)(label(x))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   26473)(content(Whitespace\" \")))))))))(Secondary((id \
                   26476)(content(Whitespace\" \"))))(Tile((id 26479)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26480)(content(Whitespace\" \"))))(Tile((id \
                   26485)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26488)(content(Whitespace\" \")))))((Secondary((id \
                   26492)(content(Whitespace\" \"))))(Tile((id \
                   26493)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26496)(content(Whitespace\" \")))))))))(Secondary((id \
                   26500)(content(Whitespace\" \"))))(Tile((id \
                   26502)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26505)(content(Whitespace\" \")))))))))(Secondary((id \
                   26507)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26511)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26512)(content(Whitespace\" \"))))(Tile((id \
                   26514)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   26515)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   26517)(content(Whitespace\" \"))))(Secondary((id \
                   26518)(content(Whitespace\" \"))))(Grout((id 26522)(shape \
                   Convex)))(Tile((id 26521)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Grout((id 26526)(shape \
                   Convex)))(Secondary((id 26524)(content(Whitespace\" \
                   \"))))(Secondary((id 26525)(content(Whitespace\" \
                   \")))))((Secondary((id 26527)(content(Whitespace\" \
                   \"))))(Tile((id 26531)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 26532)(content(Whitespace\" \
                   \"))))(Tile((id 26534)(label(x))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   26535)(content(Whitespace\" \")))))))))(Secondary((id \
                   26538)(content(Whitespace\" \"))))(Tile((id 26541)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26542)(content(Whitespace\" \"))))(Tile((id \
                   26547)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26550)(content(Whitespace\" \")))))((Secondary((id \
                   26554)(content(Whitespace\" \"))))(Tile((id \
                   26555)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26558)(content(Whitespace\" \")))))))))(Secondary((id \
                   26562)(content(Whitespace\" \"))))(Tile((id \
                   26564)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26567)(content(Whitespace\" \")))))))))(Secondary((id \
                   26569)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26573)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26574)(content(Whitespace\" \"))))(Tile((id \
                   26576)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   26577)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   26579)(content(Whitespace\" \"))))(Secondary((id \
                   26580)(content(Whitespace\" \"))))(Grout((id 26584)(shape \
                   Convex)))(Tile((id 26583)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   26585)(content(Whitespace\" \"))))(Tile((id \
                   26588)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   26589)(content(Whitespace\" \")))))((Secondary((id \
                   26591)(content(Whitespace\" \"))))(Tile((id \
                   26595)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   26596)(content(Whitespace\" \"))))(Tile((id \
                   26598)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   26599)(content(Whitespace\" \")))))))))(Secondary((id \
                   26602)(content(Whitespace\" \"))))(Tile((id 26605)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26606)(content(Whitespace\" \"))))(Tile((id \
                   26611)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26614)(content(Whitespace\" \")))))((Secondary((id \
                   26618)(content(Whitespace\" \"))))(Tile((id \
                   26619)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26622)(content(Whitespace\" \")))))))))(Secondary((id \
                   26626)(content(Whitespace\" \"))))(Tile((id \
                   26628)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26631)(content(Whitespace\" \")))))))))(Secondary((id \
                   26633)(content(Whitespace\" \"))))(Secondary((id \
                   26637)(content(Comment #err#))))(Secondary((id \
                   26638)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26642)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26643)(content(Whitespace\" \"))))(Tile((id \
                   26645)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   26646)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   26648)(content(Whitespace\" \"))))(Secondary((id \
                   26649)(content(Whitespace\" \"))))(Grout((id 26653)(shape \
                   Convex)))(Tile((id 26652)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   26654)(content(Whitespace\" \"))))(Tile((id 26655)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Grout((id 26657)(shape \
                   Convex))))))))(Secondary((id 26658)(content(Whitespace\" \
                   \")))))((Secondary((id 26660)(content(Whitespace\" \
                   \"))))(Tile((id 26664)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 26665)(content(Whitespace\" \
                   \"))))(Tile((id 26667)(label(x))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   26668)(content(Whitespace\" \")))))))))(Secondary((id \
                   26671)(content(Whitespace\" \"))))(Tile((id 26674)(label(if \
                   then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 12))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26675)(content(Whitespace\" \"))))(Tile((id \
                   26680)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26683)(content(Whitespace\" \")))))((Secondary((id \
                   26687)(content(Whitespace\" \"))))(Tile((id \
                   26688)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26691)(content(Whitespace\" \")))))))))(Secondary((id \
                   26695)(content(Whitespace\" \"))))(Tile((id \
                   26697)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26700)(content(Whitespace\" \")))))))))(Secondary((id \
                   26702)(content(Whitespace\" \"))))(Secondary((id \
                   26709)(content(Comment\"#2x err#\"))))(Secondary((id \
                   26710)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   26711)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26712)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Grout((id \
                   26714)(shape Convex))))))))(Tile((id \
                   26717)(label(::))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 6))(sort \
                   Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 26718)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 26719)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26722)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26723)(content(Whitespace\" \"))))(Tile((id \
                   26728)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26731)(content(Whitespace\" \")))))((Secondary((id \
                   26735)(content(Whitespace\" \"))))(Tile((id \
                   26736)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26739)(content(Whitespace\" \")))))))))(Secondary((id \
                   26743)(content(Whitespace\" \"))))(Tile((id \
                   26745)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                   26746)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26748)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26749)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   26752)(label(::))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 6))(sort \
                   Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 26753)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 26754)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26757)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26758)(content(Whitespace\" \"))))(Tile((id \
                   26763)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26766)(content(Whitespace\" \")))))((Secondary((id \
                   26770)(content(Whitespace\" \"))))(Tile((id \
                   26771)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26774)(content(Whitespace\" \")))))))))(Secondary((id \
                   26778)(content(Whitespace\" \"))))(Tile((id \
                   26780)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                   26781)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26783)(content(Whitespace\" \"))))(Secondary((id \
                   26787)(content(Comment #err#))))(Secondary((id \
                   26788)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26789)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26790)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   26791)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26793)(content(Whitespace\" \"))))(Tile((id \
                   26794)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   26797)(label(::))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 6))(sort \
                   Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 26798)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 26799)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26802)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26803)(content(Whitespace\" \"))))(Tile((id \
                   26808)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26811)(content(Whitespace\" \")))))((Secondary((id \
                   26815)(content(Whitespace\" \"))))(Tile((id \
                   26816)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26819)(content(Whitespace\" \")))))))))(Secondary((id \
                   26823)(content(Whitespace\" \"))))(Tile((id \
                   26825)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                   26826)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26828)(content(Whitespace\" \"))))(Secondary((id \
                   26835)(content(Comment\"#2x err#\"))))(Secondary((id \
                   26836)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   26837)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26841)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Grout((id \
                   26847)(shape Convex)))(Secondary((id \
                   26844)(content(Whitespace\" \"))))(Secondary((id \
                   26845)(content(Whitespace\" \"))))(Secondary((id \
                   26846)(content(Whitespace\" \")))))((Secondary((id \
                   26848)(content(Whitespace\" \"))))(Tile((id 26849)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 26850)(label(1))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   26851)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26853)(content(Whitespace\" \"))))(Tile((id \
                   26855)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   26856)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26858)(content(Whitespace\" \"))))(Tile((id \
                   26862)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   26865)(content(Whitespace\" \")))))))))(Secondary((id \
                   26867)(content(Whitespace\" \"))))(Secondary((id \
                   26885)(content(Comment\"#err: \
                   inconsistent#\"))))(Secondary((id \
                   26886)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26890)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26891)(content(Whitespace\" \"))))(Tile((id \
                   26893)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   26894)(content(Whitespace\" \")))))((Secondary((id \
                   26896)(content(Whitespace\" \"))))(Tile((id 26897)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 26898)(label(1))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   26899)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26901)(content(Whitespace\" \"))))(Tile((id \
                   26903)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   26904)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26906)(content(Whitespace\" \"))))(Tile((id \
                   26910)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   26913)(content(Whitespace\" \")))))))))(Secondary((id \
                   26915)(content(Whitespace\" \"))))(Secondary((id \
                   26933)(content(Comment\"#err: \
                   inconsistent#\"))))(Secondary((id \
                   26934)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26938)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26939)(content(Whitespace\" \"))))(Tile((id \
                   26941)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   26942)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Grout((id 26948)(shape \
                   Convex)))(Secondary((id 26945)(content(Whitespace\" \
                   \"))))(Secondary((id 26946)(content(Whitespace\" \
                   \"))))(Secondary((id 26947)(content(Whitespace\" \
                   \")))))((Secondary((id 26949)(content(Whitespace\" \
                   \"))))(Tile((id 26950)(label([ ]))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26951)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   26952)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26954)(content(Whitespace\" \"))))(Tile((id \
                   26956)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   26957)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26959)(content(Whitespace\" \"))))(Tile((id \
                   26963)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   26966)(content(Whitespace\" \")))))))))(Secondary((id \
                   26968)(content(Whitespace\" \"))))(Secondary((id \
                   26969)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   26973)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   26974)(content(Whitespace\" \"))))(Tile((id \
                   26976)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   26977)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   26979)(content(Whitespace\" \"))))(Tile((id 26980)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Grout((id 26982)(shape \
                   Convex))))))))(Secondary((id 26983)(content(Whitespace\" \
                   \")))))((Secondary((id 26985)(content(Whitespace\" \
                   \"))))(Tile((id 26986)(label([ ]))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   26987)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   26988)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26990)(content(Whitespace\" \"))))(Tile((id \
                   26992)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   26993)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   26995)(content(Whitespace\" \"))))(Tile((id \
                   26999)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   27002)(content(Whitespace\" \")))))))))(Secondary((id \
                   27004)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   27008)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   27009)(content(Whitespace\" \"))))(Tile((id \
                   27011)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   27012)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   27014)(content(Whitespace\" \"))))(Tile((id 27015)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 27018)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   27019)(content(Whitespace\" \")))))((Secondary((id \
                   27021)(content(Whitespace\" \"))))(Tile((id 27022)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 27023)(label(1))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   27024)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   27026)(content(Whitespace\" \"))))(Tile((id \
                   27028)(label(1.))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   27029)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   27031)(content(Whitespace\" \"))))(Tile((id \
                   27035)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   27038)(content(Whitespace\" \")))))))))(Secondary((id \
                   27040)(content(Whitespace\" \"))))(Secondary((id \
                   27047)(content(Comment\"#2x err#\"))))(Secondary((id \
                   27048)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   27049)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   27053)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   27054)(content(Whitespace\" \"))))(Tile((id \
                   27056)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   27057)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   27059)(content(Whitespace\" \"))))(Tile((id 27060)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 27063)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   27064)(content(Whitespace\" \")))))((Secondary((id \
                   27066)(content(Whitespace\" \"))))(Tile((id \
                   27067)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   27070)(label(::))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 6))(sort \
                   Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 27071)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 27072)(label(2))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   27075)(content(Whitespace\" \")))))))))(Secondary((id \
                   27077)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   27081)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   27082)(content(Whitespace\" \"))))(Tile((id \
                   27084)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   27085)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   27087)(content(Whitespace\" \"))))(Tile((id 27088)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 27091)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   27092)(content(Whitespace\" \")))))((Secondary((id \
                   27094)(content(Whitespace\" \"))))(Tile((id \
                   27097)(label(1.0))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   27100)(label(::))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 6))(sort \
                   Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 27101)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 27102)(label(2))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   27105)(content(Whitespace\" \")))))))))(Secondary((id \
                   27107)(content(Whitespace\" \"))))(Secondary((id \
                   27111)(content(Comment #err#))))(Secondary((id \
                   27112)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   27116)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   27117)(content(Whitespace\" \"))))(Tile((id \
                   27119)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   27120)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   27122)(content(Whitespace\" \"))))(Tile((id 27123)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 27126)(label(Int))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   27127)(content(Whitespace\" \")))))((Secondary((id \
                   27129)(content(Whitespace\" \"))))(Tile((id \
                   27130)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   27133)(label(::))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 6))(sort \
                   Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id 27134)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 27137)(label(2.0))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   27140)(content(Whitespace\" \")))))))))(Secondary((id \
                   27142)(content(Whitespace\" \"))))(Secondary((id \
                   27146)(content(Comment #err#))))(Secondary((id \
                   27147)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   27151)(label(\"\\\"BYE\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))()))(ancestors())))(caret \
                   Outer))";
                backup_text =
                  "#Types and type error examples#\n\n\
                   let _ = unbound in #err#\n\
                   let Undefined = Undefined in # 2x err# \n\
                   let true = 2 in #err# \n\n\
                   let   = if true then 1 else 1. in #err# \n\
                   let _ = if true then 1 else 1. in #err#\n\
                   let _:   = if true then 1 else 1. in\n\
                   let _: Int = if true then 1 else 1. in #err#\n\
                   let _: Fake = if true then 1 else true in #err#\n\
                   let _, _ = if true then 1 else 1. in #2x err#\n\
                   let _, _ = (if true then 1 else 1.),    in #err#\n\
                   let _:  , _ = (if true then 1 else 1.),    in \n\
                   let [_] = [(if true then 1 else 1.)] in \n\
                   let [_] = (if true then 1 else 1.) in #2x err# \n\n\
                   (  )(if true then 1 else 1.);\n\
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
                   let    = [1, 1., true] in #err: inconsistent#\n\
                   let _ = [1, 1., true] in #err: inconsistent#\n\
                   let _:    = [1, 1., true] in \n\
                   let _: [ ] = [1, 1., true] in\n\
                   let _: [Int] = [1, 1., true] in #2x err#\n\n\
                   let _: [Int] = 1::[2] in\n\
                   let _: [Int] = 1.0::[2] in #err#\n\
                   let _: [Int] = 1::[2.0] in #err#\n\
                   \"BYE\"";
              } ) );
          ( "ADT Statics",
            ( 32371,
              {
                zipper =
                  "((selection((focus Left)(content())(mode \
                   Normal)))(backpack())(relatives((siblings(((Secondary((id \
                   29413)(content(Comment\"#Non-recursive sum/alias \
                   tests#\"))))(Secondary((id \
                   29414)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   29470)(content(Comment\"#all lines with trailing err \
                   comment should have 1 error#\"))))(Secondary((id \
                   29471)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   29505)(content(Comment\"#no other lines should have \
                   errors#\"))))(Secondary((id \
                   29506)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   29507)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   29535)(content(Comment\"#type definitions: no \
                   errors#\"))))(Secondary((id \
                   29536)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   29541)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Grout((id \
                   29546)(shape Convex)))(Secondary((id \
                   29544)(content(Whitespace\" \"))))(Secondary((id \
                   29545)(content(Whitespace\" \")))))((Grout((id 29551)(shape \
                   Convex)))(Secondary((id 29548)(content(Whitespace\" \
                   \"))))(Secondary((id 29549)(content(Whitespace\" \
                   \")))))))))(Secondary((id \
                   29553)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   29558)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   29559)(content(Whitespace\" \"))))(Tile((id \
                   29570)(label(SingleNull))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   29571)(content(Whitespace\" \")))))((Secondary((id \
                   29573)(content(Whitespace\" \"))))(Tile((id \
                   29574)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   29577)(label(One))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   29580)(content(Whitespace\" \")))))))))(Secondary((id \
                   29582)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   29587)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   29588)(content(Whitespace\" \"))))(Tile((id \
                   29595)(label(Single))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   29596)(content(Whitespace\" \")))))((Secondary((id \
                   29598)(content(Whitespace\" \"))))(Tile((id \
                   29599)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   29600)(label(F))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   29601)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   29605)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   29608)(content(Whitespace\" \")))))))))(Secondary((id \
                   29610)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   29615)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   29616)(content(Whitespace\" \"))))(Tile((id \
                   29624)(label(GoodSum))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   29625)(content(Whitespace\" \")))))((Secondary((id \
                   29627)(content(Whitespace\" \"))))(Tile((id \
                   29628)(label(A))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   29629)(content(Whitespace\" \"))))(Tile((id \
                   29630)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   29632)(content(Whitespace\" \"))))(Tile((id \
                   29633)(label(B))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   29634)(content(Whitespace\" \"))))(Tile((id \
                   29635)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   29637)(content(Whitespace\" \"))))(Tile((id \
                   29638)(label(C))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   29639)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   29643)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   29646)(content(Whitespace\" \")))))))))(Secondary((id \
                   29648)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   29653)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   29654)(content(Whitespace\" \"))))(Tile((id \
                   29662)(label(Partial))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   29663)(content(Whitespace\" \")))))((Secondary((id \
                   29665)(content(Whitespace\" \"))))(Tile((id \
                   29667)(label(Ok))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   29668)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                   29671)(shape Convex))))))))(Secondary((id \
                   29672)(content(Whitespace\" \"))))(Tile((id \
                   29673)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Grout((id 29679)(shape \
                   Convex)))(Secondary((id 29676)(content(Whitespace\" \
                   \"))))(Secondary((id 29677)(content(Whitespace\" \
                   \")))))))))(Secondary((id \
                   29681)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   29686)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   29687)(content(Whitespace\" \"))))(Tile((id \
                   29699)(label(DoubleAlias))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   29700)(content(Whitespace\" \")))))((Secondary((id \
                   29702)(content(Whitespace\" \"))))(Tile((id \
                   29709)(label(GoodSum))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   29712)(content(Whitespace\" \")))))))))(Secondary((id \
                   29714)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   29719)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   29720)(content(Whitespace\" \"))))(Tile((id \
                   29736)(label(VerticalLeading))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   29737)(content(Whitespace\" \")))))((Secondary((id \
                   29739)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   29740)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   29741)(content(Whitespace\" \"))))(Tile((id \
                   29742)(label(A))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   29743)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   29744)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   29746)(content(Whitespace\" \"))))(Tile((id \
                   29747)(label(B))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   29748)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   29756)(label(GoodSum))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   29757)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   29758)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   29760)(content(Whitespace\" \"))))(Tile((id \
                   29761)(label(C))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   29762)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   29767)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   29770)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   29774)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   29778)(content(Whitespace\" \"))))(Secondary((id \
                   29775)(content(Whitespace\" \"))))(Secondary((id \
                   29776)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   29780)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   29781)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   29822)(content(Comment\"#incorrect or incomplete type \
                   definitions#\"))))(Secondary((id \
                   29823)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   29828)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   29829)(content(Whitespace\" \"))))(Tile((id \
                   29841)(label(badTypeName))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   29842)(content(Whitespace\" \")))))((Grout((id 29848)(shape \
                   Convex)))(Secondary((id 29845)(content(Whitespace\" \
                   \"))))(Secondary((id 29846)(content(Whitespace\" \
                   \")))))))))(Secondary((id 29850)(content(Whitespace\" \
                   \"))))(Secondary((id 29873)(content(Comment\"#err: invalid \
                   type name#\"))))(Secondary((id \
                   29874)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   29879)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   29880)(content(Whitespace\" \"))))(Tile((id \
                   29882)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Grout((id \
                   29885)(shape Convex)))(Tile((id 29884)(label(,))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 14))(sort \
                   Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Grout((id 29888)(shape \
                   Convex)))(Secondary((id 29887)(content(Whitespace\" \
                   \")))))))))(Secondary((id 29889)(content(Whitespace\" \
                   \")))))((Grout((id 29895)(shape Convex)))(Secondary((id \
                   29892)(content(Whitespace\" \"))))(Secondary((id \
                   29893)(content(Whitespace\" \")))))))))(Secondary((id \
                   29897)(content(Whitespace\" \"))))(Secondary((id \
                   29920)(content(Comment\"#err: invalid type \
                   name#\"))))(Secondary((id \
                   29921)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   29926)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Grout((id \
                   29931)(shape Convex)))(Secondary((id \
                   29929)(content(Whitespace\" \"))))(Secondary((id \
                   29930)(content(Whitespace\" \")))))((Secondary((id \
                   29932)(content(Whitespace\" \"))))(Tile((id \
                   29944)(label(badTypeToken))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   29947)(content(Whitespace\" \")))))))))(Secondary((id \
                   29949)(content(Whitespace\" \"))))(Secondary((id \
                   29973)(content(Comment\"#err: invalid type \
                   token#\"))))(Secondary((id \
                   29974)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   29979)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   29980)(content(Whitespace\" \"))))(Tile((id \
                   29988)(label(NotASum))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   29989)(content(Whitespace\" \")))))((Secondary((id \
                   29991)(content(Whitespace\" \"))))(Tile((id \
                   29999)(label(NotInSum))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   30000)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   30005)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   30008)(content(Whitespace\" \")))))))))(Secondary((id \
                   30010)(content(Whitespace\" \"))))(Secondary((id \
                   30031)(content(Comment\"#err: cons not in \
                   sum#\"))))(Secondary((id \
                   30032)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30037)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   30038)(content(Whitespace\" \"))))(Tile((id \
                   30043)(label(Bool))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   30044)(content(Whitespace\" \")))))((Grout((id 30051)(shape \
                   Convex)))(Secondary((id 30047)(content(Whitespace\" \
                   \"))))(Secondary((id 30048)(content(Whitespace\" \
                   \"))))(Secondary((id 30049)(content(Whitespace\" \
                   \")))))))))(Secondary((id 30053)(content(Whitespace\" \
                   \"))))(Secondary((id 30076)(content(Comment\"#err: shadows \
                   base type#\"))))(Secondary((id \
                   30077)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30082)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   30083)(content(Whitespace\" \"))))(Tile((id \
                   30089)(label(Dupes))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   30090)(content(Whitespace\" \")))))((Secondary((id \
                   30092)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30093)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30094)(content(Whitespace\" \"))))(Tile((id \
                   30097)(label(Guy))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   30098)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   30103)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   30104)(content(Whitespace\" \"))))(Secondary((id \
                   30111)(content(Comment\"#no err#\"))))(Secondary((id \
                   30112)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30113)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30115)(content(Whitespace\" \"))))(Tile((id \
                   30118)(label(Guy))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   30119)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   30123)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   30124)(content(Whitespace\" \"))))(Secondary((id \
                   30142)(content(Comment\"#err: already \
                   used#\"))))(Secondary((id \
                   30143)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30144)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30146)(content(Whitespace\" \"))))(Tile((id \
                   30149)(label(Guy))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30152)(content(Whitespace\" \")))))))))(Secondary((id \
                   30154)(content(Whitespace\" \"))))(Secondary((id \
                   30172)(content(Comment\"#err: already \
                   used#\"))))(Secondary((id \
                   30173)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30178)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   30179)(content(Whitespace\" \"))))(Tile((id \
                   30187)(label(BadCons))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   30188)(content(Whitespace\" \")))))((Secondary((id \
                   30190)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30191)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30192)(content(Whitespace\" \"))))(Tile((id \
                   30194)(label(Um))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   30195)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   30203)(label(Unbound))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   30204)(content(Whitespace\" \"))))(Secondary((id \
                   30226)(content(Comment\"#err: unbound type \
                   var#\"))))(Secondary((id \
                   30227)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30228)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Grout((id 30232)(shape \
                   Convex))))))))(Tile((id 30238)(label(valid))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   30239)(content(Whitespace\" \"))))(Secondary((id \
                   30252)(content(Comment\"#err: invalid#\"))))(Secondary((id \
                   30253)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30254)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   30256)(content(Whitespace\" \"))))(Tile((id \
                   30260)(label(Bool))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   30261)(content(Whitespace\" \"))))(Secondary((id \
                   30291)(content(Comment\"#err: expected cons found \
                   type#\"))))(Secondary((id \
                   30292)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30293)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   30295)(content(Whitespace\" \"))))(Tile((id \
                   30298)(label(Int))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   30299)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   30303)(label(Int))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   30304)(content(Whitespace\" \"))))(Secondary((id \
                   30334)(content(Comment\"#err: expected cons found \
                   type#\"))))(Secondary((id \
                   30335)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30336)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   30338)(content(Whitespace\" \"))))(Tile((id \
                   30339)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Grout((id \
                   30341)(shape Convex))))))))(Tile((id \
                   30342)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   30346)(label(Int))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   30347)(content(Whitespace\" \"))))(Secondary((id \
                   30377)(content(Comment\"#err: expected cons found \
                   type#\"))))(Secondary((id \
                   30378)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30379)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   5))(sort Exp))((shape(Concave 5))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   30381)(content(Whitespace\" \"))))(Tile((id \
                   30382)(label(A))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   30383)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   30388)(label(Bool))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   30389)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   30393)(label(Int))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Grout((id \
                   30396)(shape Concave)))(Tile((id \
                   30397)(label(in))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   30398)(content(Whitespace\" \"))))(Secondary((id \
                   30427)(content(Comment\"#err: expected cons found \
                   app#\"))))(Secondary((id \
                   30428)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   30429)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   30477)(content(Comment\"#sums in compound aliases dont add \
                   ctrs to scope#\"))))(Secondary((id \
                   30478)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   30533)(content(Comment\"#but compound alias types should \
                   propagate analytically#\"))))(Secondary((id \
                   30534)(content(Whitespace\"\\226\\143\\142\"))))(Grout((id \
                   30536)(shape Concave)))(Tile((id 30540)(label(type = \
                   in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 16))(sort \
                   Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   30541)(content(Whitespace\" \"))))(Tile((id \
                   30555)(label(CompoundAlias))(mold((out \
                   TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                   Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   30556)(content(Whitespace\" \")))))((Secondary((id \
                   30558)(content(Whitespace\" \"))))(Tile((id \
                   30559)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   30562)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   30563)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30565)(content(Whitespace\" \"))))(Tile((id \
                   30574)(label(Anonymous))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30575)(content(Whitespace\" \"))))(Tile((id \
                   30576)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30578)(content(Whitespace\" \"))))(Tile((id \
                   30581)(label(Sum))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   30584)(content(Whitespace\" \")))))))))(Secondary((id \
                   30586)(content(Whitespace\" \"))))(Secondary((id \
                   30587)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30591)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   30592)(content(Whitespace\" \"))))(Tile((id \
                   30594)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   30595)(content(Whitespace\" \")))))((Secondary((id \
                   30597)(content(Whitespace\" \"))))(Tile((id \
                   30598)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   30599)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   30600)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   30602)(content(Whitespace\" \"))))(Tile((id \
                   30605)(label(Sum))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   30608)(content(Whitespace\" \")))))))))(Secondary((id \
                   30610)(content(Whitespace\" \"))))(Secondary((id \
                   30627)(content(Comment\"#err: not \
                   defined#\"))))(Secondary((id \
                   30628)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30632)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   30633)(content(Whitespace\" \"))))(Tile((id \
                   30635)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   30636)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30638)(content(Whitespace\" \"))))(Tile((id \
                   30651)(label(CompoundAlias))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30652)(content(Whitespace\" \")))))((Secondary((id \
                   30654)(content(Whitespace\" \"))))(Tile((id \
                   30655)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   30656)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   30657)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   30659)(content(Whitespace\" \"))))(Tile((id \
                   30662)(label(Sum))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   30665)(content(Whitespace\" \")))))))))(Secondary((id \
                   30667)(content(Whitespace\" \"))))(Secondary((id \
                   30676)(content(Comment\"#no error#\"))))(Secondary((id \
                   30677)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30682)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   30683)(content(Whitespace\" \"))))(Tile((id \
                   30688)(label(Yorp))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   30689)(content(Whitespace\" \")))))((Secondary((id \
                   30691)(content(Whitespace\" \"))))(Tile((id \
                   30694)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30695)(content(Whitespace\" \"))))(Tile((id \
                   30698)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30699)(content(Whitespace\" \"))))(Tile((id \
                   30700)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   30706)(label(Inside))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30707)(content(Whitespace\" \"))))(Tile((id \
                   30708)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30710)(content(Whitespace\" \"))))(Tile((id \
                   30716)(label(Ouside))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   30719)(content(Whitespace\" \")))))))))(Secondary((id \
                   30721)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30725)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   30726)(content(Whitespace\" \"))))(Tile((id \
                   30728)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   30729)(content(Whitespace\" \")))))((Secondary((id \
                   30731)(content(Whitespace\" \"))))(Tile((id \
                   30735)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   30736)(content(Whitespace\" \"))))(Tile((id \
                   30738)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   30739)(content(Whitespace\" \")))))))))(Secondary((id \
                   30742)(content(Whitespace\" \"))))(Tile((id \
                   30748)(label(Inside))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   30751)(content(Whitespace\" \")))))))))(Secondary((id \
                   30753)(content(Whitespace\" \"))))(Secondary((id \
                   30770)(content(Comment\"#err: not \
                   defined#\"))))(Secondary((id \
                   30771)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30775)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   30776)(content(Whitespace\" \"))))(Tile((id \
                   30778)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   30779)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30781)(content(Whitespace\" \"))))(Tile((id \
                   30785)(label(Yorp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30786)(content(Whitespace\" \")))))((Secondary((id \
                   30788)(content(Whitespace\" \"))))(Tile((id \
                   30792)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
                   Convex)(sort Exp))((shape(Concave 13))(sort \
                   Exp))))))(shards(0 1))(children(((Secondary((id \
                   30793)(content(Whitespace\" \"))))(Tile((id \
                   30795)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   30796)(content(Whitespace\" \")))))))))(Secondary((id \
                   30799)(content(Whitespace\" \"))))(Tile((id \
                   30805)(label(Inside))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   30808)(content(Whitespace\" \")))))))))(Secondary((id \
                   30810)(content(Whitespace\" \"))))(Secondary((id \
                   30819)(content(Comment\"#no error#\"))))(Secondary((id \
                   30820)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30825)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   30826)(content(Whitespace\" \"))))(Tile((id \
                   30832)(label(Gargs))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   30833)(content(Whitespace\" \")))))((Secondary((id \
                   30835)(content(Whitespace\" \"))))(Tile((id 30836)(label([ \
                   ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                   Typ))((shape Convex)(sort Typ))))))(shards(0 \
                   1))(children(((Tile((id 30842)(label(BigGuy))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30843)(content(Whitespace\" \"))))(Tile((id \
                   30844)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30846)(content(Whitespace\" \"))))(Tile((id \
                   30851)(label(Small))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   30854)(content(Whitespace\" \")))))))))(Secondary((id \
                   30856)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30860)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   30861)(content(Whitespace\" \"))))(Tile((id \
                   30863)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   30864)(content(Whitespace\" \")))))((Secondary((id \
                   30866)(content(Whitespace\" \"))))(Tile((id \
                   30872)(label(BigGuy))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   30875)(content(Whitespace\" \")))))))))(Secondary((id \
                   30877)(content(Whitespace\" \"))))(Secondary((id \
                   30894)(content(Comment\"#err: not \
                   defined#\"))))(Secondary((id \
                   30895)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30899)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   30900)(content(Whitespace\" \"))))(Tile((id \
                   30902)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   30903)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30905)(content(Whitespace\" \"))))(Tile((id \
                   30910)(label(Gargs))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30911)(content(Whitespace\" \")))))((Secondary((id \
                   30913)(content(Whitespace\" \"))))(Tile((id 30914)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 30920)(label(BigGuy))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   30923)(content(Whitespace\" \")))))))))(Secondary((id \
                   30925)(content(Whitespace\" \"))))(Secondary((id \
                   30934)(content(Comment\"#no error#\"))))(Secondary((id \
                   30935)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   30939)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   30940)(content(Whitespace\" \"))))(Tile((id \
                   30942)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   30943)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30945)(content(Whitespace\" \"))))(Tile((id \
                   30950)(label(Gargs))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   30951)(content(Whitespace\" \")))))((Secondary((id \
                   30953)(content(Whitespace\" \"))))(Tile((id \
                   30959)(label(BigGuy))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   30960)(content(Whitespace\" \"))))(Tile((id \
                   30963)(label(::))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 6))(sort \
                   Exp))((shape(Concave 6))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   30964)(content(Whitespace\" \"))))(Tile((id 30965)(label([ \
                   ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                   Exp))((shape Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Tile((id 30971)(label(BigGuy))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   30974)(content(Whitespace\" \")))))))))(Secondary((id \
                   30976)(content(Whitespace\" \"))))(Secondary((id \
                   30985)(content(Comment\"#no error#\"))))(Secondary((id \
                   30986)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   30987)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   31030)(content(Comment\"#unbound tyvars treated as \
                   unknown-typehole#\"))))(Secondary((id \
                   31031)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31035)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31036)(content(Whitespace\" \"))))(Tile((id \
                   31038)(label(a))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   31039)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31043)(label(Bad))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31044)(content(Whitespace\" \")))))((Secondary((id \
                   31046)(content(Whitespace\" \"))))(Tile((id \
                   31047)(label(0))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   31050)(content(Whitespace\" \")))))))))(Secondary((id \
                   31052)(content(Whitespace\" \"))))(Tile((id \
                   31053)(label(a))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   31054)(content(Whitespace\" \"))))(Tile((id \
                   31057)(label(==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   31058)(content(Whitespace\" \"))))(Tile((id \
                   31059)(label(0))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   31060)(label(\";\"))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 10))(sort \
                   Exp))((shape(Concave 10))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   31062)(content(Whitespace\" \"))))(Secondary((id \
                   31077)(content(Comment\"#err: not \
                   bound#\"))))(Secondary((id \
                   31078)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   31079)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   31111)(content(Comment\"#non-sum-types cant be \
                   recursive#\"))))(Secondary((id \
                   31112)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31117)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31118)(content(Whitespace\" \"))))(Tile((id \
                   31122)(label(Lol))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   31123)(content(Whitespace\" \")))))((Secondary((id \
                   31125)(content(Whitespace\" \"))))(Tile((id \
                   31128)(label(Lol))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31131)(content(Whitespace\" \")))))))))(Secondary((id \
                   31133)(content(Whitespace\" \"))))(Secondary((id \
                   31148)(content(Comment\"#err: not \
                   bound#\"))))(Secondary((id \
                   31149)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   31150)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   31180)(content(Comment\"#no errors: analytic \
                   shadowing#\"))))(Secondary((id \
                   31181)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31186)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31187)(content(Whitespace\" \"))))(Tile((id \
                   31193)(label(Tork1))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   31194)(content(Whitespace\" \")))))((Secondary((id \
                   31196)(content(Whitespace\" \"))))(Tile((id \
                   31197)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31201)(label(Blob))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31204)(content(Whitespace\" \")))))))))(Secondary((id \
                   31206)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31211)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31212)(content(Whitespace\" \"))))(Tile((id \
                   31218)(label(Tork2))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   31219)(content(Whitespace\" \")))))((Secondary((id \
                   31221)(content(Whitespace\" \"))))(Tile((id \
                   31222)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31226)(label(Blob))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31229)(content(Whitespace\" \")))))))))(Secondary((id \
                   31231)(content(Whitespace\" \"))))(Secondary((id \
                   31232)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31236)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31237)(content(Whitespace\" \"))))(Tile((id \
                   31239)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   31240)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31246)(label(Tork1))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31247)(content(Whitespace\" \")))))((Secondary((id \
                   31249)(content(Whitespace\" \"))))(Tile((id \
                   31253)(label(Blob))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   31256)(content(Whitespace\" \")))))))))(Secondary((id \
                   31258)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   31259)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   31276)(content(Comment\"#exp tests: \
                   happy#\"))))(Secondary((id \
                   31277)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31282)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31283)(content(Whitespace\" \"))))(Tile((id \
                   31290)(label(YoDawg))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   31291)(content(Whitespace\" \")))))((Secondary((id \
                   31293)(content(Whitespace\" \"))))(Secondary((id \
                   31294)(content(Whitespace\" \"))))(Tile((id \
                   31296)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31297)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   31301)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   31302)(content(Whitespace\" \"))))(Tile((id \
                   31303)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31305)(content(Whitespace\" \"))))(Tile((id \
                   31307)(label(Bo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31308)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   31312)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Tile((id \
                   31313)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31315)(content(Whitespace\" \"))))(Tile((id \
                   31319)(label(Dawg))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31320)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   31325)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   31328)(content(Whitespace\" \")))))))))(Secondary((id \
                   31330)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31334)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31335)(content(Whitespace\" \"))))(Tile((id \
                   31337)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   31338)(content(Whitespace\" \")))))((Secondary((id \
                   31340)(content(Whitespace\" \"))))(Tile((id \
                   31342)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   31343)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   31345)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   31348)(content(Whitespace\" \")))))))))(Secondary((id \
                   31350)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31354)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31355)(content(Whitespace\" \"))))(Tile((id \
                   31357)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   31358)(content(Whitespace\" \"))))(Tile((id \
                   31359)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31361)(content(Whitespace\" \"))))(Tile((id \
                   31367)(label(YoDawg))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31368)(content(Whitespace\" \")))))((Secondary((id \
                   31370)(content(Whitespace\" \"))))(Tile((id \
                   31372)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   31373)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   31375)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   31378)(content(Whitespace\" \")))))))))(Secondary((id \
                   31380)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31384)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31385)(content(Whitespace\" \"))))(Tile((id \
                   31387)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   31388)(content(Whitespace\" \"))))(Tile((id \
                   31389)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31391)(content(Whitespace\" \"))))(Tile((id \
                   31392)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31394)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31395)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   31400)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   31401)(content(Whitespace\" \")))))((Secondary((id \
                   31403)(content(Whitespace\" \"))))(Tile((id \
                   31405)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   31406)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   31411)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   31414)(content(Whitespace\" \")))))))))(Secondary((id \
                   31416)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31420)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31421)(content(Whitespace\" \"))))(Tile((id \
                   31423)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   31424)(content(Whitespace\" \"))))(Tile((id \
                   31425)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31427)(content(Whitespace\" \"))))(Tile((id \
                   31428)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   31430)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31431)(content(Whitespace\" \"))))(Tile((id \
                   31432)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31434)(content(Whitespace\" \"))))(Tile((id \
                   31438)(label(Dawg))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31439)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31441)(content(Whitespace\" \"))))(Tile((id \
                   31444)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   31445)(content(Whitespace\" \")))))((Secondary((id \
                   31447)(content(Whitespace\" \"))))(Tile((id \
                   31448)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   31452)(label(Dawg))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   31453)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   31455)(label(5))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   31458)(content(Whitespace\" \")))))))))(Secondary((id \
                   31460)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31464)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31465)(content(Whitespace\" \"))))(Tile((id \
                   31467)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   31468)(content(Whitespace\" \"))))(Tile((id \
                   31469)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31471)(content(Whitespace\" \"))))(Tile((id \
                   31482)(label(DoubleAlias))(mold((out \
                   Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31483)(content(Whitespace\" \")))))((Secondary((id \
                   31485)(content(Whitespace\" \"))))(Tile((id \
                   31486)(label(C))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   31487)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   31489)(label(4))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   31492)(content(Whitespace\" \")))))))))(Secondary((id \
                   31494)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   31495)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   31513)(content(Comment\"#exp tests: \
                   errors#\"))))(Secondary((id \
                   31514)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31518)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31519)(content(Whitespace\" \"))))(Tile((id \
                   31521)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   31522)(content(Whitespace\" \")))))((Secondary((id \
                   31524)(content(Whitespace\" \"))))(Tile((id \
                   31525)(label(2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   31526)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   31528)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   31531)(content(Whitespace\" \")))))))))(Secondary((id \
                   31533)(content(Whitespace\" \"))))(Secondary((id \
                   31556)(content(Comment\"#err: incons with \
                   arrow#\"))))(Secondary((id \
                   31557)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31561)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31562)(content(Whitespace\" \"))))(Tile((id \
                   31564)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   31565)(content(Whitespace\" \")))))((Secondary((id \
                   31567)(content(Whitespace\" \"))))(Tile((id \
                   31576)(label(Undefined))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   31577)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   31579)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   31582)(content(Whitespace\" \")))))))))(Secondary((id \
                   31584)(content(Whitespace\" \"))))(Secondary((id \
                   31604)(content(Comment\"#err: cons \
                   undefined#\"))))(Secondary((id \
                   31605)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31609)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31610)(content(Whitespace\" \"))))(Tile((id \
                   31612)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   31613)(content(Whitespace\" \")))))((Secondary((id \
                   31615)(content(Whitespace\" \"))))(Tile((id \
                   31616)(label(B))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   31617)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   31622)(label(\"\\\"lol\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   31625)(content(Whitespace\" \")))))))))(Secondary((id \
                   31627)(content(Whitespace\" \"))))(Secondary((id \
                   31644)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   31645)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31649)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31650)(content(Whitespace\" \"))))(Tile((id \
                   31652)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   31653)(content(Whitespace\" \"))))(Tile((id \
                   31654)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31656)(content(Whitespace\" \"))))(Tile((id \
                   31657)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31659)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31660)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   31665)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   31666)(content(Whitespace\" \")))))((Secondary((id \
                   31668)(content(Whitespace\" \"))))(Tile((id \
                   31670)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   31673)(content(Whitespace\" \")))))))))(Secondary((id \
                   31675)(content(Whitespace\" \"))))(Secondary((id \
                   31692)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   31693)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31697)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31698)(content(Whitespace\" \"))))(Tile((id \
                   31700)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   31701)(content(Whitespace\" \"))))(Tile((id \
                   31702)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31704)(content(Whitespace\" \"))))(Tile((id \
                   31705)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31707)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31708)(content(Whitespace\" \")))))((Secondary((id \
                   31710)(content(Whitespace\" \"))))(Tile((id \
                   31712)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   31713)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   31718)(label(\"\\\"lol\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   31721)(content(Whitespace\" \")))))))))(Secondary((id \
                   31723)(content(Whitespace\" \"))))(Secondary((id \
                   31740)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   31741)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31745)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31746)(content(Whitespace\" \"))))(Tile((id \
                   31748)(label(_))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   31749)(content(Whitespace\" \"))))(Tile((id \
                   31750)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31752)(content(Whitespace\" \"))))(Tile((id \
                   31753)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31756)(label(One))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31757)(content(Whitespace\" \")))))((Secondary((id \
                   31759)(content(Whitespace\" \"))))(Tile((id \
                   31761)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   31762)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   31764)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   31767)(content(Whitespace\" \")))))))))(Secondary((id \
                   31769)(content(Whitespace\" \"))))(Secondary((id \
                   31786)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   31787)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   31788)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   31839)(content(Comment\"#pat tests: happy (but refutable \
                   patterns so weird)#\"))))(Secondary((id \
                   31840)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31844)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31845)(content(Whitespace\" \"))))(Tile((id \
                   31848)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   31849)(content(Whitespace\" \")))))((Secondary((id \
                   31851)(content(Whitespace\" \"))))(Tile((id \
                   31853)(label(Bo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   31856)(content(Whitespace\" \")))))))))(Secondary((id \
                   31858)(content(Whitespace\" \"))))(Secondary((id \
                   31879)(content(Comment\"#kind of a weird \
                   edge#\"))))(Secondary((id \
                   31880)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31884)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31885)(content(Whitespace\" \"))))(Tile((id \
                   31888)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   31889)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   31891)(label(1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   31892)(content(Whitespace\" \")))))((Secondary((id \
                   31894)(content(Whitespace\" \"))))(Tile((id \
                   31898)(label(Dawg))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   31899)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   31904)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   31907)(content(Whitespace\" \")))))))))(Secondary((id \
                   31909)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31913)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31914)(content(Whitespace\" \"))))(Tile((id \
                   31917)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   31918)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   31920)(label(1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   31921)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31923)(content(Whitespace\" \"))))(Tile((id \
                   31929)(label(YoDawg))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31930)(content(Whitespace\" \")))))((Secondary((id \
                   31932)(content(Whitespace\" \"))))(Tile((id \
                   31934)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   31935)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   31937)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   31940)(content(Whitespace\" \")))))))))(Secondary((id \
                   31942)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31946)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31947)(content(Whitespace\" \"))))(Tile((id \
                   31950)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   31951)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   31953)(label(1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   31954)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31956)(content(Whitespace\" \"))))(Tile((id \
                   31957)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31959)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31960)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   31964)(label(Int))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   31965)(content(Whitespace\" \")))))((Secondary((id \
                   31967)(content(Whitespace\" \"))))(Tile((id \
                   31969)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   31970)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   31972)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   31975)(content(Whitespace\" \")))))))))(Secondary((id \
                   31977)(content(Whitespace\" \"))))(Secondary((id \
                   31978)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   31982)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   31983)(content(Whitespace\" \"))))(Tile((id \
                   31986)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   31987)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31989)(content(Whitespace\" \"))))(Tile((id \
                   31990)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   31992)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   31993)(content(Whitespace\" \")))))((Secondary((id \
                   31995)(content(Whitespace\" \"))))(Tile((id \
                   31997)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   32000)(content(Whitespace\" \")))))))))(Secondary((id \
                   32002)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   32003)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   32021)(content(Comment\"#pat tests: \
                   errors#\"))))(Secondary((id \
                   32022)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   32026)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   32027)(content(Whitespace\" \"))))(Tile((id \
                   32029)(label(2))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   32030)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   32032)(label(1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   32033)(content(Whitespace\" \")))))((Secondary((id \
                   32035)(content(Whitespace\" \"))))(Tile((id \
                   32036)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   32039)(content(Whitespace\" \")))))))))(Secondary((id \
                   32041)(content(Whitespace\" \"))))(Secondary((id \
                   32064)(content(Comment\"#err: incons with \
                   arrow#\"))))(Secondary((id \
                   32065)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   32069)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   32070)(content(Whitespace\" \"))))(Tile((id \
                   32081)(label(NotDefined))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   32082)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   32084)(label(1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   32085)(content(Whitespace\" \")))))((Secondary((id \
                   32087)(content(Whitespace\" \"))))(Tile((id \
                   32088)(label(3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   32091)(content(Whitespace\" \")))))))))(Secondary((id \
                   32093)(content(Whitespace\" \"))))(Secondary((id \
                   32113)(content(Comment\"#err: cons \
                   undefined#\"))))(Secondary((id \
                   32114)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   32118)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   32119)(content(Whitespace\" \"))))(Tile((id \
                   32122)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   32123)(content(Whitespace\" \")))))((Secondary((id \
                   32125)(content(Whitespace\" \"))))(Tile((id \
                   32129)(label(Dawg))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   32132)(content(Whitespace\" \")))))))))(Secondary((id \
                   32134)(content(Whitespace\" \"))))(Secondary((id \
                   32151)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   32152)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   32156)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   32157)(content(Whitespace\" \"))))(Tile((id \
                   32160)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   32161)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   32166)(label(true))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   32167)(content(Whitespace\" \")))))((Secondary((id \
                   32169)(content(Whitespace\" \"))))(Tile((id \
                   32173)(label(Dawg))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   32174)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   32179)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   32182)(content(Whitespace\" \")))))))))(Secondary((id \
                   32184)(content(Whitespace\" \"))))(Secondary((id \
                   32201)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   32202)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   32206)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   32207)(content(Whitespace\" \"))))(Tile((id \
                   32210)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   32211)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   32213)(content(Whitespace\" \"))))(Tile((id \
                   32219)(label(YoDawg))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   32220)(content(Whitespace\" \")))))((Secondary((id \
                   32222)(content(Whitespace\" \"))))(Tile((id \
                   32224)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   32225)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   32227)(label(1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   32230)(content(Whitespace\" \")))))))))(Secondary((id \
                   32232)(content(Whitespace\" \"))))(Secondary((id \
                   32249)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   32250)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   32254)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   32255)(content(Whitespace\" \"))))(Tile((id \
                   32258)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   32259)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   32261)(label(1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   32262)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   32264)(content(Whitespace\" \"))))(Tile((id \
                   32265)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   32267)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   32268)(content(Whitespace\" \")))))((Secondary((id \
                   32270)(content(Whitespace\" \"))))(Tile((id \
                   32272)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   32275)(content(Whitespace\" \")))))))))(Secondary((id \
                   32277)(content(Whitespace\" \"))))(Secondary((id \
                   32294)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   32295)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   32299)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   32300)(content(Whitespace\" \"))))(Tile((id \
                   32303)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   32304)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   32306)(label(1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Tile((id \
                   32307)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   32309)(content(Whitespace\" \"))))(Tile((id \
                   32310)(label(+))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   32312)(label(Yo))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   32313)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   32318)(label(Bool))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   32319)(content(Whitespace\" \")))))((Secondary((id \
                   32321)(content(Whitespace\" \"))))(Tile((id \
                   32323)(label(Yo))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   32324)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   32329)(label(true))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   32332)(content(Whitespace\" \")))))))))(Secondary((id \
                   32334)(content(Whitespace\" \"))))(Secondary((id \
                   32351)(content(Comment\"#err: type \
                   incons#\"))))(Secondary((id \
                   32352)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   32369)(label(\"\\\"Thats all, folks\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   32370)(content(Whitespace\"\\226\\143\\142\")))))()))(ancestors())))(caret \
                   Outer))";
                backup_text =
                  "#Non-recursive sum/alias tests#\n\
                   #all lines with trailing err comment should have 1 error#\n\
                   #no other lines should have errors#\n\n\
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
                   + C(Bool->Bool)  \n\
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
                  \ type CompoundAlias = (Int, Anonymous + Sum) in \n\
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
          ( "ADT Dynamics",
            ( 3445,
              {
                zipper =
                  "((selection((focus Left)(content())(mode \
                   Normal)))(backpack())(relatives((siblings(((Secondary((id \
                   2515)(content(Comment\"#recursive sum type dynamics \
                   tests#\"))))(Secondary((id \
                   2516)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2575)(content(Comment\"#all calls should evaluate fully \
                   with no exns or cast fails#\"))))(Secondary((id \
                   2576)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2581)(label(type = in))(mold((out Exp)(in_(TPat \
                   Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2582)(content(Whitespace\" \"))))(Tile((id \
                   2586)(label(Exp))(mold((out TPat)(in_())(nibs(((shape \
                   Convex)(sort TPat))((shape Convex)(sort \
                   TPat))))))(shards(0))(children())))(Secondary((id \
                   2587)(content(Whitespace\" \")))))((Secondary((id \
                   2589)(content(Whitespace\" \"))))(Tile((id \
                   2592)(label(Var))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   2593)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   2600)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2601)(content(Whitespace\" \"))))(Tile((id \
                   2602)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   10))(sort Typ))((shape(Concave 10))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2604)(content(Whitespace\" \"))))(Tile((id \
                   2607)(label(Lam))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   2608)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   2615)(label(String))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   2616)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2618)(content(Whitespace\" \"))))(Tile((id \
                   2621)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   2624)(content(Whitespace\" \")))))))))(Secondary((id \
                   2626)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   2627)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2631)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2632)(content(Whitespace\" \"))))(Tile((id \
                   2635)(label(s0))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2636)(content(Whitespace\" \"))))(Tile((id \
                   2637)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2639)(content(Whitespace\" \"))))(Tile((id \
                   2640)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                   2643)(shape Convex)))(Tile((id 2642)(label(,))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 14))(sort \
                   Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2644)(content(Whitespace\" \"))))(Grout((id 2647)(shape \
                   Convex)))(Tile((id 2646)(label(,))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 14))(sort \
                   Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Grout((id 2650)(shape \
                   Convex)))(Secondary((id 2649)(content(Whitespace\" \
                   \")))))))))(Secondary((id 2651)(content(Whitespace\" \
                   \"))))(Tile((id 2654)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Grout((id 2658)(shape \
                   Convex)))(Secondary((id 2656)(content(Whitespace\" \
                   \"))))(Secondary((id 2657)(content(Whitespace\" \
                   \")))))((Secondary((id 2659)(content(Whitespace\" \
                   \"))))(Tile((id 2663)(label(fun ->))(mold((out \
                   Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2664)(content(Whitespace\" \
                   \"))))(Tile((id 2666)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   2667)(label(e))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2668)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2670)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2671)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2673)(label(v))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   2674)(content(Whitespace\" \")))))))))(Secondary((id \
                   2677)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2682)(label(case end))(mold((out \
                   Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2683)(content(Whitespace\" \
                   \"))))(Tile((id 2685)(label(e))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2686)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2687)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2689)(content(Whitespace\" \
                   \"))))(Tile((id 2692)(label(Var))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2693)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   2695)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   2696)(content(Whitespace\" \")))))))))(Secondary((id \
                   2699)(content(Whitespace\" \"))))(Tile((id \
                   2700)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2703)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2704)(content(Whitespace\" \"))))(Tile((id \
                   2706)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2707)(content(Whitespace\" \"))))(Tile((id \
                   2711)(label($==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2712)(content(Whitespace\" \"))))(Tile((id \
                   2713)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2716)(content(Whitespace\" \")))))((Secondary((id \
                   2720)(content(Whitespace\" \"))))(Tile((id \
                   2721)(label(v))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2724)(content(Whitespace\" \")))))))))(Secondary((id \
                   2728)(content(Whitespace\" \"))))(Tile((id \
                   2729)(label(e))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2730)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2731)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2733)(content(Whitespace\" \
                   \"))))(Tile((id 2736)(label(Lam))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2737)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   2739)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2740)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2742)(content(Whitespace\" \"))))(Tile((id \
                   2744)(label(e1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   2745)(content(Whitespace\" \")))))))))(Secondary((id \
                   2748)(content(Whitespace\" \"))))(Tile((id \
                   2749)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2752)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2753)(content(Whitespace\" \"))))(Tile((id \
                   2755)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2756)(content(Whitespace\" \"))))(Tile((id \
                   2760)(label($==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2761)(content(Whitespace\" \"))))(Tile((id \
                   2762)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2765)(content(Whitespace\" \")))))((Secondary((id \
                   2769)(content(Whitespace\" \"))))(Tile((id \
                   2770)(label(e))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2773)(content(Whitespace\" \")))))))))(Secondary((id \
                   2777)(content(Whitespace\" \"))))(Tile((id \
                   2780)(label(Lam))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2781)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2783)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2784)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2786)(content(Whitespace\" \"))))(Tile((id \
                   2788)(label(s0))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2789)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2792)(label(e1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2793)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2795)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2796)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2798)(label(v))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                   2802)(content(Whitespace\" \"))))(Secondary((id \
                   2799)(content(Whitespace\" \"))))(Secondary((id \
                   2800)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   2806)(content(Whitespace\" \")))))))))(Secondary((id \
                   2808)(content(Whitespace\" \"))))(Secondary((id \
                   2809)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2813)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2814)(content(Whitespace\" \"))))(Tile((id \
                   2817)(label(s1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2818)(content(Whitespace\" \"))))(Tile((id \
                   2819)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2821)(content(Whitespace\" \"))))(Tile((id \
                   2822)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                   2825)(shape Convex)))(Tile((id 2824)(label(,))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 14))(sort \
                   Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2826)(content(Whitespace\" \"))))(Grout((id 2829)(shape \
                   Convex)))(Tile((id 2828)(label(,))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 14))(sort \
                   Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Grout((id 2832)(shape \
                   Convex)))(Secondary((id 2831)(content(Whitespace\" \
                   \")))))))))(Secondary((id 2833)(content(Whitespace\" \
                   \"))))(Tile((id 2836)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2837)(content(Whitespace\" \"))))(Tile((id \
                   2840)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   2841)(content(Whitespace\" \")))))((Secondary((id \
                   2843)(content(Whitespace\" \"))))(Tile((id 2847)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2848)(content(Whitespace\" \
                   \"))))(Tile((id 2850)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   2851)(label(e))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2852)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2854)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2855)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2857)(label(v))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   2858)(content(Whitespace\" \")))))))))(Secondary((id \
                   2861)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2866)(label(case end))(mold((out \
                   Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2867)(content(Whitespace\" \
                   \"))))(Tile((id 2869)(label(e))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2870)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2871)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2873)(content(Whitespace\" \
                   \"))))(Tile((id 2876)(label(Var))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2877)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   2879)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   2880)(content(Whitespace\" \")))))))))(Secondary((id \
                   2883)(content(Whitespace\" \"))))(Tile((id \
                   2884)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2887)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2888)(content(Whitespace\" \"))))(Tile((id \
                   2890)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2891)(content(Whitespace\" \"))))(Tile((id \
                   2895)(label($==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2896)(content(Whitespace\" \"))))(Tile((id \
                   2897)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2900)(content(Whitespace\" \")))))((Secondary((id \
                   2904)(content(Whitespace\" \"))))(Tile((id \
                   2905)(label(v))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2908)(content(Whitespace\" \")))))))))(Secondary((id \
                   2912)(content(Whitespace\" \"))))(Tile((id \
                   2913)(label(e))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   2914)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2915)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 2917)(content(Whitespace\" \
                   \"))))(Tile((id 2920)(label(Lam))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   2921)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   2923)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   2924)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   2926)(content(Whitespace\" \"))))(Tile((id \
                   2928)(label(e1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   2929)(content(Whitespace\" \")))))))))(Secondary((id \
                   2932)(content(Whitespace\" \"))))(Tile((id \
                   2933)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2936)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2937)(content(Whitespace\" \"))))(Tile((id \
                   2939)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2940)(content(Whitespace\" \"))))(Tile((id \
                   2944)(label($==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2945)(content(Whitespace\" \"))))(Tile((id \
                   2946)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2949)(content(Whitespace\" \")))))((Secondary((id \
                   2953)(content(Whitespace\" \"))))(Tile((id \
                   2954)(label(e))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2957)(content(Whitespace\" \")))))))))(Secondary((id \
                   2961)(content(Whitespace\" \"))))(Tile((id \
                   2964)(label(Lam))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2965)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2967)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2968)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   2970)(content(Whitespace\" \"))))(Tile((id \
                   2972)(label(s1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2973)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   2976)(label(e1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2977)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2979)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2980)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   2982)(label(v))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                   2985)(content(Whitespace\" \"))))(Secondary((id \
                   2983)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   2989)(content(Whitespace\" \")))))))))(Secondary((id \
                   2991)(content(Whitespace\" \"))))(Secondary((id \
                   2992)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   2996)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   2997)(content(Whitespace\" \"))))(Tile((id \
                   3000)(label(s2))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3001)(content(Whitespace\" \"))))(Tile((id \
                   3002)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3004)(content(Whitespace\" \"))))(Tile((id \
                   3005)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   3008)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   3009)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3011)(content(Whitespace\" \"))))(Grout((id 3014)(shape \
                   Convex)))(Tile((id 3013)(label(,))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 14))(sort \
                   Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Grout((id 3017)(shape \
                   Convex)))(Secondary((id 3016)(content(Whitespace\" \
                   \")))))))))(Secondary((id 3018)(content(Whitespace\" \
                   \"))))(Tile((id 3021)(label(->))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 6))(sort \
                   Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3022)(content(Whitespace\" \"))))(Tile((id \
                   3025)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3026)(content(Whitespace\" \")))))((Secondary((id \
                   3028)(content(Whitespace\" \"))))(Tile((id 3032)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3033)(content(Whitespace\" \
                   \"))))(Tile((id 3035)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3036)(label(e))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3037)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3039)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3040)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3042)(label(v))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   3043)(content(Whitespace\" \")))))))))(Secondary((id \
                   3046)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3051)(label(case end))(mold((out \
                   Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3052)(content(Whitespace\" \
                   \"))))(Tile((id 3054)(label(e))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3055)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3056)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3058)(content(Whitespace\" \
                   \"))))(Tile((id 3061)(label(Var))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3062)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3064)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   3065)(content(Whitespace\" \")))))))))(Secondary((id \
                   3068)(content(Whitespace\" \"))))(Tile((id \
                   3069)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3072)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3073)(content(Whitespace\" \"))))(Tile((id \
                   3075)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3076)(content(Whitespace\" \"))))(Tile((id \
                   3080)(label($==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3081)(content(Whitespace\" \"))))(Tile((id \
                   3082)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3085)(content(Whitespace\" \")))))((Secondary((id \
                   3089)(content(Whitespace\" \"))))(Tile((id \
                   3090)(label(v))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3093)(content(Whitespace\" \")))))))))(Secondary((id \
                   3097)(content(Whitespace\" \"))))(Tile((id \
                   3098)(label(e))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   3099)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3100)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3102)(content(Whitespace\" \
                   \"))))(Tile((id 3105)(label(Lam))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3106)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3108)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3109)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3111)(content(Whitespace\" \"))))(Tile((id \
                   3113)(label(e1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   3114)(content(Whitespace\" \")))))))))(Secondary((id \
                   3117)(content(Whitespace\" \"))))(Tile((id \
                   3118)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3121)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3122)(content(Whitespace\" \"))))(Tile((id \
                   3124)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3125)(content(Whitespace\" \"))))(Tile((id \
                   3129)(label($==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3130)(content(Whitespace\" \"))))(Tile((id \
                   3131)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3134)(content(Whitespace\" \")))))((Secondary((id \
                   3138)(content(Whitespace\" \"))))(Tile((id \
                   3139)(label(e))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3142)(content(Whitespace\" \")))))))))(Secondary((id \
                   3146)(content(Whitespace\" \"))))(Tile((id \
                   3149)(label(Lam))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3150)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3152)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3153)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3155)(content(Whitespace\" \"))))(Tile((id \
                   3157)(label(s2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3158)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3161)(label(e1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3162)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3164)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3165)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3167)(label(v))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                   3170)(content(Whitespace\" \"))))(Secondary((id \
                   3168)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   3174)(content(Whitespace\" \")))))))))(Secondary((id \
                   3176)(content(Whitespace\" \"))))(Secondary((id \
                   3177)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3181)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3182)(content(Whitespace\" \"))))(Tile((id \
                   3185)(label(s3))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3186)(content(Whitespace\" \"))))(Tile((id \
                   3187)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   11))(sort Pat))((shape(Concave 11))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3189)(content(Whitespace\" \"))))(Tile((id \
                   3190)(label(\"(\"\")\"))(mold((out \
                   Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                   Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                   3193)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Tile((id \
                   3194)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   14))(sort Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3196)(content(Whitespace\" \"))))(Grout((id 3199)(shape \
                   Convex)))(Tile((id 3198)(label(,))(mold((out \
                   Typ)(in_())(nibs(((shape(Concave 14))(sort \
                   Typ))((shape(Concave 14))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3200)(content(Whitespace\" \"))))(Tile((id \
                   3203)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children()))))))))(Secondary((id \
                   3204)(content(Whitespace\" \"))))(Tile((id \
                   3207)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave \
                   6))(sort Typ))((shape(Concave 6))(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3208)(content(Whitespace\" \"))))(Tile((id \
                   3211)(label(Exp))(mold((out Typ)(in_())(nibs(((shape \
                   Convex)(sort Typ))((shape Convex)(sort \
                   Typ))))))(shards(0))(children())))(Secondary((id \
                   3212)(content(Whitespace\" \")))))((Secondary((id \
                   3214)(content(Whitespace\" \"))))(Tile((id 3218)(label(fun \
                   ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                   Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3219)(content(Whitespace\" \
                   \"))))(Tile((id 3221)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3222)(label(e))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3223)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3225)(label(x))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3226)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3228)(label(v))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   3229)(content(Whitespace\" \")))))))))(Secondary((id \
                   3232)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3237)(label(case end))(mold((out \
                   Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3238)(content(Whitespace\" \
                   \"))))(Tile((id 3240)(label(e))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3241)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3242)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3244)(content(Whitespace\" \
                   \"))))(Tile((id 3247)(label(Var))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3248)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3250)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   3251)(content(Whitespace\" \")))))))))(Secondary((id \
                   3254)(content(Whitespace\" \"))))(Tile((id \
                   3255)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3258)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3259)(content(Whitespace\" \"))))(Tile((id \
                   3261)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3262)(content(Whitespace\" \"))))(Tile((id \
                   3266)(label($==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3267)(content(Whitespace\" \"))))(Tile((id \
                   3268)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3271)(content(Whitespace\" \")))))((Secondary((id \
                   3275)(content(Whitespace\" \"))))(Tile((id \
                   3276)(label(v))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3279)(content(Whitespace\" \")))))))))(Secondary((id \
                   3283)(content(Whitespace\" \"))))(Tile((id \
                   3284)(label(e))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   3285)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3286)(label(| =>))(mold((out \
                   Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                   Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                   1))(children(((Secondary((id 3288)(content(Whitespace\" \
                   \"))))(Tile((id 3291)(label(Lam))(mold((out \
                   Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                   3292)(label(\"(\"\")\"))(mold((out \
                   Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                   Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                   3294)(label(y))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Tile((id \
                   3295)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave \
                   14))(sort Pat))((shape(Concave 14))(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3297)(content(Whitespace\" \"))))(Tile((id \
                   3299)(label(e1))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children()))))))))(Secondary((id \
                   3300)(content(Whitespace\" \")))))))))(Secondary((id \
                   3303)(content(Whitespace\" \"))))(Tile((id \
                   3304)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3307)(label(if then else))(mold((out Exp)(in_(Exp \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3308)(content(Whitespace\" \"))))(Tile((id \
                   3310)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3311)(content(Whitespace\" \"))))(Tile((id \
                   3315)(label($==))(mold((out \
                   Exp)(in_())(nibs(((shape(Concave 8))(sort \
                   Exp))((shape(Concave 8))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3316)(content(Whitespace\" \"))))(Tile((id \
                   3317)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3320)(content(Whitespace\" \")))))((Secondary((id \
                   3324)(content(Whitespace\" \"))))(Tile((id \
                   3325)(label(e))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3328)(content(Whitespace\" \")))))))))(Secondary((id \
                   3332)(content(Whitespace\" \"))))(Tile((id \
                   3335)(label(Lam))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3336)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3338)(label(y))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3339)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3341)(content(Whitespace\" \"))))(Tile((id \
                   3343)(label(s3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3344)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3347)(label(e1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3348)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3350)(label(x))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3351)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3353)(label(v))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                   3356)(content(Whitespace\" \"))))(Secondary((id \
                   3354)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                   3360)(content(Whitespace\" \")))))))))(Secondary((id \
                   3362)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                   3363)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3367)(label(let = in))(mold((out Exp)(in_(Pat \
                   Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
                   16))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
                   3368)(content(Whitespace\" \"))))(Tile((id \
                   3371)(label(in))(mold((out Pat)(in_())(nibs(((shape \
                   Convex)(sort Pat))((shape Convex)(sort \
                   Pat))))))(shards(0))(children())))(Secondary((id \
                   3372)(content(Whitespace\" \")))))((Secondary((id \
                   3374)(content(Whitespace\" \"))))(Tile((id \
                   3377)(label(Lam))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3378)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3381)(label(\"\\\"b\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   3382)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3384)(content(Whitespace\" \"))))(Tile((id \
                   3387)(label(Var))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3388)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3391)(label(\"\\\"a\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                   3392)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3395)(label(\"\\\"a\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                   3396)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3400)(label(Var))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3401)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3404)(label(\"\\\"x\\\"\"))(mold((out \
                   Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Secondary((id \
                   3407)(content(Whitespace\" \")))))))))(Secondary((id \
                   3409)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                   3410)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3412)(label(s0))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3413)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3416)(label(in))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   3417)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3419)(content(Whitespace\" \"))))(Tile((id \
                   3421)(label(s1))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3422)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3425)(label(in))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   3426)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3428)(content(Whitespace\" \"))))(Tile((id \
                   3430)(label(s2))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3431)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3434)(label(in))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children()))))))))(Tile((id \
                   3435)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave \
                   14))(sort Exp))((shape(Concave 14))(sort \
                   Exp))))))(shards(0))(children())))(Secondary((id \
                   3437)(content(Whitespace\" \"))))(Tile((id \
                   3439)(label(s3))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))(Tile((id \
                   3440)(label(\"(\"\")\"))(mold((out \
                   Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                   Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                   3443)(label(in))(mold((out Exp)(in_())(nibs(((shape \
                   Convex)(sort Exp))((shape Convex)(sort \
                   Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                   3444)(content(Whitespace\"\\226\\143\\142\")))))()))(ancestors())))(caret \
                   Outer))";
                backup_text =
                  "#recursive sum type dynamics tests#\n\
                   #all calls should evaluate fully with no exns or cast fails#\n\
                   type Exp = Var(String) + Lam(String, Exp) in\n\n\
                   let s0 : ( ,  ,  ) ->   = fun (e,x,v) ->\n\
                   case e\n\
                   | Var(y) => (if y $== x then v else e)\n\
                   | Lam(y, e1) => (if y $== x then e else Lam(y, s0(e1,x,v)))  \n\
                   end in \n\
                   let s1 : ( ,  ,  ) -> Exp = fun (e,x,v) ->\n\
                   case e\n\
                   | Var(y) => (if y $== x then v else e)\n\
                   | Lam(y, e1) => (if y $== x then e else Lam(y, s1(e1,x,v))) \n\
                   end in \n\
                   let s2 : (Exp,  ,  ) -> Exp = fun (e,x,v) ->\n\
                   case e\n\
                   | Var(y) => (if y $== x then v else e)\n\
                   | Lam(y, e1) => (if y $== x then e else Lam(y, s2(e1,x,v))) \n\
                   end in \n\
                   let s3 : (Exp,  , Exp) -> Exp = fun (e,x,v) ->\n\
                   case e\n\
                   | Var(y) => (if y $== x then v else e)\n\
                   | Lam(y, e1) => (if y $== x then e else Lam(y, s3(e1,x,v))) \n\
                   end in\n\n\
                   let in = Lam(\"b\", Var(\"a\")),\"a\",Var(\"x\") in\n\
                   (s0(in), s1(in), s2(in), s3(in))\n";
              } ) );
        ] );
  }
