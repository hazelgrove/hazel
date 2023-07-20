let intro : ScratchSlide.persistent_state =
  ( 15704,
    {
      zipper =
        "((selection((focus \
         Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
         14750)(content(Comment\"# Welcome to Hazel! #\"))))(Secondary((id \
         14838)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         14839)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         15428)(content(Comment\"# This is a program cell, which consists of a \
         structured editor  #\"))))(Secondary((id \
         14911)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         15500)(content(Comment\"# at the top and its evaluated result at the \
         bottom. Right now,  #\"))))(Secondary((id \
         14980)(content(Whitespace\"\\226\\143\\142\")))))((Secondary((id \
         15703)(content(Comment\"# that result has a question mark, as the \
         program is incomplete! #\"))))(Secondary((id \
         13612)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         15054)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         15059)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 15060)(content(Whitespace\" \
         \"))))(Tile((id 15074)(label(your_function))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         15075)(content(Whitespace\" \")))))((Secondary((id \
         15076)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         15546)(content(Comment\"# Fill the hole below to see how the result \
         changes #\"))))(Secondary((id \
         15144)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         15149)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 15150)(content(Whitespace\" \
         \"))))(Tile((id 15229)(label(parameter))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         15160)(content(Whitespace\" \")))))))))(Secondary((id \
         15171)(content(Whitespace\" \"))))(Grout((id 15697)(shape \
         Convex)))(Secondary((id 15162)(content(Whitespace\" \
         \"))))(Secondary((id 15163)(content(Whitespace\" \"))))(Secondary((id \
         15164)(content(Whitespace\" \"))))(Secondary((id \
         15165)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
         15170)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         13613)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         15594)(content(Comment\"# Here in Examples Mode, you can use the \
         upper left dropdown to  #\"))))(Secondary((id \
         14337)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         15607)(content(Comment\"# browse references for Hazel language and \
         editor features.      #\"))))(Secondary((id \
         14029)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         14084)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         15695)(content(Comment\"# Select Scratch Mode from the upper left \
         dialog to access blank #\"))))(Secondary((id \
         15252)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         15683)(content(Comment\"# cells where you can write code; use the \
         arrows to navigate.    #\"))))(Secondary((id \
         14180)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         14181)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         15435)(content(Comment\"# Select Exercise for a small functional \
         programming tutorial.   #\"))))(Secondary((id \
         3020)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         14335)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         14498)(label(your_function))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         14514)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         15220)(label(\"\\\"argument\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         15549)(content(Whitespace\" \"))))(Tile((id \
         15550)(label(+))(mold((out Exp)(in_())(nibs(((shape(Concave 5))(sort \
         Exp))((shape(Concave 5))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         15552)(content(Whitespace\" \"))))(Tile((id \
         15557)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2827)(content(Whitespace\"\\226\\143\\142\")))))))(ancestors())))(caret(Inner \
         65 64)))";
      backup_text =
        "# Welcome to Hazel! #\n\n\
         # This is a program cell, which consists of a structured editor  #\n\
         # at the top and its evaluated result at the bottom. Right now,  #\n\
         # that result has a question mark, as the program is incomplete! #\n\n\
         let your_function =\n\
         # Fill the hole below to see how the result changes #\n\
         fun parameter ->     \n\
         in\n\n\
         # Here in Scratch Mode, you can use the upper left arrows to     #\n\
         # switch between blank cells where you can store programs.       #\n\n\
         # Select Examples Mode from the upper left dialog to pick from   #\n\
         # a list of references for Hazel language and editor features.   #\n\n\
         # Select Exercise for a small functional programming tutorial.   #\n\n\
         your_function(\"argument\") + 1\n";
    } )

let lang_ref : ScratchSlide.persistent_state =
  ( 13588,
    {
      zipper =
        "((selection((focus \
         Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
         2915)(content(Comment\"# Hazel Language Quick Reference \
         #\")))))((Secondary((id \
         3020)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         3021)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         3150)(content(Comment\"# Empty holes stand for missing expressions, \
         patterns, or types #\"))))(Secondary((id \
         3030)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         3035)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 3036)(content(Whitespace\" \
         \"))))(Tile((id 3047)(label(empty_hole))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3048)(content(Whitespace\" \")))))((Secondary((id \
         3052)(content(Whitespace\" \"))))(Grout((id 3051)(shape \
         Convex)))(Secondary((id 3050)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3151)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         3152)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         2840)(content(Comment\"# Integers #\"))))(Secondary((id \
         2829)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1498)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 1499)(content(Whitespace\" \
         \"))))(Tile((id 1508)(label(int_lits))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1509)(content(Whitespace\" \"))))(Tile((id 1510)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1512)(content(Whitespace\" \"))))(Tile((id \
         1515)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1516)(content(Whitespace\" \")))))((Secondary((id \
         1519)(content(Whitespace\" \"))))(Tile((id 1520)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1523)(content(Whitespace\" \")))))))))(Secondary((id \
         1525)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1530)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 1531)(content(Whitespace\" \
         \"))))(Tile((id 1540)(label(negation))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1541)(content(Whitespace\" \")))))((Secondary((id \
         1544)(content(Whitespace\" \"))))(Tile((id 1545)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 2))(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1546)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1549)(content(Whitespace\" \")))))))))(Secondary((id \
         1551)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1556)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 1557)(content(Whitespace\" \
         \"))))(Tile((id 1568)(label(arithmetic))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1569)(content(Whitespace\" \")))))((Secondary((id \
         1572)(content(Whitespace\" \"))))(Tile((id 1573)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1574)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 4))(sort Exp))((shape(Concave \
         4))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1576)(label(2))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1577)(content(Whitespace\" \"))))(Tile((id 1578)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave \
         5))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1580)(content(Whitespace\" \"))))(Tile((id 1581)(label(8))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1582)(label(/))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 4))(sort Exp))((shape(Concave \
         4))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1584)(label(4))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1587)(content(Whitespace\" \")))))))))(Secondary((id \
         1589)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1594)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 1595)(content(Whitespace\" \
         \"))))(Tile((id 1610)(label(int_comparison))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1611)(content(Whitespace\" \")))))((Secondary((id \
         1614)(content(Whitespace\" \"))))(Tile((id \
         1615)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 1617)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1618)(content(Whitespace\" \"))))(Tile((id 1621)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 8))(sort Exp))((shape(Concave \
         8))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1622)(content(Whitespace\" \"))))(Tile((id 1624)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1625)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1627)(content(Whitespace\" \"))))(Tile((id 1628)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1629)(content(Whitespace\" \"))))(Tile((id 1630)(label(<))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave \
         5))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1632)(content(Whitespace\" \"))))(Tile((id 1633)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1634)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1636)(content(Whitespace\" \"))))(Tile((id 1637)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1638)(content(Whitespace\" \"))))(Tile((id 1641)(label(<=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 8))(sort Exp))((shape(Concave \
         8))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1642)(content(Whitespace\" \"))))(Tile((id 1643)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1644)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1646)(content(Whitespace\" \"))))(Tile((id 1647)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1648)(content(Whitespace\" \"))))(Tile((id 1649)(label(>))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave \
         5))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1651)(content(Whitespace\" \"))))(Tile((id 1652)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1653)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1655)(content(Whitespace\" \"))))(Tile((id 1656)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1657)(content(Whitespace\" \"))))(Tile((id 1660)(label(>=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 8))(sort Exp))((shape(Concave \
         8))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1661)(content(Whitespace\" \"))))(Tile((id 1662)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1665)(content(Whitespace\" \")))))))))(Secondary((id \
         3353)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         2841)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         2866)(content(Comment\"# Floating Point Numbers #\"))))(Secondary((id \
         1669)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1673)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 1674)(content(Whitespace\" \
         \"))))(Tile((id 1685)(label(float_lits))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1686)(content(Whitespace\" \"))))(Tile((id 1687)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1689)(content(Whitespace\" \"))))(Tile((id \
         1694)(label(Float))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1695)(content(Whitespace\" \")))))((Secondary((id \
         1698)(content(Whitespace\" \"))))(Tile((id \
         1701)(label(1.5))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1704)(content(Whitespace\" \")))))))))(Secondary((id \
         1706)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1711)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 1712)(content(Whitespace\" \
         \"))))(Tile((id 1724)(label(float_artih))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1725)(content(Whitespace\" \")))))((Secondary((id \
         1728)(content(Whitespace\" \"))))(Tile((id 1730)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1731)(content(Whitespace\" \"))))(Tile((id 1734)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 4))(sort Exp))((shape(Concave \
         4))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1735)(content(Whitespace\" \"))))(Tile((id 1737)(label(2.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1738)(content(Whitespace\" \"))))(Tile((id 1741)(label(+.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave \
         5))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1742)(content(Whitespace\" \"))))(Tile((id 1744)(label(8.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1745)(content(Whitespace\" \"))))(Tile((id 1748)(label(/.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 4))(sort Exp))((shape(Concave \
         4))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1749)(content(Whitespace\" \"))))(Tile((id 1751)(label(4.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1754)(content(Whitespace\" \")))))))))(Secondary((id \
         1756)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1761)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 1762)(content(Whitespace\" \
         \"))))(Tile((id 1779)(label(float_comparison))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1780)(content(Whitespace\" \")))))((Secondary((id \
         1783)(content(Whitespace\" \"))))(Tile((id \
         1784)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 1787)(label(10.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1788)(content(Whitespace\" \"))))(Tile((id \
         1792)(label(==.))(mold((out Exp)(in_())(nibs(((shape(Concave 8))(sort \
         Exp))((shape(Concave 8))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1793)(content(Whitespace\" \"))))(Tile((id \
         1796)(label(10.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         1797)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1799)(content(Whitespace\" \"))))(Tile((id 1801)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1802)(content(Whitespace\" \"))))(Tile((id 1805)(label(<.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave \
         5))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1806)(content(Whitespace\" \"))))(Tile((id 1808)(label(2.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1809)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1811)(content(Whitespace\" \"))))(Tile((id 1813)(label(2.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1814)(content(Whitespace\" \"))))(Tile((id \
         1818)(label(<=.))(mold((out Exp)(in_())(nibs(((shape(Concave 8))(sort \
         Exp))((shape(Concave 8))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1819)(content(Whitespace\" \"))))(Tile((id 1821)(label(3.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1822)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1824)(content(Whitespace\" \"))))(Tile((id 1826)(label(3.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1827)(content(Whitespace\" \"))))(Tile((id 1830)(label(>.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave \
         5))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1831)(content(Whitespace\" \"))))(Tile((id 1833)(label(2.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1834)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1836)(content(Whitespace\" \"))))(Tile((id 1838)(label(2.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1839)(content(Whitespace\" \"))))(Tile((id \
         1843)(label(>=.))(mold((out Exp)(in_())(nibs(((shape(Concave 8))(sort \
         Exp))((shape(Concave 8))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1844)(content(Whitespace\" \"))))(Tile((id 1846)(label(1.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1849)(content(Whitespace\" \")))))))))(Secondary((id \
         1851)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         2867)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         2878)(content(Comment\"# Booleans #\"))))(Secondary((id \
         1853)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1857)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 1858)(content(Whitespace\" \
         \"))))(Tile((id 1867)(label(booleans))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1868)(content(Whitespace\" \"))))(Tile((id 1869)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1871)(content(Whitespace\" \"))))(Tile((id \
         1872)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
         Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
         1))(children(((Tile((id 1876)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id 1877)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
         14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1879)(content(Whitespace\" \"))))(Tile((id \
         1883)(label(Bool))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         1884)(content(Whitespace\" \")))))((Secondary((id \
         1887)(content(Whitespace\" \"))))(Tile((id \
         1888)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 1892)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1893)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1895)(content(Whitespace\" \"))))(Tile((id \
         1900)(label(false))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1903)(content(Whitespace\" \")))))))))(Secondary((id \
         1905)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1910)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 1911)(content(Whitespace\" \
         \"))))(Tile((id 1924)(label(conditionals))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1925)(content(Whitespace\" \")))))((Secondary((id \
         1928)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1932)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 1933)(content(Whitespace\" \
         \"))))(Tile((id 1935)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id 1936)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 1937)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
         14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         1939)(content(Whitespace\" \"))))(Tile((id 1940)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1941)(content(Whitespace\" \")))))((Secondary((id \
         1944)(content(Whitespace\" \"))))(Tile((id \
         1945)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 1946)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1947)(content(Whitespace\" \"))))(Tile((id 1948)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave \
         5))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1950)(content(Whitespace\" \"))))(Tile((id 1951)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1952)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1954)(content(Whitespace\" \"))))(Tile((id 1955)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1956)(content(Whitespace\" \"))))(Tile((id 1957)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave \
         5))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1959)(content(Whitespace\" \"))))(Tile((id 1960)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1963)(content(Whitespace\" \")))))))))(Secondary((id \
         1965)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1969)(label(if then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 1970)(content(Whitespace\" \
         \"))))(Tile((id 1972)(label(y))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1973)(content(Whitespace\" \"))))(Tile((id 1974)(label(>))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave \
         5))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1976)(content(Whitespace\" \"))))(Tile((id 1977)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1980)(content(Whitespace\" \")))))((Secondary((id \
         1984)(content(Whitespace\" \"))))(Tile((id 1986)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1991)(content(Whitespace\" \"))))(Secondary((id \
         1987)(content(Whitespace\" \"))))(Secondary((id \
         1988)(content(Whitespace\" \"))))(Secondary((id \
         1989)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
         1995)(content(Whitespace\" \"))))(Tile((id 1997)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2002)(content(Whitespace\" \"))))(Secondary((id \
         1998)(content(Whitespace\" \"))))(Secondary((id \
         1999)(content(Whitespace\" \"))))(Secondary((id \
         2000)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
         2004)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         2916)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         2925)(content(Comment\"# Tuples #\"))))(Secondary((id \
         2006)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2010)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2011)(content(Whitespace\" \
         \"))))(Tile((id 2018)(label(tuples))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2019)(content(Whitespace\" \"))))(Tile((id 2020)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2022)(content(Whitespace\" \"))))(Tile((id \
         2023)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
         Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
         1))(children(((Tile((id 2026)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id 2027)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
         14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2029)(content(Whitespace\" \"))))(Tile((id \
         2033)(label(Bool))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
         2034)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave 14))(sort \
         Typ))((shape(Concave 14))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2036)(content(Whitespace\" \"))))(Tile((id \
         2037)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
         Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
         1))(children(((Tile((id 2041)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id 2042)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
         14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2044)(content(Whitespace\" \"))))(Tile((id \
         2047)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
         2048)(content(Whitespace\" \")))))((Secondary((id \
         2051)(content(Whitespace\" \"))))(Tile((id \
         2052)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 2053)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 2054)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2056)(content(Whitespace\" \"))))(Tile((id \
         2060)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         2061)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2063)(content(Whitespace\" \"))))(Tile((id \
         2064)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 2069)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 2070)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2072)(content(Whitespace\" \"))))(Tile((id 2073)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         2076)(content(Whitespace\" \")))))))))(Secondary((id \
         2078)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2083)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2084)(content(Whitespace\" \
         \"))))(Tile((id 2086)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id 2087)(label(a))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 2088)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
         14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         2090)(content(Whitespace\" \"))))(Tile((id 2091)(label(b))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 2092)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
         14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         2094)(content(Whitespace\" \"))))(Tile((id \
         2095)(label(\"(\"\")\"))(mold((out Pat)(in_(Pat))(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort Pat))))))(shards(0 \
         1))(children(((Tile((id 2096)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 2097)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
         14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         2099)(content(Whitespace\" \"))))(Tile((id 2100)(label(d))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))))))(Secondary((id \
         2101)(content(Whitespace\" \")))))((Secondary((id \
         2104)(content(Whitespace\" \"))))(Tile((id \
         2110)(label(tuples))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2113)(content(Whitespace\" \")))))))))(Secondary((id \
         2115)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         2926)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         2938)(content(Comment\"# Functions #\"))))(Secondary((id \
         2117)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2121)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2122)(content(Whitespace\" \
         \"))))(Tile((id 2124)(label(y))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2125)(content(Whitespace\" \"))))(Tile((id 2126)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2128)(content(Whitespace\" \"))))(Tile((id \
         2129)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
         Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
         1))(children(((Tile((id 2132)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id 2133)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
         14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2135)(content(Whitespace\" \"))))(Tile((id \
         2138)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
         2139)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave 14))(sort \
         Typ))((shape(Concave 14))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2141)(content(Whitespace\" \"))))(Tile((id \
         2144)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         2145)(content(Whitespace\" \"))))(Tile((id 2148)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave \
         6))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2149)(content(Whitespace\" \"))))(Tile((id \
         2152)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2153)(content(Whitespace\" \")))))((Secondary((id \
         2156)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2160)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 2161)(content(Whitespace\" \
         \"))))(Tile((id 2163)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id 2164)(label(m))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 2165)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
         14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         2167)(content(Whitespace\" \"))))(Tile((id 2168)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 2169)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
         14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         2171)(content(Whitespace\" \"))))(Tile((id 2172)(label(b))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         2173)(content(Whitespace\" \")))))))))(Secondary((id \
         2177)(content(Whitespace\" \"))))(Tile((id 2178)(label(m))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2179)(content(Whitespace\" \"))))(Tile((id 2180)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 4))(sort Exp))((shape(Concave \
         4))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2182)(content(Whitespace\" \"))))(Tile((id 2183)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2184)(content(Whitespace\" \"))))(Tile((id 2185)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave \
         5))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2187)(content(Whitespace\" \"))))(Tile((id 2188)(label(b))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2193)(content(Whitespace\" \"))))(Secondary((id \
         2189)(content(Whitespace\" \"))))(Secondary((id \
         2190)(content(Whitespace\" \"))))(Secondary((id \
         2191)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
         2195)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         2939)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         2998)(content(Comment\"# Recursive Functions (arrow type annotation \
         required) #\"))))(Secondary((id \
         2197)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2201)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2202)(content(Whitespace\" \
         \"))))(Tile((id 2221)(label(double_recursively))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2222)(content(Whitespace\" \"))))(Tile((id 2223)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2225)(content(Whitespace\" \"))))(Tile((id \
         2228)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2229)(content(Whitespace\" \"))))(Tile((id 2232)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave \
         6))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2233)(content(Whitespace\" \"))))(Tile((id \
         2236)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2237)(content(Whitespace\" \")))))((Secondary((id \
         2240)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2244)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 2245)(content(Whitespace\" \
         \"))))(Tile((id 2247)(label(n))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2248)(content(Whitespace\" \")))))))))(Secondary((id \
         2252)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2255)(label(if then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2256)(content(Whitespace\" \
         \"))))(Tile((id 2258)(label(n))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2259)(content(Whitespace\" \"))))(Tile((id 2262)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 8))(sort Exp))((shape(Concave \
         8))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2263)(content(Whitespace\" \"))))(Tile((id 2264)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2267)(content(Whitespace\" \")))))((Secondary((id \
         2271)(content(Whitespace\" \"))))(Tile((id 2273)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2278)(content(Whitespace\" \"))))(Secondary((id \
         2274)(content(Whitespace\" \"))))(Secondary((id \
         2275)(content(Whitespace\" \"))))(Secondary((id \
         2276)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
         2282)(content(Whitespace\" \"))))(Tile((id \
         2301)(label(double_recursively))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2302)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2304)(label(n))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2305)(content(Whitespace\" \"))))(Tile((id 2306)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave \
         5))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2308)(content(Whitespace\" \"))))(Tile((id 2309)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2310)(content(Whitespace\" \"))))(Tile((id 2311)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave \
         5))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2313)(content(Whitespace\" \"))))(Tile((id 2314)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2319)(content(Whitespace\" \"))))(Secondary((id \
         2315)(content(Whitespace\" \"))))(Secondary((id \
         2316)(content(Whitespace\" \"))))(Secondary((id \
         2317)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
         2321)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         2999)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         3007)(content(Comment\"# Lists #\"))))(Secondary((id \
         2323)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2327)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2328)(content(Whitespace\" \
         \"))))(Tile((id 2339)(label(empty_list))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2340)(content(Whitespace\" \"))))(Tile((id 2341)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2343)(content(Whitespace\" \"))))(Tile((id 2344)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         2347)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         2348)(content(Whitespace\" \")))))((Secondary((id \
         2351)(content(Whitespace\" \"))))(Tile((id 2354)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2357)(content(Whitespace\" \")))))))))(Secondary((id \
         2359)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2364)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2365)(content(Whitespace\" \
         \"))))(Tile((id 2380)(label(non_empty_list))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2381)(content(Whitespace\" \"))))(Tile((id 2382)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2384)(content(Whitespace\" \"))))(Tile((id 2385)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         2388)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         2389)(content(Whitespace\" \")))))((Secondary((id \
         2392)(content(Whitespace\" \"))))(Tile((id 2393)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2396)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave 6))(sort \
         Exp))((shape(Concave 6))(sort \
         Exp))))))(shards(0))(children())))(Tile((id 2397)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2400)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave 6))(sort \
         Exp))((shape(Concave 6))(sort \
         Exp))))))(shards(0))(children())))(Tile((id 2401)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2404)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave 6))(sort \
         Exp))((shape(Concave 6))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2407)(label([]))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2410)(content(Whitespace\" \")))))))))(Secondary((id \
         2412)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2417)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2418)(content(Whitespace\" \
         \"))))(Tile((id 2432)(label(list_literals))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2433)(content(Whitespace\" \"))))(Tile((id 2434)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2436)(content(Whitespace\" \"))))(Tile((id 2437)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         2440)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         2441)(content(Whitespace\" \")))))((Secondary((id \
         2444)(content(Whitespace\" \"))))(Tile((id 2445)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2446)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         2447)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2449)(content(Whitespace\" \"))))(Tile((id 2450)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 2451)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2453)(content(Whitespace\" \"))))(Tile((id 2454)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2457)(content(Whitespace\" \")))))))))(Secondary((id \
         2459)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2464)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2465)(content(Whitespace\" \
         \"))))(Tile((id 2472)(label(length))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2473)(content(Whitespace\" \"))))(Tile((id 2474)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2476)(content(Whitespace\" \"))))(Tile((id 2477)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         2480)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         2481)(content(Whitespace\" \"))))(Tile((id 2484)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave \
         6))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2485)(content(Whitespace\" \"))))(Tile((id \
         2488)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2489)(content(Whitespace\" \")))))((Secondary((id \
         2492)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2496)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 2497)(content(Whitespace\" \
         \"))))(Tile((id 2500)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2501)(content(Whitespace\" \")))))))))(Secondary((id \
         2505)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2510)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 2511)(content(Whitespace\" \
         \"))))(Tile((id 2514)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2515)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2516)(label(| =>))(mold((out Rul)(in_(Pat))(nibs(((shape(Concave \
         19))(sort Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 2518)(content(Whitespace\" \
         \"))))(Tile((id 2521)(label([]))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2522)(content(Whitespace\" \")))))))))(Secondary((id \
         2525)(content(Whitespace\" \"))))(Tile((id 2526)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2527)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2528)(label(| =>))(mold((out Rul)(in_(Pat))(nibs(((shape(Concave \
         19))(sort Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 2530)(content(Whitespace\" \
         \"))))(Tile((id 2532)(label(hd))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2535)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave 6))(sort \
         Pat))((shape(Concave 6))(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2537)(label(tl))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2538)(content(Whitespace\" \")))))))))(Secondary((id \
         2541)(content(Whitespace\" \"))))(Tile((id 2542)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2543)(content(Whitespace\" \"))))(Tile((id 2544)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave \
         5))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2546)(content(Whitespace\" \"))))(Tile((id \
         2552)(label(length))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         2553)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2556)(label(tl))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
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
         2575)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2576)(content(Whitespace\" \
         \"))))(Tile((id 2602)(label(has_at_least_two_elements))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2603)(content(Whitespace\" \"))))(Tile((id 2604)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2606)(content(Whitespace\" \"))))(Tile((id 2607)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         2610)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         2611)(content(Whitespace\" \"))))(Tile((id 2614)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave \
         6))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2615)(content(Whitespace\" \"))))(Tile((id \
         2619)(label(Bool))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2620)(content(Whitespace\" \")))))((Secondary((id \
         2623)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2627)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 2628)(content(Whitespace\" \
         \"))))(Tile((id 2631)(label(xs))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2632)(content(Whitespace\" \")))))))))(Secondary((id \
         2636)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2641)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 2642)(content(Whitespace\" \
         \"))))(Tile((id 2645)(label(xs))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2646)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2647)(label(| =>))(mold((out Rul)(in_(Pat))(nibs(((shape(Concave \
         19))(sort Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 2649)(content(Whitespace\" \
         \"))))(Tile((id 2652)(label([]))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2653)(content(Whitespace\" \")))))))))(Secondary((id \
         2656)(content(Whitespace\" \"))))(Tile((id \
         2661)(label(false))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2662)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2663)(label(| =>))(mold((out Rul)(in_(Pat))(nibs(((shape(Concave \
         19))(sort Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 2665)(content(Whitespace\" \
         \"))))(Tile((id 2667)(label(hd))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2670)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave 6))(sort \
         Pat))((shape(Concave 6))(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2673)(label([]))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2674)(content(Whitespace\" \")))))))))(Secondary((id \
         2677)(content(Whitespace\" \"))))(Tile((id \
         2682)(label(false))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2683)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2684)(label(| =>))(mold((out Rul)(in_(Pat))(nibs(((shape(Concave \
         19))(sort Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 2686)(content(Whitespace\" \
         \"))))(Tile((id 2687)(label(a))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2690)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave 6))(sort \
         Pat))((shape(Concave 6))(sort \
         Pat))))))(shards(0))(children())))(Tile((id 2691)(label(b))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2694)(label(::))(mold((out Pat)(in_())(nibs(((shape(Concave 6))(sort \
         Pat))((shape(Concave 6))(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2697)(label([]))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2698)(content(Whitespace\" \")))))))))(Secondary((id \
         2701)(content(Whitespace\" \"))))(Tile((id \
         2705)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
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
         2726)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2727)(content(Whitespace\" \
         \"))))(Tile((id 2739)(label(string_lits))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2740)(content(Whitespace\" \")))))((Secondary((id \
         2743)(content(Whitespace\" \"))))(Tile((id 2757)(label(\"\\\"Hello, \
         world!\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2760)(content(Whitespace\" \")))))))))(Secondary((id \
         2762)(content(Whitespace\" \"))))(Secondary((id \
         2764)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2768)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2769)(content(Whitespace\" \
         \"))))(Tile((id 2785)(label(string_equality))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2786)(content(Whitespace\" \")))))((Secondary((id \
         2789)(content(Whitespace\" \"))))(Tile((id \
         2800)(label(string_lits))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2801)(content(Whitespace\" \"))))(Tile((id \
         2805)(label($==))(mold((out Exp)(in_())(nibs(((shape(Concave 8))(sort \
         Exp))((shape(Concave 8))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2806)(content(Whitespace\" \"))))(Tile((id 2820)(label(\"\\\"Hello, \
         world!\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2823)(content(Whitespace\" \")))))))))(Secondary((id \
         2825)(content(Whitespace\" \"))))(Secondary((id \
         3019)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         3409)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         3466)(content(Comment\"# Non-empty holes are the red dotted boxes \
         around errors #\"))))(Secondary((id \
         3467)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         3519)(content(Comment\"# (you can still run programs with non-empty \
         holes) #\"))))(Secondary((id \
         3520)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         3524)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 3525)(content(Whitespace\" \
         \"))))(Tile((id 3540)(label(non_empty_hole))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3541)(content(Whitespace\" \"))))(Tile((id 3542)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3544)(content(Whitespace\" \"))))(Tile((id \
         3547)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3548)(content(Whitespace\" \")))))((Secondary((id \
         3551)(content(Whitespace\" \"))))(Tile((id \
         3555)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3558)(content(Whitespace\" \")))))))))(Secondary((id \
         3560)(content(Whitespace\" \"))))(Secondary((id \
         3562)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         3563)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         3564)(label(2))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3565)(content(Whitespace\" \"))))(Tile((id 3566)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 5))(sort Exp))((shape(Concave \
         5))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3568)(content(Whitespace\" \"))))(Tile((id 3569)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2827)(content(Whitespace\"\\226\\143\\142\")))))))(ancestors())))(caret \
         Outer))";
      backup_text =
        "# Hazel Language Quick Reference #\n\n\
         # Empty holes stand for missing expressions, patterns, or types #\n\
         let empty_hole =   in\n\n\
         # Integers #\n\
         let int_lits : Int = 1 in\n\
         let negation = -1 in\n\
         let arithmetic = 1*2 + 8/4 in\n\
         let int_comparison = (10 == 10, 1 < 2, 2 <= 3, 3 > 2, 2 >= 1) in\n\n\
         # Floating Point Numbers #\n\
         let float_lits : Float = 1.5 in\n\
         let float_artih = 1. *. 2. +. 8. /. 4. in\n\
         let float_comparison = (10. ==. 10., 1. <. 2., 2. <=. 3., 3. >. 2., \
         2. >=. 1.) in\n\n\
         # Booleans #\n\
         let booleans : (Bool, Bool) = (true, false) in\n\
         let conditionals =\n\
         let (x, y) = (2 + 2, 3 + 3) in\n\
         if y > x then 1   \n\
         else 2   \n\
         in\n\n\
         # Tuples #\n\
         let tuples : (Int, Bool, (Bool, Int)) = (1, true, (false, 3)) in\n\
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
    } )

let basic_type_egs : ScratchSlide.persistent_state =
  ( 25454,
    {
      zipper =
        "((selection((focus \
         Left)(content())))(backpack())(relatives((siblings(()((Secondary((id \
         23732)(content(Comment\"#Types and type error \
         examples#\"))))(Secondary((id \
         23733)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         23734)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         23738)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 25393)(content(Whitespace\" \
         \"))))(Tile((id 25396)(label(_))(mold((out Pat)(in_())(nibs(((shape \
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
         23765)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 23766)(content(Whitespace\" \
         \"))))(Tile((id 23776)(label(Undefined))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
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
         23807)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 23808)(content(Whitespace\" \
         \"))))(Tile((id 23813)(label(true))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         23814)(content(Whitespace\" \")))))((Secondary((id \
         23816)(content(Whitespace\" \"))))(Tile((id \
         23817)(label(2))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23820)(content(Whitespace\" \")))))))))(Secondary((id \
         23822)(content(Whitespace\" \"))))(Secondary((id \
         23826)(content(Comment #err#))))(Secondary((id \
         23827)(content(Whitespace\" \"))))(Secondary((id \
         23828)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         23829)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         23833)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 25453)(content(Whitespace\" \
         \"))))(Grout((id 23838)(shape Convex)))(Secondary((id \
         23834)(content(Whitespace\" \")))))((Secondary((id \
         23839)(content(Whitespace\" \"))))(Tile((id 23842)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 23843)(content(Whitespace\" \
         \"))))(Tile((id 23848)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23851)(content(Whitespace\" \")))))((Secondary((id \
         23855)(content(Whitespace\" \"))))(Tile((id \
         23856)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23859)(content(Whitespace\" \")))))))))(Secondary((id \
         23863)(content(Whitespace\" \"))))(Tile((id \
         23865)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23868)(content(Whitespace\" \")))))))))(Secondary((id \
         23870)(content(Whitespace\" \"))))(Secondary((id \
         23874)(content(Comment #err#))))(Secondary((id \
         23875)(content(Whitespace\" \"))))(Secondary((id \
         23876)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         23880)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 23881)(content(Whitespace\" \
         \"))))(Tile((id 23883)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         23884)(content(Whitespace\" \")))))((Secondary((id \
         23886)(content(Whitespace\" \"))))(Tile((id 23889)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 23890)(content(Whitespace\" \
         \"))))(Tile((id 23895)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23898)(content(Whitespace\" \")))))((Secondary((id \
         23902)(content(Whitespace\" \"))))(Tile((id \
         23903)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23906)(content(Whitespace\" \")))))))))(Secondary((id \
         23910)(content(Whitespace\" \"))))(Tile((id \
         23912)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23915)(content(Whitespace\" \")))))))))(Secondary((id \
         23917)(content(Whitespace\" \"))))(Secondary((id \
         23921)(content(Comment #err#))))(Secondary((id \
         23922)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         23926)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 23927)(content(Whitespace\" \
         \"))))(Tile((id 23929)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         23930)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         25452)(content(Whitespace\" \"))))(Grout((id 23935)(shape \
         Convex)))(Secondary((id 23932)(content(Whitespace\" \
         \")))))((Secondary((id 23936)(content(Whitespace\" \"))))(Tile((id \
         23939)(label(if then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 23940)(content(Whitespace\" \
         \"))))(Tile((id 23945)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23948)(content(Whitespace\" \")))))((Secondary((id \
         23952)(content(Whitespace\" \"))))(Tile((id \
         23953)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23956)(content(Whitespace\" \")))))))))(Secondary((id \
         23960)(content(Whitespace\" \"))))(Tile((id \
         23962)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23965)(content(Whitespace\" \")))))))))(Secondary((id \
         23967)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         23971)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 23972)(content(Whitespace\" \
         \"))))(Tile((id 23974)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         23975)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         23977)(content(Whitespace\" \"))))(Tile((id \
         23980)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         23981)(content(Whitespace\" \")))))((Secondary((id \
         23983)(content(Whitespace\" \"))))(Tile((id 23986)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 23987)(content(Whitespace\" \
         \"))))(Tile((id 23992)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         23995)(content(Whitespace\" \")))))((Secondary((id \
         23999)(content(Whitespace\" \"))))(Tile((id \
         24000)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24003)(content(Whitespace\" \")))))))))(Secondary((id \
         24007)(content(Whitespace\" \"))))(Tile((id \
         24009)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24012)(content(Whitespace\" \")))))))))(Secondary((id \
         24014)(content(Whitespace\" \"))))(Secondary((id \
         24018)(content(Comment #err#))))(Secondary((id \
         24019)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24023)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24024)(content(Whitespace\" \
         \"))))(Tile((id 24026)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         24027)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         24029)(content(Whitespace\" \"))))(Tile((id \
         24033)(label(Fake))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         24034)(content(Whitespace\" \")))))((Secondary((id \
         24036)(content(Whitespace\" \"))))(Tile((id 24039)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24040)(content(Whitespace\" \
         \"))))(Tile((id 24045)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24048)(content(Whitespace\" \")))))((Secondary((id \
         24052)(content(Whitespace\" \"))))(Tile((id \
         24053)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24056)(content(Whitespace\" \")))))))))(Secondary((id \
         24060)(content(Whitespace\" \"))))(Tile((id \
         24064)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24067)(content(Whitespace\" \")))))))))(Secondary((id \
         24069)(content(Whitespace\" \"))))(Secondary((id \
         24073)(content(Comment #err#))))(Secondary((id \
         24074)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24078)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24079)(content(Whitespace\" \
         \"))))(Tile((id 24081)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         24082)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave 14))(sort \
         Pat))((shape(Concave 14))(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         24084)(content(Whitespace\" \"))))(Tile((id \
         24085)(label(_))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         24086)(content(Whitespace\" \")))))((Secondary((id \
         24088)(content(Whitespace\" \"))))(Tile((id 24091)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24092)(content(Whitespace\" \
         \"))))(Tile((id 24097)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24100)(content(Whitespace\" \")))))((Secondary((id \
         24104)(content(Whitespace\" \"))))(Tile((id \
         24105)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24108)(content(Whitespace\" \")))))))))(Secondary((id \
         24112)(content(Whitespace\" \"))))(Tile((id \
         24114)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24117)(content(Whitespace\" \")))))))))(Secondary((id \
         24119)(content(Whitespace\" \"))))(Secondary((id \
         24126)(content(Comment\"#2x err#\"))))(Secondary((id \
         24127)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24131)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24132)(content(Whitespace\" \
         \"))))(Tile((id 24134)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         24135)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave 14))(sort \
         Pat))((shape(Concave 14))(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         24137)(content(Whitespace\" \"))))(Tile((id \
         24138)(label(_))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         24139)(content(Whitespace\" \")))))((Secondary((id \
         24141)(content(Whitespace\" \"))))(Tile((id \
         24142)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 24145)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         24146)(content(Whitespace\" \"))))(Tile((id \
         24151)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24154)(content(Whitespace\" \")))))((Secondary((id \
         24158)(content(Whitespace\" \"))))(Tile((id \
         24159)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24162)(content(Whitespace\" \")))))))))(Secondary((id \
         24166)(content(Whitespace\" \"))))(Tile((id \
         24168)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         24169)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Grout((id 24175)(shape \
         Convex)))(Secondary((id 24171)(content(Whitespace\" \
         \"))))(Secondary((id 24172)(content(Whitespace\" \"))))(Secondary((id \
         24173)(content(Whitespace\" \")))))))))(Secondary((id \
         24177)(content(Whitespace\" \"))))(Secondary((id \
         24181)(content(Comment #err#))))(Secondary((id \
         24182)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24186)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24187)(content(Whitespace\" \
         \"))))(Tile((id 24189)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         24190)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         24192)(content(Whitespace\" \"))))(Grout((id 24194)(shape \
         Convex)))(Tile((id 24193)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
         14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         24195)(content(Whitespace\" \"))))(Tile((id \
         24196)(label(_))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         24197)(content(Whitespace\" \")))))((Secondary((id \
         24199)(content(Whitespace\" \"))))(Tile((id \
         24200)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 24203)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         24204)(content(Whitespace\" \"))))(Tile((id \
         24209)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24212)(content(Whitespace\" \")))))((Secondary((id \
         24216)(content(Whitespace\" \"))))(Tile((id \
         24217)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24220)(content(Whitespace\" \")))))))))(Secondary((id \
         24224)(content(Whitespace\" \"))))(Tile((id \
         24226)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         24227)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Grout((id 24233)(shape \
         Convex)))(Secondary((id 24229)(content(Whitespace\" \
         \"))))(Secondary((id 24230)(content(Whitespace\" \"))))(Secondary((id \
         24231)(content(Whitespace\" \")))))))))(Secondary((id \
         24235)(content(Whitespace\" \"))))(Secondary((id \
         24236)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24240)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24241)(content(Whitespace\" \
         \"))))(Tile((id 24243)(label([ ]))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id 24244)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         24245)(content(Whitespace\" \")))))((Secondary((id \
         24247)(content(Whitespace\" \"))))(Tile((id 24248)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         24249)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 24252)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         24253)(content(Whitespace\" \"))))(Tile((id \
         24258)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24261)(content(Whitespace\" \")))))((Secondary((id \
         24265)(content(Whitespace\" \"))))(Tile((id \
         24266)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24269)(content(Whitespace\" \")))))))))(Secondary((id \
         24273)(content(Whitespace\" \"))))(Tile((id \
         24275)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         24278)(content(Whitespace\" \")))))))))(Secondary((id \
         24280)(content(Whitespace\" \"))))(Secondary((id \
         24281)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24285)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24286)(content(Whitespace\" \
         \"))))(Tile((id 24288)(label([ ]))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id 24289)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         24290)(content(Whitespace\" \")))))((Secondary((id \
         24292)(content(Whitespace\" \"))))(Tile((id \
         24293)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 24296)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         24297)(content(Whitespace\" \"))))(Tile((id \
         24302)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24305)(content(Whitespace\" \")))))((Secondary((id \
         24309)(content(Whitespace\" \"))))(Tile((id \
         24310)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24313)(content(Whitespace\" \")))))))))(Secondary((id \
         24317)(content(Whitespace\" \"))))(Tile((id \
         24319)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         24322)(content(Whitespace\" \")))))))))(Secondary((id \
         24324)(content(Whitespace\" \"))))(Secondary((id \
         24331)(content(Comment\"#2x err#\"))))(Secondary((id \
         24332)(content(Whitespace\" \"))))(Secondary((id \
         24333)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         24334)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         25411)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Grout((id 25413)(shape Convex)))(Secondary((id \
         25412)(content(Whitespace\" \")))))))))(Tile((id \
         24338)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         24342)(label(if then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24343)(content(Whitespace\" \
         \"))))(Tile((id 24348)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24351)(content(Whitespace\" \")))))((Secondary((id \
         24355)(content(Whitespace\" \"))))(Tile((id \
         24356)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24359)(content(Whitespace\" \")))))))))(Secondary((id \
         24363)(content(Whitespace\" \"))))(Tile((id \
         24365)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         24366)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
         10))(sort Exp))((shape(Concave 10))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24368)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24369)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         24370)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         24374)(label(if then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24375)(content(Whitespace\" \
         \"))))(Tile((id 24380)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24383)(content(Whitespace\" \")))))((Secondary((id \
         24387)(content(Whitespace\" \"))))(Tile((id \
         24388)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24391)(content(Whitespace\" \")))))))))(Secondary((id \
         24395)(content(Whitespace\" \"))))(Tile((id \
         24397)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         24398)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
         10))(sort Exp))((shape(Concave 10))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24400)(content(Whitespace\" \"))))(Secondary((id \
         24404)(content(Comment #err#))))(Secondary((id \
         24405)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24406)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 24407)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         24408)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         24412)(label(if then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24413)(content(Whitespace\" \
         \"))))(Tile((id 24418)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24421)(content(Whitespace\" \")))))((Secondary((id \
         24425)(content(Whitespace\" \"))))(Tile((id \
         24426)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24429)(content(Whitespace\" \")))))))))(Secondary((id \
         24433)(content(Whitespace\" \"))))(Tile((id \
         24435)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         24436)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
         10))(sort Exp))((shape(Concave 10))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24438)(content(Whitespace\" \"))))(Secondary((id \
         24442)(content(Comment #err#))))(Secondary((id \
         24443)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24444)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 24448)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         25439)(content(Whitespace\" \"))))(Grout((id 25440)(shape \
         Convex)))(Secondary((id 25441)(content(Whitespace\" \
         \")))))))))(Secondary((id 25442)(content(Whitespace\" \
         \"))))(Grout((id 24457)(shape Convex))))))))(Tile((id \
         24458)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         24462)(label(if then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24463)(content(Whitespace\" \
         \"))))(Tile((id 24468)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24471)(content(Whitespace\" \")))))((Secondary((id \
         24475)(content(Whitespace\" \"))))(Tile((id \
         24476)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24479)(content(Whitespace\" \")))))))))(Secondary((id \
         24483)(content(Whitespace\" \"))))(Tile((id \
         24485)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         24486)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
         10))(sort Exp))((shape(Concave 10))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24488)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24489)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 24493)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         24494)(content(Whitespace\" \"))))(Tile((id \
         24496)(label(_))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         24497)(content(Whitespace\" \")))))))))(Secondary((id \
         25443)(content(Whitespace\" \"))))(Grout((id 24502)(shape \
         Convex))))))))(Tile((id 24503)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         24507)(label(if then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24508)(content(Whitespace\" \
         \"))))(Tile((id 24513)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24516)(content(Whitespace\" \")))))((Secondary((id \
         24520)(content(Whitespace\" \"))))(Tile((id \
         24521)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24524)(content(Whitespace\" \")))))))))(Secondary((id \
         24528)(content(Whitespace\" \"))))(Tile((id \
         24530)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         24531)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
         10))(sort Exp))((shape(Concave 10))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24533)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24534)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 24538)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         24539)(content(Whitespace\" \"))))(Tile((id \
         24541)(label(_))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
         24542)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         25444)(content(Whitespace\" \"))))(Grout((id 24548)(shape \
         Convex)))(Secondary((id 24544)(content(Whitespace\" \
         \")))))))))(Secondary((id 25445)(content(Whitespace\" \
         \"))))(Grout((id 24551)(shape Convex))))))))(Tile((id \
         24552)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         24556)(label(if then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24557)(content(Whitespace\" \
         \"))))(Tile((id 24562)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24565)(content(Whitespace\" \")))))((Secondary((id \
         24569)(content(Whitespace\" \"))))(Tile((id \
         24570)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24573)(content(Whitespace\" \")))))))))(Secondary((id \
         24577)(content(Whitespace\" \"))))(Tile((id \
         24579)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         24580)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
         10))(sort Exp))((shape(Concave 10))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24582)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24583)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 24587)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         13))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         24588)(content(Whitespace\" \"))))(Tile((id \
         24590)(label(_))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
         24591)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         24593)(content(Whitespace\" \"))))(Tile((id \
         24596)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         24597)(content(Whitespace\" \")))))))))(Secondary((id \
         25446)(content(Whitespace\" \"))))(Grout((id 24602)(shape \
         Convex))))))))(Tile((id 24603)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         24607)(label(if then else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24608)(content(Whitespace\" \
         \"))))(Tile((id 24613)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24616)(content(Whitespace\" \")))))((Secondary((id \
         24620)(content(Whitespace\" \"))))(Tile((id \
         24621)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24624)(content(Whitespace\" \")))))))))(Secondary((id \
         24628)(content(Whitespace\" \"))))(Tile((id \
         24630)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         24631)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
         10))(sort Exp))((shape(Concave 10))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24633)(content(Whitespace\" \"))))(Secondary((id \
         24637)(content(Comment #err#))))(Secondary((id \
         24638)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         24639)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24643)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24644)(content(Whitespace\" \
         \"))))(Tile((id 24646)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         24647)(content(Whitespace\" \")))))((Secondary((id \
         24649)(content(Whitespace\" \"))))(Tile((id 24653)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 24654)(content(Whitespace\" \
         \"))))(Tile((id 24656)(label(x))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         24657)(content(Whitespace\" \")))))))))(Secondary((id \
         24660)(content(Whitespace\" \"))))(Tile((id 24663)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24664)(content(Whitespace\" \
         \"))))(Tile((id 24669)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24672)(content(Whitespace\" \")))))((Secondary((id \
         24676)(content(Whitespace\" \"))))(Tile((id \
         24677)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24680)(content(Whitespace\" \")))))))))(Secondary((id \
         24684)(content(Whitespace\" \"))))(Tile((id \
         24686)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24689)(content(Whitespace\" \")))))))))(Secondary((id \
         24691)(content(Whitespace\" \"))))(Secondary((id \
         24695)(content(Comment #err#))))(Secondary((id \
         24696)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24700)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24701)(content(Whitespace\" \
         \"))))(Tile((id 24703)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         24704)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         25417)(content(Whitespace\" \"))))(Grout((id 24709)(shape \
         Convex)))(Secondary((id 24706)(content(Whitespace\" \
         \")))))((Secondary((id 24710)(content(Whitespace\" \"))))(Tile((id \
         24714)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 24715)(content(Whitespace\" \
         \"))))(Tile((id 24717)(label(x))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         24718)(content(Whitespace\" \")))))))))(Secondary((id \
         24721)(content(Whitespace\" \"))))(Tile((id 24724)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24725)(content(Whitespace\" \
         \"))))(Tile((id 24730)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24733)(content(Whitespace\" \")))))((Secondary((id \
         24737)(content(Whitespace\" \"))))(Tile((id \
         24738)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24741)(content(Whitespace\" \")))))))))(Secondary((id \
         24745)(content(Whitespace\" \"))))(Tile((id \
         24747)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24750)(content(Whitespace\" \")))))))))(Secondary((id \
         24752)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24756)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24757)(content(Whitespace\" \
         \"))))(Tile((id 24759)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         24760)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         25416)(content(Whitespace\" \"))))(Grout((id 25415)(shape \
         Convex)))(Secondary((id 25419)(content(Whitespace\" \"))))(Tile((id \
         24766)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave 6))(sort \
         Typ))((shape(Concave 6))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         25418)(content(Whitespace\" \"))))(Grout((id 24771)(shape \
         Convex)))(Secondary((id 24768)(content(Whitespace\" \
         \")))))((Secondary((id 24772)(content(Whitespace\" \"))))(Tile((id \
         24776)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 24777)(content(Whitespace\" \
         \"))))(Tile((id 24779)(label(x))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         24780)(content(Whitespace\" \")))))))))(Secondary((id \
         24783)(content(Whitespace\" \"))))(Tile((id 24786)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24787)(content(Whitespace\" \
         \"))))(Tile((id 24792)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24795)(content(Whitespace\" \")))))((Secondary((id \
         24799)(content(Whitespace\" \"))))(Tile((id \
         24800)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24803)(content(Whitespace\" \")))))))))(Secondary((id \
         24807)(content(Whitespace\" \"))))(Tile((id \
         24809)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24812)(content(Whitespace\" \")))))))))(Secondary((id \
         24814)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24818)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24819)(content(Whitespace\" \
         \"))))(Tile((id 24821)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         24822)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         24824)(content(Whitespace\" \"))))(Grout((id 24829)(shape \
         Convex)))(Secondary((id 25420)(content(Whitespace\" \"))))(Tile((id \
         24828)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave 6))(sort \
         Typ))((shape(Concave 6))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         24830)(content(Whitespace\" \"))))(Tile((id \
         24833)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         24834)(content(Whitespace\" \")))))((Secondary((id \
         24836)(content(Whitespace\" \"))))(Tile((id 24840)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 24841)(content(Whitespace\" \
         \"))))(Tile((id 24843)(label(x))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         24844)(content(Whitespace\" \")))))))))(Secondary((id \
         24847)(content(Whitespace\" \"))))(Tile((id 24850)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24851)(content(Whitespace\" \
         \"))))(Tile((id 24856)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24859)(content(Whitespace\" \")))))((Secondary((id \
         24863)(content(Whitespace\" \"))))(Tile((id \
         24864)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24867)(content(Whitespace\" \")))))))))(Secondary((id \
         24871)(content(Whitespace\" \"))))(Tile((id \
         24873)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24876)(content(Whitespace\" \")))))))))(Secondary((id \
         24878)(content(Whitespace\" \"))))(Secondary((id \
         24882)(content(Comment #err#))))(Secondary((id \
         24883)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24887)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24888)(content(Whitespace\" \
         \"))))(Tile((id 24890)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         24891)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         24893)(content(Whitespace\" \"))))(Grout((id 24898)(shape \
         Convex)))(Secondary((id 25421)(content(Whitespace\" \"))))(Tile((id \
         24897)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave 6))(sort \
         Typ))((shape(Concave 6))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         24899)(content(Whitespace\" \"))))(Tile((id 25423)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id 25431)(shape \
         Convex))))))))(Secondary((id 24903)(content(Whitespace\" \
         \")))))((Secondary((id 24905)(content(Whitespace\" \"))))(Tile((id \
         24909)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 24910)(content(Whitespace\" \
         \"))))(Tile((id 24912)(label(x))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         24913)(content(Whitespace\" \")))))))))(Secondary((id \
         24916)(content(Whitespace\" \"))))(Tile((id 24919)(label(if then \
         else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 24920)(content(Whitespace\" \
         \"))))(Tile((id 24925)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24928)(content(Whitespace\" \")))))((Secondary((id \
         24932)(content(Whitespace\" \"))))(Tile((id \
         24933)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24936)(content(Whitespace\" \")))))))))(Secondary((id \
         24940)(content(Whitespace\" \"))))(Tile((id \
         24942)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24945)(content(Whitespace\" \")))))))))(Secondary((id \
         24947)(content(Whitespace\" \"))))(Secondary((id \
         24954)(content(Comment\"#2x err#\"))))(Secondary((id \
         24955)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         24956)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         25448)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Grout((id 25449)(shape Convex))))))))(Tile((id \
         24962)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave 6))(sort \
         Exp))((shape(Concave 6))(sort \
         Exp))))))(shards(0))(children())))(Tile((id 24963)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         24964)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 24967)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         24968)(content(Whitespace\" \"))))(Tile((id \
         24973)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24976)(content(Whitespace\" \")))))((Secondary((id \
         24980)(content(Whitespace\" \"))))(Tile((id \
         24981)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24984)(content(Whitespace\" \")))))))))(Secondary((id \
         24988)(content(Whitespace\" \"))))(Tile((id \
         24990)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         24991)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
         10))(sort Exp))((shape(Concave 10))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         24993)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         24994)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         24997)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave 6))(sort \
         Exp))((shape(Concave 6))(sort \
         Exp))))))(shards(0))(children())))(Tile((id 24998)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         24999)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 25002)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         25003)(content(Whitespace\" \"))))(Tile((id \
         25008)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25011)(content(Whitespace\" \")))))((Secondary((id \
         25015)(content(Whitespace\" \"))))(Tile((id \
         25016)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25019)(content(Whitespace\" \")))))))))(Secondary((id \
         25023)(content(Whitespace\" \"))))(Tile((id \
         25025)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         25026)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
         10))(sort Exp))((shape(Concave 10))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25028)(content(Whitespace\" \"))))(Secondary((id \
         25032)(content(Comment #err#))))(Secondary((id \
         25033)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         25034)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 25035)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         25036)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25451)(content(Whitespace\" \"))))(Tile((id \
         25038)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         25041)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave 6))(sort \
         Exp))((shape(Concave 6))(sort \
         Exp))))))(shards(0))(children())))(Tile((id 25042)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         25043)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 25046)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         25047)(content(Whitespace\" \"))))(Tile((id \
         25052)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25055)(content(Whitespace\" \")))))((Secondary((id \
         25059)(content(Whitespace\" \"))))(Tile((id \
         25060)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25063)(content(Whitespace\" \")))))))))(Secondary((id \
         25067)(content(Whitespace\" \"))))(Tile((id \
         25069)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         25070)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
         10))(sort Exp))((shape(Concave 10))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25072)(content(Whitespace\" \"))))(Secondary((id \
         25079)(content(Comment\"#2x err#\"))))(Secondary((id \
         25080)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         25081)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         25085)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Grout((id 25090)(shape Convex)))(Secondary((id \
         25086)(content(Whitespace\" \"))))(Secondary((id \
         25088)(content(Whitespace\" \"))))(Secondary((id \
         25089)(content(Whitespace\" \")))))((Secondary((id \
         25091)(content(Whitespace\" \"))))(Tile((id 25092)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         25093)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         25094)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25096)(content(Whitespace\" \"))))(Tile((id \
         25098)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         25099)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25101)(content(Whitespace\" \"))))(Tile((id \
         25105)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         25108)(content(Whitespace\" \")))))))))(Secondary((id \
         25110)(content(Whitespace\" \"))))(Secondary((id \
         25128)(content(Comment\"#err: inconsistent#\"))))(Secondary((id \
         25129)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         25133)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 25134)(content(Whitespace\" \
         \"))))(Tile((id 25136)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         25137)(content(Whitespace\" \")))))((Secondary((id \
         25139)(content(Whitespace\" \"))))(Tile((id 25140)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         25141)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         25142)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25144)(content(Whitespace\" \"))))(Tile((id \
         25146)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         25147)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25149)(content(Whitespace\" \"))))(Tile((id \
         25153)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         25156)(content(Whitespace\" \")))))))))(Secondary((id \
         25158)(content(Whitespace\" \"))))(Secondary((id \
         25176)(content(Comment\"#err: inconsistent#\"))))(Secondary((id \
         25177)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         25181)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 25182)(content(Whitespace\" \
         \"))))(Tile((id 25184)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         25185)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         25433)(content(Whitespace\" \"))))(Grout((id 25189)(shape \
         Convex)))(Secondary((id 25187)(content(Whitespace\" \
         \"))))(Secondary((id 25188)(content(Whitespace\" \
         \")))))((Secondary((id 25190)(content(Whitespace\" \"))))(Tile((id \
         25191)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         25192)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         25193)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25195)(content(Whitespace\" \"))))(Tile((id \
         25197)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         25198)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25200)(content(Whitespace\" \"))))(Tile((id \
         25204)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         25207)(content(Whitespace\" \")))))))))(Secondary((id \
         25209)(content(Whitespace\" \"))))(Secondary((id \
         25210)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         25214)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 25215)(content(Whitespace\" \
         \"))))(Tile((id 25217)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         25218)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         25434)(content(Whitespace\" \"))))(Tile((id 25437)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id 25436)(shape \
         Convex))))))))(Secondary((id 25223)(content(Whitespace\" \
         \")))))((Secondary((id 25225)(content(Whitespace\" \"))))(Tile((id \
         25226)(label([ ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         25227)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         25228)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25230)(content(Whitespace\" \"))))(Tile((id \
         25232)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         25233)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25235)(content(Whitespace\" \"))))(Tile((id \
         25239)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         25242)(content(Whitespace\" \")))))))))(Secondary((id \
         25244)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         25248)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 25249)(content(Whitespace\" \
         \"))))(Tile((id 25251)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         25252)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         25435)(content(Whitespace\" \"))))(Tile((id 25254)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         25257)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         25258)(content(Whitespace\" \")))))((Secondary((id \
         25260)(content(Whitespace\" \"))))(Tile((id 25261)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         25262)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         25263)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25265)(content(Whitespace\" \"))))(Tile((id \
         25267)(label(1.))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         25268)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         25270)(content(Whitespace\" \"))))(Tile((id \
         25274)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         25277)(content(Whitespace\" \")))))))))(Secondary((id \
         25279)(content(Whitespace\" \"))))(Secondary((id \
         25286)(content(Comment\"#2x err#\"))))(Secondary((id \
         25287)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         25288)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         25292)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 25293)(content(Whitespace\" \
         \"))))(Tile((id 25295)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         25296)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         25298)(content(Whitespace\" \"))))(Tile((id 25299)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         25302)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         25303)(content(Whitespace\" \")))))((Secondary((id \
         25305)(content(Whitespace\" \"))))(Tile((id \
         25306)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         25309)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave 6))(sort \
         Exp))((shape(Concave 6))(sort \
         Exp))))))(shards(0))(children())))(Tile((id 25310)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         25311)(label(2))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         25314)(content(Whitespace\" \")))))))))(Secondary((id \
         25316)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         25320)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 25321)(content(Whitespace\" \
         \"))))(Tile((id 25323)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         25324)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         25326)(content(Whitespace\" \"))))(Tile((id 25327)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         25330)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         25331)(content(Whitespace\" \")))))((Secondary((id \
         25333)(content(Whitespace\" \"))))(Tile((id \
         25336)(label(1.0))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         25339)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave 6))(sort \
         Exp))((shape(Concave 6))(sort \
         Exp))))))(shards(0))(children())))(Tile((id 25340)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         25341)(label(2))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         25344)(content(Whitespace\" \")))))))))(Secondary((id \
         25346)(content(Whitespace\" \"))))(Secondary((id \
         25350)(content(Comment #err#))))(Secondary((id \
         25351)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         25355)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 25356)(content(Whitespace\" \
         \"))))(Tile((id 25358)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         25359)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         25361)(content(Whitespace\" \"))))(Tile((id 25362)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         25365)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         25366)(content(Whitespace\" \")))))((Secondary((id \
         25368)(content(Whitespace\" \"))))(Tile((id \
         25369)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         25372)(label(::))(mold((out Exp)(in_())(nibs(((shape(Concave 6))(sort \
         Exp))((shape(Concave 6))(sort \
         Exp))))))(shards(0))(children())))(Tile((id 25373)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         25376)(label(2.0))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         25379)(content(Whitespace\" \")))))))))(Secondary((id \
         25381)(content(Whitespace\" \"))))(Secondary((id \
         25385)(content(Comment #err#))))(Secondary((id \
         25386)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         25390)(label(\"\\\"BYE\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))(ancestors())))(caret Outer))";
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
         let _:   -> [ ] = fun x -> if true then 1 else 1. in #2x err#\n\n\
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
    } )

let adt_egs : ScratchSlide.persistent_state =
  ( 29384,
    {
      zipper =
        "((selection((focus \
         Left)(content())))(backpack())(relatives((siblings(()((Secondary((id \
         8111)(content(Comment\"#Non-recursive sum/alias \
         tests#\"))))(Secondary((id \
         8112)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         8174)(content(Comment\"#all lines with trailing err comment should \
         have 1 error#\"))))(Secondary((id \
         8175)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         8209)(content(Comment\"#no other lines should have \
         errors#\"))))(Secondary((id \
         6602)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         6603)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         6930)(content(Comment\"#type definitions: no \
         errors#\"))))(Secondary((id \
         3939)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2974)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2975)(content(Whitespace\" \
         \"))))(Grout((id 2979)(shape Convex)))(Secondary((id \
         3016)(content(Whitespace\" \")))))((Secondary((id \
         2980)(content(Whitespace\" \"))))(Grout((id 8391)(shape \
         Convex)))(Secondary((id 2987)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3017)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         3023)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 3024)(content(Whitespace\" \
         \"))))(Tile((id 3054)(label(SingleNull))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         3039)(content(Whitespace\" \")))))((Secondary((id \
         3041)(content(Whitespace\" \"))))(Tile((id 3042)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3048)(label(One))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3049)(content(Whitespace\" \")))))))))(Secondary((id \
         2861)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2867)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2868)(content(Whitespace\" \
         \"))))(Tile((id 3059)(label(Single))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         2871)(content(Whitespace\" \")))))((Secondary((id \
         2873)(content(Whitespace\" \"))))(Tile((id 2874)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Tile((id 2876)(label(F))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         2878)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         2942)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         2883)(content(Whitespace\" \")))))))))(Secondary((id \
         2885)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2046)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2047)(content(Whitespace\" \
         \"))))(Tile((id 3185)(label(GoodSum))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         2050)(content(Whitespace\" \")))))((Secondary((id \
         2052)(content(Whitespace\" \"))))(Tile((id 2065)(label(A))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2080)(content(Whitespace\" \"))))(Tile((id 2068)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 10))(sort Typ))((shape(Concave \
         10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2069)(content(Whitespace\" \"))))(Tile((id 2070)(label(B))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2072)(content(Whitespace\" \"))))(Tile((id 2073)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 10))(sort Typ))((shape(Concave \
         10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2074)(content(Whitespace\" \"))))(Tile((id 2375)(label(C))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3186)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         3201)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         2077)(content(Whitespace\" \")))))))))(Secondary((id \
         6117)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         6123)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 6124)(content(Whitespace\" \
         \"))))(Tile((id 6132)(label(Partial))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         6133)(content(Whitespace\" \")))))((Secondary((id \
         6135)(content(Whitespace\" \"))))(Tile((id 6138)(label(Ok))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         6139)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id 6141)(shape \
         Convex))))))))(Secondary((id 6143)(content(Whitespace\" \
         \"))))(Tile((id 6172)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 10))(sort Typ))((shape(Concave \
         10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6181)(content(Whitespace\" \"))))(Grout((id 6180)(shape \
         Convex)))(Secondary((id 6173)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3316)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2495)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2496)(content(Whitespace\" \
         \"))))(Tile((id 3477)(label(DoubleAlias))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         2499)(content(Whitespace\" \")))))((Secondary((id \
         2501)(content(Whitespace\" \"))))(Tile((id \
         3460)(label(GoodSum))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2506)(content(Whitespace\" \")))))))))(Secondary((id \
         1428)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1434)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 1435)(content(Whitespace\" \
         \"))))(Tile((id 8239)(label(VerticalLeading))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         1438)(content(Whitespace\" \")))))((Secondary((id \
         8305)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         8307)(label(+))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8308)(content(Whitespace\" \"))))(Tile((id 8306)(label(A))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1446)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1447)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave 10))(sort \
         Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1448)(content(Whitespace\" \"))))(Tile((id 1449)(label(B))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         1451)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         3631)(label(GoodSum))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         1456)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1457)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave 10))(sort \
         Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1458)(content(Whitespace\" \"))))(Tile((id 1459)(label(C))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         1461)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         1466)(label(Bool))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
         1468)(label(->))(mold((out Typ)(in_())(nibs(((shape(Concave 6))(sort \
         Typ))((shape(Concave 6))(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         1473)(label(Bool))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         1476)(content(Whitespace\" \"))))(Secondary((id \
         1474)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
         3687)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         3951)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         6938)(content(Comment\"#incorrect or incomplete type \
         definitions#\"))))(Secondary((id \
         3860)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         3866)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 3867)(content(Whitespace\" \
         \"))))(Tile((id 3879)(label(badTypeName))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3880)(content(Whitespace\" \")))))((Secondary((id \
         3882)(content(Whitespace\" \"))))(Grout((id 7903)(shape \
         Convex)))(Secondary((id 3898)(content(Whitespace\" \
         \")))))))))(Secondary((id 6939)(content(Whitespace\" \
         \"))))(Secondary((id 7915)(content(Comment\"#err: invalid type \
         name#\"))))(Secondary((id \
         3900)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         3906)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 3907)(content(Whitespace\" \
         \"))))(Tile((id 3908)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Grout((id 3911)(shape \
         Convex)))(Tile((id 3910)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4907)(content(Whitespace\" \"))))(Grout((id 3914)(shape \
         Convex))))))))(Secondary((id 3916)(content(Whitespace\" \
         \")))))((Secondary((id 3918)(content(Whitespace\" \"))))(Grout((id \
         6260)(shape Convex)))(Secondary((id 3936)(content(Whitespace\" \
         \")))))))))(Secondary((id 6974)(content(Whitespace\" \
         \"))))(Secondary((id 7938)(content(Comment\"#err: invalid type \
         name#\"))))(Secondary((id \
         7939)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         7945)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 7946)(content(Whitespace\" \
         \"))))(Grout((id 7995)(shape Convex)))(Secondary((id \
         7959)(content(Whitespace\" \")))))((Secondary((id \
         7961)(content(Whitespace\" \"))))(Tile((id \
         7974)(label(badTypeToken))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7977)(content(Whitespace\" \")))))))))(Secondary((id \
         7979)(content(Whitespace\" \"))))(Secondary((id \
         8008)(content(Comment\"#err: invalid type token#\"))))(Secondary((id \
         6205)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         6211)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 6212)(content(Whitespace\" \
         \"))))(Tile((id 6253)(label(NotASum))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         6221)(content(Whitespace\" \")))))((Secondary((id \
         6223)(content(Whitespace\" \"))))(Tile((id \
         6232)(label(NotInSum))(mold((out Typ)(in_())(nibs(((shape \
         Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         6233)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         6238)(label(Bool))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         6241)(content(Whitespace\" \")))))))))(Secondary((id \
         6995)(content(Whitespace\" \"))))(Secondary((id \
         8363)(content(Comment\"#err: cons not in sum#\"))))(Secondary((id \
         8309)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         8315)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 8316)(content(Whitespace\" \
         \"))))(Tile((id 8371)(label(Bool))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         8321)(content(Whitespace\" \")))))((Secondary((id \
         8330)(content(Whitespace\" \"))))(Grout((id 8329)(shape \
         Convex)))(Secondary((id 8323)(content(Whitespace\" \
         \"))))(Secondary((id 8324)(content(Whitespace\" \
         \")))))))))(Secondary((id 8328)(content(Whitespace\" \
         \"))))(Secondary((id 8361)(content(Comment\"#err: shadows base \
         type#\"))))(Secondary((id \
         3688)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         3694)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 3695)(content(Whitespace\" \
         \"))))(Tile((id 3701)(label(Dupes))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         3702)(content(Whitespace\" \")))))((Secondary((id \
         8009)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         7772)(label(+))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8012)(content(Whitespace\" \"))))(Tile((id \
         3708)(label(Guy))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
         3709)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         3714)(label(Bool))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         8052)(content(Whitespace\" \"))))(Secondary((id \
         8059)(content(Comment\"#no err#\"))))(Secondary((id \
         8010)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         3723)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave 10))(sort \
         Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3724)(content(Whitespace\" \"))))(Tile((id \
         3728)(label(Guy))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
         3729)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         3733)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         8033)(content(Whitespace\" \"))))(Secondary((id \
         8051)(content(Comment\"#err: already used#\"))))(Secondary((id \
         8011)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         6271)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave 10))(sort \
         Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6273)(content(Whitespace\" \"))))(Tile((id \
         6276)(label(Guy))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6277)(content(Whitespace\" \")))))))))(Secondary((id \
         7023)(content(Whitespace\" \"))))(Secondary((id \
         8032)(content(Comment\"#err: already used#\"))))(Secondary((id \
         3800)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         3807)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 3808)(content(Whitespace\" \
         \"))))(Tile((id 3816)(label(BadCons))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         3817)(content(Whitespace\" \")))))((Secondary((id \
         7701)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         7770)(label(+))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7771)(content(Whitespace\" \"))))(Tile((id 7738)(label(Um))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         7739)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         7746)(label(Unbound))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         7747)(content(Whitespace\" \"))))(Secondary((id \
         7769)(content(Comment\"#err: unbound type var#\"))))(Secondary((id \
         7524)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         7532)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave 10))(sort \
         Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7533)(content(Whitespace\" \"))))(Tile((id \
         7531)(label(invalid))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7534)(content(Whitespace\" \"))))(Secondary((id \
         7547)(content(Comment\"#err: invalid#\"))))(Secondary((id \
         7517)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         7522)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave 10))(sort \
         Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7523)(content(Whitespace\" \"))))(Tile((id \
         7521)(label(Bool))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7548)(content(Whitespace\" \"))))(Secondary((id \
         7578)(content(Comment\"#err: expected cons found \
         type#\"))))(Secondary((id \
         7498)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         7497)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave 10))(sort \
         Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         7501)(content(Whitespace\" \"))))(Tile((id \
         3823)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
         3824)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         3828)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         7602)(content(Whitespace\" \"))))(Secondary((id \
         7632)(content(Comment\"#err: expected cons found \
         type#\"))))(Secondary((id \
         7499)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         3830)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave 10))(sort \
         Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3831)(content(Whitespace\" \"))))(Tile((id \
         3832)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
         Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
         1))(children(((Grout((id 3834)(shape Convex))))))))(Tile((id \
         3836)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         3840)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         7633)(content(Whitespace\" \"))))(Secondary((id \
         7663)(content(Comment\"#err: expected cons found \
         type#\"))))(Secondary((id \
         7500)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         3842)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave 10))(sort \
         Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3843)(content(Whitespace\" \"))))(Tile((id 3844)(label(A))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         3846)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         6020)(label(Bool))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         3849)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         3853)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         3856)(content(Whitespace\" \")))))))))(Secondary((id \
         3859)(content(Whitespace\" \"))))(Secondary((id \
         7700)(content(Comment\"#err: expected cons found \
         app#\"))))(Secondary((id \
         1563)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         3564)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         8704)(content(Comment\"#sums in compound aliases dont add ctrs to \
         scope#\"))))(Secondary((id \
         8930)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         8985)(content(Comment\"#but compound alias types should propagate \
         analytically#\"))))(Secondary((id \
         3512)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         3518)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 3519)(content(Whitespace\" \
         \"))))(Tile((id 9369)(label(CompoundAlias))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         3531)(content(Whitespace\" \")))))((Secondary((id \
         3533)(content(Whitespace\" \"))))(Tile((id \
         3534)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
         Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
         1))(children(((Tile((id 3538)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id 3539)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
         14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3540)(content(Whitespace\" \"))))(Tile((id \
         3550)(label(Anonymous))(mold((out Typ)(in_())(nibs(((shape \
         Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3551)(content(Whitespace\" \"))))(Tile((id 3552)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 10))(sort Typ))((shape(Concave \
         10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3553)(content(Whitespace\" \"))))(Tile((id \
         3557)(label(Sum))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         3560)(content(Whitespace\" \")))))))))(Secondary((id \
         3562)(content(Whitespace\" \"))))(Secondary((id \
         1485)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2243)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2244)(content(Whitespace\" \
         \"))))(Tile((id 9126)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2250)(content(Whitespace\" \")))))((Secondary((id \
         2262)(content(Whitespace\" \"))))(Tile((id \
         3503)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 8493)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 3507)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6011)(content(Whitespace\" \"))))(Tile((id \
         3511)(label(Sum))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2252)(content(Whitespace\" \")))))))))(Secondary((id \
         6892)(content(Whitespace\" \"))))(Secondary((id \
         7774)(content(Comment\"#err: not defined#\"))))(Secondary((id \
         8564)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         8569)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 8570)(content(Whitespace\" \
         \"))))(Tile((id 9128)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 8573)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9070)(content(Whitespace\" \"))))(Tile((id \
         9383)(label(CompoundAlias))(mold((out Typ)(in_())(nibs(((shape \
         Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8586)(content(Whitespace\" \")))))((Secondary((id \
         8587)(content(Whitespace\" \"))))(Tile((id \
         8588)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 8589)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 8591)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8592)(content(Whitespace\" \"))))(Tile((id \
         8596)(label(Sum))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8599)(content(Whitespace\" \")))))))))(Secondary((id \
         8987)(content(Whitespace\" \"))))(Secondary((id \
         8996)(content(Comment\"#no error#\"))))(Secondary((id \
         8805)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         8811)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 8812)(content(Whitespace\" \
         \"))))(Tile((id 9254)(label(Yorp))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         8818)(content(Whitespace\" \")))))((Secondary((id \
         8819)(content(Whitespace\" \"))))(Tile((id \
         8823)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8824)(content(Whitespace\" \"))))(Tile((id 8826)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave \
         6))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8827)(content(Whitespace\" \"))))(Tile((id \
         8836)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
         Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
         1))(children(((Tile((id 9229)(label(Inside))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8830)(content(Whitespace\" \"))))(Tile((id 8831)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 10))(sort Typ))((shape(Concave \
         10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8832)(content(Whitespace\" \"))))(Tile((id \
         9235)(label(Ouside))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         8835)(content(Whitespace\" \")))))))))(Secondary((id \
         8837)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         8842)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 8843)(content(Whitespace\" \
         \"))))(Tile((id 9129)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8847)(content(Whitespace\" \")))))((Secondary((id \
         8853)(content(Whitespace\" \"))))(Tile((id 8858)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 8859)(content(Whitespace\" \
         \"))))(Tile((id 8860)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8882)(content(Whitespace\" \")))))))))(Secondary((id \
         8884)(content(Whitespace\" \"))))(Tile((id \
         9241)(label(Inside))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8889)(content(Whitespace\" \")))))))))(Secondary((id \
         9032)(content(Whitespace\" \"))))(Secondary((id \
         9049)(content(Comment\"#err: not defined#\"))))(Secondary((id \
         8494)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         9001)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 9002)(content(Whitespace\" \
         \"))))(Tile((id 9130)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 9006)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9071)(content(Whitespace\" \"))))(Tile((id \
         9253)(label(Yorp))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9010)(content(Whitespace\" \")))))((Secondary((id \
         9011)(content(Whitespace\" \"))))(Tile((id 9016)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 9017)(content(Whitespace\" \
         \"))))(Tile((id 9018)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9020)(content(Whitespace\" \")))))))))(Secondary((id \
         9022)(content(Whitespace\" \"))))(Tile((id \
         9248)(label(Inside))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9029)(content(Whitespace\" \")))))))))(Secondary((id \
         9051)(content(Whitespace\" \"))))(Secondary((id \
         9060)(content(Comment\"#no error#\"))))(Secondary((id \
         9073)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         9079)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 9080)(content(Whitespace\" \
         \"))))(Tile((id 9094)(label(Gargs))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         9095)(content(Whitespace\" \")))))((Secondary((id \
         9096)(content(Whitespace\" \"))))(Tile((id 9097)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         9199)(label(BigGuy))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9102)(content(Whitespace\" \"))))(Tile((id 9103)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 10))(sort Typ))((shape(Concave \
         10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9104)(content(Whitespace\" \"))))(Tile((id \
         9211)(label(Small))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         9111)(content(Whitespace\" \")))))))))(Secondary((id \
         9072)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         9116)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 9117)(content(Whitespace\" \
         \"))))(Tile((id 9131)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9123)(content(Whitespace\" \")))))((Secondary((id \
         9132)(content(Whitespace\" \"))))(Tile((id \
         9206)(label(BigGuy))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9137)(content(Whitespace\" \")))))))))(Secondary((id \
         9178)(content(Whitespace\" \"))))(Secondary((id \
         9195)(content(Comment\"#err: not defined#\"))))(Secondary((id \
         9138)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         9143)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 9144)(content(Whitespace\" \
         \"))))(Tile((id 9147)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 9149)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9150)(content(Whitespace\" \"))))(Tile((id \
         9156)(label(Gargs))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9157)(content(Whitespace\" \")))))((Secondary((id \
         9158)(content(Whitespace\" \"))))(Tile((id 9159)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9217)(label(BigGuy))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9166)(content(Whitespace\" \")))))))))(Secondary((id \
         9167)(content(Whitespace\" \"))))(Secondary((id \
         9176)(content(Comment\"#no error#\"))))(Secondary((id \
         9255)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         9260)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 9261)(content(Whitespace\" \
         \"))))(Tile((id 9262)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 9264)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9265)(content(Whitespace\" \"))))(Tile((id \
         9275)(label(Gargs))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9271)(content(Whitespace\" \")))))((Secondary((id \
         9276)(content(Whitespace\" \"))))(Tile((id \
         9283)(label(BigGuy))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9284)(content(Whitespace\" \"))))(Tile((id 9286)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 6))(sort Exp))((shape(Concave \
         6))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9287)(content(Whitespace\" \"))))(Tile((id 9338)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9344)(label(BigGuy))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9291)(content(Whitespace\" \")))))))))(Secondary((id \
         9292)(content(Whitespace\" \"))))(Secondary((id \
         9354)(content(Comment\"#no error#\"))))(Secondary((id \
         9293)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         4008)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         8078)(content(Comment\"#unbound tyvars treated as \
         unknown-typehole#\"))))(Secondary((id \
         4053)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         4058)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 4059)(content(Whitespace\" \
         \"))))(Tile((id 4060)(label(a))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 4062)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Tile((id \
         4066)(label(Bad))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4067)(content(Whitespace\" \")))))((Secondary((id \
         4069)(content(Whitespace\" \"))))(Tile((id 8085)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4075)(content(Whitespace\" \")))))))))(Secondary((id \
         4077)(content(Whitespace\" \"))))(Tile((id 4078)(label(a))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8079)(content(Whitespace\" \"))))(Tile((id 8082)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 8))(sort Exp))((shape(Concave \
         8))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8083)(content(Whitespace\" \"))))(Tile((id 8086)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4080)(label(\";\"))(mold((out Exp)(in_())(nibs(((shape(Concave \
         10))(sort Exp))((shape(Concave 10))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6758)(content(Whitespace\" \"))))(Secondary((id \
         7776)(content(Comment\"#err: not bound#\"))))(Secondary((id \
         6021)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         6022)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         8093)(content(Comment\"#non-sum-types cant be \
         recursive#\"))))(Secondary((id \
         6070)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         6076)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 6077)(content(Whitespace\" \
         \"))))(Tile((id 6081)(label(Lol))(mold((out TPat)(in_())(nibs(((shape \
         Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         6082)(content(Whitespace\" \")))))((Secondary((id \
         6084)(content(Whitespace\" \"))))(Tile((id \
         6088)(label(Lol))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6091)(content(Whitespace\" \")))))))))(Secondary((id \
         6736)(content(Whitespace\" \"))))(Secondary((id \
         7778)(content(Comment\"#err: not bound#\"))))(Secondary((id \
         6466)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         6703)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         6805)(content(Comment\"#no errors: analytic \
         shadowing#\"))))(Secondary((id \
         6806)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         6812)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 6813)(content(Whitespace\" \
         \"))))(Tile((id 6819)(label(Tork1))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         6820)(content(Whitespace\" \")))))((Secondary((id \
         6822)(content(Whitespace\" \"))))(Tile((id 6823)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         6829)(label(Blob))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6832)(content(Whitespace\" \")))))))))(Secondary((id \
         6834)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         6840)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 6841)(content(Whitespace\" \
         \"))))(Tile((id 6847)(label(Tork2))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         6848)(content(Whitespace\" \")))))((Secondary((id \
         6850)(content(Whitespace\" \"))))(Tile((id 6851)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         6857)(label(Blob))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6860)(content(Whitespace\" \")))))))))(Secondary((id \
         6862)(content(Whitespace\" \"))))(Secondary((id \
         6863)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         6868)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 6869)(content(Whitespace\" \
         \"))))(Tile((id 6870)(label(x))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 6872)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Tile((id \
         6878)(label(Tork1))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6879)(content(Whitespace\" \")))))((Secondary((id \
         6881)(content(Whitespace\" \"))))(Tile((id \
         6886)(label(Blob))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6889)(content(Whitespace\" \")))))))))(Secondary((id \
         6467)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         3994)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         5074)(content(Comment\"#exp tests: happy#\"))))(Secondary((id \
         4344)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         4350)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 4351)(content(Whitespace\" \
         \"))))(Tile((id 5397)(label(YoDawg))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         4354)(content(Whitespace\" \")))))((Secondary((id \
         4356)(content(Whitespace\" \"))))(Secondary((id \
         4357)(content(Whitespace\" \"))))(Tile((id 4952)(label(Yo))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4360)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         4434)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         6501)(content(Whitespace\" \"))))(Tile((id 6507)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 10))(sort Typ))((shape(Concave \
         10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         8413)(content(Whitespace\" \"))))(Tile((id 6502)(label(Bo))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         6503)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         6506)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         4367)(label(+))(mold((out Typ)(in_())(nibs(((shape(Concave 10))(sort \
         Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4368)(content(Whitespace\" \"))))(Tile((id \
         4980)(label(Dawg))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
         4371)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         4438)(label(Bool))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         4378)(content(Whitespace\" \")))))))))(Secondary((id \
         4388)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         4393)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 4394)(content(Whitespace\" \
         \"))))(Tile((id 4728)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4397)(content(Whitespace\" \")))))((Secondary((id \
         4399)(content(Whitespace\" \"))))(Tile((id 4954)(label(Yo))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4402)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4403)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4407)(content(Whitespace\" \")))))))))(Secondary((id \
         4595)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         4600)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 4601)(content(Whitespace\" \
         \"))))(Tile((id 4730)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5016)(content(Whitespace\" \"))))(Tile((id 4604)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5013)(content(Whitespace\" \"))))(Tile((id \
         6116)(label(YoDawg))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4607)(content(Whitespace\" \")))))((Secondary((id \
         4609)(content(Whitespace\" \"))))(Tile((id 4956)(label(Yo))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4612)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4613)(label(2))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4617)(content(Whitespace\" \")))))))))(Secondary((id \
         4619)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         4624)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 4625)(content(Whitespace\" \
         \"))))(Tile((id 4732)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5017)(content(Whitespace\" \"))))(Tile((id 4628)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5012)(content(Whitespace\" \"))))(Tile((id 4630)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5007)(label(Yo))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
         4633)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         4638)(label(Bool))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         4639)(content(Whitespace\" \")))))((Secondary((id \
         4641)(content(Whitespace\" \"))))(Tile((id 5009)(label(Yo))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4644)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4649)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4652)(content(Whitespace\" \")))))))))(Secondary((id \
         4656)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         4661)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 4662)(content(Whitespace\" \
         \"))))(Tile((id 4734)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5018)(content(Whitespace\" \"))))(Tile((id 4665)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5014)(content(Whitespace\" \"))))(Tile((id \
         4667)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
         Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
         1))(children(((Tile((id 5005)(label(Yo))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5010)(content(Whitespace\" \"))))(Tile((id 4670)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 10))(sort Typ))((shape(Concave \
         10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5011)(content(Whitespace\" \"))))(Tile((id \
         4984)(label(Dawg))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
         4673)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave 14))(sort \
         Typ))((shape(Concave 14))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4674)(content(Whitespace\" \"))))(Tile((id \
         4678)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         4679)(content(Whitespace\" \")))))((Secondary((id \
         4681)(content(Whitespace\" \"))))(Tile((id \
         4682)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 4989)(label(Dawg))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 4685)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4686)(label(5))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4690)(content(Whitespace\" \")))))))))(Secondary((id \
         4692)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         4697)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 4698)(content(Whitespace\" \
         \"))))(Tile((id 4736)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5019)(content(Whitespace\" \"))))(Tile((id 4701)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5015)(content(Whitespace\" \"))))(Tile((id \
         4713)(label(DoubleAlias))(mold((out Typ)(in_())(nibs(((shape \
         Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4714)(content(Whitespace\" \")))))((Secondary((id \
         4716)(content(Whitespace\" \"))))(Tile((id 4717)(label(C))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4719)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4720)(label(4))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4724)(content(Whitespace\" \")))))))))(Secondary((id \
         4782)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         4474)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         4851)(content(Comment\"#exp tests: errors#\"))))(Secondary((id \
         4783)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         4788)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 4789)(content(Whitespace\" \
         \"))))(Tile((id 4837)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4792)(content(Whitespace\" \")))))((Secondary((id \
         4794)(content(Whitespace\" \"))))(Tile((id 4795)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4797)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4798)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4802)(content(Whitespace\" \")))))))))(Secondary((id \
         7107)(content(Whitespace\" \"))))(Secondary((id \
         7780)(content(Comment\"#err: incons with arrow#\"))))(Secondary((id \
         4804)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         4809)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 4810)(content(Whitespace\" \
         \"))))(Tile((id 4835)(label(_))(mold((out Pat)(in_())(nibs(((shape \
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
         4827)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4831)(content(Whitespace\" \")))))))))(Secondary((id \
         7135)(content(Whitespace\" \"))))(Secondary((id \
         7782)(content(Comment\"#err: cons undefined#\"))))(Secondary((id \
         4737)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         4479)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 4480)(content(Whitespace\" \
         \"))))(Tile((id 4739)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4483)(content(Whitespace\" \")))))((Secondary((id \
         4485)(content(Whitespace\" \"))))(Tile((id 4486)(label(B))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4488)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4493)(label(\"\\\"lol\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4496)(content(Whitespace\" \")))))))))(Secondary((id \
         7158)(content(Whitespace\" \"))))(Secondary((id \
         7794)(content(Comment\"#err: type incons#\"))))(Secondary((id \
         4444)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         4546)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 4547)(content(Whitespace\" \
         \"))))(Tile((id 4741)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5020)(content(Whitespace\" \"))))(Tile((id 4550)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4551)(content(Whitespace\" \"))))(Tile((id 4552)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5000)(label(Yo))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
         4555)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         4560)(label(Bool))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         4561)(content(Whitespace\" \")))))((Secondary((id \
         4563)(content(Whitespace\" \"))))(Tile((id 4958)(label(Yo))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4574)(content(Whitespace\" \")))))))))(Secondary((id \
         7183)(content(Whitespace\" \"))))(Secondary((id \
         7793)(content(Comment\"#err: type incons#\"))))(Secondary((id \
         4410)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1756)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 1757)(content(Whitespace\" \
         \"))))(Tile((id 4743)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5021)(content(Whitespace\" \"))))(Tile((id 1760)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1761)(content(Whitespace\" \"))))(Tile((id 1762)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5003)(label(Yo))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1765)(content(Whitespace\" \")))))((Secondary((id \
         1767)(content(Whitespace\" \"))))(Tile((id 4960)(label(Yo))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1770)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4582)(label(\"\\\"lol\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1778)(content(Whitespace\" \")))))))))(Secondary((id \
         1780)(content(Whitespace\" \"))))(Secondary((id \
         7792)(content(Comment\"#err: type incons#\"))))(Secondary((id \
         1713)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1718)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 1719)(content(Whitespace\" \
         \"))))(Tile((id 4745)(label(_))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5022)(content(Whitespace\" \"))))(Tile((id 1722)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1723)(content(Whitespace\" \"))))(Tile((id 4590)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         4593)(label(One))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1735)(content(Whitespace\" \")))))((Secondary((id \
         1737)(content(Whitespace\" \"))))(Tile((id 4962)(label(Yo))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1740)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4594)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1748)(content(Whitespace\" \")))))))))(Secondary((id \
         7220)(content(Whitespace\" \"))))(Secondary((id \
         7791)(content(Comment\"#err: type incons#\"))))(Secondary((id \
         5051)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         5052)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         7272)(content(Comment\"#pat tests: happy (but refutable patterns so \
         weird)#\"))))(Secondary((id \
         5239)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         5244)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 5245)(content(Whitespace\" \
         \"))))(Tile((id 6526)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5249)(content(Whitespace\" \")))))((Secondary((id \
         5251)(content(Whitespace\" \"))))(Tile((id 6509)(label(Bo))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5257)(content(Whitespace\" \")))))))))(Secondary((id \
         5286)(content(Whitespace\" \"))))(Secondary((id \
         5307)(content(Comment\"#kind of a weird edge#\"))))(Secondary((id \
         5075)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         5080)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 5081)(content(Whitespace\" \
         \"))))(Tile((id 5139)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5140)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         5341)(label(1))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         5084)(content(Whitespace\" \")))))((Secondary((id \
         5154)(content(Whitespace\" \"))))(Tile((id \
         5159)(label(Dawg))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         5149)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5164)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5086)(content(Whitespace\" \")))))))))(Secondary((id \
         5428)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         5433)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 5434)(content(Whitespace\" \
         \"))))(Tile((id 5437)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5438)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         5757)(label(1))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         5441)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5442)(content(Whitespace\" \"))))(Tile((id \
         5449)(label(YoDawg))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5450)(content(Whitespace\" \")))))((Secondary((id \
         5460)(content(Whitespace\" \"))))(Tile((id 5466)(label(Yo))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5467)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5469)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5452)(content(Whitespace\" \")))))))))(Secondary((id \
         5708)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         5713)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 5714)(content(Whitespace\" \
         \"))))(Tile((id 5717)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5718)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         5755)(label(1))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         5721)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5722)(content(Whitespace\" \"))))(Tile((id 5753)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5747)(label(Yo))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
         5748)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         5752)(label(Int))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         5730)(content(Whitespace\" \")))))((Secondary((id \
         5732)(content(Whitespace\" \"))))(Tile((id 5735)(label(Yo))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5736)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5737)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5741)(content(Whitespace\" \")))))))))(Secondary((id \
         5743)(content(Whitespace\" \"))))(Secondary((id \
         2700)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         5092)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 5093)(content(Whitespace\" \
         \"))))(Tile((id 5167)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 5171)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5180)(content(Whitespace\" \"))))(Tile((id 5173)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5175)(label(Yo))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5096)(content(Whitespace\" \")))))((Secondary((id \
         5177)(content(Whitespace\" \"))))(Tile((id 5179)(label(Yo))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5098)(content(Whitespace\" \")))))))))(Secondary((id \
         5103)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         5120)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         5208)(content(Comment\"#pat tests: errors#\"))))(Secondary((id \
         5522)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         5527)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 5528)(content(Whitespace\" \
         \"))))(Tile((id 5590)(label(2))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5555)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         5594)(label(1))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         5540)(content(Whitespace\" \")))))((Secondary((id \
         5542)(content(Whitespace\" \"))))(Tile((id 5596)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5551)(content(Whitespace\" \")))))))))(Secondary((id \
         7273)(content(Whitespace\" \"))))(Secondary((id \
         7801)(content(Comment\"#err: incons with arrow#\"))))(Secondary((id \
         5559)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         5564)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 5565)(content(Whitespace\" \
         \"))))(Tile((id 6317)(label(NotDefined))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5568)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         5592)(label(1))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         5571)(content(Whitespace\" \")))))((Secondary((id \
         5573)(content(Whitespace\" \"))))(Tile((id 5598)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5578)(content(Whitespace\" \")))))))))(Secondary((id \
         5580)(content(Whitespace\" \"))))(Secondary((id \
         7803)(content(Comment\"#err: cons undefined#\"))))(Secondary((id \
         5209)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         5264)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 5265)(content(Whitespace\" \
         \"))))(Tile((id 5268)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5269)(content(Whitespace\" \")))))((Secondary((id \
         5271)(content(Whitespace\" \"))))(Tile((id \
         5285)(label(Dawg))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5277)(content(Whitespace\" \")))))))))(Secondary((id \
         5279)(content(Whitespace\" \"))))(Secondary((id \
         7805)(content(Comment\"#err: type incons#\"))))(Secondary((id \
         5329)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         5346)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 5347)(content(Whitespace\" \
         \"))))(Tile((id 5350)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5351)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         5377)(label(true))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         5354)(content(Whitespace\" \")))))((Secondary((id \
         5356)(content(Whitespace\" \"))))(Tile((id \
         5361)(label(Dawg))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         5362)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5367)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5370)(content(Whitespace\" \")))))))))(Secondary((id \
         7339)(content(Whitespace\" \"))))(Secondary((id \
         7807)(content(Comment\"#err: type incons#\"))))(Secondary((id \
         5470)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         5475)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 5476)(content(Whitespace\" \
         \"))))(Tile((id 5479)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 5505)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5758)(content(Whitespace\" \"))))(Tile((id \
         5512)(label(YoDawg))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5486)(content(Whitespace\" \")))))((Secondary((id \
         5488)(content(Whitespace\" \"))))(Tile((id 5518)(label(Yo))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5519)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5521)(label(1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5502)(content(Whitespace\" \")))))))))(Secondary((id \
         7358)(content(Whitespace\" \"))))(Secondary((id \
         7809)(content(Comment\"#err: type incons#\"))))(Secondary((id \
         5599)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         5604)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 5605)(content(Whitespace\" \
         \"))))(Tile((id 5608)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5663)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         5665)(label(1))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         5609)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5759)(content(Whitespace\" \"))))(Tile((id 5666)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5669)(label(Yo))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5617)(content(Whitespace\" \")))))((Secondary((id \
         5619)(content(Whitespace\" \"))))(Tile((id 5622)(label(Yo))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5628)(content(Whitespace\" \")))))))))(Secondary((id \
         5630)(content(Whitespace\" \"))))(Secondary((id \
         7811)(content(Comment\"#err: type incons#\"))))(Secondary((id \
         5631)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         5636)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 5637)(content(Whitespace\" \
         \"))))(Tile((id 5640)(label(Yo))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         5687)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         5703)(label(1))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         5671)(label(:))(mold((out Pat)(in_())(nibs(((shape(Concave 11))(sort \
         Pat))((shape(Concave 11))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         5760)(content(Whitespace\" \"))))(Tile((id 5673)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 10))(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         5675)(label(Yo))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
         5676)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         5697)(label(Bool))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         5649)(content(Whitespace\" \")))))((Secondary((id \
         5651)(content(Whitespace\" \"))))(Tile((id 5654)(label(Yo))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5655)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5702)(label(true))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5660)(content(Whitespace\" \")))))))))(Secondary((id \
         7395)(content(Whitespace\" \"))))(Secondary((id \
         7813)(content(Comment\"#err: type incons#\"))))(Secondary((id \
         6564)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         7846)(label(\"\\\"Thats all, folks\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2231)(content(Whitespace\"\\226\\143\\142\")))))))(ancestors())))(caret \
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
    } )

let adt_dynamics_tests : ScratchSlide.persistent_state =
  ( 2482,
    {
      zipper =
        "((selection((focus \
         Right)(content())))(backpack())(relatives((siblings(((Secondary((id \
         2389)(content(Comment\"#recursive sum type dynamics \
         tests#\"))))(Secondary((id \
         2390)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         2467)(content(Comment\"#all calls should evaluate fully with no exns \
         or cast fails#\"))))(Secondary((id \
         2352)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         337)(label(type = in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 338)(content(Whitespace\" \
         \"))))(Tile((id 342)(label(Exp))(mold((out TPat)(in_())(nibs(((shape \
         Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         343)(content(Whitespace\" \")))))((Secondary((id \
         2350)(content(Whitespace\" \"))))(Tile((id 352)(label(Var))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         353)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape(Concave \
         1))(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
         1))(children(((Tile((id 360)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         2351)(content(Whitespace\" \"))))(Tile((id 362)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 10))(sort Typ))((shape(Concave \
         10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         364)(content(Whitespace\" \"))))(Tile((id 2480)(label(Lam))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         371)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape(Concave \
         1))(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
         1))(children(((Tile((id 378)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id 379)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
         14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         381)(content(Whitespace\" \"))))(Tile((id 384)(label(Exp))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         405)(content(Whitespace\" \")))))))))(Secondary((id \
         2472)(content(Whitespace\"\\226\\143\\142\")))))((Secondary((id \
         2097)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2102)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 2103)(content(Whitespace\" \
         \"))))(Tile((id 2297)(label(s0))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2110)(content(Whitespace\" \"))))(Tile((id 2111)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2112)(content(Whitespace\" \"))))(Tile((id \
         2113)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
         Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
         1))(children(((Grout((id 2116)(shape Convex)))(Tile((id \
         2115)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave 14))(sort \
         Typ))((shape(Concave 14))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2117)(content(Whitespace\" \"))))(Grout((id 2120)(shape \
         Convex)))(Tile((id 2119)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
         14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2295)(content(Whitespace\" \"))))(Grout((id 2123)(shape \
         Convex))))))))(Secondary((id 2125)(content(Whitespace\" \
         \"))))(Tile((id 2127)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave \
         6))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2128)(content(Whitespace\" \"))))(Grout((id 2294)(shape \
         Convex)))(Secondary((id 2133)(content(Whitespace\" \
         \")))))((Secondary((id 2135)(content(Whitespace\" \"))))(Tile((id \
         2140)(label(fun ->))(mold((out Exp)(in_(Pat))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 2141)(content(Whitespace\" \
         \"))))(Tile((id 2142)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id 2143)(label(e))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 2145)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
         14))(sort Pat))))))(shards(0))(children())))(Tile((id \
         2146)(label(x))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
         2148)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave 14))(sort \
         Pat))((shape(Concave 14))(sort \
         Pat))))))(shards(0))(children())))(Tile((id 2149)(label(v))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         2151)(content(Whitespace\" \")))))))))(Secondary((id \
         2154)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2160)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 2161)(content(Whitespace\" \
         \"))))(Tile((id 2162)(label(e))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2164)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2165)(label(| =>))(mold((out Rul)(in_(Pat))(nibs(((shape(Concave \
         19))(sort Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 2166)(content(Whitespace\" \
         \"))))(Tile((id 2170)(label(Var))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2171)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         2172)(label(y))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         2174)(content(Whitespace\" \")))))))))(Secondary((id \
         2176)(content(Whitespace\" \"))))(Tile((id \
         2177)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 2181)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2182)(content(Whitespace\" \"))))(Tile((id 2183)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2185)(content(Whitespace\" \"))))(Tile((id \
         2188)(label($==))(mold((out Exp)(in_())(nibs(((shape(Concave 8))(sort \
         Exp))((shape(Concave 8))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2189)(content(Whitespace\" \"))))(Tile((id 2190)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2194)(content(Whitespace\" \")))))((Secondary((id \
         2198)(content(Whitespace\" \"))))(Tile((id 2199)(label(v))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2203)(content(Whitespace\" \")))))))))(Secondary((id \
         2207)(content(Whitespace\" \"))))(Tile((id 2208)(label(e))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2210)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         2211)(label(| =>))(mold((out Rul)(in_(Pat))(nibs(((shape(Concave \
         19))(sort Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 2212)(content(Whitespace\" \
         \"))))(Tile((id 2216)(label(Lam))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         2217)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         2218)(label(y))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
         2220)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave 14))(sort \
         Pat))((shape(Concave 14))(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2221)(content(Whitespace\" \"))))(Tile((id 2224)(label(e1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         2225)(content(Whitespace\" \")))))))))(Secondary((id \
         2227)(content(Whitespace\" \"))))(Tile((id \
         2228)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 2232)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2233)(content(Whitespace\" \"))))(Tile((id 2234)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2236)(content(Whitespace\" \"))))(Tile((id \
         2239)(label($==))(mold((out Exp)(in_())(nibs(((shape(Concave 8))(sort \
         Exp))((shape(Concave 8))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2240)(content(Whitespace\" \"))))(Tile((id 2241)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2245)(content(Whitespace\" \")))))((Secondary((id \
         2249)(content(Whitespace\" \"))))(Tile((id 2250)(label(e))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2254)(content(Whitespace\" \")))))))))(Secondary((id \
         2258)(content(Whitespace\" \"))))(Tile((id \
         2262)(label(Lam))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         2263)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2264)(label(y))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         2266)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2267)(content(Whitespace\" \"))))(Tile((id 2299)(label(s0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2274)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2277)(label(e1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         2278)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Tile((id 2279)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 2281)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2282)(label(v))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         2286)(content(Whitespace\" \"))))(Secondary((id \
         2284)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
         2291)(content(Whitespace\" \")))))))))(Secondary((id \
         2293)(content(Whitespace\" \"))))(Secondary((id \
         1804)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1809)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 1810)(content(Whitespace\" \
         \"))))(Tile((id 2301)(label(s1))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1817)(content(Whitespace\" \"))))(Tile((id 1818)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1819)(content(Whitespace\" \"))))(Tile((id \
         1820)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
         Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
         1))(children(((Grout((id 2071)(shape Convex)))(Tile((id \
         1825)(label(,))(mold((out Typ)(in_())(nibs(((shape(Concave 14))(sort \
         Typ))((shape(Concave 14))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1826)(content(Whitespace\" \"))))(Grout((id 1830)(shape \
         Convex)))(Tile((id 1829)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
         14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2072)(content(Whitespace\" \"))))(Grout((id 1833)(shape \
         Convex))))))))(Secondary((id 1835)(content(Whitespace\" \
         \"))))(Tile((id 1837)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave \
         6))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1838)(content(Whitespace\" \"))))(Tile((id \
         1842)(label(Exp))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1843)(content(Whitespace\" \")))))((Secondary((id \
         1845)(content(Whitespace\" \"))))(Tile((id 1850)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 1851)(content(Whitespace\" \
         \"))))(Tile((id 1852)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id 1853)(label(e))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 1855)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
         14))(sort Pat))))))(shards(0))(children())))(Tile((id \
         1856)(label(x))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
         1858)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave 14))(sort \
         Pat))((shape(Concave 14))(sort \
         Pat))))))(shards(0))(children())))(Tile((id 1859)(label(v))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1861)(content(Whitespace\" \")))))))))(Secondary((id \
         1864)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1870)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 1871)(content(Whitespace\" \
         \"))))(Tile((id 1872)(label(e))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1874)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1875)(label(| =>))(mold((out Rul)(in_(Pat))(nibs(((shape(Concave \
         19))(sort Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 1876)(content(Whitespace\" \
         \"))))(Tile((id 1880)(label(Var))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1881)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         1882)(label(y))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1884)(content(Whitespace\" \")))))))))(Secondary((id \
         1886)(content(Whitespace\" \"))))(Tile((id \
         1887)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 1891)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1892)(content(Whitespace\" \"))))(Tile((id 1893)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1895)(content(Whitespace\" \"))))(Tile((id \
         1898)(label($==))(mold((out Exp)(in_())(nibs(((shape(Concave 8))(sort \
         Exp))((shape(Concave 8))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1899)(content(Whitespace\" \"))))(Tile((id 1900)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1904)(content(Whitespace\" \")))))((Secondary((id \
         1908)(content(Whitespace\" \"))))(Tile((id 1909)(label(v))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1913)(content(Whitespace\" \")))))))))(Secondary((id \
         1917)(content(Whitespace\" \"))))(Tile((id 1918)(label(e))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1920)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1921)(label(| =>))(mold((out Rul)(in_(Pat))(nibs(((shape(Concave \
         19))(sort Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 1922)(content(Whitespace\" \
         \"))))(Tile((id 2078)(label(Lam))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1930)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         1931)(label(y))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
         1933)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave 14))(sort \
         Pat))((shape(Concave 14))(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1934)(content(Whitespace\" \"))))(Tile((id 1937)(label(e1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1938)(content(Whitespace\" \")))))))))(Secondary((id \
         1940)(content(Whitespace\" \"))))(Tile((id \
         1941)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 1945)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1946)(content(Whitespace\" \"))))(Tile((id 1947)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1949)(content(Whitespace\" \"))))(Tile((id \
         1952)(label($==))(mold((out Exp)(in_())(nibs(((shape(Concave 8))(sort \
         Exp))((shape(Concave 8))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1953)(content(Whitespace\" \"))))(Tile((id 1954)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1958)(content(Whitespace\" \")))))((Secondary((id \
         1962)(content(Whitespace\" \"))))(Tile((id 1963)(label(e))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1967)(content(Whitespace\" \")))))))))(Secondary((id \
         1971)(content(Whitespace\" \"))))(Tile((id \
         2075)(label(Lam))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         1979)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1980)(label(y))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         1982)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1983)(content(Whitespace\" \"))))(Tile((id 2303)(label(s1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1990)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1993)(label(e1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         1994)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1995)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1997)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1998)(label(v))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         2000)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
         2064)(content(Whitespace\" \")))))))))(Secondary((id \
         2066)(content(Whitespace\" \"))))(Secondary((id \
         1540)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1545)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 1546)(content(Whitespace\" \
         \"))))(Tile((id 2305)(label(s2))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1553)(content(Whitespace\" \"))))(Tile((id 1554)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1555)(content(Whitespace\" \"))))(Tile((id \
         1556)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
         Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
         1))(children(((Tile((id 1560)(label(Exp))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id 1561)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
         14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1562)(content(Whitespace\" \"))))(Grout((id 1565)(shape \
         Convex)))(Tile((id 1564)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
         14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1566)(content(Whitespace\" \"))))(Grout((id 1803)(shape \
         Convex))))))))(Secondary((id 1571)(content(Whitespace\" \
         \"))))(Tile((id 1573)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave \
         6))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         1574)(content(Whitespace\" \"))))(Tile((id \
         1578)(label(Exp))(mold((out Typ)(in_())(nibs(((shape Convex)(sort \
         Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         1579)(content(Whitespace\" \")))))((Secondary((id \
         1581)(content(Whitespace\" \"))))(Tile((id 1586)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 1587)(content(Whitespace\" \
         \"))))(Tile((id 1588)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id 1589)(label(e))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 1591)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
         14))(sort Pat))))))(shards(0))(children())))(Tile((id \
         1592)(label(x))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
         1594)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave 14))(sort \
         Pat))((shape(Concave 14))(sort \
         Pat))))))(shards(0))(children())))(Tile((id 1595)(label(v))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1597)(content(Whitespace\" \")))))))))(Secondary((id \
         1600)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1606)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 1607)(content(Whitespace\" \
         \"))))(Tile((id 1608)(label(e))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1610)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1611)(label(| =>))(mold((out Rul)(in_(Pat))(nibs(((shape(Concave \
         19))(sort Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 1612)(content(Whitespace\" \
         \"))))(Tile((id 1616)(label(Var))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1617)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         1618)(label(y))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1620)(content(Whitespace\" \")))))))))(Secondary((id \
         1622)(content(Whitespace\" \"))))(Tile((id \
         1623)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 1627)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1628)(content(Whitespace\" \"))))(Tile((id 1629)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1631)(content(Whitespace\" \"))))(Tile((id \
         1634)(label($==))(mold((out Exp)(in_())(nibs(((shape(Concave 8))(sort \
         Exp))((shape(Concave 8))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1635)(content(Whitespace\" \"))))(Tile((id 1636)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1640)(content(Whitespace\" \")))))((Secondary((id \
         1644)(content(Whitespace\" \"))))(Tile((id 1645)(label(v))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1649)(content(Whitespace\" \")))))))))(Secondary((id \
         1653)(content(Whitespace\" \"))))(Tile((id 1654)(label(e))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1656)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1657)(label(| =>))(mold((out Rul)(in_(Pat))(nibs(((shape(Concave \
         19))(sort Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 1658)(content(Whitespace\" \
         \"))))(Tile((id 2081)(label(Lam))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1666)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
         Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
         1667)(label(y))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
         1669)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave 14))(sort \
         Pat))((shape(Concave 14))(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         1670)(content(Whitespace\" \"))))(Tile((id 1673)(label(e1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1674)(content(Whitespace\" \")))))))))(Secondary((id \
         1676)(content(Whitespace\" \"))))(Tile((id \
         1677)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 1681)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1682)(content(Whitespace\" \"))))(Tile((id 1683)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1685)(content(Whitespace\" \"))))(Tile((id \
         1688)(label($==))(mold((out Exp)(in_())(nibs(((shape(Concave 8))(sort \
         Exp))((shape(Concave 8))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1689)(content(Whitespace\" \"))))(Tile((id 1690)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1694)(content(Whitespace\" \")))))((Secondary((id \
         1698)(content(Whitespace\" \"))))(Tile((id 1699)(label(e))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1703)(content(Whitespace\" \")))))))))(Secondary((id \
         1707)(content(Whitespace\" \"))))(Tile((id \
         2084)(label(Lam))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         1715)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1716)(label(y))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         1718)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1719)(content(Whitespace\" \"))))(Tile((id 2307)(label(s2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1726)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1729)(label(e1))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         1730)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1731)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1733)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1734)(label(v))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         2068)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
         1800)(content(Whitespace\" \")))))))))(Secondary((id \
         1802)(content(Whitespace\" \"))))(Secondary((id \
         410)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         414)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 415)(content(Whitespace\" \
         \"))))(Tile((id 2309)(label(s3))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         422)(content(Whitespace\" \"))))(Tile((id 423)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 11))(sort Pat))((shape(Concave \
         11))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         425)(content(Whitespace\" \"))))(Tile((id \
         426)(label(\"(\"\")\"))(mold((out Typ)(in_(Typ))(nibs(((shape \
         Convex)(sort Typ))((shape Convex)(sort Typ))))))(shards(0 \
         1))(children(((Tile((id 1209)(label(Exp))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id 430)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
         14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         432)(content(Whitespace\" \"))))(Grout((id 1215)(shape \
         Convex)))(Tile((id 434)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 14))(sort Typ))((shape(Concave \
         14))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         994)(content(Whitespace\" \"))))(Tile((id 1206)(label(Exp))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         440)(content(Whitespace\" \"))))(Tile((id 443)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 6))(sort Typ))((shape(Concave \
         6))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         444)(content(Whitespace\" \"))))(Tile((id 1292)(label(Exp))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         448)(content(Whitespace\" \")))))((Secondary((id \
         451)(content(Whitespace\" \"))))(Tile((id 455)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 456)(content(Whitespace\" \
         \"))))(Tile((id 458)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id 459)(label(e))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 460)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
         14))(sort Pat))))))(shards(0))(children())))(Tile((id \
         462)(label(x))(mold((out Pat)(in_())(nibs(((shape Convex)(sort \
         Pat))((shape Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
         463)(label(,))(mold((out Pat)(in_())(nibs(((shape(Concave 14))(sort \
         Pat))((shape(Concave 14))(sort \
         Pat))))))(shards(0))(children())))(Tile((id 465)(label(v))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         466)(content(Whitespace\" \")))))))))(Secondary((id \
         470)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         475)(label(case end))(mold((out Exp)(in_(Rul))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 476)(content(Whitespace\" \
         \"))))(Tile((id 478)(label(e))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         479)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id 480)(label(| \
         =>))(mold((out Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
         Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 482)(content(Whitespace\" \
         \"))))(Tile((id 485)(label(Var))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         486)(label(\"(\"\")\"))(mold((out Pat)(in_(Pat))(nibs(((shape(Concave \
         1))(sort Pat))((shape Convex)(sort Pat))))))(shards(0 \
         1))(children(((Tile((id 488)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         489)(content(Whitespace\" \")))))))))(Secondary((id \
         492)(content(Whitespace\" \"))))(Tile((id \
         493)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 496)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         497)(content(Whitespace\" \"))))(Tile((id 499)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         500)(content(Whitespace\" \"))))(Tile((id 504)(label($==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 8))(sort Exp))((shape(Concave \
         8))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         505)(content(Whitespace\" \"))))(Tile((id 506)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         509)(content(Whitespace\" \")))))((Secondary((id \
         513)(content(Whitespace\" \"))))(Tile((id 515)(label(v))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         518)(content(Whitespace\" \")))))))))(Secondary((id \
         522)(content(Whitespace\" \"))))(Tile((id 1418)(label(e))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         525)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id 526)(label(| \
         =>))(mold((out Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
         Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id 528)(content(Whitespace\" \
         \"))))(Tile((id 2087)(label(Lam))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         535)(label(\"(\"\")\"))(mold((out Pat)(in_(Pat))(nibs(((shape(Concave \
         1))(sort Pat))((shape Convex)(sort Pat))))))(shards(0 \
         1))(children(((Tile((id 537)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id 538)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 14))(sort Pat))((shape(Concave \
         14))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         540)(content(Whitespace\" \"))))(Tile((id 542)(label(e1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         543)(content(Whitespace\" \")))))))))(Secondary((id \
         546)(content(Whitespace\" \"))))(Tile((id \
         547)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 550)(label(if then else))(mold((out \
         Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         12))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         551)(content(Whitespace\" \"))))(Tile((id 553)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         554)(content(Whitespace\" \"))))(Tile((id 558)(label($==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 8))(sort Exp))((shape(Concave \
         8))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         559)(content(Whitespace\" \"))))(Tile((id 560)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         563)(content(Whitespace\" \")))))((Secondary((id \
         567)(content(Whitespace\" \"))))(Tile((id 569)(label(e))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         572)(content(Whitespace\" \")))))))))(Secondary((id \
         576)(content(Whitespace\" \"))))(Tile((id 2090)(label(Lam))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         584)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape(Concave \
         1))(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 586)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 587)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         589)(content(Whitespace\" \"))))(Tile((id 2311)(label(s3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         595)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape(Concave \
         1))(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 598)(label(e1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 599)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Tile((id \
         601)(label(x))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         602)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Tile((id 604)(label(v))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
         605)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
         669)(content(Whitespace\" \")))))))))(Secondary((id \
         1308)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
         902)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         906)(label(let = in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape \
         Convex)(sort Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id 907)(content(Whitespace\" \
         \"))))(Tile((id 1463)(label(in))(mold((out Pat)(in_())(nibs(((shape \
         Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         914)(content(Whitespace\" \")))))((Secondary((id \
         917)(content(Whitespace\" \"))))(Tile((id 2478)(label(Lam))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1471)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1474)(label(\"\\\"b\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1475)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1477)(content(Whitespace\" \"))))(Tile((id \
         1480)(label(Var))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         1481)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1484)(label(\"\\\"a\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         1485)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1488)(label(\"\\\"a\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id 1489)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 14))(sort Exp))((shape(Concave \
         14))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1493)(label(Var))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         1494)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1497)(label(\"\\\"x\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         954)(content(Whitespace\" \")))))))))(Secondary((id \
         956)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
         1539)(label(\"(\"\")\"))(mold((out Exp)(in_(Exp))(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort Exp))))))(shards(0 \
         1))(children(((Tile((id 2313)(label(s0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1535)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1538)(label(in))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2348)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2349)(content(Whitespace\" \"))))(Tile((id 2317)(label(s1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2318)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2321)(label(in))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2322)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2339)(content(Whitespace\" \"))))(Tile((id 2325)(label(s2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2326)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2329)(label(in))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         2330)(label(,))(mold((out Exp)(in_())(nibs(((shape(Concave 14))(sort \
         Exp))((shape(Concave 14))(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2340)(content(Whitespace\" \"))))(Tile((id 2333)(label(s3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2334)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2337)(label(in))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         674)(content(Whitespace\"\\226\\143\\142\")))))))(ancestors())))(caret \
         Outer))";
      backup_text =
        "#recursive sum type dynamics tests#\n\
         #all calls should evaluate fully with no exns or cast fails#\n\
         type Exp = Var(String) + Lam(String, Exp) in\n\n\
         let s0 : ( ,  ,  ) ->   = fun (e,x,v) ->\n\
         case e\n\
         | Var(y) => (if y $== x then v else e)\n\
         | Lam(y, e1) => (if y $== x then e else Lam(y, s0(e1,x,v))) \n\
         end in \n\
         let s1 : ( ,  ,  ) -> Exp = fun (e,x,v) ->\n\
         case e\n\
         | Var(y) => (if y $== x then v else e)\n\
         | Lam(y, e1) => (if y $== x then e else Lam(y, s1(e1,x,v)))\n\
         end in \n\
         let s2 : (Exp,  ,  ) -> Exp = fun (e,x,v) ->\n\
         case e\n\
         | Var(y) => (if y $== x then v else e)\n\
         | Lam(y, e1) => (if y $== x then e else Lam(y, s2(e1,x,v)))\n\
         end in \n\
         let s3 : (Exp,  , Exp) -> Exp = fun (e,x,v) ->\n\
         case e\n\
         | Var(y) => (if y $== x then v else e)\n\
         | Lam(y, e1) => (if y $== x then e else Lam(y, s3(e1,x,v)))\n\
         end in\n\n\
         let in = Lam(\"b\", Var(\"a\")),\"a\",Var(\"x\") in\n\
         (s0(in), s1(in), s2(in), s3(in))\n";
    } )
