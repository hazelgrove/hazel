open Virtual_dom;

let view:
  (~width: int=?, ~pos: int=?, ~strategy_guide: bool=?, HTyp.t) => Vdom.Node.t;
