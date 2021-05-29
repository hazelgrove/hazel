open Virtual_dom;

let get_type: CursorInfo.t => option(HTyp.t);

let type_driven: list(Vdom.Node.t) => Vdom.Node.t;

let fill_space: Vdom.Node.t;

let code_node: string => Vdom.Node.t;

let example_lit_node: string => Vdom.Node.t;

let shortcut_node: string => Vdom.Node.t;

let mini_option: list(Vdom.Node.t) => Vdom.Node.t;

let option: list(Vdom.Node.t) => Vdom.Node.t;

let int_lit: Vdom.Node.t;

let float_lit: Vdom.Node.t;

let bool_lit: Vdom.Node.t;
