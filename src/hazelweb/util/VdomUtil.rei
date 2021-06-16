module Node = Virtual_dom.Vdom.Node;

/* more meaningful type aliases */
type tag = string;
type key = string;
type value = string;
type attr = (key, value);
type children = list(Node.t);
type mapper = (tag, list(attr), children) => option(Node.t);

/** `parsemap s mapper` parses s to a Node (using Markup.ml library)
 *  The caller can provide a mapper to customize how the Node is generated.
 *  If the mapper returns None, the corresponding Node is generated.
 *  Only supports text nodes and elements. Ignores comments and other things.
 */
let parsemap: (string, mapper) => option(Node.t);

/** `parse s == parsemap s (_ => None)`
 *  (parses the given string, with the trivial mapper)
 */
let parse: string => option(Node.t);
