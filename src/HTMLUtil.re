open Tyxml_js;

open Html5;

let span_c1 = (cls, children) => span(~a=[a_class([cls])], children);

let span_cs = (classes, children) => span(~a=[a_class(classes)], children);

let div_c1 = (cls, children) => div(~a=[a_class([cls])], children);

let div_cs = (classes, children) => div(~a=[a_class(classes)], children);

type span_attribs = list(Html5.attrib(Html_types.span_attrib));

type div_attribs = list(Html5.attrib(Html_types.div_attrib));
