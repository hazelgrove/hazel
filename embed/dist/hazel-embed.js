import Q, { useRef as jr, useEffect as ee, useCallback as Wr, useState as Ur } from "react";
import Yr from "react-dom";
function Gr(t) {
  return t && t.__esModule && Object.prototype.hasOwnProperty.call(t, "default") ? t.default : t;
}
function Vr(t) {
  if (Object.prototype.hasOwnProperty.call(t, "__esModule")) return t;
  var e = t.default;
  if (typeof e == "function") {
    var n = function r() {
      return this instanceof r ? Reflect.construct(e, arguments, this.constructor) : e.apply(this, arguments);
    };
    n.prototype = e.prototype;
  } else n = {};
  return Object.defineProperty(n, "__esModule", { value: !0 }), Object.keys(t).forEach(function(r) {
    var i = Object.getOwnPropertyDescriptor(t, r);
    Object.defineProperty(n, r, i.get ? i : {
      enumerable: !0,
      get: function() {
        return t[r];
      }
    });
  }), n;
}
var Ft = { exports: {} }, Et = {};
/**
 * @license React
 * react-jsx-runtime.production.js
 *
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
var ln;
function Br() {
  if (ln) return Et;
  ln = 1;
  var t = Symbol.for("react.transitional.element"), e = Symbol.for("react.fragment");
  function n(r, i, o) {
    var a = null;
    if (o !== void 0 && (a = "" + o), i.key !== void 0 && (a = "" + i.key), "key" in i) {
      o = {};
      for (var s in i)
        s !== "key" && (o[s] = i[s]);
    } else o = i;
    return i = o.ref, {
      $$typeof: t,
      type: r,
      key: a,
      ref: i !== void 0 ? i : null,
      props: o
    };
  }
  return Et.Fragment = e, Et.jsx = n, Et.jsxs = n, Et;
}
var St = {};
/**
 * @license React
 * react-jsx-runtime.development.js
 *
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
var cn;
function Hr() {
  return cn || (cn = 1, process.env.NODE_ENV !== "production" && function() {
    function t(f) {
      if (f == null) return null;
      if (typeof f == "function")
        return f.$$typeof === b ? null : f.displayName || f.name || null;
      if (typeof f == "string") return f;
      switch (f) {
        case S:
          return "Fragment";
        case T:
          return "Profiler";
        case I:
          return "StrictMode";
        case d:
          return "Suspense";
        case D:
          return "SuspenseList";
        case E:
          return "Activity";
      }
      if (typeof f == "object")
        switch (typeof f.tag == "number" && console.error(
          "Received an unexpected object in getComponentNameFromType(). This is likely a bug in React. Please file an issue."
        ), f.$$typeof) {
          case x:
            return "Portal";
          case R:
            return (f.displayName || "Context") + ".Provider";
          case L:
            return (f._context.displayName || "Context") + ".Consumer";
          case _:
            var y = f.render;
            return f = f.displayName, f || (f = y.displayName || y.name || "", f = f !== "" ? "ForwardRef(" + f + ")" : "ForwardRef"), f;
          case $:
            return y = f.displayName || null, y !== null ? y : t(f.type) || "Memo";
          case w:
            y = f._payload, f = f._init;
            try {
              return t(f(y));
            } catch {
            }
        }
      return null;
    }
    function e(f) {
      return "" + f;
    }
    function n(f) {
      try {
        e(f);
        var y = !1;
      } catch {
        y = !0;
      }
      if (y) {
        y = console;
        var O = y.error, z = typeof Symbol == "function" && Symbol.toStringTag && f[Symbol.toStringTag] || f.constructor.name || "Object";
        return O.call(
          y,
          "The provided key is an unsupported type %s. This value must be coerced to a string before using it here.",
          z
        ), e(f);
      }
    }
    function r(f) {
      if (f === S) return "<>";
      if (typeof f == "object" && f !== null && f.$$typeof === w)
        return "<...>";
      try {
        var y = t(f);
        return y ? "<" + y + ">" : "<...>";
      } catch {
        return "<...>";
      }
    }
    function i() {
      var f = A.A;
      return f === null ? null : f.getOwner();
    }
    function o() {
      return Error("react-stack-top-frame");
    }
    function a(f) {
      if (Y.call(f, "key")) {
        var y = Object.getOwnPropertyDescriptor(f, "key").get;
        if (y && y.isReactWarning) return !1;
      }
      return f.key !== void 0;
    }
    function s(f, y) {
      function O() {
        N || (N = !0, console.error(
          "%s: `key` is not a prop. Trying to access it will result in `undefined` being returned. If you need to access the same value within the child component, you should pass it as a different prop. (https://react.dev/link/special-props)",
          y
        ));
      }
      O.isReactWarning = !0, Object.defineProperty(f, "key", {
        get: O,
        configurable: !0
      });
    }
    function l() {
      var f = t(this.type);
      return v[f] || (v[f] = !0, console.error(
        "Accessing element.ref was removed in React 19. ref is now a regular prop. It will be removed from the JSX Element type in a future release."
      )), f = this.props.ref, f !== void 0 ? f : null;
    }
    function u(f, y, O, z, k, F, U, W) {
      return O = F.ref, f = {
        $$typeof: g,
        type: f,
        key: y,
        props: F,
        _owner: k
      }, (O !== void 0 ? O : null) !== null ? Object.defineProperty(f, "ref", {
        enumerable: !1,
        get: l
      }) : Object.defineProperty(f, "ref", { enumerable: !1, value: null }), f._store = {}, Object.defineProperty(f._store, "validated", {
        configurable: !1,
        enumerable: !1,
        writable: !0,
        value: 0
      }), Object.defineProperty(f, "_debugInfo", {
        configurable: !1,
        enumerable: !1,
        writable: !0,
        value: null
      }), Object.defineProperty(f, "_debugStack", {
        configurable: !1,
        enumerable: !1,
        writable: !0,
        value: U
      }), Object.defineProperty(f, "_debugTask", {
        configurable: !1,
        enumerable: !1,
        writable: !0,
        value: W
      }), Object.freeze && (Object.freeze(f.props), Object.freeze(f)), f;
    }
    function c(f, y, O, z, k, F, U, W) {
      var q = y.children;
      if (q !== void 0)
        if (z)
          if (H(q)) {
            for (z = 0; z < q.length; z++)
              m(q[z]);
            Object.freeze && Object.freeze(q);
          } else
            console.error(
              "React.jsx: Static children should always be an array. You are likely explicitly calling React.jsxs or React.jsxDEV. Use the Babel transform instead."
            );
        else m(q);
      if (Y.call(y, "key")) {
        q = t(f);
        var G = Object.keys(y).filter(function(Z) {
          return Z !== "key";
        });
        z = 0 < G.length ? "{key: someKey, " + G.join(": ..., ") + ": ...}" : "{key: someKey}", j[q + z] || (G = 0 < G.length ? "{" + G.join(": ..., ") + ": ...}" : "{}", console.error(
          `A props object containing a "key" prop is being spread into JSX:
  let props = %s;
  <%s {...props} />
React keys must be passed directly to JSX without using spread:
  let props = %s;
  <%s key={someKey} {...props} />`,
          z,
          q,
          G,
          q
        ), j[q + z] = !0);
      }
      if (q = null, O !== void 0 && (n(O), q = "" + O), a(y) && (n(y.key), q = "" + y.key), "key" in y) {
        O = {};
        for (var B in y)
          B !== "key" && (O[B] = y[B]);
      } else O = y;
      return q && s(
        O,
        typeof f == "function" ? f.displayName || f.name || "Unknown" : f
      ), u(
        f,
        q,
        F,
        k,
        i(),
        O,
        U,
        W
      );
    }
    function m(f) {
      typeof f == "object" && f !== null && f.$$typeof === g && f._store && (f._store.validated = 1);
    }
    var h = Q, g = Symbol.for("react.transitional.element"), x = Symbol.for("react.portal"), S = Symbol.for("react.fragment"), I = Symbol.for("react.strict_mode"), T = Symbol.for("react.profiler"), L = Symbol.for("react.consumer"), R = Symbol.for("react.context"), _ = Symbol.for("react.forward_ref"), d = Symbol.for("react.suspense"), D = Symbol.for("react.suspense_list"), $ = Symbol.for("react.memo"), w = Symbol.for("react.lazy"), E = Symbol.for("react.activity"), b = Symbol.for("react.client.reference"), A = h.__CLIENT_INTERNALS_DO_NOT_USE_OR_WARN_USERS_THEY_CANNOT_UPGRADE, Y = Object.prototype.hasOwnProperty, H = Array.isArray, p = console.createTask ? console.createTask : function() {
      return null;
    };
    h = {
      "react-stack-bottom-frame": function(f) {
        return f();
      }
    };
    var N, v = {}, C = h["react-stack-bottom-frame"].bind(
      h,
      o
    )(), P = p(r(o)), j = {};
    St.Fragment = S, St.jsx = function(f, y, O, z, k) {
      var F = 1e4 > A.recentlyCreatedOwnerStacks++;
      return c(
        f,
        y,
        O,
        !1,
        z,
        k,
        F ? Error("react-stack-top-frame") : C,
        F ? p(r(f)) : P
      );
    }, St.jsxs = function(f, y, O, z, k) {
      var F = 1e4 > A.recentlyCreatedOwnerStacks++;
      return c(
        f,
        y,
        O,
        !0,
        z,
        k,
        F ? Error("react-stack-top-frame") : C,
        F ? p(r(f)) : P
      );
    };
  }()), St;
}
var fn;
function Xr() {
  return fn || (fn = 1, process.env.NODE_ENV === "production" ? Ft.exports = Br() : Ft.exports = Hr()), Ft.exports;
}
var ft = Xr();
const ve = (t, e) => {
  t.contentWindow && t.contentWindow.postMessage(e, "*");
}, ju = ({
  instanceId: t,
  onMessage: e,
  registerSendMessage: n,
  url: r = "https://hazel.org/build/patchwork/"
}) => {
  const i = jr(null);
  ee(() => {
    i.current && (i.current.onload = () => {
      const a = {
        t: "init",
        message: `Hello, you are instance ${t}!`
      };
      ve(i.current, a);
    });
  }, [t]), ee(() => {
    const a = (s) => {
      var u, c;
      if ((c = (u = s.data) == null ? void 0 : u.source) != null && c.includes("react")) return;
      const l = s.data;
      e(l, t);
    };
    return window.addEventListener("message", a), () => {
      window.removeEventListener("message", a);
    };
  }, [t, e]);
  const o = Wr(
    (a) => {
      if (i.current)
        if (a.t === "state") {
          const s = {
            t: "state",
            state: a.state
          };
          ve(i.current, s);
        } else
          ve(i.current, a);
      else
        console.error("Hazel iframe is not available.");
    },
    [t]
  );
  return ee(() => {
    n(o);
  }, [n, o]), /* @__PURE__ */ ft.jsx("div", { className: "hazel-embed-container", children: /* @__PURE__ */ ft.jsx(
    "iframe",
    {
      src: r,
      style: { width: "100%", height: "100%", border: "none" },
      ref: i
    }
  ) });
};
function Kr(t) {
  var e = 0, n = t.children, r = n && n.length;
  if (!r) e = 1;
  else for (; --r >= 0; ) e += n[r].value;
  t.value = e;
}
function Qr() {
  return this.eachAfter(Kr);
}
function Jr(t) {
  var e = this, n, r = [e], i, o, a;
  do
    for (n = r.reverse(), r = []; e = n.pop(); )
      if (t(e), i = e.children, i) for (o = 0, a = i.length; o < a; ++o)
        r.push(i[o]);
  while (r.length);
  return this;
}
function Zr(t) {
  for (var e = this, n = [e], r, i; e = n.pop(); )
    if (t(e), r = e.children, r) for (i = r.length - 1; i >= 0; --i)
      n.push(r[i]);
  return this;
}
function ti(t) {
  for (var e = this, n = [e], r = [], i, o, a; e = n.pop(); )
    if (r.push(e), i = e.children, i) for (o = 0, a = i.length; o < a; ++o)
      n.push(i[o]);
  for (; e = r.pop(); )
    t(e);
  return this;
}
function ei(t) {
  return this.eachAfter(function(e) {
    for (var n = +t(e.data) || 0, r = e.children, i = r && r.length; --i >= 0; ) n += r[i].value;
    e.value = n;
  });
}
function ni(t) {
  return this.eachBefore(function(e) {
    e.children && e.children.sort(t);
  });
}
function ri(t) {
  for (var e = this, n = ii(e, t), r = [e]; e !== n; )
    e = e.parent, r.push(e);
  for (var i = r.length; t !== n; )
    r.splice(i, 0, t), t = t.parent;
  return r;
}
function ii(t, e) {
  if (t === e) return t;
  var n = t.ancestors(), r = e.ancestors(), i = null;
  for (t = n.pop(), e = r.pop(); t === e; )
    i = t, t = n.pop(), e = r.pop();
  return i;
}
function oi() {
  for (var t = this, e = [t]; t = t.parent; )
    e.push(t);
  return e;
}
function ai() {
  var t = [];
  return this.each(function(e) {
    t.push(e);
  }), t;
}
function si() {
  var t = [];
  return this.eachBefore(function(e) {
    e.children || t.push(e);
  }), t;
}
function ui() {
  var t = this, e = [];
  return t.each(function(n) {
    n !== t && e.push({ source: n.parent, target: n });
  }), e;
}
function Qe(t, e) {
  var n = new Pt(t), r = +t.value && (n.value = t.value), i, o = [n], a, s, l, u;
  for (e == null && (e = ci); i = o.pop(); )
    if (r && (i.value = +i.data.value), (s = e(i.data)) && (u = s.length))
      for (i.children = new Array(u), l = u - 1; l >= 0; --l)
        o.push(a = i.children[l] = new Pt(s[l])), a.parent = i, a.depth = i.depth + 1;
  return n.eachBefore(hi);
}
function li() {
  return Qe(this).eachBefore(fi);
}
function ci(t) {
  return t.children;
}
function fi(t) {
  t.data = t.data.data;
}
function hi(t) {
  var e = 0;
  do
    t.height = e;
  while ((t = t.parent) && t.height < ++e);
}
function Pt(t) {
  this.data = t, this.depth = this.height = 0, this.parent = null;
}
Pt.prototype = Qe.prototype = {
  constructor: Pt,
  count: Qr,
  each: Jr,
  eachAfter: ti,
  eachBefore: Zr,
  sum: ei,
  sort: ni,
  path: ri,
  ancestors: oi,
  descendants: ai,
  leaves: si,
  links: ui,
  copy: li
};
function di(t, e) {
  return t.parent === e.parent ? 1 : 2;
}
function _e(t) {
  var e = t.children;
  return e ? e[0] : t.t;
}
function we(t) {
  var e = t.children;
  return e ? e[e.length - 1] : t.t;
}
function pi(t, e, n) {
  var r = n / (e.i - t.i);
  e.c -= r, e.s += n, t.c += r, e.z += n, e.m += n;
}
function mi(t) {
  for (var e = 0, n = 0, r = t.children, i = r.length, o; --i >= 0; )
    o = r[i], o.z += e, o.m += e, e += o.s + (n += o.c);
}
function gi(t, e, n) {
  return t.a.parent === e.parent ? t.a : n;
}
function ne(t, e) {
  this._ = t, this.parent = null, this.children = null, this.A = null, this.a = this, this.z = 0, this.m = 0, this.c = 0, this.s = 0, this.t = null, this.i = e;
}
ne.prototype = Object.create(Pt.prototype);
function yi(t) {
  for (var e = new ne(t, 0), n, r = [e], i, o, a, s; n = r.pop(); )
    if (o = n._.children)
      for (n.children = new Array(s = o.length), a = s - 1; a >= 0; --a)
        r.push(i = n.children[a] = new ne(o[a], a)), i.parent = n;
  return (e.parent = new ne(null, 0)).children = [e], e;
}
function vi() {
  var t = di, e = 1, n = 1, r = null;
  function i(u) {
    var c = yi(u);
    if (c.eachAfter(o), c.parent.m = -c.z, c.eachBefore(a), r) u.eachBefore(l);
    else {
      var m = u, h = u, g = u;
      u.eachBefore(function(L) {
        L.x < m.x && (m = L), L.x > h.x && (h = L), L.depth > g.depth && (g = L);
      });
      var x = m === h ? 1 : t(m, h) / 2, S = x - m.x, I = e / (h.x + x + S), T = n / (g.depth || 1);
      u.eachBefore(function(L) {
        L.x = (L.x + S) * I, L.y = L.depth * T;
      });
    }
    return u;
  }
  function o(u) {
    var c = u.children, m = u.parent.children, h = u.i ? m[u.i - 1] : null;
    if (c) {
      mi(u);
      var g = (c[0].z + c[c.length - 1].z) / 2;
      h ? (u.z = h.z + t(u._, h._), u.m = u.z - g) : u.z = g;
    } else h && (u.z = h.z + t(u._, h._));
    u.parent.A = s(u, h, u.parent.A || m[0]);
  }
  function a(u) {
    u._.x = u.z + u.parent.m, u.m += u.parent.m;
  }
  function s(u, c, m) {
    if (c) {
      for (var h = u, g = u, x = c, S = h.parent.children[0], I = h.m, T = g.m, L = x.m, R = S.m, _; x = we(x), h = _e(h), x && h; )
        S = _e(S), g = we(g), g.a = u, _ = x.z + L - h.z - I + t(x._, h._), _ > 0 && (pi(gi(x, u, m), u, _), I += _, T += _), L += x.m, I += h.m, R += S.m, T += g.m;
      x && !we(g) && (g.t = x, g.m += L - T), h && !_e(S) && (S.t = h, S.m += I - R, m = u);
    }
    return m;
  }
  function l(u) {
    u.x *= e, u.y = u.depth * n;
  }
  return i.separation = function(u) {
    return arguments.length ? (t = u, i) : t;
  }, i.size = function(u) {
    return arguments.length ? (r = !1, e = +u[0], n = +u[1], i) : r ? null : [e, n];
  }, i.nodeSize = function(u) {
    return arguments.length ? (r = !0, e = +u[0], n = +u[1], i) : r ? [e, n] : null;
  }, i;
}
var qe = "http://www.w3.org/1999/xhtml";
const hn = {
  svg: "http://www.w3.org/2000/svg",
  xhtml: qe,
  xlink: "http://www.w3.org/1999/xlink",
  xml: "http://www.w3.org/XML/1998/namespace",
  xmlns: "http://www.w3.org/2000/xmlns/"
};
function pe(t) {
  var e = t += "", n = e.indexOf(":");
  return n >= 0 && (e = t.slice(0, n)) !== "xmlns" && (t = t.slice(n + 1)), hn.hasOwnProperty(e) ? { space: hn[e], local: t } : t;
}
function _i(t) {
  return function() {
    var e = this.ownerDocument, n = this.namespaceURI;
    return n === qe && e.documentElement.namespaceURI === qe ? e.createElement(t) : e.createElementNS(n, t);
  };
}
function wi(t) {
  return function() {
    return this.ownerDocument.createElementNS(t.space, t.local);
  };
}
function or(t) {
  var e = pe(t);
  return (e.local ? wi : _i)(e);
}
function bi() {
}
function Je(t) {
  return t == null ? bi : function() {
    return this.querySelector(t);
  };
}
function xi(t) {
  typeof t != "function" && (t = Je(t));
  for (var e = this._groups, n = e.length, r = new Array(n), i = 0; i < n; ++i)
    for (var o = e[i], a = o.length, s = r[i] = new Array(a), l, u, c = 0; c < a; ++c)
      (l = o[c]) && (u = t.call(l, l.__data__, c, o)) && ("__data__" in l && (u.__data__ = l.__data__), s[c] = u);
  return new it(r, this._parents);
}
function Ti(t) {
  return t == null ? [] : Array.isArray(t) ? t : Array.from(t);
}
function Ei() {
  return [];
}
function ar(t) {
  return t == null ? Ei : function() {
    return this.querySelectorAll(t);
  };
}
function Si(t) {
  return function() {
    return Ti(t.apply(this, arguments));
  };
}
function Oi(t) {
  typeof t == "function" ? t = Si(t) : t = ar(t);
  for (var e = this._groups, n = e.length, r = [], i = [], o = 0; o < n; ++o)
    for (var a = e[o], s = a.length, l, u = 0; u < s; ++u)
      (l = a[u]) && (r.push(t.call(l, l.__data__, u, a)), i.push(l));
  return new it(r, i);
}
function sr(t) {
  return function() {
    return this.matches(t);
  };
}
function ur(t) {
  return function(e) {
    return e.matches(t);
  };
}
var Ci = Array.prototype.find;
function Ri(t) {
  return function() {
    return Ci.call(this.children, t);
  };
}
function Ni() {
  return this.firstElementChild;
}
function Ai(t) {
  return this.select(t == null ? Ni : Ri(typeof t == "function" ? t : ur(t)));
}
var Mi = Array.prototype.filter;
function Pi() {
  return Array.from(this.children);
}
function ki(t) {
  return function() {
    return Mi.call(this.children, t);
  };
}
function Di(t) {
  return this.selectAll(t == null ? Pi : ki(typeof t == "function" ? t : ur(t)));
}
function $i(t) {
  typeof t != "function" && (t = sr(t));
  for (var e = this._groups, n = e.length, r = new Array(n), i = 0; i < n; ++i)
    for (var o = e[i], a = o.length, s = r[i] = [], l, u = 0; u < a; ++u)
      (l = o[u]) && t.call(l, l.__data__, u, o) && s.push(l);
  return new it(r, this._parents);
}
function lr(t) {
  return new Array(t.length);
}
function zi() {
  return new it(this._enter || this._groups.map(lr), this._parents);
}
function se(t, e) {
  this.ownerDocument = t.ownerDocument, this.namespaceURI = t.namespaceURI, this._next = null, this._parent = t, this.__data__ = e;
}
se.prototype = {
  constructor: se,
  appendChild: function(t) {
    return this._parent.insertBefore(t, this._next);
  },
  insertBefore: function(t, e) {
    return this._parent.insertBefore(t, e);
  },
  querySelector: function(t) {
    return this._parent.querySelector(t);
  },
  querySelectorAll: function(t) {
    return this._parent.querySelectorAll(t);
  }
};
function Ii(t) {
  return function() {
    return t;
  };
}
function Li(t, e, n, r, i, o) {
  for (var a = 0, s, l = e.length, u = o.length; a < u; ++a)
    (s = e[a]) ? (s.__data__ = o[a], r[a] = s) : n[a] = new se(t, o[a]);
  for (; a < l; ++a)
    (s = e[a]) && (i[a] = s);
}
function qi(t, e, n, r, i, o, a) {
  var s, l, u = /* @__PURE__ */ new Map(), c = e.length, m = o.length, h = new Array(c), g;
  for (s = 0; s < c; ++s)
    (l = e[s]) && (h[s] = g = a.call(l, l.__data__, s, e) + "", u.has(g) ? i[s] = l : u.set(g, l));
  for (s = 0; s < m; ++s)
    g = a.call(t, o[s], s, o) + "", (l = u.get(g)) ? (r[s] = l, l.__data__ = o[s], u.delete(g)) : n[s] = new se(t, o[s]);
  for (s = 0; s < c; ++s)
    (l = e[s]) && u.get(h[s]) === l && (i[s] = l);
}
function Fi(t) {
  return t.__data__;
}
function ji(t, e) {
  if (!arguments.length) return Array.from(this, Fi);
  var n = e ? qi : Li, r = this._parents, i = this._groups;
  typeof t != "function" && (t = Ii(t));
  for (var o = i.length, a = new Array(o), s = new Array(o), l = new Array(o), u = 0; u < o; ++u) {
    var c = r[u], m = i[u], h = m.length, g = Wi(t.call(c, c && c.__data__, u, r)), x = g.length, S = s[u] = new Array(x), I = a[u] = new Array(x), T = l[u] = new Array(h);
    n(c, m, S, I, T, g, e);
    for (var L = 0, R = 0, _, d; L < x; ++L)
      if (_ = S[L]) {
        for (L >= R && (R = L + 1); !(d = I[R]) && ++R < x; ) ;
        _._next = d || null;
      }
  }
  return a = new it(a, r), a._enter = s, a._exit = l, a;
}
function Wi(t) {
  return typeof t == "object" && "length" in t ? t : Array.from(t);
}
function Ui() {
  return new it(this._exit || this._groups.map(lr), this._parents);
}
function Yi(t, e, n) {
  var r = this.enter(), i = this, o = this.exit();
  return typeof t == "function" ? (r = t(r), r && (r = r.selection())) : r = r.append(t + ""), e != null && (i = e(i), i && (i = i.selection())), n == null ? o.remove() : n(o), r && i ? r.merge(i).order() : i;
}
function Gi(t) {
  for (var e = t.selection ? t.selection() : t, n = this._groups, r = e._groups, i = n.length, o = r.length, a = Math.min(i, o), s = new Array(i), l = 0; l < a; ++l)
    for (var u = n[l], c = r[l], m = u.length, h = s[l] = new Array(m), g, x = 0; x < m; ++x)
      (g = u[x] || c[x]) && (h[x] = g);
  for (; l < i; ++l)
    s[l] = n[l];
  return new it(s, this._parents);
}
function Vi() {
  for (var t = this._groups, e = -1, n = t.length; ++e < n; )
    for (var r = t[e], i = r.length - 1, o = r[i], a; --i >= 0; )
      (a = r[i]) && (o && a.compareDocumentPosition(o) ^ 4 && o.parentNode.insertBefore(a, o), o = a);
  return this;
}
function Bi(t) {
  t || (t = Hi);
  function e(m, h) {
    return m && h ? t(m.__data__, h.__data__) : !m - !h;
  }
  for (var n = this._groups, r = n.length, i = new Array(r), o = 0; o < r; ++o) {
    for (var a = n[o], s = a.length, l = i[o] = new Array(s), u, c = 0; c < s; ++c)
      (u = a[c]) && (l[c] = u);
    l.sort(e);
  }
  return new it(i, this._parents).order();
}
function Hi(t, e) {
  return t < e ? -1 : t > e ? 1 : t >= e ? 0 : NaN;
}
function Xi() {
  var t = arguments[0];
  return arguments[0] = this, t.apply(null, arguments), this;
}
function Ki() {
  return Array.from(this);
}
function Qi() {
  for (var t = this._groups, e = 0, n = t.length; e < n; ++e)
    for (var r = t[e], i = 0, o = r.length; i < o; ++i) {
      var a = r[i];
      if (a) return a;
    }
  return null;
}
function Ji() {
  let t = 0;
  for (const e of this) ++t;
  return t;
}
function Zi() {
  return !this.node();
}
function to(t) {
  for (var e = this._groups, n = 0, r = e.length; n < r; ++n)
    for (var i = e[n], o = 0, a = i.length, s; o < a; ++o)
      (s = i[o]) && t.call(s, s.__data__, o, i);
  return this;
}
function eo(t) {
  return function() {
    this.removeAttribute(t);
  };
}
function no(t) {
  return function() {
    this.removeAttributeNS(t.space, t.local);
  };
}
function ro(t, e) {
  return function() {
    this.setAttribute(t, e);
  };
}
function io(t, e) {
  return function() {
    this.setAttributeNS(t.space, t.local, e);
  };
}
function oo(t, e) {
  return function() {
    var n = e.apply(this, arguments);
    n == null ? this.removeAttribute(t) : this.setAttribute(t, n);
  };
}
function ao(t, e) {
  return function() {
    var n = e.apply(this, arguments);
    n == null ? this.removeAttributeNS(t.space, t.local) : this.setAttributeNS(t.space, t.local, n);
  };
}
function so(t, e) {
  var n = pe(t);
  if (arguments.length < 2) {
    var r = this.node();
    return n.local ? r.getAttributeNS(n.space, n.local) : r.getAttribute(n);
  }
  return this.each((e == null ? n.local ? no : eo : typeof e == "function" ? n.local ? ao : oo : n.local ? io : ro)(n, e));
}
function cr(t) {
  return t.ownerDocument && t.ownerDocument.defaultView || t.document && t || t.defaultView;
}
function uo(t) {
  return function() {
    this.style.removeProperty(t);
  };
}
function lo(t, e, n) {
  return function() {
    this.style.setProperty(t, e, n);
  };
}
function co(t, e, n) {
  return function() {
    var r = e.apply(this, arguments);
    r == null ? this.style.removeProperty(t) : this.style.setProperty(t, r, n);
  };
}
function fo(t, e, n) {
  return arguments.length > 1 ? this.each((e == null ? uo : typeof e == "function" ? co : lo)(t, e, n ?? "")) : xt(this.node(), t);
}
function xt(t, e) {
  return t.style.getPropertyValue(e) || cr(t).getComputedStyle(t, null).getPropertyValue(e);
}
function ho(t) {
  return function() {
    delete this[t];
  };
}
function po(t, e) {
  return function() {
    this[t] = e;
  };
}
function mo(t, e) {
  return function() {
    var n = e.apply(this, arguments);
    n == null ? delete this[t] : this[t] = n;
  };
}
function go(t, e) {
  return arguments.length > 1 ? this.each((e == null ? ho : typeof e == "function" ? mo : po)(t, e)) : this.node()[t];
}
function fr(t) {
  return t.trim().split(/^|\s+/);
}
function Ze(t) {
  return t.classList || new hr(t);
}
function hr(t) {
  this._node = t, this._names = fr(t.getAttribute("class") || "");
}
hr.prototype = {
  add: function(t) {
    var e = this._names.indexOf(t);
    e < 0 && (this._names.push(t), this._node.setAttribute("class", this._names.join(" ")));
  },
  remove: function(t) {
    var e = this._names.indexOf(t);
    e >= 0 && (this._names.splice(e, 1), this._node.setAttribute("class", this._names.join(" ")));
  },
  contains: function(t) {
    return this._names.indexOf(t) >= 0;
  }
};
function dr(t, e) {
  for (var n = Ze(t), r = -1, i = e.length; ++r < i; ) n.add(e[r]);
}
function pr(t, e) {
  for (var n = Ze(t), r = -1, i = e.length; ++r < i; ) n.remove(e[r]);
}
function yo(t) {
  return function() {
    dr(this, t);
  };
}
function vo(t) {
  return function() {
    pr(this, t);
  };
}
function _o(t, e) {
  return function() {
    (e.apply(this, arguments) ? dr : pr)(this, t);
  };
}
function wo(t, e) {
  var n = fr(t + "");
  if (arguments.length < 2) {
    for (var r = Ze(this.node()), i = -1, o = n.length; ++i < o; ) if (!r.contains(n[i])) return !1;
    return !0;
  }
  return this.each((typeof e == "function" ? _o : e ? yo : vo)(n, e));
}
function bo() {
  this.textContent = "";
}
function xo(t) {
  return function() {
    this.textContent = t;
  };
}
function To(t) {
  return function() {
    var e = t.apply(this, arguments);
    this.textContent = e ?? "";
  };
}
function Eo(t) {
  return arguments.length ? this.each(t == null ? bo : (typeof t == "function" ? To : xo)(t)) : this.node().textContent;
}
function So() {
  this.innerHTML = "";
}
function Oo(t) {
  return function() {
    this.innerHTML = t;
  };
}
function Co(t) {
  return function() {
    var e = t.apply(this, arguments);
    this.innerHTML = e ?? "";
  };
}
function Ro(t) {
  return arguments.length ? this.each(t == null ? So : (typeof t == "function" ? Co : Oo)(t)) : this.node().innerHTML;
}
function No() {
  this.nextSibling && this.parentNode.appendChild(this);
}
function Ao() {
  return this.each(No);
}
function Mo() {
  this.previousSibling && this.parentNode.insertBefore(this, this.parentNode.firstChild);
}
function Po() {
  return this.each(Mo);
}
function ko(t) {
  var e = typeof t == "function" ? t : or(t);
  return this.select(function() {
    return this.appendChild(e.apply(this, arguments));
  });
}
function Do() {
  return null;
}
function $o(t, e) {
  var n = typeof t == "function" ? t : or(t), r = e == null ? Do : typeof e == "function" ? e : Je(e);
  return this.select(function() {
    return this.insertBefore(n.apply(this, arguments), r.apply(this, arguments) || null);
  });
}
function zo() {
  var t = this.parentNode;
  t && t.removeChild(this);
}
function Io() {
  return this.each(zo);
}
function Lo() {
  var t = this.cloneNode(!1), e = this.parentNode;
  return e ? e.insertBefore(t, this.nextSibling) : t;
}
function qo() {
  var t = this.cloneNode(!0), e = this.parentNode;
  return e ? e.insertBefore(t, this.nextSibling) : t;
}
function Fo(t) {
  return this.select(t ? qo : Lo);
}
function jo(t) {
  return arguments.length ? this.property("__data__", t) : this.node().__data__;
}
function Wo(t) {
  return function(e) {
    t.call(this, e, this.__data__);
  };
}
function Uo(t) {
  return t.trim().split(/^|\s+/).map(function(e) {
    var n = "", r = e.indexOf(".");
    return r >= 0 && (n = e.slice(r + 1), e = e.slice(0, r)), { type: e, name: n };
  });
}
function Yo(t) {
  return function() {
    var e = this.__on;
    if (e) {
      for (var n = 0, r = -1, i = e.length, o; n < i; ++n)
        o = e[n], (!t.type || o.type === t.type) && o.name === t.name ? this.removeEventListener(o.type, o.listener, o.options) : e[++r] = o;
      ++r ? e.length = r : delete this.__on;
    }
  };
}
function Go(t, e, n) {
  return function() {
    var r = this.__on, i, o = Wo(e);
    if (r) {
      for (var a = 0, s = r.length; a < s; ++a)
        if ((i = r[a]).type === t.type && i.name === t.name) {
          this.removeEventListener(i.type, i.listener, i.options), this.addEventListener(i.type, i.listener = o, i.options = n), i.value = e;
          return;
        }
    }
    this.addEventListener(t.type, o, n), i = { type: t.type, name: t.name, value: e, listener: o, options: n }, r ? r.push(i) : this.__on = [i];
  };
}
function Vo(t, e, n) {
  var r = Uo(t + ""), i, o = r.length, a;
  if (arguments.length < 2) {
    var s = this.node().__on;
    if (s) {
      for (var l = 0, u = s.length, c; l < u; ++l)
        for (i = 0, c = s[l]; i < o; ++i)
          if ((a = r[i]).type === c.type && a.name === c.name)
            return c.value;
    }
    return;
  }
  for (s = e ? Go : Yo, i = 0; i < o; ++i) this.each(s(r[i], e, n));
  return this;
}
function mr(t, e, n) {
  var r = cr(t), i = r.CustomEvent;
  typeof i == "function" ? i = new i(e, n) : (i = r.document.createEvent("Event"), n ? (i.initEvent(e, n.bubbles, n.cancelable), i.detail = n.detail) : i.initEvent(e, !1, !1)), t.dispatchEvent(i);
}
function Bo(t, e) {
  return function() {
    return mr(this, t, e);
  };
}
function Ho(t, e) {
  return function() {
    return mr(this, t, e.apply(this, arguments));
  };
}
function Xo(t, e) {
  return this.each((typeof e == "function" ? Ho : Bo)(t, e));
}
function* Ko() {
  for (var t = this._groups, e = 0, n = t.length; e < n; ++e)
    for (var r = t[e], i = 0, o = r.length, a; i < o; ++i)
      (a = r[i]) && (yield a);
}
var gr = [null];
function it(t, e) {
  this._groups = t, this._parents = e;
}
function It() {
  return new it([[document.documentElement]], gr);
}
function Qo() {
  return this;
}
it.prototype = It.prototype = {
  constructor: it,
  select: xi,
  selectAll: Oi,
  selectChild: Ai,
  selectChildren: Di,
  filter: $i,
  data: ji,
  enter: zi,
  exit: Ui,
  join: Yi,
  merge: Gi,
  selection: Qo,
  order: Vi,
  sort: Bi,
  call: Xi,
  nodes: Ki,
  node: Qi,
  size: Ji,
  empty: Zi,
  each: to,
  attr: so,
  style: fo,
  property: go,
  classed: wo,
  text: Eo,
  html: Ro,
  raise: Ao,
  lower: Po,
  append: ko,
  insert: $o,
  remove: Io,
  clone: Fo,
  datum: jo,
  on: Vo,
  dispatch: Xo,
  [Symbol.iterator]: Ko
};
function et(t) {
  return typeof t == "string" ? new it([[document.querySelector(t)]], [document.documentElement]) : new it([[t]], gr);
}
function Jo(t) {
  let e;
  for (; e = t.sourceEvent; ) t = e;
  return t;
}
function mt(t, e) {
  if (t = Jo(t), e === void 0 && (e = t.currentTarget), e) {
    var n = e.ownerSVGElement || e;
    if (n.createSVGPoint) {
      var r = n.createSVGPoint();
      return r.x = t.clientX, r.y = t.clientY, r = r.matrixTransform(e.getScreenCTM().inverse()), [r.x, r.y];
    }
    if (e.getBoundingClientRect) {
      var i = e.getBoundingClientRect();
      return [t.clientX - i.left - e.clientLeft, t.clientY - i.top - e.clientTop];
    }
  }
  return [t.pageX, t.pageY];
}
var Zo = { value: () => {
} };
function tn() {
  for (var t = 0, e = arguments.length, n = {}, r; t < e; ++t) {
    if (!(r = arguments[t] + "") || r in n || /[\s.]/.test(r)) throw new Error("illegal type: " + r);
    n[r] = [];
  }
  return new re(n);
}
function re(t) {
  this._ = t;
}
function ta(t, e) {
  return t.trim().split(/^|\s+/).map(function(n) {
    var r = "", i = n.indexOf(".");
    if (i >= 0 && (r = n.slice(i + 1), n = n.slice(0, i)), n && !e.hasOwnProperty(n)) throw new Error("unknown type: " + n);
    return { type: n, name: r };
  });
}
re.prototype = tn.prototype = {
  constructor: re,
  on: function(t, e) {
    var n = this._, r = ta(t + "", n), i, o = -1, a = r.length;
    if (arguments.length < 2) {
      for (; ++o < a; ) if ((i = (t = r[o]).type) && (i = ea(n[i], t.name))) return i;
      return;
    }
    if (e != null && typeof e != "function") throw new Error("invalid callback: " + e);
    for (; ++o < a; )
      if (i = (t = r[o]).type) n[i] = dn(n[i], t.name, e);
      else if (e == null) for (i in n) n[i] = dn(n[i], t.name, null);
    return this;
  },
  copy: function() {
    var t = {}, e = this._;
    for (var n in e) t[n] = e[n].slice();
    return new re(t);
  },
  call: function(t, e) {
    if ((i = arguments.length - 2) > 0) for (var n = new Array(i), r = 0, i, o; r < i; ++r) n[r] = arguments[r + 2];
    if (!this._.hasOwnProperty(t)) throw new Error("unknown type: " + t);
    for (o = this._[t], r = 0, i = o.length; r < i; ++r) o[r].value.apply(e, n);
  },
  apply: function(t, e, n) {
    if (!this._.hasOwnProperty(t)) throw new Error("unknown type: " + t);
    for (var r = this._[t], i = 0, o = r.length; i < o; ++i) r[i].value.apply(e, n);
  }
};
function ea(t, e) {
  for (var n = 0, r = t.length, i; n < r; ++n)
    if ((i = t[n]).name === e)
      return i.value;
}
function dn(t, e, n) {
  for (var r = 0, i = t.length; r < i; ++r)
    if (t[r].name === e) {
      t[r] = Zo, t = t.slice(0, r).concat(t.slice(r + 1));
      break;
    }
  return n != null && t.push({ name: e, value: n }), t;
}
const Fe = { capture: !0, passive: !1 };
function je(t) {
  t.preventDefault(), t.stopImmediatePropagation();
}
function na(t) {
  var e = t.document.documentElement, n = et(t).on("dragstart.drag", je, Fe);
  "onselectstart" in e ? n.on("selectstart.drag", je, Fe) : (e.__noselect = e.style.MozUserSelect, e.style.MozUserSelect = "none");
}
function ra(t, e) {
  var n = t.document.documentElement, r = et(t).on("dragstart.drag", null);
  e && (r.on("click.drag", je, Fe), setTimeout(function() {
    r.on("click.drag", null);
  }, 0)), "onselectstart" in n ? r.on("selectstart.drag", null) : (n.style.MozUserSelect = n.__noselect, delete n.__noselect);
}
function en(t, e, n) {
  t.prototype = e.prototype = n, n.constructor = t;
}
function yr(t, e) {
  var n = Object.create(t.prototype);
  for (var r in e) n[r] = e[r];
  return n;
}
function Lt() {
}
var kt = 0.7, ue = 1 / kt, bt = "\\s*([+-]?\\d+)\\s*", Dt = "\\s*([+-]?(?:\\d*\\.)?\\d+(?:[eE][+-]?\\d+)?)\\s*", ut = "\\s*([+-]?(?:\\d*\\.)?\\d+(?:[eE][+-]?\\d+)?)%\\s*", ia = /^#([0-9a-f]{3,8})$/, oa = new RegExp(`^rgb\\(${bt},${bt},${bt}\\)$`), aa = new RegExp(`^rgb\\(${ut},${ut},${ut}\\)$`), sa = new RegExp(`^rgba\\(${bt},${bt},${bt},${Dt}\\)$`), ua = new RegExp(`^rgba\\(${ut},${ut},${ut},${Dt}\\)$`), la = new RegExp(`^hsl\\(${Dt},${ut},${ut}\\)$`), ca = new RegExp(`^hsla\\(${Dt},${ut},${ut},${Dt}\\)$`), pn = {
  aliceblue: 15792383,
  antiquewhite: 16444375,
  aqua: 65535,
  aquamarine: 8388564,
  azure: 15794175,
  beige: 16119260,
  bisque: 16770244,
  black: 0,
  blanchedalmond: 16772045,
  blue: 255,
  blueviolet: 9055202,
  brown: 10824234,
  burlywood: 14596231,
  cadetblue: 6266528,
  chartreuse: 8388352,
  chocolate: 13789470,
  coral: 16744272,
  cornflowerblue: 6591981,
  cornsilk: 16775388,
  crimson: 14423100,
  cyan: 65535,
  darkblue: 139,
  darkcyan: 35723,
  darkgoldenrod: 12092939,
  darkgray: 11119017,
  darkgreen: 25600,
  darkgrey: 11119017,
  darkkhaki: 12433259,
  darkmagenta: 9109643,
  darkolivegreen: 5597999,
  darkorange: 16747520,
  darkorchid: 10040012,
  darkred: 9109504,
  darksalmon: 15308410,
  darkseagreen: 9419919,
  darkslateblue: 4734347,
  darkslategray: 3100495,
  darkslategrey: 3100495,
  darkturquoise: 52945,
  darkviolet: 9699539,
  deeppink: 16716947,
  deepskyblue: 49151,
  dimgray: 6908265,
  dimgrey: 6908265,
  dodgerblue: 2003199,
  firebrick: 11674146,
  floralwhite: 16775920,
  forestgreen: 2263842,
  fuchsia: 16711935,
  gainsboro: 14474460,
  ghostwhite: 16316671,
  gold: 16766720,
  goldenrod: 14329120,
  gray: 8421504,
  green: 32768,
  greenyellow: 11403055,
  grey: 8421504,
  honeydew: 15794160,
  hotpink: 16738740,
  indianred: 13458524,
  indigo: 4915330,
  ivory: 16777200,
  khaki: 15787660,
  lavender: 15132410,
  lavenderblush: 16773365,
  lawngreen: 8190976,
  lemonchiffon: 16775885,
  lightblue: 11393254,
  lightcoral: 15761536,
  lightcyan: 14745599,
  lightgoldenrodyellow: 16448210,
  lightgray: 13882323,
  lightgreen: 9498256,
  lightgrey: 13882323,
  lightpink: 16758465,
  lightsalmon: 16752762,
  lightseagreen: 2142890,
  lightskyblue: 8900346,
  lightslategray: 7833753,
  lightslategrey: 7833753,
  lightsteelblue: 11584734,
  lightyellow: 16777184,
  lime: 65280,
  limegreen: 3329330,
  linen: 16445670,
  magenta: 16711935,
  maroon: 8388608,
  mediumaquamarine: 6737322,
  mediumblue: 205,
  mediumorchid: 12211667,
  mediumpurple: 9662683,
  mediumseagreen: 3978097,
  mediumslateblue: 8087790,
  mediumspringgreen: 64154,
  mediumturquoise: 4772300,
  mediumvioletred: 13047173,
  midnightblue: 1644912,
  mintcream: 16121850,
  mistyrose: 16770273,
  moccasin: 16770229,
  navajowhite: 16768685,
  navy: 128,
  oldlace: 16643558,
  olive: 8421376,
  olivedrab: 7048739,
  orange: 16753920,
  orangered: 16729344,
  orchid: 14315734,
  palegoldenrod: 15657130,
  palegreen: 10025880,
  paleturquoise: 11529966,
  palevioletred: 14381203,
  papayawhip: 16773077,
  peachpuff: 16767673,
  peru: 13468991,
  pink: 16761035,
  plum: 14524637,
  powderblue: 11591910,
  purple: 8388736,
  rebeccapurple: 6697881,
  red: 16711680,
  rosybrown: 12357519,
  royalblue: 4286945,
  saddlebrown: 9127187,
  salmon: 16416882,
  sandybrown: 16032864,
  seagreen: 3050327,
  seashell: 16774638,
  sienna: 10506797,
  silver: 12632256,
  skyblue: 8900331,
  slateblue: 6970061,
  slategray: 7372944,
  slategrey: 7372944,
  snow: 16775930,
  springgreen: 65407,
  steelblue: 4620980,
  tan: 13808780,
  teal: 32896,
  thistle: 14204888,
  tomato: 16737095,
  turquoise: 4251856,
  violet: 15631086,
  wheat: 16113331,
  white: 16777215,
  whitesmoke: 16119285,
  yellow: 16776960,
  yellowgreen: 10145074
};
en(Lt, $t, {
  copy(t) {
    return Object.assign(new this.constructor(), this, t);
  },
  displayable() {
    return this.rgb().displayable();
  },
  hex: mn,
  // Deprecated! Use color.formatHex.
  formatHex: mn,
  formatHex8: fa,
  formatHsl: ha,
  formatRgb: gn,
  toString: gn
});
function mn() {
  return this.rgb().formatHex();
}
function fa() {
  return this.rgb().formatHex8();
}
function ha() {
  return vr(this).formatHsl();
}
function gn() {
  return this.rgb().formatRgb();
}
function $t(t) {
  var e, n;
  return t = (t + "").trim().toLowerCase(), (e = ia.exec(t)) ? (n = e[1].length, e = parseInt(e[1], 16), n === 6 ? yn(e) : n === 3 ? new nt(e >> 8 & 15 | e >> 4 & 240, e >> 4 & 15 | e & 240, (e & 15) << 4 | e & 15, 1) : n === 8 ? jt(e >> 24 & 255, e >> 16 & 255, e >> 8 & 255, (e & 255) / 255) : n === 4 ? jt(e >> 12 & 15 | e >> 8 & 240, e >> 8 & 15 | e >> 4 & 240, e >> 4 & 15 | e & 240, ((e & 15) << 4 | e & 15) / 255) : null) : (e = oa.exec(t)) ? new nt(e[1], e[2], e[3], 1) : (e = aa.exec(t)) ? new nt(e[1] * 255 / 100, e[2] * 255 / 100, e[3] * 255 / 100, 1) : (e = sa.exec(t)) ? jt(e[1], e[2], e[3], e[4]) : (e = ua.exec(t)) ? jt(e[1] * 255 / 100, e[2] * 255 / 100, e[3] * 255 / 100, e[4]) : (e = la.exec(t)) ? wn(e[1], e[2] / 100, e[3] / 100, 1) : (e = ca.exec(t)) ? wn(e[1], e[2] / 100, e[3] / 100, e[4]) : pn.hasOwnProperty(t) ? yn(pn[t]) : t === "transparent" ? new nt(NaN, NaN, NaN, 0) : null;
}
function yn(t) {
  return new nt(t >> 16 & 255, t >> 8 & 255, t & 255, 1);
}
function jt(t, e, n, r) {
  return r <= 0 && (t = e = n = NaN), new nt(t, e, n, r);
}
function da(t) {
  return t instanceof Lt || (t = $t(t)), t ? (t = t.rgb(), new nt(t.r, t.g, t.b, t.opacity)) : new nt();
}
function We(t, e, n, r) {
  return arguments.length === 1 ? da(t) : new nt(t, e, n, r ?? 1);
}
function nt(t, e, n, r) {
  this.r = +t, this.g = +e, this.b = +n, this.opacity = +r;
}
en(nt, We, yr(Lt, {
  brighter(t) {
    return t = t == null ? ue : Math.pow(ue, t), new nt(this.r * t, this.g * t, this.b * t, this.opacity);
  },
  darker(t) {
    return t = t == null ? kt : Math.pow(kt, t), new nt(this.r * t, this.g * t, this.b * t, this.opacity);
  },
  rgb() {
    return this;
  },
  clamp() {
    return new nt(vt(this.r), vt(this.g), vt(this.b), le(this.opacity));
  },
  displayable() {
    return -0.5 <= this.r && this.r < 255.5 && -0.5 <= this.g && this.g < 255.5 && -0.5 <= this.b && this.b < 255.5 && 0 <= this.opacity && this.opacity <= 1;
  },
  hex: vn,
  // Deprecated! Use color.formatHex.
  formatHex: vn,
  formatHex8: pa,
  formatRgb: _n,
  toString: _n
}));
function vn() {
  return `#${yt(this.r)}${yt(this.g)}${yt(this.b)}`;
}
function pa() {
  return `#${yt(this.r)}${yt(this.g)}${yt(this.b)}${yt((isNaN(this.opacity) ? 1 : this.opacity) * 255)}`;
}
function _n() {
  const t = le(this.opacity);
  return `${t === 1 ? "rgb(" : "rgba("}${vt(this.r)}, ${vt(this.g)}, ${vt(this.b)}${t === 1 ? ")" : `, ${t})`}`;
}
function le(t) {
  return isNaN(t) ? 1 : Math.max(0, Math.min(1, t));
}
function vt(t) {
  return Math.max(0, Math.min(255, Math.round(t) || 0));
}
function yt(t) {
  return t = vt(t), (t < 16 ? "0" : "") + t.toString(16);
}
function wn(t, e, n, r) {
  return r <= 0 ? t = e = n = NaN : n <= 0 || n >= 1 ? t = e = NaN : e <= 0 && (t = NaN), new ot(t, e, n, r);
}
function vr(t) {
  if (t instanceof ot) return new ot(t.h, t.s, t.l, t.opacity);
  if (t instanceof Lt || (t = $t(t)), !t) return new ot();
  if (t instanceof ot) return t;
  t = t.rgb();
  var e = t.r / 255, n = t.g / 255, r = t.b / 255, i = Math.min(e, n, r), o = Math.max(e, n, r), a = NaN, s = o - i, l = (o + i) / 2;
  return s ? (e === o ? a = (n - r) / s + (n < r) * 6 : n === o ? a = (r - e) / s + 2 : a = (e - n) / s + 4, s /= l < 0.5 ? o + i : 2 - o - i, a *= 60) : s = l > 0 && l < 1 ? 0 : a, new ot(a, s, l, t.opacity);
}
function ma(t, e, n, r) {
  return arguments.length === 1 ? vr(t) : new ot(t, e, n, r ?? 1);
}
function ot(t, e, n, r) {
  this.h = +t, this.s = +e, this.l = +n, this.opacity = +r;
}
en(ot, ma, yr(Lt, {
  brighter(t) {
    return t = t == null ? ue : Math.pow(ue, t), new ot(this.h, this.s, this.l * t, this.opacity);
  },
  darker(t) {
    return t = t == null ? kt : Math.pow(kt, t), new ot(this.h, this.s, this.l * t, this.opacity);
  },
  rgb() {
    var t = this.h % 360 + (this.h < 0) * 360, e = isNaN(t) || isNaN(this.s) ? 0 : this.s, n = this.l, r = n + (n < 0.5 ? n : 1 - n) * e, i = 2 * n - r;
    return new nt(
      be(t >= 240 ? t - 240 : t + 120, i, r),
      be(t, i, r),
      be(t < 120 ? t + 240 : t - 120, i, r),
      this.opacity
    );
  },
  clamp() {
    return new ot(bn(this.h), Wt(this.s), Wt(this.l), le(this.opacity));
  },
  displayable() {
    return (0 <= this.s && this.s <= 1 || isNaN(this.s)) && 0 <= this.l && this.l <= 1 && 0 <= this.opacity && this.opacity <= 1;
  },
  formatHsl() {
    const t = le(this.opacity);
    return `${t === 1 ? "hsl(" : "hsla("}${bn(this.h)}, ${Wt(this.s) * 100}%, ${Wt(this.l) * 100}%${t === 1 ? ")" : `, ${t})`}`;
  }
}));
function bn(t) {
  return t = (t || 0) % 360, t < 0 ? t + 360 : t;
}
function Wt(t) {
  return Math.max(0, Math.min(1, t || 0));
}
function be(t, e, n) {
  return (t < 60 ? e + (n - e) * t / 60 : t < 180 ? n : t < 240 ? e + (n - e) * (240 - t) / 60 : e) * 255;
}
const _r = (t) => () => t;
function ga(t, e) {
  return function(n) {
    return t + n * e;
  };
}
function ya(t, e, n) {
  return t = Math.pow(t, n), e = Math.pow(e, n) - t, n = 1 / n, function(r) {
    return Math.pow(t + r * e, n);
  };
}
function va(t) {
  return (t = +t) == 1 ? wr : function(e, n) {
    return n - e ? ya(e, n, t) : _r(isNaN(e) ? n : e);
  };
}
function wr(t, e) {
  var n = e - t;
  return n ? ga(t, n) : _r(isNaN(t) ? e : t);
}
const xn = function t(e) {
  var n = va(e);
  function r(i, o) {
    var a = n((i = We(i)).r, (o = We(o)).r), s = n(i.g, o.g), l = n(i.b, o.b), u = wr(i.opacity, o.opacity);
    return function(c) {
      return i.r = a(c), i.g = s(c), i.b = l(c), i.opacity = u(c), i + "";
    };
  }
  return r.gamma = t, r;
}(1);
function pt(t, e) {
  return t = +t, e = +e, function(n) {
    return t * (1 - n) + e * n;
  };
}
var Ue = /[-+]?(?:\d+\.?\d*|\.?\d+)(?:[eE][-+]?\d+)?/g, xe = new RegExp(Ue.source, "g");
function _a(t) {
  return function() {
    return t;
  };
}
function wa(t) {
  return function(e) {
    return t(e) + "";
  };
}
function ba(t, e) {
  var n = Ue.lastIndex = xe.lastIndex = 0, r, i, o, a = -1, s = [], l = [];
  for (t = t + "", e = e + ""; (r = Ue.exec(t)) && (i = xe.exec(e)); )
    (o = i.index) > n && (o = e.slice(n, o), s[a] ? s[a] += o : s[++a] = o), (r = r[0]) === (i = i[0]) ? s[a] ? s[a] += i : s[++a] = i : (s[++a] = null, l.push({ i: a, x: pt(r, i) })), n = xe.lastIndex;
  return n < e.length && (o = e.slice(n), s[a] ? s[a] += o : s[++a] = o), s.length < 2 ? l[0] ? wa(l[0].x) : _a(e) : (e = l.length, function(u) {
    for (var c = 0, m; c < e; ++c) s[(m = l[c]).i] = m.x(u);
    return s.join("");
  });
}
var Tn = 180 / Math.PI, Ye = {
  translateX: 0,
  translateY: 0,
  rotate: 0,
  skewX: 0,
  scaleX: 1,
  scaleY: 1
};
function br(t, e, n, r, i, o) {
  var a, s, l;
  return (a = Math.sqrt(t * t + e * e)) && (t /= a, e /= a), (l = t * n + e * r) && (n -= t * l, r -= e * l), (s = Math.sqrt(n * n + r * r)) && (n /= s, r /= s, l /= s), t * r < e * n && (t = -t, e = -e, l = -l, a = -a), {
    translateX: i,
    translateY: o,
    rotate: Math.atan2(e, t) * Tn,
    skewX: Math.atan(l) * Tn,
    scaleX: a,
    scaleY: s
  };
}
var Ut;
function xa(t) {
  const e = new (typeof DOMMatrix == "function" ? DOMMatrix : WebKitCSSMatrix)(t + "");
  return e.isIdentity ? Ye : br(e.a, e.b, e.c, e.d, e.e, e.f);
}
function Ta(t) {
  return t == null || (Ut || (Ut = document.createElementNS("http://www.w3.org/2000/svg", "g")), Ut.setAttribute("transform", t), !(t = Ut.transform.baseVal.consolidate())) ? Ye : (t = t.matrix, br(t.a, t.b, t.c, t.d, t.e, t.f));
}
function xr(t, e, n, r) {
  function i(u) {
    return u.length ? u.pop() + " " : "";
  }
  function o(u, c, m, h, g, x) {
    if (u !== m || c !== h) {
      var S = g.push("translate(", null, e, null, n);
      x.push({ i: S - 4, x: pt(u, m) }, { i: S - 2, x: pt(c, h) });
    } else (m || h) && g.push("translate(" + m + e + h + n);
  }
  function a(u, c, m, h) {
    u !== c ? (u - c > 180 ? c += 360 : c - u > 180 && (u += 360), h.push({ i: m.push(i(m) + "rotate(", null, r) - 2, x: pt(u, c) })) : c && m.push(i(m) + "rotate(" + c + r);
  }
  function s(u, c, m, h) {
    u !== c ? h.push({ i: m.push(i(m) + "skewX(", null, r) - 2, x: pt(u, c) }) : c && m.push(i(m) + "skewX(" + c + r);
  }
  function l(u, c, m, h, g, x) {
    if (u !== m || c !== h) {
      var S = g.push(i(g) + "scale(", null, ",", null, ")");
      x.push({ i: S - 4, x: pt(u, m) }, { i: S - 2, x: pt(c, h) });
    } else (m !== 1 || h !== 1) && g.push(i(g) + "scale(" + m + "," + h + ")");
  }
  return function(u, c) {
    var m = [], h = [];
    return u = t(u), c = t(c), o(u.translateX, u.translateY, c.translateX, c.translateY, m, h), a(u.rotate, c.rotate, m, h), s(u.skewX, c.skewX, m, h), l(u.scaleX, u.scaleY, c.scaleX, c.scaleY, m, h), u = c = null, function(g) {
      for (var x = -1, S = h.length, I; ++x < S; ) m[(I = h[x]).i] = I.x(g);
      return m.join("");
    };
  };
}
var Ea = xr(xa, "px, ", "px)", "deg)"), Sa = xr(Ta, ", ", ")", ")"), Oa = 1e-12;
function En(t) {
  return ((t = Math.exp(t)) + 1 / t) / 2;
}
function Ca(t) {
  return ((t = Math.exp(t)) - 1 / t) / 2;
}
function Ra(t) {
  return ((t = Math.exp(2 * t)) - 1) / (t + 1);
}
const Na = function t(e, n, r) {
  function i(o, a) {
    var s = o[0], l = o[1], u = o[2], c = a[0], m = a[1], h = a[2], g = c - s, x = m - l, S = g * g + x * x, I, T;
    if (S < Oa)
      T = Math.log(h / u) / e, I = function($) {
        return [
          s + $ * g,
          l + $ * x,
          u * Math.exp(e * $ * T)
        ];
      };
    else {
      var L = Math.sqrt(S), R = (h * h - u * u + r * S) / (2 * u * n * L), _ = (h * h - u * u - r * S) / (2 * h * n * L), d = Math.log(Math.sqrt(R * R + 1) - R), D = Math.log(Math.sqrt(_ * _ + 1) - _);
      T = (D - d) / e, I = function($) {
        var w = $ * T, E = En(d), b = u / (n * L) * (E * Ra(e * w + d) - Ca(d));
        return [
          s + b * g,
          l + b * x,
          u * E / En(e * w + d)
        ];
      };
    }
    return I.duration = T * 1e3 * e / Math.SQRT2, I;
  }
  return i.rho = function(o) {
    var a = Math.max(1e-3, +o), s = a * a, l = s * s;
    return t(a, s, l);
  }, i;
}(Math.SQRT2, 2, 4);
var Tt = 0, Nt = 0, Ot = 0, Tr = 1e3, ce, At, fe = 0, _t = 0, me = 0, zt = typeof performance == "object" && performance.now ? performance : Date, Er = typeof window == "object" && window.requestAnimationFrame ? window.requestAnimationFrame.bind(window) : function(t) {
  setTimeout(t, 17);
};
function nn() {
  return _t || (Er(Aa), _t = zt.now() + me);
}
function Aa() {
  _t = 0;
}
function he() {
  this._call = this._time = this._next = null;
}
he.prototype = Sr.prototype = {
  constructor: he,
  restart: function(t, e, n) {
    if (typeof t != "function") throw new TypeError("callback is not a function");
    n = (n == null ? nn() : +n) + (e == null ? 0 : +e), !this._next && At !== this && (At ? At._next = this : ce = this, At = this), this._call = t, this._time = n, Ge();
  },
  stop: function() {
    this._call && (this._call = null, this._time = 1 / 0, Ge());
  }
};
function Sr(t, e, n) {
  var r = new he();
  return r.restart(t, e, n), r;
}
function Ma() {
  nn(), ++Tt;
  for (var t = ce, e; t; )
    (e = _t - t._time) >= 0 && t._call.call(void 0, e), t = t._next;
  --Tt;
}
function Sn() {
  _t = (fe = zt.now()) + me, Tt = Nt = 0;
  try {
    Ma();
  } finally {
    Tt = 0, ka(), _t = 0;
  }
}
function Pa() {
  var t = zt.now(), e = t - fe;
  e > Tr && (me -= e, fe = t);
}
function ka() {
  for (var t, e = ce, n, r = 1 / 0; e; )
    e._call ? (r > e._time && (r = e._time), t = e, e = e._next) : (n = e._next, e._next = null, e = t ? t._next = n : ce = n);
  At = t, Ge(r);
}
function Ge(t) {
  if (!Tt) {
    Nt && (Nt = clearTimeout(Nt));
    var e = t - _t;
    e > 24 ? (t < 1 / 0 && (Nt = setTimeout(Sn, t - zt.now() - me)), Ot && (Ot = clearInterval(Ot))) : (Ot || (fe = zt.now(), Ot = setInterval(Pa, Tr)), Tt = 1, Er(Sn));
  }
}
function On(t, e, n) {
  var r = new he();
  return e = e == null ? 0 : +e, r.restart((i) => {
    r.stop(), t(i + e);
  }, e, n), r;
}
var Da = tn("start", "end", "cancel", "interrupt"), $a = [], Or = 0, Cn = 1, Ve = 2, ie = 3, Rn = 4, Be = 5, oe = 6;
function ge(t, e, n, r, i, o) {
  var a = t.__transition;
  if (!a) t.__transition = {};
  else if (n in a) return;
  za(t, n, {
    name: e,
    index: r,
    // For context during callback.
    group: i,
    // For context during callback.
    on: Da,
    tween: $a,
    time: o.time,
    delay: o.delay,
    duration: o.duration,
    ease: o.ease,
    timer: null,
    state: Or
  });
}
function rn(t, e) {
  var n = at(t, e);
  if (n.state > Or) throw new Error("too late; already scheduled");
  return n;
}
function lt(t, e) {
  var n = at(t, e);
  if (n.state > ie) throw new Error("too late; already running");
  return n;
}
function at(t, e) {
  var n = t.__transition;
  if (!n || !(n = n[e])) throw new Error("transition not found");
  return n;
}
function za(t, e, n) {
  var r = t.__transition, i;
  r[e] = n, n.timer = Sr(o, 0, n.time);
  function o(u) {
    n.state = Cn, n.timer.restart(a, n.delay, n.time), n.delay <= u && a(u - n.delay);
  }
  function a(u) {
    var c, m, h, g;
    if (n.state !== Cn) return l();
    for (c in r)
      if (g = r[c], g.name === n.name) {
        if (g.state === ie) return On(a);
        g.state === Rn ? (g.state = oe, g.timer.stop(), g.on.call("interrupt", t, t.__data__, g.index, g.group), delete r[c]) : +c < e && (g.state = oe, g.timer.stop(), g.on.call("cancel", t, t.__data__, g.index, g.group), delete r[c]);
      }
    if (On(function() {
      n.state === ie && (n.state = Rn, n.timer.restart(s, n.delay, n.time), s(u));
    }), n.state = Ve, n.on.call("start", t, t.__data__, n.index, n.group), n.state === Ve) {
      for (n.state = ie, i = new Array(h = n.tween.length), c = 0, m = -1; c < h; ++c)
        (g = n.tween[c].value.call(t, t.__data__, n.index, n.group)) && (i[++m] = g);
      i.length = m + 1;
    }
  }
  function s(u) {
    for (var c = u < n.duration ? n.ease.call(null, u / n.duration) : (n.timer.restart(l), n.state = Be, 1), m = -1, h = i.length; ++m < h; )
      i[m].call(t, c);
    n.state === Be && (n.on.call("end", t, t.__data__, n.index, n.group), l());
  }
  function l() {
    n.state = oe, n.timer.stop(), delete r[e];
    for (var u in r) return;
    delete t.__transition;
  }
}
function ae(t, e) {
  var n = t.__transition, r, i, o = !0, a;
  if (n) {
    e = e == null ? null : e + "";
    for (a in n) {
      if ((r = n[a]).name !== e) {
        o = !1;
        continue;
      }
      i = r.state > Ve && r.state < Be, r.state = oe, r.timer.stop(), r.on.call(i ? "interrupt" : "cancel", t, t.__data__, r.index, r.group), delete n[a];
    }
    o && delete t.__transition;
  }
}
function Ia(t) {
  return this.each(function() {
    ae(this, t);
  });
}
function La(t, e) {
  var n, r;
  return function() {
    var i = lt(this, t), o = i.tween;
    if (o !== n) {
      r = n = o;
      for (var a = 0, s = r.length; a < s; ++a)
        if (r[a].name === e) {
          r = r.slice(), r.splice(a, 1);
          break;
        }
    }
    i.tween = r;
  };
}
function qa(t, e, n) {
  var r, i;
  if (typeof n != "function") throw new Error();
  return function() {
    var o = lt(this, t), a = o.tween;
    if (a !== r) {
      i = (r = a).slice();
      for (var s = { name: e, value: n }, l = 0, u = i.length; l < u; ++l)
        if (i[l].name === e) {
          i[l] = s;
          break;
        }
      l === u && i.push(s);
    }
    o.tween = i;
  };
}
function Fa(t, e) {
  var n = this._id;
  if (t += "", arguments.length < 2) {
    for (var r = at(this.node(), n).tween, i = 0, o = r.length, a; i < o; ++i)
      if ((a = r[i]).name === t)
        return a.value;
    return null;
  }
  return this.each((e == null ? La : qa)(n, t, e));
}
function on(t, e, n) {
  var r = t._id;
  return t.each(function() {
    var i = lt(this, r);
    (i.value || (i.value = {}))[e] = n.apply(this, arguments);
  }), function(i) {
    return at(i, r).value[e];
  };
}
function Cr(t, e) {
  var n;
  return (typeof e == "number" ? pt : e instanceof $t ? xn : (n = $t(e)) ? (e = n, xn) : ba)(t, e);
}
function ja(t) {
  return function() {
    this.removeAttribute(t);
  };
}
function Wa(t) {
  return function() {
    this.removeAttributeNS(t.space, t.local);
  };
}
function Ua(t, e, n) {
  var r, i = n + "", o;
  return function() {
    var a = this.getAttribute(t);
    return a === i ? null : a === r ? o : o = e(r = a, n);
  };
}
function Ya(t, e, n) {
  var r, i = n + "", o;
  return function() {
    var a = this.getAttributeNS(t.space, t.local);
    return a === i ? null : a === r ? o : o = e(r = a, n);
  };
}
function Ga(t, e, n) {
  var r, i, o;
  return function() {
    var a, s = n(this), l;
    return s == null ? void this.removeAttribute(t) : (a = this.getAttribute(t), l = s + "", a === l ? null : a === r && l === i ? o : (i = l, o = e(r = a, s)));
  };
}
function Va(t, e, n) {
  var r, i, o;
  return function() {
    var a, s = n(this), l;
    return s == null ? void this.removeAttributeNS(t.space, t.local) : (a = this.getAttributeNS(t.space, t.local), l = s + "", a === l ? null : a === r && l === i ? o : (i = l, o = e(r = a, s)));
  };
}
function Ba(t, e) {
  var n = pe(t), r = n === "transform" ? Sa : Cr;
  return this.attrTween(t, typeof e == "function" ? (n.local ? Va : Ga)(n, r, on(this, "attr." + t, e)) : e == null ? (n.local ? Wa : ja)(n) : (n.local ? Ya : Ua)(n, r, e));
}
function Ha(t, e) {
  return function(n) {
    this.setAttribute(t, e.call(this, n));
  };
}
function Xa(t, e) {
  return function(n) {
    this.setAttributeNS(t.space, t.local, e.call(this, n));
  };
}
function Ka(t, e) {
  var n, r;
  function i() {
    var o = e.apply(this, arguments);
    return o !== r && (n = (r = o) && Xa(t, o)), n;
  }
  return i._value = e, i;
}
function Qa(t, e) {
  var n, r;
  function i() {
    var o = e.apply(this, arguments);
    return o !== r && (n = (r = o) && Ha(t, o)), n;
  }
  return i._value = e, i;
}
function Ja(t, e) {
  var n = "attr." + t;
  if (arguments.length < 2) return (n = this.tween(n)) && n._value;
  if (e == null) return this.tween(n, null);
  if (typeof e != "function") throw new Error();
  var r = pe(t);
  return this.tween(n, (r.local ? Ka : Qa)(r, e));
}
function Za(t, e) {
  return function() {
    rn(this, t).delay = +e.apply(this, arguments);
  };
}
function ts(t, e) {
  return e = +e, function() {
    rn(this, t).delay = e;
  };
}
function es(t) {
  var e = this._id;
  return arguments.length ? this.each((typeof t == "function" ? Za : ts)(e, t)) : at(this.node(), e).delay;
}
function ns(t, e) {
  return function() {
    lt(this, t).duration = +e.apply(this, arguments);
  };
}
function rs(t, e) {
  return e = +e, function() {
    lt(this, t).duration = e;
  };
}
function is(t) {
  var e = this._id;
  return arguments.length ? this.each((typeof t == "function" ? ns : rs)(e, t)) : at(this.node(), e).duration;
}
function os(t, e) {
  if (typeof e != "function") throw new Error();
  return function() {
    lt(this, t).ease = e;
  };
}
function as(t) {
  var e = this._id;
  return arguments.length ? this.each(os(e, t)) : at(this.node(), e).ease;
}
function ss(t, e) {
  return function() {
    var n = e.apply(this, arguments);
    if (typeof n != "function") throw new Error();
    lt(this, t).ease = n;
  };
}
function us(t) {
  if (typeof t != "function") throw new Error();
  return this.each(ss(this._id, t));
}
function ls(t) {
  typeof t != "function" && (t = sr(t));
  for (var e = this._groups, n = e.length, r = new Array(n), i = 0; i < n; ++i)
    for (var o = e[i], a = o.length, s = r[i] = [], l, u = 0; u < a; ++u)
      (l = o[u]) && t.call(l, l.__data__, u, o) && s.push(l);
  return new dt(r, this._parents, this._name, this._id);
}
function cs(t) {
  if (t._id !== this._id) throw new Error();
  for (var e = this._groups, n = t._groups, r = e.length, i = n.length, o = Math.min(r, i), a = new Array(r), s = 0; s < o; ++s)
    for (var l = e[s], u = n[s], c = l.length, m = a[s] = new Array(c), h, g = 0; g < c; ++g)
      (h = l[g] || u[g]) && (m[g] = h);
  for (; s < r; ++s)
    a[s] = e[s];
  return new dt(a, this._parents, this._name, this._id);
}
function fs(t) {
  return (t + "").trim().split(/^|\s+/).every(function(e) {
    var n = e.indexOf(".");
    return n >= 0 && (e = e.slice(0, n)), !e || e === "start";
  });
}
function hs(t, e, n) {
  var r, i, o = fs(e) ? rn : lt;
  return function() {
    var a = o(this, t), s = a.on;
    s !== r && (i = (r = s).copy()).on(e, n), a.on = i;
  };
}
function ds(t, e) {
  var n = this._id;
  return arguments.length < 2 ? at(this.node(), n).on.on(t) : this.each(hs(n, t, e));
}
function ps(t) {
  return function() {
    var e = this.parentNode;
    for (var n in this.__transition) if (+n !== t) return;
    e && e.removeChild(this);
  };
}
function ms() {
  return this.on("end.remove", ps(this._id));
}
function gs(t) {
  var e = this._name, n = this._id;
  typeof t != "function" && (t = Je(t));
  for (var r = this._groups, i = r.length, o = new Array(i), a = 0; a < i; ++a)
    for (var s = r[a], l = s.length, u = o[a] = new Array(l), c, m, h = 0; h < l; ++h)
      (c = s[h]) && (m = t.call(c, c.__data__, h, s)) && ("__data__" in c && (m.__data__ = c.__data__), u[h] = m, ge(u[h], e, n, h, u, at(c, n)));
  return new dt(o, this._parents, e, n);
}
function ys(t) {
  var e = this._name, n = this._id;
  typeof t != "function" && (t = ar(t));
  for (var r = this._groups, i = r.length, o = [], a = [], s = 0; s < i; ++s)
    for (var l = r[s], u = l.length, c, m = 0; m < u; ++m)
      if (c = l[m]) {
        for (var h = t.call(c, c.__data__, m, l), g, x = at(c, n), S = 0, I = h.length; S < I; ++S)
          (g = h[S]) && ge(g, e, n, S, h, x);
        o.push(h), a.push(c);
      }
  return new dt(o, a, e, n);
}
var vs = It.prototype.constructor;
function _s() {
  return new vs(this._groups, this._parents);
}
function ws(t, e) {
  var n, r, i;
  return function() {
    var o = xt(this, t), a = (this.style.removeProperty(t), xt(this, t));
    return o === a ? null : o === n && a === r ? i : i = e(n = o, r = a);
  };
}
function Rr(t) {
  return function() {
    this.style.removeProperty(t);
  };
}
function bs(t, e, n) {
  var r, i = n + "", o;
  return function() {
    var a = xt(this, t);
    return a === i ? null : a === r ? o : o = e(r = a, n);
  };
}
function xs(t, e, n) {
  var r, i, o;
  return function() {
    var a = xt(this, t), s = n(this), l = s + "";
    return s == null && (l = s = (this.style.removeProperty(t), xt(this, t))), a === l ? null : a === r && l === i ? o : (i = l, o = e(r = a, s));
  };
}
function Ts(t, e) {
  var n, r, i, o = "style." + e, a = "end." + o, s;
  return function() {
    var l = lt(this, t), u = l.on, c = l.value[o] == null ? s || (s = Rr(e)) : void 0;
    (u !== n || i !== c) && (r = (n = u).copy()).on(a, i = c), l.on = r;
  };
}
function Es(t, e, n) {
  var r = (t += "") == "transform" ? Ea : Cr;
  return e == null ? this.styleTween(t, ws(t, r)).on("end.style." + t, Rr(t)) : typeof e == "function" ? this.styleTween(t, xs(t, r, on(this, "style." + t, e))).each(Ts(this._id, t)) : this.styleTween(t, bs(t, r, e), n).on("end.style." + t, null);
}
function Ss(t, e, n) {
  return function(r) {
    this.style.setProperty(t, e.call(this, r), n);
  };
}
function Os(t, e, n) {
  var r, i;
  function o() {
    var a = e.apply(this, arguments);
    return a !== i && (r = (i = a) && Ss(t, a, n)), r;
  }
  return o._value = e, o;
}
function Cs(t, e, n) {
  var r = "style." + (t += "");
  if (arguments.length < 2) return (r = this.tween(r)) && r._value;
  if (e == null) return this.tween(r, null);
  if (typeof e != "function") throw new Error();
  return this.tween(r, Os(t, e, n ?? ""));
}
function Rs(t) {
  return function() {
    this.textContent = t;
  };
}
function Ns(t) {
  return function() {
    var e = t(this);
    this.textContent = e ?? "";
  };
}
function As(t) {
  return this.tween("text", typeof t == "function" ? Ns(on(this, "text", t)) : Rs(t == null ? "" : t + ""));
}
function Ms(t) {
  return function(e) {
    this.textContent = t.call(this, e);
  };
}
function Ps(t) {
  var e, n;
  function r() {
    var i = t.apply(this, arguments);
    return i !== n && (e = (n = i) && Ms(i)), e;
  }
  return r._value = t, r;
}
function ks(t) {
  var e = "text";
  if (arguments.length < 1) return (e = this.tween(e)) && e._value;
  if (t == null) return this.tween(e, null);
  if (typeof t != "function") throw new Error();
  return this.tween(e, Ps(t));
}
function Ds() {
  for (var t = this._name, e = this._id, n = Nr(), r = this._groups, i = r.length, o = 0; o < i; ++o)
    for (var a = r[o], s = a.length, l, u = 0; u < s; ++u)
      if (l = a[u]) {
        var c = at(l, e);
        ge(l, t, n, u, a, {
          time: c.time + c.delay + c.duration,
          delay: 0,
          duration: c.duration,
          ease: c.ease
        });
      }
  return new dt(r, this._parents, t, n);
}
function $s() {
  var t, e, n = this, r = n._id, i = n.size();
  return new Promise(function(o, a) {
    var s = { value: a }, l = { value: function() {
      --i === 0 && o();
    } };
    n.each(function() {
      var u = lt(this, r), c = u.on;
      c !== t && (e = (t = c).copy(), e._.cancel.push(s), e._.interrupt.push(s), e._.end.push(l)), u.on = e;
    }), i === 0 && o();
  });
}
var zs = 0;
function dt(t, e, n, r) {
  this._groups = t, this._parents = e, this._name = n, this._id = r;
}
function Nr() {
  return ++zs;
}
var ct = It.prototype;
dt.prototype = {
  constructor: dt,
  select: gs,
  selectAll: ys,
  selectChild: ct.selectChild,
  selectChildren: ct.selectChildren,
  filter: ls,
  merge: cs,
  selection: _s,
  transition: Ds,
  call: ct.call,
  nodes: ct.nodes,
  node: ct.node,
  size: ct.size,
  empty: ct.empty,
  each: ct.each,
  on: ds,
  attr: Ba,
  attrTween: Ja,
  style: Es,
  styleTween: Cs,
  text: As,
  textTween: ks,
  remove: ms,
  tween: Fa,
  delay: es,
  duration: is,
  ease: as,
  easeVarying: us,
  end: $s,
  [Symbol.iterator]: ct[Symbol.iterator]
};
function Is(t) {
  return ((t *= 2) <= 1 ? t * t * t : (t -= 2) * t * t + 2) / 2;
}
var Ls = {
  time: null,
  // Set on use.
  delay: 0,
  duration: 250,
  ease: Is
};
function qs(t, e) {
  for (var n; !(n = t.__transition) || !(n = n[e]); )
    if (!(t = t.parentNode))
      throw new Error(`transition ${e} not found`);
  return n;
}
function Fs(t) {
  var e, n;
  t instanceof dt ? (e = t._id, t = t._name) : (e = Nr(), (n = Ls).time = nn(), t = t == null ? null : t + "");
  for (var r = this._groups, i = r.length, o = 0; o < i; ++o)
    for (var a = r[o], s = a.length, l, u = 0; u < s; ++u)
      (l = a[u]) && ge(l, t, e, u, a, n || qs(l, e));
  return new dt(r, this._parents, t, e);
}
It.prototype.interrupt = Ia;
It.prototype.transition = Fs;
const Yt = (t) => () => t;
function js(t, {
  sourceEvent: e,
  target: n,
  transform: r,
  dispatch: i
}) {
  Object.defineProperties(this, {
    type: { value: t, enumerable: !0, configurable: !0 },
    sourceEvent: { value: e, enumerable: !0, configurable: !0 },
    target: { value: n, enumerable: !0, configurable: !0 },
    transform: { value: r, enumerable: !0, configurable: !0 },
    _: { value: i }
  });
}
function ht(t, e, n) {
  this.k = t, this.x = e, this.y = n;
}
ht.prototype = {
  constructor: ht,
  scale: function(t) {
    return t === 1 ? this : new ht(this.k * t, this.x, this.y);
  },
  translate: function(t, e) {
    return t === 0 & e === 0 ? this : new ht(this.k, this.x + this.k * t, this.y + this.k * e);
  },
  apply: function(t) {
    return [t[0] * this.k + this.x, t[1] * this.k + this.y];
  },
  applyX: function(t) {
    return t * this.k + this.x;
  },
  applyY: function(t) {
    return t * this.k + this.y;
  },
  invert: function(t) {
    return [(t[0] - this.x) / this.k, (t[1] - this.y) / this.k];
  },
  invertX: function(t) {
    return (t - this.x) / this.k;
  },
  invertY: function(t) {
    return (t - this.y) / this.k;
  },
  rescaleX: function(t) {
    return t.copy().domain(t.range().map(this.invertX, this).map(t.invert, t));
  },
  rescaleY: function(t) {
    return t.copy().domain(t.range().map(this.invertY, this).map(t.invert, t));
  },
  toString: function() {
    return "translate(" + this.x + "," + this.y + ") scale(" + this.k + ")";
  }
};
var de = new ht(1, 0, 0);
ht.prototype;
function Te(t) {
  t.stopImmediatePropagation();
}
function Ct(t) {
  t.preventDefault(), t.stopImmediatePropagation();
}
function Ws(t) {
  return (!t.ctrlKey || t.type === "wheel") && !t.button;
}
function Us() {
  var t = this;
  return t instanceof SVGElement ? (t = t.ownerSVGElement || t, t.hasAttribute("viewBox") ? (t = t.viewBox.baseVal, [[t.x, t.y], [t.x + t.width, t.y + t.height]]) : [[0, 0], [t.width.baseVal.value, t.height.baseVal.value]]) : [[0, 0], [t.clientWidth, t.clientHeight]];
}
function Nn() {
  return this.__zoom || de;
}
function Ys(t) {
  return -t.deltaY * (t.deltaMode === 1 ? 0.05 : t.deltaMode ? 1 : 2e-3) * (t.ctrlKey ? 10 : 1);
}
function Gs() {
  return navigator.maxTouchPoints || "ontouchstart" in this;
}
function Vs(t, e, n) {
  var r = t.invertX(e[0][0]) - n[0][0], i = t.invertX(e[1][0]) - n[1][0], o = t.invertY(e[0][1]) - n[0][1], a = t.invertY(e[1][1]) - n[1][1];
  return t.translate(
    i > r ? (r + i) / 2 : Math.min(0, r) || Math.max(0, i),
    a > o ? (o + a) / 2 : Math.min(0, o) || Math.max(0, a)
  );
}
function Ee() {
  var t = Ws, e = Us, n = Vs, r = Ys, i = Gs, o = [0, 1 / 0], a = [[-1 / 0, -1 / 0], [1 / 0, 1 / 0]], s = 250, l = Na, u = tn("start", "zoom", "end"), c, m, h, g = 500, x = 150, S = 0, I = 10;
  function T(p) {
    p.property("__zoom", Nn).on("wheel.zoom", w, { passive: !1 }).on("mousedown.zoom", E).on("dblclick.zoom", b).filter(i).on("touchstart.zoom", A).on("touchmove.zoom", Y).on("touchend.zoom touchcancel.zoom", H).style("-webkit-tap-highlight-color", "rgba(0,0,0,0)");
  }
  T.transform = function(p, N, v, C) {
    var P = p.selection ? p.selection() : p;
    P.property("__zoom", Nn), p !== P ? d(p, N, v, C) : P.interrupt().each(function() {
      D(this, arguments).event(C).start().zoom(null, typeof N == "function" ? N.apply(this, arguments) : N).end();
    });
  }, T.scaleBy = function(p, N, v, C) {
    T.scaleTo(p, function() {
      var P = this.__zoom.k, j = typeof N == "function" ? N.apply(this, arguments) : N;
      return P * j;
    }, v, C);
  }, T.scaleTo = function(p, N, v, C) {
    T.transform(p, function() {
      var P = e.apply(this, arguments), j = this.__zoom, f = v == null ? _(P) : typeof v == "function" ? v.apply(this, arguments) : v, y = j.invert(f), O = typeof N == "function" ? N.apply(this, arguments) : N;
      return n(R(L(j, O), f, y), P, a);
    }, v, C);
  }, T.translateBy = function(p, N, v, C) {
    T.transform(p, function() {
      return n(this.__zoom.translate(
        typeof N == "function" ? N.apply(this, arguments) : N,
        typeof v == "function" ? v.apply(this, arguments) : v
      ), e.apply(this, arguments), a);
    }, null, C);
  }, T.translateTo = function(p, N, v, C, P) {
    T.transform(p, function() {
      var j = e.apply(this, arguments), f = this.__zoom, y = C == null ? _(j) : typeof C == "function" ? C.apply(this, arguments) : C;
      return n(de.translate(y[0], y[1]).scale(f.k).translate(
        typeof N == "function" ? -N.apply(this, arguments) : -N,
        typeof v == "function" ? -v.apply(this, arguments) : -v
      ), j, a);
    }, C, P);
  };
  function L(p, N) {
    return N = Math.max(o[0], Math.min(o[1], N)), N === p.k ? p : new ht(N, p.x, p.y);
  }
  function R(p, N, v) {
    var C = N[0] - v[0] * p.k, P = N[1] - v[1] * p.k;
    return C === p.x && P === p.y ? p : new ht(p.k, C, P);
  }
  function _(p) {
    return [(+p[0][0] + +p[1][0]) / 2, (+p[0][1] + +p[1][1]) / 2];
  }
  function d(p, N, v, C) {
    p.on("start.zoom", function() {
      D(this, arguments).event(C).start();
    }).on("interrupt.zoom end.zoom", function() {
      D(this, arguments).event(C).end();
    }).tween("zoom", function() {
      var P = this, j = arguments, f = D(P, j).event(C), y = e.apply(P, j), O = v == null ? _(y) : typeof v == "function" ? v.apply(P, j) : v, z = Math.max(y[1][0] - y[0][0], y[1][1] - y[0][1]), k = P.__zoom, F = typeof N == "function" ? N.apply(P, j) : N, U = l(k.invert(O).concat(z / k.k), F.invert(O).concat(z / F.k));
      return function(W) {
        if (W === 1) W = F;
        else {
          var q = U(W), G = z / q[2];
          W = new ht(G, O[0] - q[0] * G, O[1] - q[1] * G);
        }
        f.zoom(null, W);
      };
    });
  }
  function D(p, N, v) {
    return !v && p.__zooming || new $(p, N);
  }
  function $(p, N) {
    this.that = p, this.args = N, this.active = 0, this.sourceEvent = null, this.extent = e.apply(p, N), this.taps = 0;
  }
  $.prototype = {
    event: function(p) {
      return p && (this.sourceEvent = p), this;
    },
    start: function() {
      return ++this.active === 1 && (this.that.__zooming = this, this.emit("start")), this;
    },
    zoom: function(p, N) {
      return this.mouse && p !== "mouse" && (this.mouse[1] = N.invert(this.mouse[0])), this.touch0 && p !== "touch" && (this.touch0[1] = N.invert(this.touch0[0])), this.touch1 && p !== "touch" && (this.touch1[1] = N.invert(this.touch1[0])), this.that.__zoom = N, this.emit("zoom"), this;
    },
    end: function() {
      return --this.active === 0 && (delete this.that.__zooming, this.emit("end")), this;
    },
    emit: function(p) {
      var N = et(this.that).datum();
      u.call(
        p,
        this.that,
        new js(p, {
          sourceEvent: this.sourceEvent,
          target: T,
          transform: this.that.__zoom,
          dispatch: u
        }),
        N
      );
    }
  };
  function w(p, ...N) {
    if (!t.apply(this, arguments)) return;
    var v = D(this, N).event(p), C = this.__zoom, P = Math.max(o[0], Math.min(o[1], C.k * Math.pow(2, r.apply(this, arguments)))), j = mt(p);
    if (v.wheel)
      (v.mouse[0][0] !== j[0] || v.mouse[0][1] !== j[1]) && (v.mouse[1] = C.invert(v.mouse[0] = j)), clearTimeout(v.wheel);
    else {
      if (C.k === P) return;
      v.mouse = [j, C.invert(j)], ae(this), v.start();
    }
    Ct(p), v.wheel = setTimeout(f, x), v.zoom("mouse", n(R(L(C, P), v.mouse[0], v.mouse[1]), v.extent, a));
    function f() {
      v.wheel = null, v.end();
    }
  }
  function E(p, ...N) {
    if (h || !t.apply(this, arguments)) return;
    var v = p.currentTarget, C = D(this, N, !0).event(p), P = et(p.view).on("mousemove.zoom", O, !0).on("mouseup.zoom", z, !0), j = mt(p, v), f = p.clientX, y = p.clientY;
    na(p.view), Te(p), C.mouse = [j, this.__zoom.invert(j)], ae(this), C.start();
    function O(k) {
      if (Ct(k), !C.moved) {
        var F = k.clientX - f, U = k.clientY - y;
        C.moved = F * F + U * U > S;
      }
      C.event(k).zoom("mouse", n(R(C.that.__zoom, C.mouse[0] = mt(k, v), C.mouse[1]), C.extent, a));
    }
    function z(k) {
      P.on("mousemove.zoom mouseup.zoom", null), ra(k.view, C.moved), Ct(k), C.event(k).end();
    }
  }
  function b(p, ...N) {
    if (t.apply(this, arguments)) {
      var v = this.__zoom, C = mt(p.changedTouches ? p.changedTouches[0] : p, this), P = v.invert(C), j = v.k * (p.shiftKey ? 0.5 : 2), f = n(R(L(v, j), C, P), e.apply(this, N), a);
      Ct(p), s > 0 ? et(this).transition().duration(s).call(d, f, C, p) : et(this).call(T.transform, f, C, p);
    }
  }
  function A(p, ...N) {
    if (t.apply(this, arguments)) {
      var v = p.touches, C = v.length, P = D(this, N, p.changedTouches.length === C).event(p), j, f, y, O;
      for (Te(p), f = 0; f < C; ++f)
        y = v[f], O = mt(y, this), O = [O, this.__zoom.invert(O), y.identifier], P.touch0 ? !P.touch1 && P.touch0[2] !== O[2] && (P.touch1 = O, P.taps = 0) : (P.touch0 = O, j = !0, P.taps = 1 + !!c);
      c && (c = clearTimeout(c)), j && (P.taps < 2 && (m = O[0], c = setTimeout(function() {
        c = null;
      }, g)), ae(this), P.start());
    }
  }
  function Y(p, ...N) {
    if (this.__zooming) {
      var v = D(this, N).event(p), C = p.changedTouches, P = C.length, j, f, y, O;
      for (Ct(p), j = 0; j < P; ++j)
        f = C[j], y = mt(f, this), v.touch0 && v.touch0[2] === f.identifier ? v.touch0[0] = y : v.touch1 && v.touch1[2] === f.identifier && (v.touch1[0] = y);
      if (f = v.that.__zoom, v.touch1) {
        var z = v.touch0[0], k = v.touch0[1], F = v.touch1[0], U = v.touch1[1], W = (W = F[0] - z[0]) * W + (W = F[1] - z[1]) * W, q = (q = U[0] - k[0]) * q + (q = U[1] - k[1]) * q;
        f = L(f, Math.sqrt(W / q)), y = [(z[0] + F[0]) / 2, (z[1] + F[1]) / 2], O = [(k[0] + U[0]) / 2, (k[1] + U[1]) / 2];
      } else if (v.touch0) y = v.touch0[0], O = v.touch0[1];
      else return;
      v.zoom("touch", n(R(f, y, O), v.extent, a));
    }
  }
  function H(p, ...N) {
    if (this.__zooming) {
      var v = D(this, N).event(p), C = p.changedTouches, P = C.length, j, f;
      for (Te(p), h && clearTimeout(h), h = setTimeout(function() {
        h = null;
      }, g), j = 0; j < P; ++j)
        f = C[j], v.touch0 && v.touch0[2] === f.identifier ? delete v.touch0 : v.touch1 && v.touch1[2] === f.identifier && delete v.touch1;
      if (v.touch1 && !v.touch0 && (v.touch0 = v.touch1, delete v.touch1), v.touch0) v.touch0[1] = this.__zoom.invert(v.touch0[0]);
      else if (v.end(), v.taps === 2 && (f = mt(f, this), Math.hypot(m[0] - f[0], m[1] - f[1]) < I)) {
        var y = et(this).on("dblclick.zoom");
        y && y.apply(this, arguments);
      }
    }
  }
  return T.wheelDelta = function(p) {
    return arguments.length ? (r = typeof p == "function" ? p : Yt(+p), T) : r;
  }, T.filter = function(p) {
    return arguments.length ? (t = typeof p == "function" ? p : Yt(!!p), T) : t;
  }, T.touchable = function(p) {
    return arguments.length ? (i = typeof p == "function" ? p : Yt(!!p), T) : i;
  }, T.extent = function(p) {
    return arguments.length ? (e = typeof p == "function" ? p : Yt([[+p[0][0], +p[0][1]], [+p[1][0], +p[1][1]]]), T) : e;
  }, T.scaleExtent = function(p) {
    return arguments.length ? (o[0] = +p[0], o[1] = +p[1], T) : [o[0], o[1]];
  }, T.translateExtent = function(p) {
    return arguments.length ? (a[0][0] = +p[0][0], a[1][0] = +p[1][0], a[0][1] = +p[0][1], a[1][1] = +p[1][1], T) : [[a[0][0], a[0][1]], [a[1][0], a[1][1]]];
  }, T.constrain = function(p) {
    return arguments.length ? (n = p, T) : n;
  }, T.duration = function(p) {
    return arguments.length ? (s = +p, T) : s;
  }, T.interpolate = function(p) {
    return arguments.length ? (l = p, T) : l;
  }, T.on = function() {
    var p = u.on.apply(u, arguments);
    return p === u ? T : p;
  }, T.clickDistance = function(p) {
    return arguments.length ? (S = (p = +p) * p, T) : Math.sqrt(S);
  }, T.tapDistance = function(p) {
    return arguments.length ? (I = +p, T) : I;
  }, T;
}
var An = Object.prototype.hasOwnProperty;
function Mt(t, e) {
  var n, r;
  if (t === e) return !0;
  if (t && e && (n = t.constructor) === e.constructor) {
    if (n === Date) return t.getTime() === e.getTime();
    if (n === RegExp) return t.toString() === e.toString();
    if (n === Array) {
      if ((r = t.length) === e.length)
        for (; r-- && Mt(t[r], e[r]); ) ;
      return r === -1;
    }
    if (!n || typeof t == "object") {
      r = 0;
      for (n in t)
        if (An.call(t, n) && ++r && !An.call(e, n) || !(n in e) || !Mt(t[n], e[n])) return !1;
      return Object.keys(e).length === r;
    }
  }
  return t !== t && e !== e;
}
var Se = { exports: {} }, Mn;
function Bs() {
  return Mn || (Mn = 1, function(t) {
    var e = function() {
      function n(h, g) {
        return g != null && h instanceof g;
      }
      var r;
      try {
        r = Map;
      } catch {
        r = function() {
        };
      }
      var i;
      try {
        i = Set;
      } catch {
        i = function() {
        };
      }
      var o;
      try {
        o = Promise;
      } catch {
        o = function() {
        };
      }
      function a(h, g, x, S, I) {
        typeof g == "object" && (x = g.depth, S = g.prototype, I = g.includeNonEnumerable, g = g.circular);
        var T = [], L = [], R = typeof Buffer < "u";
        typeof g > "u" && (g = !0), typeof x > "u" && (x = 1 / 0);
        function _(d, D) {
          if (d === null)
            return null;
          if (D === 0)
            return d;
          var $, w;
          if (typeof d != "object")
            return d;
          if (n(d, r))
            $ = new r();
          else if (n(d, i))
            $ = new i();
          else if (n(d, o))
            $ = new o(function(C, P) {
              d.then(function(j) {
                C(_(j, D - 1));
              }, function(j) {
                P(_(j, D - 1));
              });
            });
          else if (a.__isArray(d))
            $ = [];
          else if (a.__isRegExp(d))
            $ = new RegExp(d.source, m(d)), d.lastIndex && ($.lastIndex = d.lastIndex);
          else if (a.__isDate(d))
            $ = new Date(d.getTime());
          else {
            if (R && Buffer.isBuffer(d))
              return Buffer.allocUnsafe ? $ = Buffer.allocUnsafe(d.length) : $ = new Buffer(d.length), d.copy($), $;
            n(d, Error) ? $ = Object.create(d) : typeof S > "u" ? (w = Object.getPrototypeOf(d), $ = Object.create(w)) : ($ = Object.create(S), w = S);
          }
          if (g) {
            var E = T.indexOf(d);
            if (E != -1)
              return L[E];
            T.push(d), L.push($);
          }
          n(d, r) && d.forEach(function(C, P) {
            var j = _(P, D - 1), f = _(C, D - 1);
            $.set(j, f);
          }), n(d, i) && d.forEach(function(C) {
            var P = _(C, D - 1);
            $.add(P);
          });
          for (var b in d) {
            var A;
            w && (A = Object.getOwnPropertyDescriptor(w, b)), !(A && A.set == null) && ($[b] = _(d[b], D - 1));
          }
          if (Object.getOwnPropertySymbols)
            for (var Y = Object.getOwnPropertySymbols(d), b = 0; b < Y.length; b++) {
              var H = Y[b], p = Object.getOwnPropertyDescriptor(d, H);
              p && !p.enumerable && !I || ($[H] = _(d[H], D - 1), p.enumerable || Object.defineProperty($, H, {
                enumerable: !1
              }));
            }
          if (I)
            for (var N = Object.getOwnPropertyNames(d), b = 0; b < N.length; b++) {
              var v = N[b], p = Object.getOwnPropertyDescriptor(d, v);
              p && p.enumerable || ($[v] = _(d[v], D - 1), Object.defineProperty($, v, {
                enumerable: !1
              }));
            }
          return $;
        }
        return _(h, x);
      }
      a.clonePrototype = function(g) {
        if (g === null)
          return null;
        var x = function() {
        };
        return x.prototype = g, new x();
      };
      function s(h) {
        return Object.prototype.toString.call(h);
      }
      a.__objToStr = s;
      function l(h) {
        return typeof h == "object" && s(h) === "[object Date]";
      }
      a.__isDate = l;
      function u(h) {
        return typeof h == "object" && s(h) === "[object Array]";
      }
      a.__isArray = u;
      function c(h) {
        return typeof h == "object" && s(h) === "[object RegExp]";
      }
      a.__isRegExp = c;
      function m(h) {
        var g = "";
        return h.global && (g += "g"), h.ignoreCase && (g += "i"), h.multiline && (g += "m"), g;
      }
      return a.__getRegExpFlags = m, a;
    }();
    t.exports && (t.exports = e);
  }(Se)), Se.exports;
}
var Hs = Bs();
const tt = /* @__PURE__ */ Gr(Hs);
var Gt, Xs = new Uint8Array(16);
function Ks() {
  if (!Gt && (Gt = typeof crypto < "u" && crypto.getRandomValues && crypto.getRandomValues.bind(crypto) || typeof msCrypto < "u" && typeof msCrypto.getRandomValues == "function" && msCrypto.getRandomValues.bind(msCrypto), !Gt))
    throw new Error("crypto.getRandomValues() not supported. See https://github.com/uuidjs/uuid#getrandomvalues-not-supported");
  return Gt(Xs);
}
const Qs = /^(?:[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}|00000000-0000-0000-0000-000000000000)$/i;
function Js(t) {
  return typeof t == "string" && Qs.test(t);
}
var J = [];
for (var Oe = 0; Oe < 256; ++Oe)
  J.push((Oe + 256).toString(16).substr(1));
function Zs(t) {
  var e = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : 0, n = (J[t[e + 0]] + J[t[e + 1]] + J[t[e + 2]] + J[t[e + 3]] + "-" + J[t[e + 4]] + J[t[e + 5]] + "-" + J[t[e + 6]] + J[t[e + 7]] + "-" + J[t[e + 8]] + J[t[e + 9]] + "-" + J[t[e + 10]] + J[t[e + 11]] + J[t[e + 12]] + J[t[e + 13]] + J[t[e + 14]] + J[t[e + 15]]).toLowerCase();
  if (!Js(n))
    throw TypeError("Stringified UUID is invalid");
  return n;
}
function Ce(t, e, n) {
  t = t || {};
  var r = t.random || (t.rng || Ks)();
  return r[6] = r[6] & 15 | 64, r[8] = r[8] & 63 | 128, Zs(r);
}
var Vt = { exports: {} }, Bt = { exports: {} }, Ht = { exports: {} }, X = {};
/** @license React v16.13.1
 * react-is.production.min.js
 *
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
var Pn;
function tu() {
  if (Pn) return X;
  Pn = 1;
  var t = typeof Symbol == "function" && Symbol.for, e = t ? Symbol.for("react.element") : 60103, n = t ? Symbol.for("react.portal") : 60106, r = t ? Symbol.for("react.fragment") : 60107, i = t ? Symbol.for("react.strict_mode") : 60108, o = t ? Symbol.for("react.profiler") : 60114, a = t ? Symbol.for("react.provider") : 60109, s = t ? Symbol.for("react.context") : 60110, l = t ? Symbol.for("react.async_mode") : 60111, u = t ? Symbol.for("react.concurrent_mode") : 60111, c = t ? Symbol.for("react.forward_ref") : 60112, m = t ? Symbol.for("react.suspense") : 60113, h = t ? Symbol.for("react.suspense_list") : 60120, g = t ? Symbol.for("react.memo") : 60115, x = t ? Symbol.for("react.lazy") : 60116, S = t ? Symbol.for("react.block") : 60121, I = t ? Symbol.for("react.fundamental") : 60117, T = t ? Symbol.for("react.responder") : 60118, L = t ? Symbol.for("react.scope") : 60119;
  function R(d) {
    if (typeof d == "object" && d !== null) {
      var D = d.$$typeof;
      switch (D) {
        case e:
          switch (d = d.type, d) {
            case l:
            case u:
            case r:
            case o:
            case i:
            case m:
              return d;
            default:
              switch (d = d && d.$$typeof, d) {
                case s:
                case c:
                case x:
                case g:
                case a:
                  return d;
                default:
                  return D;
              }
          }
        case n:
          return D;
      }
    }
  }
  function _(d) {
    return R(d) === u;
  }
  return X.AsyncMode = l, X.ConcurrentMode = u, X.ContextConsumer = s, X.ContextProvider = a, X.Element = e, X.ForwardRef = c, X.Fragment = r, X.Lazy = x, X.Memo = g, X.Portal = n, X.Profiler = o, X.StrictMode = i, X.Suspense = m, X.isAsyncMode = function(d) {
    return _(d) || R(d) === l;
  }, X.isConcurrentMode = _, X.isContextConsumer = function(d) {
    return R(d) === s;
  }, X.isContextProvider = function(d) {
    return R(d) === a;
  }, X.isElement = function(d) {
    return typeof d == "object" && d !== null && d.$$typeof === e;
  }, X.isForwardRef = function(d) {
    return R(d) === c;
  }, X.isFragment = function(d) {
    return R(d) === r;
  }, X.isLazy = function(d) {
    return R(d) === x;
  }, X.isMemo = function(d) {
    return R(d) === g;
  }, X.isPortal = function(d) {
    return R(d) === n;
  }, X.isProfiler = function(d) {
    return R(d) === o;
  }, X.isStrictMode = function(d) {
    return R(d) === i;
  }, X.isSuspense = function(d) {
    return R(d) === m;
  }, X.isValidElementType = function(d) {
    return typeof d == "string" || typeof d == "function" || d === r || d === u || d === o || d === i || d === m || d === h || typeof d == "object" && d !== null && (d.$$typeof === x || d.$$typeof === g || d.$$typeof === a || d.$$typeof === s || d.$$typeof === c || d.$$typeof === I || d.$$typeof === T || d.$$typeof === L || d.$$typeof === S);
  }, X.typeOf = R, X;
}
var K = {};
/** @license React v16.13.1
 * react-is.development.js
 *
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
var kn;
function eu() {
  return kn || (kn = 1, process.env.NODE_ENV !== "production" && function() {
    var t = typeof Symbol == "function" && Symbol.for, e = t ? Symbol.for("react.element") : 60103, n = t ? Symbol.for("react.portal") : 60106, r = t ? Symbol.for("react.fragment") : 60107, i = t ? Symbol.for("react.strict_mode") : 60108, o = t ? Symbol.for("react.profiler") : 60114, a = t ? Symbol.for("react.provider") : 60109, s = t ? Symbol.for("react.context") : 60110, l = t ? Symbol.for("react.async_mode") : 60111, u = t ? Symbol.for("react.concurrent_mode") : 60111, c = t ? Symbol.for("react.forward_ref") : 60112, m = t ? Symbol.for("react.suspense") : 60113, h = t ? Symbol.for("react.suspense_list") : 60120, g = t ? Symbol.for("react.memo") : 60115, x = t ? Symbol.for("react.lazy") : 60116, S = t ? Symbol.for("react.block") : 60121, I = t ? Symbol.for("react.fundamental") : 60117, T = t ? Symbol.for("react.responder") : 60118, L = t ? Symbol.for("react.scope") : 60119;
    function R(M) {
      return typeof M == "string" || typeof M == "function" || // Note: its typeof might be other than 'symbol' or 'number' if it's a polyfill.
      M === r || M === u || M === o || M === i || M === m || M === h || typeof M == "object" && M !== null && (M.$$typeof === x || M.$$typeof === g || M.$$typeof === a || M.$$typeof === s || M.$$typeof === c || M.$$typeof === I || M.$$typeof === T || M.$$typeof === L || M.$$typeof === S);
    }
    function _(M) {
      if (typeof M == "object" && M !== null) {
        var st = M.$$typeof;
        switch (st) {
          case e:
            var qt = M.type;
            switch (qt) {
              case l:
              case u:
              case r:
              case o:
              case i:
              case m:
                return qt;
              default:
                var un = qt && qt.$$typeof;
                switch (un) {
                  case s:
                  case c:
                  case x:
                  case g:
                  case a:
                    return un;
                  default:
                    return st;
                }
            }
          case n:
            return st;
        }
      }
    }
    var d = l, D = u, $ = s, w = a, E = e, b = c, A = r, Y = x, H = g, p = n, N = o, v = i, C = m, P = !1;
    function j(M) {
      return P || (P = !0, console.warn("The ReactIs.isAsyncMode() alias has been deprecated, and will be removed in React 17+. Update your code to use ReactIs.isConcurrentMode() instead. It has the exact same API.")), f(M) || _(M) === l;
    }
    function f(M) {
      return _(M) === u;
    }
    function y(M) {
      return _(M) === s;
    }
    function O(M) {
      return _(M) === a;
    }
    function z(M) {
      return typeof M == "object" && M !== null && M.$$typeof === e;
    }
    function k(M) {
      return _(M) === c;
    }
    function F(M) {
      return _(M) === r;
    }
    function U(M) {
      return _(M) === x;
    }
    function W(M) {
      return _(M) === g;
    }
    function q(M) {
      return _(M) === n;
    }
    function G(M) {
      return _(M) === o;
    }
    function B(M) {
      return _(M) === i;
    }
    function Z(M) {
      return _(M) === m;
    }
    K.AsyncMode = d, K.ConcurrentMode = D, K.ContextConsumer = $, K.ContextProvider = w, K.Element = E, K.ForwardRef = b, K.Fragment = A, K.Lazy = Y, K.Memo = H, K.Portal = p, K.Profiler = N, K.StrictMode = v, K.Suspense = C, K.isAsyncMode = j, K.isConcurrentMode = f, K.isContextConsumer = y, K.isContextProvider = O, K.isElement = z, K.isForwardRef = k, K.isFragment = F, K.isLazy = U, K.isMemo = W, K.isPortal = q, K.isProfiler = G, K.isStrictMode = B, K.isSuspense = Z, K.isValidElementType = R, K.typeOf = _;
  }()), K;
}
var Dn;
function Ar() {
  return Dn || (Dn = 1, process.env.NODE_ENV === "production" ? Ht.exports = tu() : Ht.exports = eu()), Ht.exports;
}
/*
object-assign
(c) Sindre Sorhus
@license MIT
*/
var Re, $n;
function nu() {
  if ($n) return Re;
  $n = 1;
  var t = Object.getOwnPropertySymbols, e = Object.prototype.hasOwnProperty, n = Object.prototype.propertyIsEnumerable;
  function r(o) {
    if (o == null)
      throw new TypeError("Object.assign cannot be called with null or undefined");
    return Object(o);
  }
  function i() {
    try {
      if (!Object.assign)
        return !1;
      var o = new String("abc");
      if (o[5] = "de", Object.getOwnPropertyNames(o)[0] === "5")
        return !1;
      for (var a = {}, s = 0; s < 10; s++)
        a["_" + String.fromCharCode(s)] = s;
      var l = Object.getOwnPropertyNames(a).map(function(c) {
        return a[c];
      });
      if (l.join("") !== "0123456789")
        return !1;
      var u = {};
      return "abcdefghijklmnopqrst".split("").forEach(function(c) {
        u[c] = c;
      }), Object.keys(Object.assign({}, u)).join("") === "abcdefghijklmnopqrst";
    } catch {
      return !1;
    }
  }
  return Re = i() ? Object.assign : function(o, a) {
    for (var s, l = r(o), u, c = 1; c < arguments.length; c++) {
      s = Object(arguments[c]);
      for (var m in s)
        e.call(s, m) && (l[m] = s[m]);
      if (t) {
        u = t(s);
        for (var h = 0; h < u.length; h++)
          n.call(s, u[h]) && (l[u[h]] = s[u[h]]);
      }
    }
    return l;
  }, Re;
}
var Ne, zn;
function an() {
  if (zn) return Ne;
  zn = 1;
  var t = "SECRET_DO_NOT_PASS_THIS_OR_YOU_WILL_BE_FIRED";
  return Ne = t, Ne;
}
var Ae, In;
function Mr() {
  return In || (In = 1, Ae = Function.call.bind(Object.prototype.hasOwnProperty)), Ae;
}
var Me, Ln;
function ru() {
  if (Ln) return Me;
  Ln = 1;
  var t = function() {
  };
  if (process.env.NODE_ENV !== "production") {
    var e = /* @__PURE__ */ an(), n = {}, r = /* @__PURE__ */ Mr();
    t = function(o) {
      var a = "Warning: " + o;
      typeof console < "u" && console.error(a);
      try {
        throw new Error(a);
      } catch {
      }
    };
  }
  function i(o, a, s, l, u) {
    if (process.env.NODE_ENV !== "production") {
      for (var c in o)
        if (r(o, c)) {
          var m;
          try {
            if (typeof o[c] != "function") {
              var h = Error(
                (l || "React class") + ": " + s + " type `" + c + "` is invalid; it must be a function, usually from the `prop-types` package, but received `" + typeof o[c] + "`.This often happens because of typos such as `PropTypes.function` instead of `PropTypes.func`."
              );
              throw h.name = "Invariant Violation", h;
            }
            m = o[c](a, c, l, s, null, e);
          } catch (x) {
            m = x;
          }
          if (m && !(m instanceof Error) && t(
            (l || "React class") + ": type specification of " + s + " `" + c + "` is invalid; the type checker function must return `null` or an `Error` but returned a " + typeof m + ". You may have forgotten to pass an argument to the type checker creator (arrayOf, instanceOf, objectOf, oneOf, oneOfType, and shape all require an argument)."
          ), m instanceof Error && !(m.message in n)) {
            n[m.message] = !0;
            var g = u ? u() : "";
            t(
              "Failed " + s + " type: " + m.message + (g ?? "")
            );
          }
        }
    }
  }
  return i.resetWarningCache = function() {
    process.env.NODE_ENV !== "production" && (n = {});
  }, Me = i, Me;
}
var Pe, qn;
function iu() {
  if (qn) return Pe;
  qn = 1;
  var t = Ar(), e = nu(), n = /* @__PURE__ */ an(), r = /* @__PURE__ */ Mr(), i = /* @__PURE__ */ ru(), o = function() {
  };
  process.env.NODE_ENV !== "production" && (o = function(s) {
    var l = "Warning: " + s;
    typeof console < "u" && console.error(l);
    try {
      throw new Error(l);
    } catch {
    }
  });
  function a() {
    return null;
  }
  return Pe = function(s, l) {
    var u = typeof Symbol == "function" && Symbol.iterator, c = "@@iterator";
    function m(f) {
      var y = f && (u && f[u] || f[c]);
      if (typeof y == "function")
        return y;
    }
    var h = "<<anonymous>>", g = {
      array: T("array"),
      bigint: T("bigint"),
      bool: T("boolean"),
      func: T("function"),
      number: T("number"),
      object: T("object"),
      string: T("string"),
      symbol: T("symbol"),
      any: L(),
      arrayOf: R,
      element: _(),
      elementType: d(),
      instanceOf: D,
      node: b(),
      objectOf: w,
      oneOf: $,
      oneOfType: E,
      shape: Y,
      exact: H
    };
    function x(f, y) {
      return f === y ? f !== 0 || 1 / f === 1 / y : f !== f && y !== y;
    }
    function S(f, y) {
      this.message = f, this.data = y && typeof y == "object" ? y : {}, this.stack = "";
    }
    S.prototype = Error.prototype;
    function I(f) {
      if (process.env.NODE_ENV !== "production")
        var y = {}, O = 0;
      function z(F, U, W, q, G, B, Z) {
        if (q = q || h, B = B || W, Z !== n) {
          if (l) {
            var M = new Error(
              "Calling PropTypes validators directly is not supported by the `prop-types` package. Use `PropTypes.checkPropTypes()` to call them. Read more at http://fb.me/use-check-prop-types"
            );
            throw M.name = "Invariant Violation", M;
          } else if (process.env.NODE_ENV !== "production" && typeof console < "u") {
            var st = q + ":" + W;
            !y[st] && // Avoid spamming the console because they are often not actionable except for lib authors
            O < 3 && (o(
              "You are manually calling a React.PropTypes validation function for the `" + B + "` prop on `" + q + "`. This is deprecated and will throw in the standalone `prop-types` package. You may be seeing this warning due to a third-party PropTypes library. See https://fb.me/react-warning-dont-call-proptypes for details."
            ), y[st] = !0, O++);
          }
        }
        return U[W] == null ? F ? U[W] === null ? new S("The " + G + " `" + B + "` is marked as required " + ("in `" + q + "`, but its value is `null`.")) : new S("The " + G + " `" + B + "` is marked as required in " + ("`" + q + "`, but its value is `undefined`.")) : null : f(U, W, q, G, B);
      }
      var k = z.bind(null, !1);
      return k.isRequired = z.bind(null, !0), k;
    }
    function T(f) {
      function y(O, z, k, F, U, W) {
        var q = O[z], G = v(q);
        if (G !== f) {
          var B = C(q);
          return new S(
            "Invalid " + F + " `" + U + "` of type " + ("`" + B + "` supplied to `" + k + "`, expected ") + ("`" + f + "`."),
            { expectedType: f }
          );
        }
        return null;
      }
      return I(y);
    }
    function L() {
      return I(a);
    }
    function R(f) {
      function y(O, z, k, F, U) {
        if (typeof f != "function")
          return new S("Property `" + U + "` of component `" + k + "` has invalid PropType notation inside arrayOf.");
        var W = O[z];
        if (!Array.isArray(W)) {
          var q = v(W);
          return new S("Invalid " + F + " `" + U + "` of type " + ("`" + q + "` supplied to `" + k + "`, expected an array."));
        }
        for (var G = 0; G < W.length; G++) {
          var B = f(W, G, k, F, U + "[" + G + "]", n);
          if (B instanceof Error)
            return B;
        }
        return null;
      }
      return I(y);
    }
    function _() {
      function f(y, O, z, k, F) {
        var U = y[O];
        if (!s(U)) {
          var W = v(U);
          return new S("Invalid " + k + " `" + F + "` of type " + ("`" + W + "` supplied to `" + z + "`, expected a single ReactElement."));
        }
        return null;
      }
      return I(f);
    }
    function d() {
      function f(y, O, z, k, F) {
        var U = y[O];
        if (!t.isValidElementType(U)) {
          var W = v(U);
          return new S("Invalid " + k + " `" + F + "` of type " + ("`" + W + "` supplied to `" + z + "`, expected a single ReactElement type."));
        }
        return null;
      }
      return I(f);
    }
    function D(f) {
      function y(O, z, k, F, U) {
        if (!(O[z] instanceof f)) {
          var W = f.name || h, q = j(O[z]);
          return new S("Invalid " + F + " `" + U + "` of type " + ("`" + q + "` supplied to `" + k + "`, expected ") + ("instance of `" + W + "`."));
        }
        return null;
      }
      return I(y);
    }
    function $(f) {
      if (!Array.isArray(f))
        return process.env.NODE_ENV !== "production" && (arguments.length > 1 ? o(
          "Invalid arguments supplied to oneOf, expected an array, got " + arguments.length + " arguments. A common mistake is to write oneOf(x, y, z) instead of oneOf([x, y, z])."
        ) : o("Invalid argument supplied to oneOf, expected an array.")), a;
      function y(O, z, k, F, U) {
        for (var W = O[z], q = 0; q < f.length; q++)
          if (x(W, f[q]))
            return null;
        var G = JSON.stringify(f, function(Z, M) {
          var st = C(M);
          return st === "symbol" ? String(M) : M;
        });
        return new S("Invalid " + F + " `" + U + "` of value `" + String(W) + "` " + ("supplied to `" + k + "`, expected one of " + G + "."));
      }
      return I(y);
    }
    function w(f) {
      function y(O, z, k, F, U) {
        if (typeof f != "function")
          return new S("Property `" + U + "` of component `" + k + "` has invalid PropType notation inside objectOf.");
        var W = O[z], q = v(W);
        if (q !== "object")
          return new S("Invalid " + F + " `" + U + "` of type " + ("`" + q + "` supplied to `" + k + "`, expected an object."));
        for (var G in W)
          if (r(W, G)) {
            var B = f(W, G, k, F, U + "." + G, n);
            if (B instanceof Error)
              return B;
          }
        return null;
      }
      return I(y);
    }
    function E(f) {
      if (!Array.isArray(f))
        return process.env.NODE_ENV !== "production" && o("Invalid argument supplied to oneOfType, expected an instance of array."), a;
      for (var y = 0; y < f.length; y++) {
        var O = f[y];
        if (typeof O != "function")
          return o(
            "Invalid argument supplied to oneOfType. Expected an array of check functions, but received " + P(O) + " at index " + y + "."
          ), a;
      }
      function z(k, F, U, W, q) {
        for (var G = [], B = 0; B < f.length; B++) {
          var Z = f[B], M = Z(k, F, U, W, q, n);
          if (M == null)
            return null;
          M.data && r(M.data, "expectedType") && G.push(M.data.expectedType);
        }
        var st = G.length > 0 ? ", expected one of type [" + G.join(", ") + "]" : "";
        return new S("Invalid " + W + " `" + q + "` supplied to " + ("`" + U + "`" + st + "."));
      }
      return I(z);
    }
    function b() {
      function f(y, O, z, k, F) {
        return p(y[O]) ? null : new S("Invalid " + k + " `" + F + "` supplied to " + ("`" + z + "`, expected a ReactNode."));
      }
      return I(f);
    }
    function A(f, y, O, z, k) {
      return new S(
        (f || "React class") + ": " + y + " type `" + O + "." + z + "` is invalid; it must be a function, usually from the `prop-types` package, but received `" + k + "`."
      );
    }
    function Y(f) {
      function y(O, z, k, F, U) {
        var W = O[z], q = v(W);
        if (q !== "object")
          return new S("Invalid " + F + " `" + U + "` of type `" + q + "` " + ("supplied to `" + k + "`, expected `object`."));
        for (var G in f) {
          var B = f[G];
          if (typeof B != "function")
            return A(k, F, U, G, C(B));
          var Z = B(W, G, k, F, U + "." + G, n);
          if (Z)
            return Z;
        }
        return null;
      }
      return I(y);
    }
    function H(f) {
      function y(O, z, k, F, U) {
        var W = O[z], q = v(W);
        if (q !== "object")
          return new S("Invalid " + F + " `" + U + "` of type `" + q + "` " + ("supplied to `" + k + "`, expected `object`."));
        var G = e({}, O[z], f);
        for (var B in G) {
          var Z = f[B];
          if (r(f, B) && typeof Z != "function")
            return A(k, F, U, B, C(Z));
          if (!Z)
            return new S(
              "Invalid " + F + " `" + U + "` key `" + B + "` supplied to `" + k + "`.\nBad object: " + JSON.stringify(O[z], null, "  ") + `
Valid keys: ` + JSON.stringify(Object.keys(f), null, "  ")
            );
          var M = Z(W, B, k, F, U + "." + B, n);
          if (M)
            return M;
        }
        return null;
      }
      return I(y);
    }
    function p(f) {
      switch (typeof f) {
        case "number":
        case "string":
        case "undefined":
          return !0;
        case "boolean":
          return !f;
        case "object":
          if (Array.isArray(f))
            return f.every(p);
          if (f === null || s(f))
            return !0;
          var y = m(f);
          if (y) {
            var O = y.call(f), z;
            if (y !== f.entries) {
              for (; !(z = O.next()).done; )
                if (!p(z.value))
                  return !1;
            } else
              for (; !(z = O.next()).done; ) {
                var k = z.value;
                if (k && !p(k[1]))
                  return !1;
              }
          } else
            return !1;
          return !0;
        default:
          return !1;
      }
    }
    function N(f, y) {
      return f === "symbol" ? !0 : y ? y["@@toStringTag"] === "Symbol" || typeof Symbol == "function" && y instanceof Symbol : !1;
    }
    function v(f) {
      var y = typeof f;
      return Array.isArray(f) ? "array" : f instanceof RegExp ? "object" : N(y, f) ? "symbol" : y;
    }
    function C(f) {
      if (typeof f > "u" || f === null)
        return "" + f;
      var y = v(f);
      if (y === "object") {
        if (f instanceof Date)
          return "date";
        if (f instanceof RegExp)
          return "regexp";
      }
      return y;
    }
    function P(f) {
      var y = C(f);
      switch (y) {
        case "array":
        case "object":
          return "an " + y;
        case "boolean":
        case "date":
        case "regexp":
          return "a " + y;
        default:
          return y;
      }
    }
    function j(f) {
      return !f.constructor || !f.constructor.name ? h : f.constructor.name;
    }
    return g.checkPropTypes = i, g.resetWarningCache = i.resetWarningCache, g.PropTypes = g, g;
  }, Pe;
}
var ke, Fn;
function ou() {
  if (Fn) return ke;
  Fn = 1;
  var t = /* @__PURE__ */ an();
  function e() {
  }
  function n() {
  }
  return n.resetWarningCache = e, ke = function() {
    function r(a, s, l, u, c, m) {
      if (m !== t) {
        var h = new Error(
          "Calling PropTypes validators directly is not supported by the `prop-types` package. Use PropTypes.checkPropTypes() to call them. Read more at http://fb.me/use-check-prop-types"
        );
        throw h.name = "Invariant Violation", h;
      }
    }
    r.isRequired = r;
    function i() {
      return r;
    }
    var o = {
      array: r,
      bigint: r,
      bool: r,
      func: r,
      number: r,
      object: r,
      string: r,
      symbol: r,
      any: r,
      arrayOf: i,
      element: r,
      elementType: r,
      instanceOf: i,
      node: r,
      objectOf: i,
      oneOf: i,
      oneOfType: i,
      shape: i,
      exact: i,
      checkPropTypes: n,
      resetWarningCache: e
    };
    return o.PropTypes = o, o;
  }, ke;
}
var jn;
function ye() {
  if (jn) return Bt.exports;
  if (jn = 1, process.env.NODE_ENV !== "production") {
    var t = Ar(), e = !0;
    Bt.exports = /* @__PURE__ */ iu()(t.isElement, e);
  } else
    Bt.exports = /* @__PURE__ */ ou()();
  return Bt.exports;
}
var Xt = { exports: {} }, De, Wn;
function au() {
  return Wn || (Wn = 1, De = function() {
    for (var e = arguments.length, n = [], r = 0; r < e; r++)
      n[r] = arguments[r];
    if (n = n.filter(function(i) {
      return i != null;
    }), n.length !== 0)
      return n.length === 1 ? n[0] : n.reduce(function(i, o) {
        return function() {
          i.apply(this, arguments), o.apply(this, arguments);
        };
      });
  }), De;
}
var $e, Un;
function su() {
  if (Un) return $e;
  Un = 1;
  var t = function() {
  };
  return process.env.NODE_ENV !== "production" && (t = function(e, n, r) {
    var i = arguments.length;
    r = new Array(i > 2 ? i - 2 : 0);
    for (var o = 2; o < i; o++)
      r[o - 2] = arguments[o];
    if (n === void 0)
      throw new Error(
        "`warning(condition, format, ...args)` requires a warning message argument"
      );
    if (n.length < 10 || /^[s\W]*$/.test(n))
      throw new Error(
        "The warning format should be able to uniquely identify this warning. Please, use a more descriptive format than: " + n
      );
    if (!e) {
      var a = 0, s = "Warning: " + n.replace(/%s/g, function() {
        return r[a++];
      });
      typeof console < "u" && console.error(s);
      try {
        throw new Error(s);
      } catch {
      }
    }
  }), $e = t, $e;
}
function Pr() {
  var t = this.constructor.getDerivedStateFromProps(this.props, this.state);
  t != null && this.setState(t);
}
function kr(t) {
  function e(n) {
    var r = this.constructor.getDerivedStateFromProps(t, n);
    return r ?? null;
  }
  this.setState(e.bind(this));
}
function Dr(t, e) {
  try {
    var n = this.props, r = this.state;
    this.props = t, this.state = e, this.__reactInternalSnapshotFlag = !0, this.__reactInternalSnapshot = this.getSnapshotBeforeUpdate(
      n,
      r
    );
  } finally {
    this.props = n, this.state = r;
  }
}
Pr.__suppressDeprecationWarning = !0;
kr.__suppressDeprecationWarning = !0;
Dr.__suppressDeprecationWarning = !0;
function uu(t) {
  var e = t.prototype;
  if (!e || !e.isReactComponent)
    throw new Error("Can only polyfill class components");
  if (typeof t.getDerivedStateFromProps != "function" && typeof e.getSnapshotBeforeUpdate != "function")
    return t;
  var n = null, r = null, i = null;
  if (typeof e.componentWillMount == "function" ? n = "componentWillMount" : typeof e.UNSAFE_componentWillMount == "function" && (n = "UNSAFE_componentWillMount"), typeof e.componentWillReceiveProps == "function" ? r = "componentWillReceiveProps" : typeof e.UNSAFE_componentWillReceiveProps == "function" && (r = "UNSAFE_componentWillReceiveProps"), typeof e.componentWillUpdate == "function" ? i = "componentWillUpdate" : typeof e.UNSAFE_componentWillUpdate == "function" && (i = "UNSAFE_componentWillUpdate"), n !== null || r !== null || i !== null) {
    var o = t.displayName || t.name, a = typeof t.getDerivedStateFromProps == "function" ? "getDerivedStateFromProps()" : "getSnapshotBeforeUpdate()";
    throw Error(
      `Unsafe legacy lifecycles will not be called for components using new component APIs.

` + o + " uses " + a + " but also contains the following legacy lifecycles:" + (n !== null ? `
  ` + n : "") + (r !== null ? `
  ` + r : "") + (i !== null ? `
  ` + i : "") + `

The above lifecycles should be removed. Learn more about this warning here:
https://fb.me/react-async-component-lifecycle-hooks`
    );
  }
  if (typeof t.getDerivedStateFromProps == "function" && (e.componentWillMount = Pr, e.componentWillReceiveProps = kr), typeof e.getSnapshotBeforeUpdate == "function") {
    if (typeof e.componentDidUpdate != "function")
      throw new Error(
        "Cannot polyfill getSnapshotBeforeUpdate() for components that do not define componentDidUpdate() on the prototype"
      );
    e.componentWillUpdate = Dr;
    var s = e.componentDidUpdate;
    e.componentDidUpdate = function(u, c, m) {
      var h = this.__reactInternalSnapshotFlag ? this.__reactInternalSnapshot : m;
      s.call(this, u, c, h);
    };
  }
  return t;
}
const lu = /* @__PURE__ */ Object.freeze(/* @__PURE__ */ Object.defineProperty({
  __proto__: null,
  polyfill: uu
}, Symbol.toStringTag, { value: "Module" })), cu = /* @__PURE__ */ Vr(lu);
var Rt = {}, Yn;
function fu() {
  if (Yn) return Rt;
  Yn = 1, Rt.__esModule = !0, Rt.getChildMapping = e, Rt.mergeChildMappings = n;
  var t = Q;
  function e(r) {
    if (!r)
      return r;
    var i = {};
    return t.Children.map(r, function(o) {
      return o;
    }).forEach(function(o) {
      i[o.key] = o;
    }), i;
  }
  function n(r, i) {
    r = r || {}, i = i || {};
    function o(g) {
      return i.hasOwnProperty(g) ? i[g] : r[g];
    }
    var a = {}, s = [];
    for (var l in r)
      i.hasOwnProperty(l) ? s.length && (a[l] = s, s = []) : s.push(l);
    var u = void 0, c = {};
    for (var m in i) {
      if (a.hasOwnProperty(m))
        for (u = 0; u < a[m].length; u++) {
          var h = a[m][u];
          c[a[m][u]] = o(h);
        }
      c[m] = o(m);
    }
    for (u = 0; u < s.length; u++)
      c[s[u]] = o(s[u]);
    return c;
  }
  return Rt;
}
var Gn;
function $r() {
  return Gn || (Gn = 1, function(t, e) {
    e.__esModule = !0;
    var n = Object.assign || function(_) {
      for (var d = 1; d < arguments.length; d++) {
        var D = arguments[d];
        for (var $ in D)
          Object.prototype.hasOwnProperty.call(D, $) && (_[$] = D[$]);
      }
      return _;
    }, r = au(), i = g(r), o = Q, a = g(o), s = /* @__PURE__ */ ye(), l = g(s), u = su(), c = g(u), m = cu, h = fu();
    function g(_) {
      return _ && _.__esModule ? _ : { default: _ };
    }
    function x(_, d) {
      if (!(_ instanceof d))
        throw new TypeError("Cannot call a class as a function");
    }
    function S(_, d) {
      if (!_)
        throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
      return d && (typeof d == "object" || typeof d == "function") ? d : _;
    }
    function I(_, d) {
      if (typeof d != "function" && d !== null)
        throw new TypeError("Super expression must either be null or a function, not " + typeof d);
      _.prototype = Object.create(d && d.prototype, { constructor: { value: _, enumerable: !1, writable: !0, configurable: !0 } }), d && (Object.setPrototypeOf ? Object.setPrototypeOf(_, d) : _.__proto__ = d);
    }
    var T = {
      component: l.default.any,
      childFactory: l.default.func,
      children: l.default.node
    }, L = {
      component: "span",
      childFactory: function(d) {
        return d;
      }
    }, R = function(_) {
      I(d, _);
      function d(D, $) {
        x(this, d);
        var w = S(this, _.call(this, D, $));
        return w.performAppear = function(E, b) {
          w.currentlyTransitioningKeys[E] = !0, b.componentWillAppear ? b.componentWillAppear(w._handleDoneAppearing.bind(w, E, b)) : w._handleDoneAppearing(E, b);
        }, w._handleDoneAppearing = function(E, b) {
          b && b.componentDidAppear && b.componentDidAppear(), delete w.currentlyTransitioningKeys[E];
          var A = (0, h.getChildMapping)(w.props.children);
          (!A || !A.hasOwnProperty(E)) && w.performLeave(E, b);
        }, w.performEnter = function(E, b) {
          w.currentlyTransitioningKeys[E] = !0, b.componentWillEnter ? b.componentWillEnter(w._handleDoneEntering.bind(w, E, b)) : w._handleDoneEntering(E, b);
        }, w._handleDoneEntering = function(E, b) {
          b && b.componentDidEnter && b.componentDidEnter(), delete w.currentlyTransitioningKeys[E];
          var A = (0, h.getChildMapping)(w.props.children);
          (!A || !A.hasOwnProperty(E)) && w.performLeave(E, b);
        }, w.performLeave = function(E, b) {
          w.currentlyTransitioningKeys[E] = !0, b && b.componentWillLeave ? b.componentWillLeave(w._handleDoneLeaving.bind(w, E, b)) : w._handleDoneLeaving(E, b);
        }, w._handleDoneLeaving = function(E, b) {
          b && b.componentDidLeave && b.componentDidLeave(), delete w.currentlyTransitioningKeys[E];
          var A = (0, h.getChildMapping)(w.props.children);
          A && A.hasOwnProperty(E) ? w.keysToEnter.push(E) : w.setState(function(Y) {
            var H = n({}, Y.children);
            return delete H[E], { children: H };
          });
        }, w.childRefs = /* @__PURE__ */ Object.create(null), w.currentlyTransitioningKeys = {}, w.keysToEnter = [], w.keysToLeave = [], w.state = {
          children: (0, h.getChildMapping)(D.children)
        }, w;
      }
      return d.prototype.componentDidMount = function() {
        var $ = this.state.children;
        for (var w in $)
          $[w] && this.performAppear(w, this.childRefs[w]);
      }, d.getDerivedStateFromProps = function($, w) {
        var E = (0, h.getChildMapping)($.children), b = w.children;
        return {
          children: (0, h.mergeChildMappings)(b, E)
        };
      }, d.prototype.componentDidUpdate = function($, w) {
        var E = this, b = (0, h.getChildMapping)(this.props.children), A = w.children;
        for (var Y in b) {
          var H = A && A.hasOwnProperty(Y);
          b[Y] && !H && !this.currentlyTransitioningKeys[Y] && this.keysToEnter.push(Y);
        }
        for (var p in A) {
          var N = b && b.hasOwnProperty(p);
          A[p] && !N && !this.currentlyTransitioningKeys[p] && this.keysToLeave.push(p);
        }
        var v = this.keysToEnter;
        this.keysToEnter = [], v.forEach(function(P) {
          return E.performEnter(P, E.childRefs[P]);
        });
        var C = this.keysToLeave;
        this.keysToLeave = [], C.forEach(function(P) {
          return E.performLeave(P, E.childRefs[P]);
        });
      }, d.prototype.render = function() {
        var $ = this, w = [], E = function(H) {
          var p = $.state.children[H];
          if (p) {
            var N = typeof p.ref != "string", v = $.props.childFactory(p), C = function(j) {
              $.childRefs[H] = j;
            };
            process.env.NODE_ENV !== "production" && (0, c.default)(N, "string refs are not supported on children of TransitionGroup and will be ignored. Please use a callback ref instead: https://facebook.github.io/react/docs/refs-and-the-dom.html#the-ref-callback-attribute"), v === p && N && (C = (0, i.default)(p.ref, C)), w.push(a.default.cloneElement(v, {
              key: H,
              ref: C
            }));
          }
        };
        for (var b in this.state.children)
          E(b);
        var A = n({}, this.props);
        return delete A.transitionLeave, delete A.transitionName, delete A.transitionAppear, delete A.transitionEnter, delete A.childFactory, delete A.transitionLeaveTimeout, delete A.transitionEnterTimeout, delete A.transitionAppearTimeout, delete A.component, a.default.createElement(this.props.component, A, w);
      }, d;
    }(a.default.Component);
    R.displayName = "TransitionGroup", R.propTypes = process.env.NODE_ENV !== "production" ? T : {}, R.defaultProps = L, e.default = (0, m.polyfill)(R), t.exports = e.default;
  }(Xt, Xt.exports)), Xt.exports;
}
var Kt = { exports: {} }, Qt = { exports: {} }, ze = { exports: {} }, Vn;
function sn() {
  return Vn || (Vn = 1, function(t) {
    function e(n) {
      return n && n.__esModule ? n : {
        default: n
      };
    }
    t.exports = e, t.exports.__esModule = !0, t.exports.default = t.exports;
  }(ze)), ze.exports;
}
var Jt = { exports: {} }, Bn;
function hu() {
  return Bn || (Bn = 1, function(t, e) {
    e.__esModule = !0, e.default = n;
    function n(r, i) {
      return r.classList ? !!i && r.classList.contains(i) : (" " + (r.className.baseVal || r.className) + " ").indexOf(" " + i + " ") !== -1;
    }
    t.exports = e.default;
  }(Jt, Jt.exports)), Jt.exports;
}
var Hn;
function du() {
  return Hn || (Hn = 1, function(t, e) {
    var n = sn();
    e.__esModule = !0, e.default = i;
    var r = n(hu());
    function i(o, a) {
      o.classList ? o.classList.add(a) : (0, r.default)(o, a) || (typeof o.className == "string" ? o.className = o.className + " " + a : o.setAttribute("class", (o.className && o.className.baseVal || "") + " " + a));
    }
    t.exports = e.default;
  }(Qt, Qt.exports)), Qt.exports;
}
var Ie, Xn;
function pu() {
  if (Xn) return Ie;
  Xn = 1;
  function t(e, n) {
    return e.replace(new RegExp("(^|\\s)" + n + "(?:\\s|$)", "g"), "$1").replace(/\s+/g, " ").replace(/^\s*|\s*$/g, "");
  }
  return Ie = function(n, r) {
    n.classList ? n.classList.remove(r) : typeof n.className == "string" ? n.className = t(n.className, r) : n.setAttribute("class", t(n.className && n.className.baseVal || "", r));
  }, Ie;
}
var Zt = { exports: {} }, te = { exports: {} }, Kn;
function zr() {
  return Kn || (Kn = 1, function(t, e) {
    e.__esModule = !0, e.default = void 0;
    var n = !!(typeof window < "u" && window.document && window.document.createElement);
    e.default = n, t.exports = e.default;
  }(te, te.exports)), te.exports;
}
var Qn;
function mu() {
  return Qn || (Qn = 1, function(t, e) {
    var n = sn();
    e.__esModule = !0, e.default = void 0;
    var r = n(zr()), i = ["", "webkit", "moz", "o", "ms"], o = "clearTimeout", a = c, s, l = function(g, x) {
      return g + (g ? x[0].toUpperCase() + x.substr(1) : x) + "AnimationFrame";
    };
    r.default && i.some(function(h) {
      var g = l(h, "request");
      if (g in window)
        return o = l(h, "cancel"), a = function(S) {
          return window[g](S);
        };
    });
    var u = (/* @__PURE__ */ new Date()).getTime();
    function c(h) {
      var g = (/* @__PURE__ */ new Date()).getTime(), x = Math.max(0, 16 - (g - u)), S = setTimeout(h, x);
      return u = g, S;
    }
    s = function(g) {
      return a(g);
    }, s.cancel = function(h) {
      window[o] && typeof window[o] == "function" && window[o](h);
    };
    var m = s;
    e.default = m, t.exports = e.default;
  }(Zt, Zt.exports)), Zt.exports;
}
var V = {}, Jn;
function gu() {
  if (Jn) return V;
  Jn = 1;
  var t = sn();
  V.__esModule = !0, V.default = V.animationEnd = V.animationDelay = V.animationTiming = V.animationDuration = V.animationName = V.transitionEnd = V.transitionDuration = V.transitionDelay = V.transitionTiming = V.transitionProperty = V.transform = void 0;
  var e = t(zr()), n = "transform";
  V.transform = n;
  var r, i, o;
  V.animationEnd = o, V.transitionEnd = i;
  var a, s, l, u;
  V.transitionDelay = u, V.transitionTiming = l, V.transitionDuration = s, V.transitionProperty = a;
  var c, m, h, g;
  if (V.animationDelay = g, V.animationTiming = h, V.animationDuration = m, V.animationName = c, e.default) {
    var x = I();
    r = x.prefix, V.transitionEnd = i = x.transitionEnd, V.animationEnd = o = x.animationEnd, V.transform = n = r + "-" + n, V.transitionProperty = a = r + "-transition-property", V.transitionDuration = s = r + "-transition-duration", V.transitionDelay = u = r + "-transition-delay", V.transitionTiming = l = r + "-transition-timing-function", V.animationName = c = r + "-animation-name", V.animationDuration = m = r + "-animation-duration", V.animationTiming = h = r + "-animation-delay", V.animationDelay = g = r + "-animation-timing-function";
  }
  var S = {
    transform: n,
    end: i,
    property: a,
    timing: l,
    delay: u,
    duration: s
  };
  V.default = S;
  function I() {
    for (var T = document.createElement("div").style, L = {
      O: function(b) {
        return "o" + b.toLowerCase();
      },
      Moz: function(b) {
        return b.toLowerCase();
      },
      Webkit: function(b) {
        return "webkit" + b;
      },
      ms: function(b) {
        return "MS" + b;
      }
    }, R = Object.keys(L), _, d, D = "", $ = 0; $ < R.length; $++) {
      var w = R[$];
      if (w + "TransitionProperty" in T) {
        D = "-" + w.toLowerCase(), _ = L[w]("TransitionEnd"), d = L[w]("AnimationEnd");
        break;
      }
    }
    return !_ && "transitionProperty" in T && (_ = "transitionend"), !d && "animationName" in T && (d = "animationend"), T = null, {
      animationEnd: d,
      transitionEnd: _,
      prefix: D
    };
  }
  return V;
}
var wt = {}, Zn;
function Ir() {
  if (Zn) return wt;
  Zn = 1, wt.__esModule = !0, wt.nameShape = void 0, wt.transitionTimeout = i;
  var t = Q;
  r(t);
  var e = /* @__PURE__ */ ye(), n = r(e);
  function r(o) {
    return o && o.__esModule ? o : { default: o };
  }
  function i(o) {
    var a = "transition" + o + "Timeout", s = "transition" + o;
    return function(l) {
      if (l[s]) {
        if (l[a] == null)
          return new Error(a + " wasn't supplied to CSSTransitionGroup: this can cause unreliable animations and won't be supported in a future version of React. See https://fb.me/react-animation-transition-group-timeout for more information.");
        if (typeof l[a] != "number")
          return new Error(a + " must be a number (in milliseconds)");
      }
      return null;
    };
  }
  return wt.nameShape = n.default.oneOfType([n.default.string, n.default.shape({
    enter: n.default.string,
    leave: n.default.string,
    active: n.default.string
  }), n.default.shape({
    enter: n.default.string,
    enterActive: n.default.string,
    leave: n.default.string,
    leaveActive: n.default.string,
    appear: n.default.string,
    appearActive: n.default.string
  })]), wt;
}
var tr;
function yu() {
  return tr || (tr = 1, function(t, e) {
    e.__esModule = !0;
    var n = Object.assign || function(w) {
      for (var E = 1; E < arguments.length; E++) {
        var b = arguments[E];
        for (var A in b)
          Object.prototype.hasOwnProperty.call(b, A) && (w[A] = b[A]);
      }
      return w;
    }, r = du(), i = I(r), o = pu(), a = I(o), s = mu(), l = I(s), u = gu(), c = Q, m = I(c), h = /* @__PURE__ */ ye(), g = I(h), x = Yr, S = Ir();
    function I(w) {
      return w && w.__esModule ? w : { default: w };
    }
    function T(w, E) {
      if (!(w instanceof E))
        throw new TypeError("Cannot call a class as a function");
    }
    function L(w, E) {
      if (!w)
        throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
      return E && (typeof E == "object" || typeof E == "function") ? E : w;
    }
    function R(w, E) {
      if (typeof E != "function" && E !== null)
        throw new TypeError("Super expression must either be null or a function, not " + typeof E);
      w.prototype = Object.create(E && E.prototype, { constructor: { value: w, enumerable: !1, writable: !0, configurable: !0 } }), E && (Object.setPrototypeOf ? Object.setPrototypeOf(w, E) : w.__proto__ = E);
    }
    var _ = [];
    u.transitionEnd && _.push(u.transitionEnd), u.animationEnd && _.push(u.animationEnd);
    function d(w, E) {
      return _.length ? _.forEach(function(b) {
        return w.addEventListener(b, E, !1);
      }) : setTimeout(E, 0), function() {
        _.length && _.forEach(function(b) {
          return w.removeEventListener(b, E, !1);
        });
      };
    }
    var D = {
      children: g.default.node,
      name: S.nameShape.isRequired,
      // Once we require timeouts to be specified, we can remove the
      // boolean flags (appear etc.) and just accept a number
      // or a bool for the timeout flags (appearTimeout etc.)
      appear: g.default.bool,
      enter: g.default.bool,
      leave: g.default.bool,
      appearTimeout: g.default.number,
      enterTimeout: g.default.number,
      leaveTimeout: g.default.number
    }, $ = function(w) {
      R(E, w);
      function E(b, A) {
        T(this, E);
        var Y = L(this, w.call(this, b, A));
        return Y.componentWillAppear = function(H) {
          Y.props.appear ? Y.transition("appear", H, Y.props.appearTimeout) : H();
        }, Y.componentWillEnter = function(H) {
          Y.props.enter ? Y.transition("enter", H, Y.props.enterTimeout) : H();
        }, Y.componentWillLeave = function(H) {
          Y.props.leave ? Y.transition("leave", H, Y.props.leaveTimeout) : H();
        }, Y.classNameAndNodeQueue = [], Y.transitionTimeouts = [], Y;
      }
      return E.prototype.componentWillUnmount = function() {
        this.unmounted = !0, this.timeout && clearTimeout(this.timeout), this.transitionTimeouts.forEach(function(A) {
          clearTimeout(A);
        }), this.classNameAndNodeQueue.length = 0;
      }, E.prototype.transition = function(A, Y, H) {
        var p = (0, x.findDOMNode)(this);
        if (!p) {
          Y && Y();
          return;
        }
        var N = this.props.name[A] || this.props.name + "-" + A, v = this.props.name[A + "Active"] || N + "-active", C = null, P = void 0;
        (0, i.default)(p, N), this.queueClassAndNode(v, p);
        var j = function(y) {
          y && y.target !== p || (clearTimeout(C), P && P(), (0, a.default)(p, N), (0, a.default)(p, v), P && P(), Y && Y());
        };
        H ? (C = setTimeout(j, H), this.transitionTimeouts.push(C)) : u.transitionEnd && (P = d(p, j));
      }, E.prototype.queueClassAndNode = function(A, Y) {
        var H = this;
        this.classNameAndNodeQueue.push({
          className: A,
          node: Y
        }), this.rafHandle || (this.rafHandle = (0, l.default)(function() {
          return H.flushClassNameAndNodeQueue();
        }));
      }, E.prototype.flushClassNameAndNodeQueue = function() {
        this.unmounted || this.classNameAndNodeQueue.forEach(function(A) {
          A.node.scrollTop, (0, i.default)(A.node, A.className);
        }), this.classNameAndNodeQueue.length = 0, this.rafHandle = null;
      }, E.prototype.render = function() {
        var A = n({}, this.props);
        return delete A.name, delete A.appear, delete A.enter, delete A.leave, delete A.appearTimeout, delete A.enterTimeout, delete A.leaveTimeout, delete A.children, m.default.cloneElement(m.default.Children.only(this.props.children), A);
      }, E;
    }(m.default.Component);
    $.displayName = "CSSTransitionGroupChild", $.propTypes = process.env.NODE_ENV !== "production" ? D : {}, e.default = $, t.exports = e.default;
  }(Kt, Kt.exports)), Kt.exports;
}
var er;
function vu() {
  return er || (er = 1, function(t, e) {
    e.__esModule = !0;
    var n = Object.assign || function(R) {
      for (var _ = 1; _ < arguments.length; _++) {
        var d = arguments[_];
        for (var D in d)
          Object.prototype.hasOwnProperty.call(d, D) && (R[D] = d[D]);
      }
      return R;
    }, r = Q, i = h(r), o = /* @__PURE__ */ ye(), a = h(o), s = $r(), l = h(s), u = yu(), c = h(u), m = Ir();
    function h(R) {
      return R && R.__esModule ? R : { default: R };
    }
    function g(R, _) {
      if (!(R instanceof _))
        throw new TypeError("Cannot call a class as a function");
    }
    function x(R, _) {
      if (!R)
        throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
      return _ && (typeof _ == "object" || typeof _ == "function") ? _ : R;
    }
    function S(R, _) {
      if (typeof _ != "function" && _ !== null)
        throw new TypeError("Super expression must either be null or a function, not " + typeof _);
      R.prototype = Object.create(_ && _.prototype, { constructor: { value: R, enumerable: !1, writable: !0, configurable: !0 } }), _ && (Object.setPrototypeOf ? Object.setPrototypeOf(R, _) : R.__proto__ = _);
    }
    var I = {
      transitionName: m.nameShape.isRequired,
      transitionAppear: a.default.bool,
      transitionEnter: a.default.bool,
      transitionLeave: a.default.bool,
      transitionAppearTimeout: (0, m.transitionTimeout)("Appear"),
      transitionEnterTimeout: (0, m.transitionTimeout)("Enter"),
      transitionLeaveTimeout: (0, m.transitionTimeout)("Leave")
    }, T = {
      transitionAppear: !1,
      transitionEnter: !0,
      transitionLeave: !0
    }, L = function(R) {
      S(_, R);
      function _() {
        var d, D, $;
        g(this, _);
        for (var w = arguments.length, E = Array(w), b = 0; b < w; b++)
          E[b] = arguments[b];
        return $ = (d = (D = x(this, R.call.apply(R, [this].concat(E))), D), D._wrapChild = function(A) {
          return i.default.createElement(c.default, {
            name: D.props.transitionName,
            appear: D.props.transitionAppear,
            enter: D.props.transitionEnter,
            leave: D.props.transitionLeave,
            appearTimeout: D.props.transitionAppearTimeout,
            enterTimeout: D.props.transitionEnterTimeout,
            leaveTimeout: D.props.transitionLeaveTimeout
          }, A);
        }, d), x(D, $);
      }
      return _.prototype.render = function() {
        return i.default.createElement(l.default, n({}, this.props, { childFactory: this._wrapChild }));
      }, _;
    }(i.default.Component);
    L.displayName = "CSSTransitionGroup", L.propTypes = process.env.NODE_ENV !== "production" ? I : {}, L.defaultProps = T, e.default = L, t.exports = e.default;
  }(Vt, Vt.exports)), Vt.exports;
}
var Le, nr;
function _u() {
  if (nr) return Le;
  nr = 1;
  var t = vu(), e = i(t), n = $r(), r = i(n);
  function i(o) {
    return o && o.__esModule ? o : { default: o };
  }
  return Le = {
    TransitionGroup: r.default,
    CSSTransitionGroup: e.default
  }, Le;
}
var wu = _u();
const bu = (t) => t.enableLegacyTransitions ? Q.createElement(wu.TransitionGroup, { component: t.component, className: t.className, transform: t.transform }, t.children) : Q.createElement("g", { className: t.className, transform: t.transform }, t.children), xu = 15, rr = {
  title: {
    textAnchor: "start",
    x: 40
  },
  attribute: {
    x: 40,
    dy: "1.2em"
  }
}, Tu = ({ nodeDatum: t, toggleNode: e, onNodeClick: n, onNodeMouseOver: r, onNodeMouseOut: i }) => Q.createElement(
  Q.Fragment,
  null,
  Q.createElement("circle", { r: xu, onClick: (o) => {
    e(), n(o);
  }, onMouseOver: r, onMouseOut: i }),
  Q.createElement(
    "g",
    { className: "rd3t-label" },
    Q.createElement("text", Object.assign({ className: "rd3t-label__title" }, rr.title), t.name),
    Q.createElement("text", { className: "rd3t-label__attributes" }, t.attributes && Object.entries(t.attributes).map(([o, a], s) => Q.createElement(
      "tspan",
      Object.assign({ key: `${o}-${s}` }, rr.attribute),
      o,
      ": ",
      typeof a == "boolean" ? a.toString() : a
    )))
  )
);
class Eu extends Q.Component {
  constructor() {
    super(...arguments), this.nodeRef = null, this.state = {
      transform: this.setTransform(this.props.position, this.props.parent, this.props.orientation, !0),
      initialStyle: {
        opacity: 0
      },
      wasClicked: !1
    }, this.shouldNodeTransform = (e, n, r, i) => n.subscriptions !== e.subscriptions || n.position.x !== e.position.x || n.position.y !== e.position.y || n.orientation !== e.orientation || i.wasClicked !== r.wasClicked, this.renderNodeElement = () => {
      const { data: e, hierarchyPointNode: n, renderCustomNodeElement: r } = this.props, i = typeof r == "function" ? r : Tu, o = {
        hierarchyPointNode: n,
        nodeDatum: e,
        toggleNode: this.handleNodeToggle,
        onNodeClick: this.handleOnClick,
        onNodeMouseOver: this.handleOnMouseOver,
        onNodeMouseOut: this.handleOnMouseOut,
        addChildren: this.handleAddChildren
      };
      return i(o);
    }, this.handleNodeToggle = () => {
      this.setState({ wasClicked: !0 }), this.props.onNodeToggle(this.props.data.__rd3t.id);
    }, this.handleOnClick = (e) => {
      this.setState({ wasClicked: !0 }), this.props.onNodeClick(this.props.hierarchyPointNode, e);
    }, this.handleOnMouseOver = (e) => {
      this.props.onNodeMouseOver(this.props.hierarchyPointNode, e);
    }, this.handleOnMouseOut = (e) => {
      this.props.onNodeMouseOut(this.props.hierarchyPointNode, e);
    }, this.handleAddChildren = (e) => {
      this.props.handleAddChildrenToNode(this.props.data.__rd3t.id, e);
    };
  }
  componentDidMount() {
    this.commitTransform();
  }
  componentDidUpdate() {
    this.state.wasClicked && (this.props.centerNode(this.props.hierarchyPointNode), this.setState({ wasClicked: !1 })), this.commitTransform();
  }
  shouldComponentUpdate(e, n) {
    return this.shouldNodeTransform(this.props, e, this.state, n);
  }
  setTransform(e, n, r, i = !1) {
    if (i) {
      const o = n != null, a = o ? n.x : 0, s = o ? n.y : 0;
      return r === "horizontal" ? `translate(${s},${a})` : `translate(${a},${s})`;
    }
    return r === "horizontal" ? `translate(${e.y},${e.x})` : `translate(${e.x},${e.y})`;
  }
  applyTransform(e, n, r = 1, i = () => {
  }) {
    this.props.enableLegacyTransitions ? et(this.nodeRef).transition().duration(n).attr("transform", e).style("opacity", r).on("end", i) : (et(this.nodeRef).attr("transform", e).style("opacity", r), i());
  }
  commitTransform() {
    const { orientation: e, transitionDuration: n, position: r, parent: i } = this.props, o = this.setTransform(r, i, e);
    this.applyTransform(o, n);
  }
  componentWillLeave(e) {
    const { orientation: n, transitionDuration: r, position: i, parent: o } = this.props, a = this.setTransform(i, o, n, !0);
    this.applyTransform(a, r, 0, e);
  }
  render() {
    const { data: e, nodeClassName: n } = this.props;
    return Q.createElement("g", { id: e.__rd3t.id, ref: (r) => {
      this.nodeRef = r;
    }, style: this.state.initialStyle, className: [
      e.children && e.children.length > 0 ? "rd3t-node" : "rd3t-leaf-node",
      n
    ].join(" ").trim(), transform: this.state.transform }, this.renderNodeElement());
  }
}
var He = Math.PI, Xe = 2 * He, gt = 1e-6, Su = Xe - gt;
function Ke() {
  this._x0 = this._y0 = // start of current subpath
  this._x1 = this._y1 = null, this._ = "";
}
function Lr() {
  return new Ke();
}
Ke.prototype = Lr.prototype = {
  constructor: Ke,
  moveTo: function(t, e) {
    this._ += "M" + (this._x0 = this._x1 = +t) + "," + (this._y0 = this._y1 = +e);
  },
  closePath: function() {
    this._x1 !== null && (this._x1 = this._x0, this._y1 = this._y0, this._ += "Z");
  },
  lineTo: function(t, e) {
    this._ += "L" + (this._x1 = +t) + "," + (this._y1 = +e);
  },
  quadraticCurveTo: function(t, e, n, r) {
    this._ += "Q" + +t + "," + +e + "," + (this._x1 = +n) + "," + (this._y1 = +r);
  },
  bezierCurveTo: function(t, e, n, r, i, o) {
    this._ += "C" + +t + "," + +e + "," + +n + "," + +r + "," + (this._x1 = +i) + "," + (this._y1 = +o);
  },
  arcTo: function(t, e, n, r, i) {
    t = +t, e = +e, n = +n, r = +r, i = +i;
    var o = this._x1, a = this._y1, s = n - t, l = r - e, u = o - t, c = a - e, m = u * u + c * c;
    if (i < 0) throw new Error("negative radius: " + i);
    if (this._x1 === null)
      this._ += "M" + (this._x1 = t) + "," + (this._y1 = e);
    else if (m > gt) if (!(Math.abs(c * s - l * u) > gt) || !i)
      this._ += "L" + (this._x1 = t) + "," + (this._y1 = e);
    else {
      var h = n - o, g = r - a, x = s * s + l * l, S = h * h + g * g, I = Math.sqrt(x), T = Math.sqrt(m), L = i * Math.tan((He - Math.acos((x + m - S) / (2 * I * T))) / 2), R = L / T, _ = L / I;
      Math.abs(R - 1) > gt && (this._ += "L" + (t + R * u) + "," + (e + R * c)), this._ += "A" + i + "," + i + ",0,0," + +(c * h > u * g) + "," + (this._x1 = t + _ * s) + "," + (this._y1 = e + _ * l);
    }
  },
  arc: function(t, e, n, r, i, o) {
    t = +t, e = +e, n = +n, o = !!o;
    var a = n * Math.cos(r), s = n * Math.sin(r), l = t + a, u = e + s, c = 1 ^ o, m = o ? r - i : i - r;
    if (n < 0) throw new Error("negative radius: " + n);
    this._x1 === null ? this._ += "M" + l + "," + u : (Math.abs(this._x1 - l) > gt || Math.abs(this._y1 - u) > gt) && (this._ += "L" + l + "," + u), n && (m < 0 && (m = m % Xe + Xe), m > Su ? this._ += "A" + n + "," + n + ",0,1," + c + "," + (t - a) + "," + (e - s) + "A" + n + "," + n + ",0,1," + c + "," + (this._x1 = l) + "," + (this._y1 = u) : m > gt && (this._ += "A" + n + "," + n + ",0," + +(m >= He) + "," + c + "," + (this._x1 = t + n * Math.cos(i)) + "," + (this._y1 = e + n * Math.sin(i))));
  },
  rect: function(t, e, n, r) {
    this._ += "M" + (this._x0 = this._x1 = +t) + "," + (this._y0 = this._y1 = +e) + "h" + +n + "v" + +r + "h" + -n + "Z";
  },
  toString: function() {
    return this._;
  }
};
function ir(t) {
  return function() {
    return t;
  };
}
function Ou(t) {
  return t[0];
}
function Cu(t) {
  return t[1];
}
var Ru = Array.prototype.slice;
function Nu(t) {
  return t.source;
}
function Au(t) {
  return t.target;
}
function qr(t) {
  var e = Nu, n = Au, r = Ou, i = Cu, o = null;
  function a() {
    var s, l = Ru.call(arguments), u = e.apply(this, l), c = n.apply(this, l);
    if (o || (o = s = Lr()), t(o, +r.apply(this, (l[0] = u, l)), +i.apply(this, l), +r.apply(this, (l[0] = c, l)), +i.apply(this, l)), s) return o = null, s + "" || null;
  }
  return a.source = function(s) {
    return arguments.length ? (e = s, a) : e;
  }, a.target = function(s) {
    return arguments.length ? (n = s, a) : n;
  }, a.x = function(s) {
    return arguments.length ? (r = typeof s == "function" ? s : ir(+s), a) : r;
  }, a.y = function(s) {
    return arguments.length ? (i = typeof s == "function" ? s : ir(+s), a) : i;
  }, a.context = function(s) {
    return arguments.length ? (o = s ?? null, a) : o;
  }, a;
}
function Mu(t, e, n, r, i) {
  t.moveTo(e, n), t.bezierCurveTo(e = (e + r) / 2, n, e, i, r, i);
}
function Pu(t, e, n, r, i) {
  t.moveTo(e, n), t.bezierCurveTo(e, n = (n + i) / 2, r, n, r, i);
}
function ku() {
  return qr(Mu);
}
function Du() {
  return qr(Pu);
}
class $u extends Q.PureComponent {
  constructor() {
    super(...arguments), this.linkRef = null, this.state = {
      initialStyle: {
        opacity: 0
      }
    }, this.handleOnClick = (e) => {
      this.props.onClick(this.props.linkData.source, this.props.linkData.target, e);
    }, this.handleOnMouseOver = (e) => {
      this.props.onMouseOver(this.props.linkData.source, this.props.linkData.target, e);
    }, this.handleOnMouseOut = (e) => {
      this.props.onMouseOut(this.props.linkData.source, this.props.linkData.target, e);
    };
  }
  componentDidMount() {
    this.applyOpacity(1, this.props.transitionDuration);
  }
  componentWillLeave(e) {
    this.applyOpacity(0, this.props.transitionDuration, e);
  }
  applyOpacity(e, n, r = () => {
  }) {
    this.props.enableLegacyTransitions ? et(this.linkRef).transition().duration(n).style("opacity", e).on("end", r) : (et(this.linkRef).style("opacity", e), r());
  }
  drawStepPath(e, n) {
    const { source: r, target: i } = e, o = i.y - r.y;
    return n === "horizontal" ? `M${r.y},${r.x} H${r.y + o / 2} V${i.x} H${i.y}` : `M${r.x},${r.y} V${r.y + o / 2} H${i.x} V${i.y}`;
  }
  drawDiagonalPath(e, n) {
    const { source: r, target: i } = e;
    return n === "horizontal" ? ku()({
      source: [r.y, r.x],
      target: [i.y, i.x]
    }) : Du()({
      source: [r.x, r.y],
      target: [i.x, i.y]
    });
  }
  drawStraightPath(e, n) {
    const { source: r, target: i } = e;
    return n === "horizontal" ? `M${r.y},${r.x}L${i.y},${i.x}` : `M${r.x},${r.y}L${i.x},${i.y}`;
  }
  drawElbowPath(e, n) {
    return n === "horizontal" ? `M${e.source.y},${e.source.x}V${e.target.x}H${e.target.y}` : `M${e.source.x},${e.source.y}V${e.target.y}H${e.target.x}`;
  }
  drawPath() {
    const { linkData: e, orientation: n, pathFunc: r } = this.props;
    return typeof r == "function" ? r(e, n) : r === "elbow" ? this.drawElbowPath(e, n) : r === "straight" ? this.drawStraightPath(e, n) : r === "step" ? this.drawStepPath(e, n) : this.drawDiagonalPath(e, n);
  }
  getClassNames() {
    const { linkData: e, orientation: n, pathClassFunc: r } = this.props, i = ["rd3t-link"];
    return typeof r == "function" && i.push(r(e, n)), i.join(" ").trim();
  }
  render() {
    const { linkData: e } = this.props;
    return Q.createElement("path", { ref: (n) => {
      this.linkRef = n;
    }, style: Object.assign({}, this.state.initialStyle), className: this.getClassNames(), d: this.drawPath(), onClick: this.handleOnClick, onMouseOver: this.handleOnMouseOver, onMouseOut: this.handleOnMouseOut, "data-source-id": e.source.id, "data-target-id": e.target.id });
  }
}
const zu = `
/* Tree */
.rd3t-tree-container {
  width: 100%;
  height: 100%;
}

.rd3t-grabbable {
  cursor: move; /* fallback if grab cursor is unsupported */
  cursor: grab;
  cursor: -moz-grab;
  cursor: -webkit-grab;
}
.rd3t-grabbable:active {
    cursor: grabbing;
    cursor: -moz-grabbing;
    cursor: -webkit-grabbing;
}

/* Node */
.rd3t-node {
  cursor: pointer;
  fill: #777;
  stroke: #000;
  stroke-width: 2;
}

.rd3t-leaf-node {
  cursor: pointer;
  fill: transparent;
  stroke: #000;
  stroke-width: 1;
}

.rd3t-label__title {
  fill: #000;
  stroke: none;
  font-weight: bolder;
}

.rd3t-label__attributes {
  fill: #777;
  stroke: none;
  font-weight: bolder;
  font-size: smaller;
}

/* Link */
.rd3t-link {
  fill: none;
  stroke: #000;
}
`;
class rt extends Q.Component {
  constructor() {
    super(...arguments), this.state = {
      dataRef: this.props.data,
      data: rt.assignInternalProperties(tt(this.props.data)),
      d3: rt.calculateD3Geometry(this.props),
      isTransitioning: !1,
      isInitialRenderForDataset: !0,
      dataKey: this.props.dataKey
    }, this.internalState = {
      targetNode: null,
      isTransitioning: !1
    }, this.svgInstanceRef = `rd3t-svg-${Ce()}`, this.gInstanceRef = `rd3t-g-${Ce()}`, this.handleNodeToggle = (e) => {
      const n = tt(this.state.data), i = this.findNodesById(e, n, [])[0];
      this.props.collapsible && !this.state.isTransitioning && (i.__rd3t.collapsed ? (rt.expandNode(i), this.props.shouldCollapseNeighborNodes && this.collapseNeighborNodes(i, n)) : rt.collapseNode(i), this.props.enableLegacyTransitions ? (this.setState({ data: n, isTransitioning: !0 }), setTimeout(() => this.setState({ isTransitioning: !1 }), this.props.transitionDuration + 10)) : this.setState({ data: n }), this.internalState.targetNode = i);
    }, this.handleAddChildrenToNode = (e, n) => {
      const r = tt(this.state.data), i = this.findNodesById(e, r, []);
      if (i.length > 0) {
        const o = i[0], a = o.__rd3t.depth, s = tt(n).map((l) => rt.assignInternalProperties([l], a + 1));
        o.children.push(...s.flat()), this.setState({ data: r });
      }
    }, this.handleOnNodeClickCb = (e, n) => {
      const { onNodeClick: r } = this.props;
      r && typeof r == "function" && (n.persist(), r(tt(e), n));
    }, this.handleOnLinkClickCb = (e, n, r) => {
      const { onLinkClick: i } = this.props;
      i && typeof i == "function" && (r.persist(), i(tt(e), tt(n), r));
    }, this.handleOnNodeMouseOverCb = (e, n) => {
      const { onNodeMouseOver: r } = this.props;
      r && typeof r == "function" && (n.persist(), r(tt(e), n));
    }, this.handleOnLinkMouseOverCb = (e, n, r) => {
      const { onLinkMouseOver: i } = this.props;
      i && typeof i == "function" && (r.persist(), i(tt(e), tt(n), r));
    }, this.handleOnNodeMouseOutCb = (e, n) => {
      const { onNodeMouseOut: r } = this.props;
      r && typeof r == "function" && (n.persist(), r(tt(e), n));
    }, this.handleOnLinkMouseOutCb = (e, n, r) => {
      const { onLinkMouseOut: i } = this.props;
      i && typeof i == "function" && (r.persist(), i(tt(e), tt(n), r));
    }, this.centerNode = (e) => {
      const { dimensions: n, orientation: r, zoom: i, centeringTransitionDuration: o } = this.props;
      if (n) {
        const a = et(`.${this.gInstanceRef}`), s = et(`.${this.svgInstanceRef}`), l = this.state.d3.scale;
        let u, c;
        r === "horizontal" ? (c = -e.x * l + n.height / 2, u = -e.y * l + n.width / 2) : (u = -e.x * l + n.width / 2, c = -e.y * l + n.height / 2), a.transition().duration(o).attr("transform", "translate(" + u + "," + c + ")scale(" + l + ")"), s.call(Ee().transform, de.translate(u, c).scale(i));
      }
    }, this.getNodeClassName = (e, n) => {
      const { rootNodeClassName: r, branchNodeClassName: i, leafNodeClassName: o } = this.props;
      return e != null ? n.children ? i : o : r;
    };
  }
  static getDerivedStateFromProps(e, n) {
    let r = null;
    const i = !e.dataKey || n.dataKey !== e.dataKey;
    e.data !== n.dataRef && i && (r = {
      dataRef: e.data,
      data: rt.assignInternalProperties(tt(e.data)),
      isInitialRenderForDataset: !0,
      dataKey: e.dataKey
    });
    const o = rt.calculateD3Geometry(e);
    return Mt(o, n.d3) || (r = r || {}, r.d3 = o), r;
  }
  componentDidMount() {
    this.bindZoomListener(this.props), this.setState({ isInitialRenderForDataset: !1 });
  }
  componentDidUpdate(e) {
    this.props.data !== e.data && this.setState({ isInitialRenderForDataset: !1 }), (!Mt(this.props.translate, e.translate) || !Mt(this.props.scaleExtent, e.scaleExtent) || this.props.zoomable !== e.zoomable || this.props.draggable !== e.draggable || this.props.zoom !== e.zoom || this.props.enableLegacyTransitions !== e.enableLegacyTransitions) && this.bindZoomListener(this.props), typeof this.props.onUpdate == "function" && this.props.onUpdate({
      node: this.internalState.targetNode ? tt(this.internalState.targetNode) : null,
      zoom: this.state.d3.scale,
      translate: this.state.d3.translate
    }), this.internalState.targetNode = null;
  }
  /**
   * Collapses all tree nodes with a `depth` larger than `initialDepth`.
   *
   * @param {array} nodeSet Array of nodes generated by `generateTree`
   * @param {number} initialDepth Maximum initial depth the tree should render
   */
  setInitialTreeDepth(e, n) {
    e.forEach((r) => {
      r.data.__rd3t.collapsed = r.depth >= n;
    });
  }
  /**
   * bindZoomListener - If `props.zoomable`, binds a listener for
   * "zoom" events to the SVG and sets scaleExtent to min/max
   * specified in `props.scaleExtent`.
   */
  bindZoomListener(e) {
    const { zoomable: n, scaleExtent: r, translate: i, zoom: o, onUpdate: a, hasInteractiveNodes: s } = e, l = et(`.${this.svgInstanceRef}`), u = et(`.${this.gInstanceRef}`);
    l.call(Ee().transform, de.translate(i.x, i.y).scale(o)), l.call(Ee().scaleExtent(n ? [r.min, r.max] : [o, o]).filter((c) => s ? c.target.classList.contains(this.svgInstanceRef) || c.target.classList.contains(this.gInstanceRef) || c.shiftKey : !0).on("zoom", (c) => {
      !this.props.draggable && ["mousemove", "touchmove", "dblclick"].includes(c.sourceEvent.type) || (u.attr("transform", c.transform), typeof a == "function" && (a({
        node: null,
        zoom: c.transform.k,
        translate: { x: c.transform.x, y: c.transform.y }
      }), this.state.d3.scale = c.transform.k, this.state.d3.translate = {
        x: c.transform.x,
        y: c.transform.y
      }));
    }));
  }
  /**
   * Assigns internal properties that are required for tree
   * manipulation to each node in the `data` set and returns a new `data` array.
   *
   * @static
   */
  static assignInternalProperties(e, n = 0) {
    return (Array.isArray(e) ? e : [e]).map((i) => {
      const o = i;
      return o.__rd3t = { id: null, depth: null, collapsed: !1 }, o.__rd3t.id = Ce(), o.__rd3t.depth = n, o.children && o.children.length > 0 && (o.children = rt.assignInternalProperties(o.children, n + 1)), o;
    });
  }
  /**
   * Recursively walks the nested `nodeSet` until a node matching `nodeId` is found.
   */
  findNodesById(e, n, r) {
    return r.length > 0 || (r = r.concat(n.filter((i) => i.__rd3t.id === e)), n.forEach((i) => {
      i.children && i.children.length > 0 && (r = this.findNodesById(e, i.children, r));
    })), r;
  }
  /**
   * Recursively walks the nested `nodeSet` until all nodes at `depth` have been found.
   *
   * @param {number} depth Target depth for which nodes should be returned
   * @param {array} nodeSet Array of nested `node` objects
   * @param {array} accumulator Accumulator for matches, passed between recursive calls
   */
  findNodesAtDepth(e, n, r) {
    return r = r.concat(n.filter((i) => i.__rd3t.depth === e)), n.forEach((i) => {
      i.children && i.children.length > 0 && (r = this.findNodesAtDepth(e, i.children, r));
    }), r;
  }
  /**
   * Recursively sets the internal `collapsed` property of
   * the passed `TreeNodeDatum` and its children to `true`.
   *
   * @static
   */
  static collapseNode(e) {
    e.__rd3t.collapsed = !0, e.children && e.children.length > 0 && e.children.forEach((n) => {
      rt.collapseNode(n);
    });
  }
  /**
   * Sets the internal `collapsed` property of
   * the passed `TreeNodeDatum` object to `false`.
   *
   * @static
   */
  static expandNode(e) {
    e.__rd3t.collapsed = !1;
  }
  /**
   * Collapses all nodes in `nodeSet` that are neighbors (same depth) of `targetNode`.
   */
  collapseNeighborNodes(e, n) {
    this.findNodesAtDepth(e.__rd3t.depth, n, []).filter((i) => i.__rd3t.id !== e.__rd3t.id).forEach((i) => rt.collapseNode(i));
  }
  /**
   * Generates tree elements (`nodes` and `links`) by
   * grabbing the rootNode from `this.state.data[0]`.
   * Restricts tree depth to `props.initialDepth` if defined and if this is
   * the initial render of the tree.
   */
  generateTree() {
    const { initialDepth: e, depthFactor: n, separation: r, nodeSize: i, orientation: o } = this.props, { isInitialRenderForDataset: a } = this.state, l = vi().nodeSize(o === "horizontal" ? [i.y, i.x] : [i.x, i.y]).separation((m, h) => m.parent.data.__rd3t.id === h.parent.data.__rd3t.id ? r.siblings : r.nonSiblings)(Qe(this.state.data[0], (m) => m.__rd3t.collapsed ? null : m.children));
    let u = l.descendants();
    const c = l.links();
    return e !== void 0 && a && this.setInitialTreeDepth(u, e), n && u.forEach((m) => {
      m.y = m.depth * n;
    }), { nodes: u, links: c };
  }
  /**
   * Set initial zoom and position.
   * Also limit zoom level according to `scaleExtent` on initial display. This is necessary,
   * because the first time we are setting it as an SVG property, instead of going
   * through D3's scaling mechanism, which would have picked up both properties.
   *
   * @static
   */
  static calculateD3Geometry(e) {
    let n;
    return e.zoom > e.scaleExtent.max ? n = e.scaleExtent.max : e.zoom < e.scaleExtent.min ? n = e.scaleExtent.min : n = e.zoom, {
      translate: e.translate,
      scale: n
    };
  }
  render() {
    const { nodes: e, links: n } = this.generateTree(), { renderCustomNodeElement: r, orientation: i, pathFunc: o, transitionDuration: a, nodeSize: s, depthFactor: l, initialDepth: u, separation: c, enableLegacyTransitions: m, svgClassName: h, pathClassFunc: g } = this.props, { translate: x, scale: S } = this.state.d3, I = Object.assign(Object.assign(Object.assign({}, s), c), {
      depthFactor: l,
      initialDepth: u
    });
    return Q.createElement(
      "div",
      { className: "rd3t-tree-container rd3t-grabbable" },
      Q.createElement("style", null, zu),
      Q.createElement(
        "svg",
        { className: `rd3t-svg ${this.svgInstanceRef} ${h}`, width: "100%", height: "100%" },
        Q.createElement(
          bu,
          { enableLegacyTransitions: m, component: "g", className: `rd3t-g ${this.gInstanceRef}`, transform: `translate(${x.x},${x.y}) scale(${S})` },
          n.map((T, L) => Q.createElement($u, { key: "link-" + L, orientation: i, pathFunc: o, pathClassFunc: g, linkData: T, onClick: this.handleOnLinkClickCb, onMouseOver: this.handleOnLinkMouseOverCb, onMouseOut: this.handleOnLinkMouseOutCb, enableLegacyTransitions: m, transitionDuration: a })),
          e.map((T, L) => {
            const { data: R, x: _, y: d, parent: D } = T;
            return Q.createElement(Eu, { key: "node-" + L, data: R, position: { x: _, y: d }, hierarchyPointNode: T, parent: D, nodeClassName: this.getNodeClassName(D, R), renderCustomNodeElement: r, nodeSize: s, orientation: i, enableLegacyTransitions: m, transitionDuration: a, onNodeToggle: this.handleNodeToggle, onNodeClick: this.handleOnNodeClickCb, onNodeMouseOver: this.handleOnNodeMouseOverCb, onNodeMouseOut: this.handleOnNodeMouseOutCb, handleAddChildrenToNode: this.handleAddChildrenToNode, subscriptions: I, centerNode: this.centerNode });
          })
        )
      )
    );
  }
}
rt.defaultProps = {
  onNodeClick: void 0,
  onNodeMouseOver: void 0,
  onNodeMouseOut: void 0,
  onLinkClick: void 0,
  onLinkMouseOver: void 0,
  onLinkMouseOut: void 0,
  onUpdate: void 0,
  orientation: "horizontal",
  translate: { x: 0, y: 0 },
  pathFunc: "diagonal",
  pathClassFunc: void 0,
  transitionDuration: 500,
  depthFactor: void 0,
  collapsible: !0,
  initialDepth: void 0,
  zoomable: !0,
  draggable: !0,
  zoom: 1,
  scaleExtent: { min: 0.1, max: 1 },
  nodeSize: { x: 140, y: 140 },
  separation: { siblings: 1, nonSiblings: 2 },
  shouldCollapseNeighborNodes: !1,
  svgClassName: "",
  rootNodeClassName: "",
  branchNodeClassName: "",
  leafNodeClassName: "",
  renderCustomNodeElement: void 0,
  enableLegacyTransitions: !1,
  hasInteractiveNodes: !1,
  dimensions: void 0,
  centeringTransitionDuration: 800,
  dataKey: void 0
};
const Iu = (t) => {
  if (!t) return { name: "No document loaded" };
  const e = Lu(t);
  return e ? Fr(e, t) : { name: "Cannot determine root node" };
}, Lu = (t) => {
  var n;
  if (!t.tiles.length) return null;
  const e = /* @__PURE__ */ new Set();
  for (const [r, i] of t.tiles.entries())
    i.t === "Tile" && i.children.forEach((o) => {
      o.forEach((a) => {
        e.add(a);
      });
    });
  for (const [r, i] of t.tiles.entries())
    if (!e.has(i.id))
      return i.id;
  return ((n = t.tiles[0]) == null ? void 0 : n.id) || null;
}, Fr = (t, e) => {
  const n = e.tiles.find((i) => i.id === t);
  if (!n)
    return { name: `Unknown (${t.substring(0, 6)}...)` };
  if (n.t === "Grout")
    return {
      name: `Grout (${t.substring(0, 6)}...)`,
      attributes: {
        shape: n.shape
      }
    };
  if (n.t === "Secondary")
    return {
      name: `Secondary (${t.substring(0, 6)}...)`,
      attributes: {
        type: n.content.t,
        content: n.content.content.substring(0, 15) + (n.content.content.length > 15 ? "..." : "")
      }
    };
  const r = n.children.flatMap(
    (i) => i.map((o) => Fr(o, e))
  );
  return {
    name: n.label.join(" ") || `Tile (${t.substring(0, 6)}...)`,
    attributes: {
      id: t.substring(0, 6) + "...",
      mold: `${n.mold.out} <- ${n.mold.in.join(", ")}`
    },
    children: r.length > 0 ? r : void 0
  };
}, Wu = ({ docState: t }) => {
  const [e, n] = Ur(null);
  return ee(() => {
    n(Iu(t));
  }, [t]), e ? /* @__PURE__ */ ft.jsx("div", { style: { width: "100%", height: "400px" }, children: /* @__PURE__ */ ft.jsx(
    rt,
    {
      data: e,
      orientation: "vertical",
      pathFunc: "step",
      collapsible: !0,
      translate: { x: 150, y: 50 },
      nodeSize: { x: 150, y: 100 },
      separation: { siblings: 1.5, nonSiblings: 2 },
      renderCustomNodeElement: (r) => /* @__PURE__ */ ft.jsxs("g", { children: [
        /* @__PURE__ */ ft.jsx("circle", { r: 10, fill: "lightsteelblue" }),
        /* @__PURE__ */ ft.jsx(
          "text",
          {
            dy: ".31em",
            x: 15,
            textAnchor: "start",
            style: { fontSize: "12px" },
            children: r.nodeDatum.name
          }
        ),
        r.nodeDatum.attributes && /* @__PURE__ */ ft.jsx(
          "text",
          {
            dy: "1.31em",
            x: 15,
            textAnchor: "start",
            style: { fontSize: "10px", fill: "#666" },
            children: Object.entries(r.nodeDatum.attributes).map(
              ([i, o], a) => `${i}: ${o}`
            ).join(", ")
          }
        )
      ] })
    }
  ) }) : /* @__PURE__ */ ft.jsx("div", { children: "Loading graph..." });
};
export {
  Wu as DocGraph,
  ju as HazelEmbed
};
//# sourceMappingURL=hazel-embed.js.map
