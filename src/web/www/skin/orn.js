// Procedural ornament engine. Strokes are grown from curvature programs and
// Bezier spans, given width profiles, and outlined as ribbons. Two finishes:
//   fil    - filigree: a light gold body inside a thin contour, so wide strokes
//            read as a double line and tapering ends become hairlines
//   relief - cast gilt: body, shade and highlight offset by the light
// plus "hair" for plain single lines. Passes are fused across a whole
// ornament, so strokes that touch merge into one piece.
(function (root) {
  "use strict";

  const TAU = Math.PI * 2;
  const r2 = (v) => Math.round(v * 100) / 100;

  function mulberry(seed) {
    return function () {
      seed |= 0;
      seed = (seed + 0x6d2b79f5) | 0;
      let t = Math.imul(seed ^ (seed >>> 15), 1 | seed);
      t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
      return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
  }
  function hashStr(s) {
    let h = 2166136261 >>> 0;
    for (let i = 0; i < s.length; i++) {
      h ^= s.charCodeAt(i);
      h = Math.imul(h, 16777619);
    }
    return h >>> 0;
  }

  class Turtle {
    constructor(x, y, h) {
      this.x = x;
      this.y = y;
      this.h = h;
      this.s = 0;
      this.pts = [{ x, y, s: 0 }];
    }
    step(ds, dh) {
      this.h += dh / 2;
      this.x += Math.cos(this.h) * ds;
      this.y += Math.sin(this.h) * ds;
      this.h += dh / 2;
      this.s += ds;
      this.pts.push({ x: this.x, y: this.y, s: this.s });
      return this;
    }
    line(len) {
      const n = Math.max(2, Math.ceil(len / 1.2));
      for (let i = 0; i < n; i++) this.step(len / n, 0);
      return this;
    }
    arc(len, k) {
      const n = Math.max(3, Math.ceil(len / 0.7));
      for (let i = 0; i < n; i++) this.step(len / n, (k * len) / n);
      return this;
    }
    // turn by `ang` radians on a circle of radius r (sign of ang = direction)
    turn(ang, r) {
      return this.arc(Math.abs(ang) * r, Math.sign(ang) / r);
    }
    clothoid(len, k0, k1) {
      const n = Math.max(3, Math.ceil(len / 0.5));
      for (let i = 0; i < n; i++) {
        const k = k0 + ((k1 - k0) * (i + 0.5)) / n;
        this.step(len / n, (k * len) / n);
      }
      return this;
    }
    // logarithmic spiral driven by heading; radius shrinks by `shrink` per turn
    volute(r0, turns, dir, shrink = 0.6) {
      const total = turns * TAU,
        n = Math.max(10, Math.ceil(turns * 72)),
        b = -Math.log(shrink) / TAU,
        dphi = total / n;
      for (let i = 0; i < n; i++) {
        const r = r0 * Math.exp(-b * (i + 0.5) * dphi);
        this.step(r * dphi, dir * dphi);
      }
      return this;
    }
    // curvature ramps in from zero so a scroll leaves a straight run smoothly
    scroll(r0, turns, dir, shrink) {
      this.clothoid(r0 * 0.9, 0, dir / r0);
      return this.volute(r0, turns, dir, shrink);
    }
  }

  function bez(p0, p1, p2, p3, n = 28) {
    const pts = [];
    let s = 0,
      px = p0[0],
      py = p0[1];
    for (let i = 0; i <= n; i++) {
      const t = i / n,
        u = 1 - t;
      const x = u * u * u * p0[0] + 3 * u * u * t * p1[0] + 3 * u * t * t * p2[0] + t * t * t * p3[0];
      const y = u * u * u * p0[1] + 3 * u * u * t * p1[1] + 3 * u * t * t * p2[1] + t * t * t * p3[1];
      if (i) s += Math.hypot(x - px, y - py);
      pts.push({ x, y, s });
      px = x;
      py = y;
    }
    return pts;
  }
  function join(...lists) {
    const out = [];
    for (const L of lists) {
      if (!L || !L.length) continue;
      const off = out.length ? out[out.length - 1].s : 0;
      for (let i = out.length ? 1 : 0; i < L.length; i++)
        out.push({ x: L[i].x, y: L[i].y, s: L[i].s - L[0].s + off });
    }
    return out;
  }
  function reversePts(pts) {
    const L = pts[pts.length - 1].s;
    return pts.map((p) => ({ x: p.x, y: p.y, s: L - p.s })).reverse();
  }
  function endOf(pts) {
    const a = pts[pts.length - 2],
      b = pts[pts.length - 1];
    return { x: b.x, y: b.y, h: Math.atan2(b.y - a.y, b.x - a.x) };
  }
  function tangentAt(pts, i) {
    const a = pts[Math.max(0, i - 1)],
      b = pts[Math.min(pts.length - 1, i + 1)];
    return Math.atan2(b.y - a.y, b.x - a.x);
  }
  function mapPts(pts, f) {
    return pts.map((p) => {
      const [x, y] = f(p.x, p.y);
      return { x, y, s: p.s };
    });
  }
  function wprof(keys) {
    return (t) => {
      if (t <= keys[0][0]) return keys[0][1];
      for (let i = 1; i < keys.length; i++) {
        if (t <= keys[i][0]) {
          const [t0, w0] = keys[i - 1],
            [t1, w1] = keys[i];
          let u = (t - t0) / (t1 - t0 || 1);
          u = u * u * (3 - 2 * u);
          return w0 + (w1 - w0) * u;
        }
      }
      return keys[keys.length - 1][1];
    };
  }
  const constW = (w) => () => w;

  // Outline of a variable-width stroke with round caps. `shift(nx, ny, W)`
  // moves the centre line along the normal (shade and highlight ribbons).
  function ribbon(pts, wf, scale = 1, shift = null) {
    const n = pts.length;
    if (n < 2) return "";
    const L = pts[n - 1].s || 1,
      left = [],
      right = [];
    let c0, cN;
    for (let i = 0; i < n; i++) {
      const a = pts[Math.max(0, i - 1)],
        b = pts[Math.min(n - 1, i + 1)];
      let tx = b.x - a.x,
        ty = b.y - a.y;
      const tl = Math.hypot(tx, ty) || 1;
      tx /= tl;
      ty /= tl;
      const nx = -ty,
        ny = tx;
      const W = wf(pts[i].s / L),
        w = (W * scale) / 2;
      const o = shift ? shift(nx, ny, W) : 0;
      const cx = pts[i].x + nx * o,
        cy = pts[i].y + ny * o;
      left.push([cx + nx * w, cy + ny * w]);
      right.push([cx - nx * w, cy - ny * w]);
      if (i === 0) c0 = { cx, cy, nx, ny, tx, ty, w };
      if (i === n - 1) cN = { cx, cy, nx, ny, tx, ty, w };
    }
    const cap = (c, fwd) => {
      const out = [];
      for (let k = 1; k < 6; k++) {
        const a = (Math.PI * k) / 6,
          co = Math.cos(a),
          si = Math.sin(a);
        const dx = fwd ? c.nx * co + c.tx * si : -c.nx * co - c.tx * si;
        const dy = fwd ? c.ny * co + c.ty * si : -c.ny * co - c.ty * si;
        out.push([c.cx + dx * c.w, c.cy + dy * c.w]);
      }
      return out;
    };
    const ring = simplify([...left, ...cap(cN, true), ...right.reverse(), ...cap(c0, false)], 0.07);
    return "M" + ring.map((p) => r2(p[0]) + " " + r2(p[1])).join("L") + "Z";
  }

  // Ramer-Douglas-Peucker on a closed ring of [x, y] points.
  function simplify(pts, tol) {
    if (pts.length < 8) return pts;
    const keep = new Uint8Array(pts.length);
    keep[0] = keep[pts.length - 1] = 1;
    const stack = [[0, pts.length - 1]];
    while (stack.length) {
      const [a, b] = stack.pop();
      const [ax, ay] = pts[a],
        [bx, by] = pts[b];
      const dx = bx - ax,
        dy = by - ay,
        L = Math.hypot(dx, dy) || 1e-9;
      let best = -1,
        bi = -1;
      for (let i = a + 1; i < b; i++) {
        const d = Math.abs((pts[i][0] - ax) * dy - (pts[i][1] - ay) * dx) / L;
        if (d > best) {
          best = d;
          bi = i;
        }
      }
      if (best > tol) {
        keep[bi] = 1;
        stack.push([a, bi], [bi, b]);
      }
    }
    return pts.filter((_, i) => keep[i]);
  }
  function polyline(pts) {
    return "M" + pts.map((p) => r2(p.x) + " " + r2(p.y)).join("L");
  }

  // An ornament: strokes, beads and filled shapes, each tagged with a role
  //   cast - fixed piece at an end, corner or joint
  //   run  - elastic connector re-solved for the length
  //   grow - motif whose count is recomputed for the length
  // and a finish (fil, relief, hair).
  class Orn {
    constructor() {
      this.strokes = [];
      this.beads = [];
      this.fills = [];
    }
    stroke(pts, wf, role, finish = "fil") {
      if (pts && pts.length > 1) this.strokes.push({ pts, wf, role, finish });
      return this;
    }
    bead(x, y, r, role, finish = "fil") {
      this.beads.push({ x, y, r, role, finish });
      return this;
    }
    fill(pts, role, finish = "relief") {
      this.fills.push({ pts, role, finish });
      return this;
    }
    add(o) {
      this.strokes.push(...o.strokes);
      this.beads.push(...o.beads);
      this.fills.push(...o.fills);
      return this;
    }
    map(f) {
      const o = new Orn();
      o.strokes = this.strokes.map((s) => ({ ...s, pts: mapPts(s.pts, f) }));
      o.beads = this.beads.map((b) => {
        const [x, y] = f(b.x, b.y);
        return { ...b, x, y };
      });
      o.fills = this.fills.map((p) => ({ ...p, pts: mapPts(p.pts, f) }));
      return o;
    }
    finish(fin) {
      this.strokes.forEach((s) => (s.finish = fin));
      this.beads.forEach((b) => (b.finish = fin));
      return this;
    }
  }

  // Colours sampled from the Baroque Probe Frames restyle: no dark outlines;
  // the darkest gold is a warm mid-brown.
  const PALETTE = {
    fil: { contour: "#bc8b42", body: "#f1dba4", hi: "#fff8de" },
    relief: { line: "#b68c48", body: "#dcbc78", shade: "#c0924a", hi: "#fff6d6" },
    hair: "#c49b56",
  };
  const ROLE_COLORS = { cast: "#d6402b", run: "#1689a8", grow: "#2f9a45" };

  function renderOrn(orn, opts = {}) {
    const L = opts.light || { x: -0.707, y: -0.707 };
    const P = opts.palette || PALETTE;
    const contour = opts.contour ?? 0.62;
    const outline = opts.outline ?? 0.36;
    const facing = (nx, ny) => nx * L.x + ny * L.y;
    const F = { ul: [], body: [], hi: [] },
      R = { ul: [], body: [], sh: [], hi: [] },
      H = [],
      Rib = [];
    const circ = (x, y, r) => `<circle cx="${r2(x)}" cy="${r2(y)}" r="${r2(r)}"/>`;
    if (opts.anatomy) {
      const out = [];
      for (const s of orn.strokes)
        out.push(
          s.finish === "hair"
            ? `<path d="${polyline(s.pts)}" fill="none" stroke="${ROLE_COLORS[s.role]}" stroke-width="1.2"/>`
            : `<path d="${ribbon(s.pts, s.wf, 1.6)}" fill="${ROLE_COLORS[s.role]}"/>`
        );
      for (const b of orn.beads) out.push(`<circle cx="${r2(b.x)}" cy="${r2(b.y)}" r="${r2(b.r * 1.2)}" fill="${ROLE_COLORS[b.role]}"/>`);
      for (const p of orn.fills) out.push(`<path d="${polyline(p.pts)}Z" fill="${ROLE_COLORS[p.role]}"/>`);
      return `<g fill-opacity=".85">${out.join("")}</g>`;
    }
    for (const s of orn.strokes) {
      if (s.finish === "hair" || s.finish === "rib") {
        (s.finish === "rib" ? Rib : H).push(`<path d="${polyline(s.pts)}" stroke-width="${r2(s.wf(0.5))}"/>`);
        continue;
      }
      const d = ribbon(s.pts, s.wf);
      if (s.finish === "relief") {
        R.ul.push(`<path d="${d}"/>`);
        R.body.push(`<path d="${d}"/>`);
        R.sh.push(`<path d="${ribbon(s.pts, s.wf, 0.42, (nx, ny, W) => -facing(nx, ny) * W * 0.27)}"/>`);
        R.hi.push(`<path d="${ribbon(s.pts, s.wf, 0.3, (nx, ny, W) => facing(nx, ny) * W * 0.26)}"/>`);
      } else {
        F.ul.push(`<path d="${d}"/>`);
        F.body.push(`<path d="${d}"/>`);
        F.hi.push(`<path d="${ribbon(s.pts, s.wf, 0.3, (nx, ny, W) => facing(nx, ny) * W * 0.22)}"/>`);
      }
    }
    for (const b of orn.beads) {
      const T = b.finish === "relief" ? R : F;
      T.ul.push(circ(b.x, b.y, b.r));
      T.body.push(circ(b.x, b.y, b.r));
      if (T === R) R.sh.push(circ(b.x - L.x * b.r * 0.28, b.y - L.y * b.r * 0.28, b.r * 0.68));
      T.hi.push(circ(b.x + L.x * b.r * 0.36, b.y + L.y * b.r * 0.36, b.r * 0.36));
    }
    const fills = [];
    for (const p of orn.fills) {
      const d = polyline(p.pts) + "Z";
      fills.push(`<path d="${d}" fill="url(#ornGrad)" stroke="${P.relief.line}" stroke-width="${outline}" stroke-linejoin="round"/>`);
    }
    const lx = r2(0.5 - L.x * 0.5),
      ly = r2(0.5 - L.y * 0.5);
    return (
      (fills.length
        ? `<defs><linearGradient id="ornGrad" x1="${r2(1 - lx)}" y1="${r2(1 - ly)}" x2="${lx}" y2="${ly}"><stop offset="0" stop-color="${P.relief.hi}"/><stop offset=".45" stop-color="${P.relief.body}"/><stop offset="1" stop-color="${P.relief.shade}"/></linearGradient></defs>`
        : "") +
      (H.length ? `<g fill="none" stroke="${P.hair}" stroke-linecap="round" stroke-linejoin="round">${H.join("")}</g>` : "") +
      (F.ul.length
        ? `<g fill="${P.fil.contour}" stroke="${P.fil.contour}" stroke-width="${2 * contour}" stroke-linejoin="round">${F.ul.join("")}</g>` +
          `<g fill="${P.fil.body}">${F.body.join("")}</g>` +
          `<g fill="${P.fil.hi}" fill-opacity=".8">${F.hi.join("")}</g>`
        : "") +
      fills.join("") +
      (R.ul.length
        ? `<g fill="${P.relief.line}" stroke="${P.relief.line}" stroke-width="${2 * outline}" stroke-linejoin="round">${R.ul.join("")}</g>` +
          `<g fill="${P.relief.body}">${R.body.join("")}</g>` +
          `<g fill="${P.relief.shade}" fill-opacity=".55">${R.sh.join("")}</g>` +
          `<g fill="${P.relief.hi}" fill-opacity=".95">${R.hi.join("")}</g>`
        : "") +
      (Rib.length ? `<g fill="none" stroke="${P.relief.shade}" stroke-opacity=".9" stroke-linecap="round">${Rib.join("")}</g>` : "")
    );
  }

  // ---------- small pieces ----------

  // Teardrop leaf from (x, y) along heading h, bowed sideways by `bend`.
  function leaf(x, y, h, len, wid, bend, role = "cast", finish = "fil") {
    const c = Math.cos(h),
      s = Math.sin(h),
      nx = -s,
      ny = c;
    const p = (f, b) => [x + c * len * f + nx * b * len, y + s * len * f + ny * b * len];
    return new Orn().stroke(bez([x, y], p(0.35, bend), p(0.75, bend), p(1, 0), 14), wprof([[0, wid * 0.3], [0.4, wid], [1, wid * 0.04]]), role, finish);
  }

  // A bud pointing along dirx (+1 right), its tip at (x, y): a slim leaf and,
  // at full detail, two small sepal curls at its base.
  function budTip(x, y, R, lod, dirx = 1) {
    const o = new Orn();
    const len = 0.2 * R;
    o.add(leaf(x - dirx * len, y, dirx > 0 ? 0 : Math.PI, len, 0.085 * R, 0, "cast"));
    if (lod >= 3) {
      for (const sg of [-1, 1]) {
        const t = new Turtle(x - dirx * len * 0.95, y, (dirx > 0 ? 0 : Math.PI) + sg * 1.1 * dirx);
        t.line(0.03 * R).scroll(0.04 * R, 0.9, sg * dirx, 0.45);
        o.stroke(t.pts, wprof([[0, 0.035 * R], [1, 0.015 * R]]), "cast");
      }
    }
    return o;
  }

  // A small closed loop on a short stalk: the hooks along the restyle's vines.
  function loopBud(x, y, h, dir, R, size = 1, role = "grow") {
    const t = new Turtle(x, y, h);
    t.line(0.07 * R * size).scroll(0.055 * R * size, 1.25, dir, 0.32);
    return new Orn().stroke(t.pts, wprof([[0, 0.045 * R], [0.45, 0.04 * R], [1, 0.03 * R]]), role);
  }

  // A short tapered spur.
  function spur(x, y, h, R, len = 0.16) {
    return leaf(x, y, h, len * R, 0.04 * R, 0.08, "grow");
  }

  // Hazel's term-indication arm: a hairline bracket opening right.
  function arm(xa, y0, y1, C, R) {
    const r = Math.min(0.16 * R, (y1 - y0) / 4);
    const t = new Turtle(xa + 0.45 * C, y0, Math.PI);
    t.line(Math.max(0.1, 0.45 * C - r)).arc((Math.PI / 2) * r, -1 / r);
    t.line(Math.max(0.1, y1 - y0 - 2 * r)).arc((Math.PI / 2) * r, -1 / r).line(Math.max(0.1, 0.45 * C - r));
    return new Orn().stroke(t.pts, constW(1), "run", "hair");
  }

  // ---------- braces beside nested code ----------

  // Curly brace after the restyle: tails at both ends sweep toward the code,
  // a closed loop sits at each shoulder, the spine bows gently between them.
  function curlyBrace(xs, y0, y1, R, lod) {
    const o = new Orn();
    const H = y1 - y0;
    const tail = 0.38 * R;
    const sh1 = y0 + Math.min(0.32 * H, 0.75 * R),
      sh2 = y1 - Math.min(0.32 * H, 0.75 * R);
    const bow = Math.min(0.14 * R, 0.06 * H);
    const lr = Math.min(0.075 * R, 0.05 * H) * (lod >= 2 ? 1 : 0.7);
    const top = bez([xs + tail, y0], [xs + 0.06 * R, y0 - 0.02 * R], [xs - 0.1 * R, sh1 - 0.35 * (sh1 - y0)], [xs - 0.06 * R, sh1], 18);
    const mid = bez([xs - 0.06 * R, sh1], [xs + bow, sh1 + 0.3 * (sh2 - sh1)], [xs + bow, sh2 - 0.3 * (sh2 - sh1)], [xs - 0.06 * R, sh2], 24);
    const bot = bez([xs - 0.06 * R, sh2], [xs - 0.1 * R, sh2 + 0.35 * (y1 - sh2)], [xs + 0.06 * R, y1 + 0.02 * R], [xs + tail, y1], 18);
    o.stroke(top, wprof([[0, 0.02 * R], [0.6, 0.05 * R], [1, 0.06 * R]]), "cast");
    o.stroke(mid, wprof([[0, 0.06 * R], [0.5, 0.075 * R], [1, 0.06 * R]]), "run");
    o.stroke(bot, wprof([[0, 0.06 * R], [0.4, 0.05 * R], [1, 0.02 * R]]), "cast");
    if (lod >= 2) {
      for (const [y, h] of [[sh1, endOf(top).h], [sh2, endOf(mid).h]]) {
        const t = new Turtle(xs - 0.06 * R, y, h);
        t.turn(TAU * 0.98, lr);
        o.stroke(t.pts, constW(0.042 * R), "cast");
      }
    }
    if (lod >= 3) {
      o.add(spur(xs + 0.02 * R, (sh1 + sh2) / 2, Math.PI + 0.5, R, 0.14));
    }
    return o;
  }

  // Rinceau: a running vine beside a long block. A lobe swings out into each
  // outer bay and a smaller curl into each bay on the code side; the wave
  // count is recomputed for the height, so it always ends cleanly.
  function rinceau(xs, y0, y1, R, lod, rnd) {
    const o = new Orn();
    const top = y0 + 0.45 * R,
      bot = y1 - 0.45 * R;
    const span = Math.max(1, bot - top);
    const n = Math.max(1, Math.round(span / (0.95 * R)));
    const A = 0.13 * R;
    const xAt = (y) => xs + A * Math.sin(((y - top) / span) * n * Math.PI);
    const N = Math.max(12, Math.ceil(span / 1.0));
    const stem = [];
    let s = 0,
      px = xAt(top),
      py = top;
    for (let i = 0; i <= N; i++) {
      const y = top + (span * i) / N,
        x = xAt(y);
      if (i) s += Math.hypot(x - px, y - py);
      stem.push({ x, y, s });
      px = x;
      py = y;
    }
    const endR = 0.24 * R;
    const hookTop = new Turtle(stem[0].x, stem[0].y, -Math.PI / 2).line(0.04 * R).scroll(endR, lod >= 2 ? 1.15 : 0.3, -1, 0.42);
    const hookBot = new Turtle(stem[N].x, stem[N].y, Math.PI / 2).line(0.04 * R).scroll(endR, lod >= 2 ? 1.15 : 0.3, 1, 0.42);
    o.stroke(join(reversePts(hookTop.pts), stem, hookBot.pts), wprof([[0, 0.025 * R], [0.06, 0.08 * R], [0.5, 0.072 * R], [0.94, 0.08 * R], [1, 0.025 * R]]), "run");
    if (lod < 2) return o;
    for (let k = 0; k < n; k++) {
      const yk = top + ((k + 0.5) / n) * span;
      const x = xAt(yk);
      const outer = k % 2 === 1; // the stem swings left on odd half-waves, leaving the bay on the right... and vice versa
      const up = Math.floor(k / 2) % 2 === 1;
      if (outer) {
        const h = up ? -Math.PI / 2 - 0.6 : Math.PI / 2 + 0.6;
        const t = new Turtle(x, yk, h);
        t.line(0.03 * R).scroll(0.24 * R * (0.92 + 0.16 * rnd()), 1.2, up ? 1 : -1, 0.45);
        o.stroke(t.pts, wprof([[0, 0.06 * R], [0.3, 0.055 * R], [1, 0.022 * R]]), "grow");
        if (lod >= 3) o.add(spur(x, yk, up ? Math.PI / 2 + 0.3 : -Math.PI / 2 - 0.3, R, 0.15));
      } else if (lod >= 3) {
        const h = up ? -Math.PI / 2 + 0.7 : Math.PI / 2 - 0.7;
        const t = new Turtle(x, yk, h);
        t.line(0.03 * R).scroll(0.11 * R, 1.05, up ? -1 : 1, 0.42);
        o.stroke(t.pts, wprof([[0, 0.05 * R], [1, 0.02 * R]]), "grow");
      }
    }
    return o;
  }

  // A tendril that ends a branch: it runs on past (x, y) and curls into a
  // small spiral, up or down; at full detail a leaf springs from its base.
  function curlTip(x, y, R, lod, up = true) {
    const o = new Orn();
    const t = new Turtle(x, y, 0);
    t.line(0.05 * R).scroll(0.085 * R, 1.15, up ? -1 : 1, 0.4);
    o.stroke(t.pts, wprof([[0, 0.05 * R], [0.4, 0.045 * R], [1, 0.02 * R]]), "cast");
    if (lod >= 3) o.add(leaf(x + 0.01 * R, y, up ? 0.9 : -0.9, 0.17 * R, 0.06 * R, up ? -0.1 : 0.1, "cast"));
    return o;
  }

  // Vine for a block with children (a tuple's fields, a sum's variants):
  // one arc from the last child round to the first, a branch to each child
  // between, each ending in a curling tendril beside its child; loop buds
  // are regrown along the arc and, on taller blocks, a second stem rises
  // inside it.
  function vine(xt, ys, R, lod, rnd, minX = -Infinity) {
    const o = new Orn();
    const yF = ys[0],
      yL = ys[ys.length - 1];
    const span = Math.max(0.5 * R, yL - yF);
    const base = xt - 0.36 * R;
    // the arc's leftmost point is base - reach; keep it right of minX
    const reach = Math.max(0.45 * R, Math.min(0.6 * R + 0.07 * span, base - minX));
    const xc = base - reach / 0.75;
    const arc = bez([base, yL], [xc, yL], [xc, yF], [base, yF], 64);
    o.stroke(arc, wprof([[0, 0.04 * R], [0.14, 0.07 * R], [0.5, 0.085 * R], [0.86, 0.07 * R], [1, 0.04 * R]]), "run");
    if (lod >= 2) {
      o.add(curlTip(base, yL, R, lod, false));
      o.add(curlTip(base, yF, R, lod, true));
    }
    const nearArc = (y) => {
      let best = arc[0];
      for (const p of arc) if (p.x < base - 0.3 * reach && Math.abs(p.y - y) < Math.abs(best.y - y)) best = p;
      return best;
    };
    const forks = [];
    ys.slice(1, -1).forEach((yc, i) => {
      const p = nearArc(Math.min(yL - 0.3 * R, yc + 0.42 * R));
      forks.push(p.y);
      const br = bez([p.x, p.y], [p.x + 0.12 * R, p.y - 0.22 * R], [base - 0.32 * R, yc], [base, yc], 20);
      o.stroke(br, wprof([[0, 0.06 * R], [1, 0.04 * R]]), "run");
      if (lod >= 2) o.add(curlTip(base, yc, R, lod, i % 2 === 0));
    });
    if (lod >= 3) {
      const total = arc[arc.length - 1].s;
      let side = 1;
      for (let s = 0.45 * R; s < total - 0.45 * R; s += 0.55 * R * (0.9 + 0.2 * rnd())) {
        const i = arc.findIndex((p) => p.s >= s);
        const p = arc[i];
        if (forks.some((y) => Math.abs(y - p.y) < 0.35 * R)) continue;
        const th = tangentAt(arc, i);
        let nx = -Math.sin(th),
          ny = Math.cos(th);
        if (nx > 0) {
          nx = -nx;
          ny = -ny;
        }
        const out = Math.atan2(ny, nx);
        o.add(loopBud(p.x, p.y, side > 0 ? out + 0.6 : out - 0.6, side > 0 ? 1 : -1, R, 1.5));
        side = -side;
      }
      if (span >= 2.5 * R) {
        // a second, straighter stem inside the arc, with alternate leaves
        const x0 = base - reach * 0.42;
        const stem = bez([base - 0.15 * R, yL - 0.05 * R], [x0, yL - 0.3 * span], [x0, yF + 0.45 * span], [x0 + 0.06 * R, yF + 0.28 * span], 30);
        o.stroke(stem, wprof([[0, 0.055 * R], [1, 0.03 * R]]), "grow");
        const e = endOf(stem);
        o.add(leaf(e.x, e.y, e.h, 0.22 * R, 0.08 * R, 0, "grow"));
        for (let f = 0.3, sg = 1; f < 0.92; f += 0.2, sg = -sg) {
          const i = Math.floor(f * (stem.length - 1));
          const q = stem[i];
          o.add(leaf(q.x, q.y, tangentAt(stem, i) + sg * 0.8, 0.16 * R, 0.06 * R, sg * 0.1, "grow"));
        }
      }
    }
    return o;
  }

  // ---------- frames ----------

  // Cartouche end cap, canonical left side, for a box of height h: a rounded
  // bracket whose arms run into the moldings, a boss at its outermost point,
  // and a curl turning outward at each corner.
  function capOrn(h, lod) {
    const o = new Orn();
    const W = h * 0.56,
      ym = h / 2,
      yj = 1.5,
      xb = 0.13 * h;
    const upper = bez([W - 0.6, yj], [0.3 * W, yj], [xb, ym - 0.36 * h], [xb, ym], 22);
    const lower = mapPts(upper, (x, y) => [x, h - y]);
    const wf = wprof([[0, 0.08 * h], [0.5, 0.092 * h], [1, 0.072 * h]]);
    o.stroke(upper, wf, "cast", "relief");
    o.stroke(lower, wf, "cast", "relief");
    if (lod >= 2) {
      for (const sg of [-1, 1]) {
        const y0 = sg < 0 ? 0.2 * h : h - 0.2 * h;
        const t = new Turtle(0.36 * W, y0, sg < 0 ? -Math.PI / 2 - 0.9 : Math.PI / 2 + 0.9);
        t.line(0.01 * h).scroll(0.066 * h, 1.1, sg < 0 ? -1 : 1, 0.42);
        o.stroke(t.pts, wprof([[0, 0.055 * h], [1, 0.02 * h]]), "cast", "relief");
      }
      o.bead(xb, ym, 0.088 * h, "cast", "relief");
    }
    if (lod >= 3) {
      for (const sg of [-1, 1]) {
        o.add(leaf(xb + 0.05 * h, ym + sg * 0.06 * h, sg * 0.75, 0.15 * h, 0.05 * h, -sg * 0.12, "cast", "relief"));
      }
    }
    return o;
  }

  // Small corner flourish for a framed box (canonical top-left, size S):
  // a C-scroll hugging the corner with both ends curling inward.
  function boxCorner(S, lod = 3) {
    const o = new Orn();
    const a = [0.95 * S, 0.12 * S],
      b = [0.12 * S, 0.95 * S];
    const c = bez(a, [0.3 * S, 0.06 * S], [0.06 * S, 0.3 * S], b, 30);
    const ea = new Turtle(a[0], a[1], 0).line(0.02 * S).scroll(0.12 * S, lod >= 2 ? 1.1 : 0.3, 1, 0.42);
    const eb = new Turtle(b[0], b[1], Math.PI / 2).line(0.02 * S).scroll(0.12 * S, lod >= 2 ? 1.1 : 0.3, -1, 0.42);
    o.stroke(join(reversePts(ea.pts), c, eb.pts), wprof([[0, 0.025 * S], [0.15, 0.06 * S], [0.5, 0.075 * S], [0.85, 0.06 * S], [1, 0.025 * S]]), "cast");
    if (lod >= 3) {
      o.add(leaf(0.24 * S, 0.24 * S, Math.PI / 4, 0.3 * S, 0.09 * S, 0, "cast"));
      o.bead(0.2 * S, 0.2 * S, 0.045 * S, "cast");
    }
    return o;
  }

  // Panel corner for large frames (canonical top-left, size S): C-scroll
  // with inward volutes and a shell fan, cast in relief.
  function cornerOrn(S, lod) {
    const o = new Orn();
    const a = [0.66 * S, 0.17 * S],
      b = [0.17 * S, 0.66 * S];
    const c = bez(a, [0.22 * S, 0.1 * S], [0.1 * S, 0.22 * S], b, 34);
    const turns = lod >= 2 ? 1.2 : 0.35;
    const ea = new Turtle(a[0], a[1], 0).line(0.02 * S).scroll(0.1 * S, turns, 1, 0.5);
    const eb = new Turtle(b[0], b[1], Math.PI / 2).line(0.02 * S).scroll(0.1 * S, turns, -1, 0.5);
    o.stroke(join(reversePts(ea.pts), c, eb.pts), wprof([[0, 0.02 * S], [0.14, 0.05 * S], [0.5, 0.065 * S], [0.86, 0.05 * S], [1, 0.02 * S]]), "cast", "relief");
    if (lod >= 3) {
      o.bead(ea.x, ea.y, 0.028 * S, "cast", "relief");
      o.bead(eb.x, eb.y, 0.028 * S, "cast", "relief");
    }
    if (lod >= 2) {
      const hub = [0.25 * S, 0.25 * S];
      const petals = lod >= 3 ? 5 : 3;
      for (let i = 0; i < petals; i++) {
        const off = i - (petals - 1) / 2;
        const ang = Math.PI / 4 + off * (lod >= 3 ? 0.36 : 0.5);
        const len = (off === 0 ? 0.28 : Math.abs(off) === 1 ? 0.23 : 0.17) * S;
        o.add(leaf(hub[0], hub[1], ang, len, 0.065 * S, -off * 0.05, "cast", "relief"));
      }
      o.bead(hub[0], hub[1], 0.045 * S, "cast", "relief");
    }
    return o;
  }

  // Hedera (ivy-leaf fleuron), centred on (cx, cy), pointing down, size s.
  function fleuron(cx, cy, s, role = "cast") {
    const o = new Orn();
    const lobe = (sg) =>
      bez([cx, cy + 0.5 * s], [cx + sg * 0.62 * s, cy + 0.1 * s], [cx + sg * 0.55 * s, cy - 0.5 * s], [cx, cy - 0.22 * s], 18);
    const right = lobe(1),
      left = reversePts(lobe(-1));
    o.fill(join(right, left), role);
    const t = new Turtle(cx, cy - 0.24 * s, -Math.PI / 2).line(0.08 * s).scroll(0.13 * s, 1.0, 1, 0.45);
    o.stroke(t.pts, wprof([[0, 0.07 * s], [1, 0.03 * s]]), role, "relief");
    return o;
  }

  // A rule with tapering arms and a fleuron or lozenge at the centre.
  function rule(w, h, centre = "fleuron") {
    const o = new Orn();
    const cy = h / 2,
      gap = h * 0.75;
    for (const sg of [-1, 1]) {
      const x0 = w / 2 + sg * gap,
        x1 = sg < 0 ? 2 : w - 2;
      o.stroke(bez([x0, cy], [x0 + sg * (w / 6), cy], [x1 - sg * (w / 6), cy], [x1, cy], 24), wprof([[0, 0.12 * h], [1, 0.02 * h]]), "run");
      const t = new Turtle(x0, cy, sg < 0 ? Math.PI - 0.6 : 0.6);
      t.line(0.04 * h).scroll(0.16 * h, 1.0, sg < 0 ? -1 : 1, 0.45);
      o.stroke(t.pts, wprof([[0, 0.08 * h], [1, 0.025 * h]]), "cast");
    }
    if (centre === "fleuron") o.add(fleuron(w / 2, cy, h * 0.85).map((x, y) => [x, y]));
    else o.bead(w / 2, cy, h * 0.16, "cast", "relief");
    return o;
  }

  // Foliage for an initial: four tendrils climb from the middle of each side
  // toward the corners and curl in, each with two leaves. Seeded by the letter.
  function versalFoliage(S, rnd) {
    const o = new Orn();
    for (const side of [-1, 1]) {
      for (const vert of [-1, 1]) {
        const dir = side * vert;
        const t = new Turtle(side < 0 ? 0.15 * S : 0.85 * S, 0.5 * S + vert * 0.04 * S, vert < 0 ? -Math.PI / 2 : Math.PI / 2);
        t.line(0.1 * S).clothoid(0.16 * S, 0, (dir * (7 + 2 * rnd())) / S).scroll(0.085 * S, 1.1, dir, 0.5);
        o.stroke(t.pts, wprof([[0, 0.028 * S], [0.6, 0.022 * S], [1, 0.01 * S]]), "grow");
        for (const f of [0.22, 0.48]) {
          const i = Math.floor(f * (t.pts.length - 1));
          const p = t.pts[i];
          const out = tangentAt(t.pts, i) - dir * (0.9 + 0.3 * rnd());
          o.add(leaf(p.x, p.y, out, 0.12 * S, 0.045 * S, -dir * 0.1, "grow"));
        }
      }
    }
    return o;
  }

  // ---------- nine-slice frames ----------
  // Frame images for CSS border-image. Corners carry the ornament; edges carry
  // moldings of constant cross-section, so stretching the edge slices shows
  // no seam, and corner and molding are one drawing, so they never misalign.

  // Molding bands as concentric rect strokes; profile lists [width, colour]
  // from the outer edge inward, starting `inset` in from the image edge.
  function bands(W, H, inset, profile, radius = 0) {
    let off = inset,
      out = "";
    for (const [w, col] of profile) {
      const a = off + w / 2;
      out += `<rect x="${r2(a)}" y="${r2(a)}" width="${r2(W - 2 * a)}" height="${r2(H - 2 * a)}" rx="${r2(Math.max(0, radius - a))}" fill="none" stroke="${col}" stroke-width="${r2(w)}"/>`;
      off += w;
    }
    return { svg: out, depth: off };
  }

  // Lighter, tonal molding: no dark outline, a bright arris, a soft hollow.
  const MAIN_PROFILE = [[1.0, "#dcbf80"], [1.2, "#fff5d6"], [2.0, "#d9b46c"], [0.9, "#c79e56"], [2.6, "#efd8a0"], [0.9, "#c9a35e"], [1.4, "#f5e3b2"], [0.8, "#cfaa63"]];
  // where the running ornament sits: the wide light band, 5th in the profile
  const TORUS = 1.5 + 1.0 + 1.2 + 2.0 + 0.9 + 1.3;

  // Scallop shell with its hinge at (hx, hy), opening along heading h.
  function shell(hx, hy, h, rad, ribs = 7, spread = 1.75) {
    const o = new Orn();
    const edge = [];
    for (let i = 0; i <= ribs * 4; i++) {
      const t = i / (ribs * 4),
        a = h - spread / 2 + spread * t;
      const bump = 1 + 0.07 * Math.abs(Math.sin(t * ribs * Math.PI));
      edge.push({ x: hx + Math.cos(a) * rad * bump, y: hy + Math.sin(a) * rad * bump, s: 0 });
    }
    o.fill([{ x: hx, y: hy, s: 0 }, ...edge], "cast");
    for (let i = 1; i < ribs; i++) {
      const a = h - spread / 2 + (spread * i) / ribs;
      const tip = { x: hx + Math.cos(a) * rad * 0.93, y: hy + Math.sin(a) * rad * 0.93 };
      o.stroke(bez([hx, hy], [hx + (tip.x - hx) / 3, hy + (tip.y - hy) / 3], [hx + (2 * (tip.x - hx)) / 3, hy + (2 * (tip.y - hy)) / 3], [tip.x, tip.y], 6), constW(Math.max(0.45, rad * 0.035)), "cast", "rib");
    }
    o.bead(hx, hy, rad * 0.14, "cast", "relief");
    return o;
  }

  // Acanthus leaf: a curving rib with lobes that curl outward, smaller toward
  // a tip that rolls over.
  function acanthus(x, y, h, len, R) {
    const o = new Orn();
    const t = new Turtle(x, y, h);
    t.clothoid(len * 0.8, 0, 0.6 / len).scroll(len * 0.11, 0.85, 1, 0.45);
    const rib = t.pts;
    o.stroke(rib, wprof([[0, 0.09 * R], [0.6, 0.07 * R], [1, 0.02 * R]]), "cast", "relief");
    [[0.18, 0.34], [0.4, 0.27], [0.62, 0.19]].forEach(([f, l]) => {
      const i = Math.floor(f * (rib.length - 1));
      const p = rib[i],
        th = tangentAt(rib, i);
      for (const sg of [-1, 1]) {
        const lt = new Turtle(p.x, p.y, th + sg * 0.95);
        lt.clothoid(len * l * 0.75, 0, sg * 1.4 / (len * l)).scroll(len * l * 0.16, 0.6, sg, 0.45);
        o.stroke(lt.pts, wprof([[0, 0.065 * R], [0.5, 0.05 * R], [1, 0.015 * R]]), "cast", "relief");
      }
    });
    return o;
  }

  // A rosette: petals round a bead, centred on (cx, cy).
  function rosette(cx, cy, r, petals = 8) {
    const o = new Orn();
    for (let i = 0; i < petals; i++) {
      const a = (i / petals) * TAU + Math.PI / petals;
      o.add(leaf(cx + Math.cos(a) * r * 0.18, cy + Math.sin(a) * r * 0.18, a, r * 0.82, r * 0.42, 0, "cast", "relief"));
    }
    o.bead(cx, cy, r * 0.26, "cast", "relief");
    return o;
  }

  // Acanthus laid along a molding: the rib follows the molding's centre line
  // from (x, y) along heading h, lobes alternate to either side and curl.
  function leafRun(x, y, h, len, wid) {
    const o = new Orn();
    const c = Math.cos(h),
      sn = Math.sin(h);
    const rib = bez([x, y], [x + c * len * 0.33, y + sn * len * 0.33], [x + c * len * 0.66, y + sn * len * 0.66], [x + c * len, y + sn * len], 20);
    o.stroke(rib, wprof([[0, wid * 0.55], [0.7, wid * 0.4], [1, wid * 0.12]]), "cast", "relief");
    const n = 4;
    for (let k = 0; k < n; k++) {
      const f = 0.12 + (k / n) * 0.78,
        sg = k % 2 ? 1 : -1;
      const px = x + c * len * f,
        py = y + sn * len * f;
      const l = len * (0.36 - 0.05 * k);
      const t = new Turtle(px, py, h + sg * 0.55);
      t.clothoid(l * 0.7, 0, (-sg * 1.6) / l).scroll(l * 0.14, 0.7, -sg, 0.45);
      o.stroke(t.pts, wprof([[0, wid * 0.5], [0.5, wid * 0.36], [1, wid * 0.08]]), "cast", "relief");
    }
    return o;
  }

  // Corner for the big frames (canonical top-left, slice C): a rosette block
  // over the molding's mitre, acanthus carved along both moldings, and a
  // pair of small scrolls turning into the mat.
  function frameCorner(C, depth, torus) {
    const o = new Orn();
    const blk = depth + 4.5,
      mid = torus ?? 1.5 + (depth - 1.5) / 2;
    const plate = [
      { x: 0.8, y: 0.8, s: 0 },
      { x: blk, y: 0.8, s: 0 },
      { x: blk, y: blk, s: 0 },
      { x: 0.8, y: blk, s: 0 },
    ];
    o.fill(plate, "cast");
    o.add(rosette(blk / 2 + 0.4, blk / 2 + 0.4, blk * 0.44, 8));
    o.add(leafRun(blk - 0.5, mid, 0, C - blk - 2.5, 3.4));
    o.add(leafRun(mid, blk - 0.5, Math.PI / 2, C - blk - 2.5, 3.4));
    const a = depth + 3.2;
    for (const sg of [0, 1]) {
      const f = sg ? (x, y) => [y, x] : (x, y) => [x, y];
      const t = new Turtle(blk + 1, a + 0.6, Math.PI / 2 - 0.35);
      t.line(0.04 * C).scroll(0.075 * C, 1.15, -1, 0.42);
      o.stroke(mapPts(t.pts, f), wprof([[0, 0.045 * C], [1, 0.016 * C]]), "cast", "relief");
    }
    return o;
  }

  // Bead and reel running along each edge's middle slice (period P), so with
  // border-image-repeat: round the whole frame is carved, not just its corners.
  function runningMotif(S, C, P, t) {
    const o = new Orn();
    const unit = (u, along) => {
      // along: map (u along the edge, v across it) to image coordinates
      o.fill(ellipse(along(u + P / 2, t), 2.6, 1.15, along === horiz ? 0 : Math.PI / 2), "grow");
      o.bead(...along(u, t), 0.95, "grow", "relief");
      o.bead(...along(u + P, t), 0.95, "grow", "relief");
    };
    const horiz = (u, v) => [u, v];
    const vert = (u, v) => [v, u];
    const bottom = (u, v) => [u, S - v];
    const right = (u, v) => [S - v, u];
    for (const f of [horiz, vert, bottom, right]) unit(C, f);
    return o;
  }
  function ellipse([cx, cy], rx, ry, rot) {
    const pts = [];
    for (let i = 0; i < 20; i++) {
      const a = (i / 20) * TAU,
        x = Math.cos(a) * rx,
        y = Math.sin(a) * ry;
      pts.push({ x: cx + x * Math.cos(rot) - y * Math.sin(rot), y: cy + x * Math.sin(rot) + y * Math.cos(rot), s: 0 });
    }
    return pts;
  }

  function frameImage(C, light, P = 14) {
    const S = 2 * C + P;
    const b = bands(S, S, 1.5, MAIN_PROFILE);
    const fillet = bands(S, S, b.depth + 3.2, [[0.7, "#d6b373"]]);
    const corner = frameCorner(C, b.depth, TORUS);
    const orn = new Orn();
    orn.add(corner);
    orn.add(corner.map((x, y) => [S - x, y]));
    orn.add(corner.map((x, y) => [x, S - y]));
    orn.add(corner.map((x, y) => [S - x, S - y]));
    orn.add(runningMotif(S, C, P, TORUS));
    return { svg: svgDoc(S, S, b.svg + fillet.svg + renderOrn(orn, { light, outline: 0.4 }), { dx: -light.x * 0.5, dy: -light.y * 0.7, blur: 0.45, opacity: 0.26 }), slice: C };
  }

  // An agrafe for the middle of a frame's top edge: a shell hinged on the
  // molding opening into the mat, flanked by C-scrolls that curl outward.
  function agrafe(cx, d, b) {
    const o = new Orn();
    const mat = b - d;
    o.add(shell(cx, d - 1.2, Math.PI / 2, mat * 0.78, 9, 2.25));
    for (const sg of [-1, 1]) {
      const t = new Turtle(cx + sg * mat * 0.42, d + mat * 0.12, sg > 0 ? 0.25 : Math.PI - 0.25);
      t.line(mat * 0.18).scroll(mat * 0.2, 1.15, sg > 0 ? 1 : -1, 0.42);
      o.stroke(t.pts, wprof([[0, 0.085 * mat], [0.4, 0.07 * mat], [1, 0.025 * mat]]), "cast", "relief");
      o.add(leaf(cx + sg * mat * 0.4, d + mat * 0.1, sg > 0 ? -0.15 : Math.PI + 0.15, mat * 0.5, mat * 0.13, sg * 0.1, "cast", "relief"));
    }
    return o;
  }

  // Corner of an exact-size frame (canonical top-left): a rosette block over
  // the molding's mitre; in the mat, a C-scroll hugging the corner with both
  // ends rolled into volutes and a fan of leaves at its middle; beyond the
  // volutes a running vine carried along each edge for `run` pixels.
  function exactCorner(b, d, run, rnd) {
    const o = new Orn();
    const blk = d + 3.4;
    o.fill([{ x: 0.8, y: 0.8, s: 0 }, { x: blk, y: 0.8, s: 0 }, { x: blk, y: blk, s: 0 }, { x: 0.8, y: blk, s: 0 }], "cast");
    o.add(rosette(blk / 2 + 0.4, blk / 2 + 0.4, blk * 0.42, 8));
    const mat = b - d,
      mc = d + mat / 2;
    const reach = blk + 1.25 * mat;
    const A = [reach, d + 0.32 * mat],
      B = [d + 0.32 * mat, reach];
    const k = d + 0.3 * mat;
    const curve = bez(A, [k + 0.25 * mat, k - 0.05 * mat], [k - 0.05 * mat, k + 0.25 * mat], B, 30);
    const va = new Turtle(A[0], A[1], 0).line(0.05 * mat).scroll(0.2 * mat, 1.2, 1, 0.42);
    const vb = new Turtle(B[0], B[1], Math.PI / 2).line(0.05 * mat).scroll(0.2 * mat, 1.2, -1, 0.42);
    o.stroke(join(reversePts(va.pts), curve, vb.pts), wprof([[0, 0.03 * mat], [0.16, 0.085 * mat], [0.5, 0.11 * mat], [0.84, 0.085 * mat], [1, 0.03 * mat]]), "cast", "relief");
    o.bead(va.x, va.y, 0.05 * mat, "cast", "relief");
    o.bead(vb.x, vb.y, 0.05 * mat, "cast", "relief");
    // leaf fan from the middle of the C, opening into the content's corner
    const hub = [k + 0.1 * mat, k + 0.1 * mat];
    [-0.62, -0.31, 0, 0.31, 0.62].forEach((off, i) => {
      const len = (i === 2 ? 0.62 : Math.abs(off) < 0.4 ? 0.52 : 0.4) * mat;
      o.add(leaf(hub[0], hub[1], Math.PI / 4 + off, len, 0.17 * mat, -off * 0.12, "cast", "relief"));
    });
    o.bead(hub[0], hub[1], 0.07 * mat, "cast", "relief");
    const R = mat * 0.82;
    const v = rinceau(mc, reach + 0.55 * mat, Math.max(reach + 1.6 * mat, blk + run), R, 3, rnd);
    o.add(v);
    o.add(v.map((x, y) => [y, x]));
    return o;
  }

  // A bead-and-reel strip on the molding's torus from u0 to u1, as an SVG
  // pattern whose pitch is re-solved so a whole number of units fits.
  function beadRun(id, horizontal, u0, u1, t, light, P0 = 13) {
    const len = u1 - u0;
    if (len < P0) return { defs: "", rect: "" };
    const n = Math.max(1, Math.round(len / P0)),
      P = len / n;
    const unit = new Orn();
    const at = (u, v) => (horizontal ? [u, v] : [v, u]);
    unit.fill(ellipse(at(P / 2, 1.8), 2.5, 1.1, horizontal ? 0 : Math.PI / 2), "grow");
    unit.bead(...at(0, 1.8), 0.95, "grow", "relief");
    unit.bead(...at(P, 1.8), 0.95, "grow", "relief");
    const [px, py] = horizontal ? [u0, t - 1.8] : [t - 1.8, u0];
    const [pw, ph] = horizontal ? [P, 3.6] : [3.6, P];
    const defs = `<pattern id="${id}" patternUnits="userSpaceOnUse" x="${r2(px)}" y="${r2(py)}" width="${r2(pw)}" height="${r2(ph)}">${renderOrn(unit, { light, outline: 0.3 })}</pattern>`;
    const [rw, rh] = horizontal ? [len, 3.6] : [3.6, len];
    return { defs, rect: `<rect x="${r2(px)}" y="${r2(py)}" width="${r2(rw)}" height="${r2(rh)}" fill="url(#${id})"/>` };
  }

  // A whole frame drawn for one element at its exact size W x H, with a
  // border band b wide: molding, corners, agrafes and regrown bead runs.
  // Used as border-image at slice b, so it is never stretched or tiled.
  function frameExact(W, H, b, light, seed = 7) {
    const m = bands(W, H, 1.5, MAIN_PROFILE);
    const d = m.depth;
    const lines = bands(W, H, d + 2, [[0.6, "#d8b878"]]).svg + bands(W, H, b - 1.6, [[0.6, "#dcc086"]]).svg;
    const run = Math.min(0.2 * Math.min(W, H), 140);
    const rnd = mulberry(seed);
    const corner = exactCorner(b, d, run, rnd);
    const orn = new Orn();
    orn.add(corner);
    orn.add(corner.map((x, y) => [W - x, y]));
    orn.add(corner.map((x, y) => [x, H - y]));
    orn.add(corner.map((x, y) => [W - x, H - y]));
    const gap = (b - d) * 1.25;
    const topAg = W > 320,
      sideAg = H > 320;
    if (topAg) {
      const a = agrafe(W / 2, d, b);
      orn.add(a);
      orn.add(a.map((x, y) => [x, H - y]));
    }
    if (sideAg) {
      const a = agrafe(H / 2, d, b).map((x, y) => [y, x]);
      orn.add(a);
      orn.add(a.map((x, y) => [W - x, y]));
    }
    const t = TORUS;
    const blk = d + 3.4;
    let defs = "",
      rects = "";
    const strip = (id, horizontal, len, hasAg, far) => {
      const segs = hasAg ? [[blk, len / 2 - gap], [len / 2 + gap, len - blk]] : [[blk, len - blk]];
      segs.forEach(([u0, u1], i) => {
        const r = beadRun(`${id}${i}`, horizontal, u0, u1, far ? (horizontal ? H - t : W - t) : t, light);
        defs += r.defs;
        rects += r.rect;
      });
    };
    strip("bt", true, W, topAg, false);
    strip("bb", true, W, topAg, true);
    strip("bl", false, H, sideAg, false);
    strip("br", false, H, sideAg, true);
    const body = `<defs>${defs}</defs>` + m.svg + rects + lines + renderOrn(orn, { light });
    return svgDoc(W, H, body, { dx: -light.x * 0.45, dy: -light.y * 0.65, blur: 0.4, opacity: 0.24 });
  }

  // Box frame for panels (canonical slice C): a double line whose outer line
  // runs into each corner and curls inward there, with a leaf between the
  // two curls; the inner line turns the corner on a small radius.
  function boxImage(C, light, ink = { line: "#c79e58", inner: "#dcc086" }) {
    const S = 2 * C + 2;
    const o0 = 1.6;
    let svg = "";
    // outer line, straight between the corner curls
    const L = 0.62 * C;
    svg += `<path d="M${r2(L)} ${o0}H${r2(S - L)}M${r2(L)} ${r2(S - o0)}H${r2(S - L)}M${o0} ${r2(L)}V${r2(S - L)}M${r2(S - o0)} ${r2(L)}V${r2(S - L)}" stroke="${ink.line}" stroke-width="1.1" fill="none"/>`;
    const inner = bands(S, S, o0 + 2.6, [[0.65, ink.inner]], 0.45 * C);
    svg += inner.svg;
    const corner = new Orn();
    const top = new Turtle(L, o0, Math.PI).line(0.1 * C).scroll(0.17 * C, 1.1, -1, 0.42);
    const side = new Turtle(o0, L, -Math.PI / 2).line(0.1 * C).scroll(0.17 * C, 1.1, 1, 0.42);
    corner.stroke(top.pts, wprof([[0, 1.1], [0.4, 0.075 * C], [1, 0.03 * C]]), "cast");
    corner.stroke(side.pts, wprof([[0, 1.1], [0.4, 0.075 * C], [1, 0.03 * C]]), "cast");
    corner.add(leaf(0.27 * C, 0.27 * C, -3 * Math.PI / 4, 0.24 * C, 0.1 * C, 0, "cast"));
    corner.add(leaf(0.27 * C, 0.27 * C, Math.PI / 4, 0.2 * C, 0.08 * C, 0, "cast"));
    corner.bead(0.27 * C, 0.27 * C, 0.055 * C, "cast", "relief");
    const orn = new Orn();
    orn.add(corner);
    orn.add(corner.map((x, y) => [S - x, y]));
    orn.add(corner.map((x, y) => [x, S - y]));
    orn.add(corner.map((x, y) => [S - x, S - y]));
    return { svg: svgDoc(S, S, svg + renderOrn(orn, { light }), { dx: -light.x * 0.35, dy: -light.y * 0.5, blur: 0.3, opacity: 0.22 }), slice: C };
  }

  // One-piece cartouche for a probe sample, height h, tinted `tint`: the tint
  // fills to the inner edge of both brackets, moldings run unbroken between
  // the caps, and the caps are drawn in the same image so nothing seams.
  function cartImage(h, tint, light, lod = 3) {
    const W = Math.round(h * 0.56),
      S = 2 * W + 4;
    const cap = capOrn(h, lod);
    const right = cap.map((x, y) => [S - x, y]);
    const upper = cap.strokes[0].pts,
      lower = cap.strokes[1].pts;
    const edge = (pts, f) => pts.map((p) => f(p.x, p.y));
    // left cap top half, then its bottom half back out, then the right cap
    // the same way round, so the tint is one simple polygon
    const ring = [
      ...edge(upper, (x, y) => [x, y]),
      ...edge(lower.slice().reverse(), (x, y) => [x, y]),
      ...edge(lower, (x, y) => [S - x, y]),
      ...edge(upper.slice().reverse(), (x, y) => [S - x, y]),
    ];
    const tintPath = "M" + ring.map(([x, y]) => r2(Math.min(Math.max(x, 0), S)) + " " + r2(y)).join("L") + "Z";
    const yj = 1.5;
    const mold = (y, dir) => {
      const prof = [[0.7, "#c9a15a"], [0.8, "#fff4d4"], [0.8, "#d4ac62"], [0.7, "rgba(160,95,45,0.25)"]];
      let off = 0,
        out = "";
      for (const [w, col] of prof) {
        const yy = y + dir * (off + w / 2);
        out += `<path d="M${r2(0.3 * W)} ${r2(yy)}H${r2(S - 0.3 * W)}" stroke="${col}" stroke-width="${w}"/>`;
        off += w;
      }
      return out;
    };
    const shade = `<defs><linearGradient id="cs" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#5a3a10" stop-opacity="0.12"/><stop offset="0.3" stop-color="#5a3a10" stop-opacity="0"/><stop offset="0.75" stop-color="#ffffff" stop-opacity="0"/><stop offset="1" stop-color="#ffffff" stop-opacity="0.22"/></linearGradient></defs>`;
    const body =
      shade +
      `<path d="${tintPath}" fill="${tint}"/><path d="${tintPath}" fill="url(#cs)"/>` +
      mold(yj - 0.75, 1) +
      mold(h - yj + 0.75, -1) +
      renderOrn(new Orn().add(cap).add(right), { light, outline: 0.42 });
    return { svg: svgDoc(S, h, body, { dx: -light.x * 0.3, dy: -light.y * 0.45, blur: 0.3, opacity: 0.25 }), slice: W };
  }

  // An SVG document for use as a CSS background image.
  function svgDoc(w, h, body, shadow) {
    const f = shadow
      ? `<filter id="d" x="-30%" y="-30%" width="160%" height="160%"><feDropShadow dx="${r2(shadow.dx)}" dy="${r2(shadow.dy)}" stdDeviation="${shadow.blur ?? 0.3}" flood-color="${shadow.color ?? "#5a3a0c"}" flood-opacity="${shadow.opacity ?? 0.3}"/></filter>`
      : "";
    return `<svg xmlns="http://www.w3.org/2000/svg" width="${r2(w)}" height="${r2(h)}" viewBox="0 0 ${r2(w)} ${r2(h)}">${f}<g${shadow ? ' filter="url(#d)"' : ""}>${body}</g></svg>`;
  }
  function cssURL(svg) {
    return `url("data:image/svg+xml,${encodeURIComponent(svg)}")`;
  }

  root.Orn = {
    TAU, Turtle, Orn, bez, join, reversePts, endOf, tangentAt, mapPts, wprof, constW, ribbon, renderOrn, mulberry, hashStr,
    PALETTE, ROLE_COLORS, leaf, budTip, loopBud, spur, arm, curlyBrace, rinceau, vine, capOrn, boxCorner, cornerOrn, fleuron, rule,
    versalFoliage, svgDoc, cssURL, bands, shell, acanthus, rosette, leafRun, frameCorner, runningMotif, frameImage, boxImage, cartImage, agrafe, exactCorner, beadRun, frameExact,
  };
})(typeof window !== "undefined" ? window : globalThis);
