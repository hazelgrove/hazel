// Baroque skin. Sets html.skin-baroque, grows the ornament the stylesheet
// uses (SVG images in custom properties) and answers the editor's requests
// for braces beside multi-line terms (HazelSkin.braceSVG, called from
// SkinDec.re). Ctrl+Alt+B toggles it; the choice is kept in localStorage.
(function () {
  "use strict";
  const O = window.Orn;
  const KEY = "HAZEL_SKIN";
  const root = document.documentElement;
  const light = { x: -0.707, y: -0.707 };

  let on = true;
  try {
    const saved = localStorage.getItem(KEY);
    if (saved !== null) on = saved === "baroque";
  } catch (e) {}

  const shadow = (k = 1) => ({ dx: -light.x * 0.45 * k, dy: -light.y * 0.6 * k, blur: 0.35, opacity: 0.3 });

  // ---------- braces beside multi-line terms ----------

  const braces = new Map();
  // x: the block's text column in px; y0..y1: its rows; kids: the y centres
  // of its child rows; depth: how many blocks enclose it.
  function braceSVG(x, y0, y1, R, C, kids, depth) {
    const key = `${x}|${y0}|${y1}|${R}|${C}|${depth}|${Array.from(kids).join(",")}`;
    const hit = braces.get(key);
    if (hit !== undefined) return hit;
    const rows = Math.round((y1 - y0) / R);
    const lod = depth <= 1 ? 3 : depth === 2 ? 2 : 1;
    const rnd = O.mulberry(O.hashStr(key));
    const orn = new O.Orn();
    const yt = y0 + 0.12 * R,
      yb = y1 - 0.12 * R;
    if (kids.length >= 2) orn.add(O.vine(x - 0.12 * C, Array.from(kids), R, lod, rnd, -12));
    else if (rows <= 4) orn.add(O.curlyBrace(x - 0.15 * C - 0.4 * R, yt + 0.04 * R, yb - 0.04 * R, R, lod));
    else orn.add(O.rinceau(x - 0.15 * C - 0.42 * R, yt, yb, R, lod, rnd));
    const svg = `<g class="orn-g">${O.renderOrn(orn, { light })}</g>`;
    if (braces.size > 800) braces.clear();
    braces.set(key, svg);
    return svg;
  }

  // The carved base under the lens apparatus in the Probearium.
  function pedestal(W, H) {
    const o = new O.Orn();
    const cx = W / 2,
      base = H - 7;
    for (const sg of [-1, 1]) {
      const f = (x, y) => [cx + sg * (x - cx), y];
      const t = new O.Turtle(cx + 9, base, -0.12);
      t.line(W * 0.17).clothoid(W * 0.12, 0, -1 / (H * 0.45)).scroll(H * 0.2, 1.2, -1, 0.45);
      o.stroke(O.mapPts(t.pts, f), O.wprof([[0, 3.0], [0.5, 2.4], [1, 0.8]]), "cast", "relief");
      o.add(O.acanthus(cx + 12, base - 3, -0.55, W * 0.2, 13).map(f));
      const v = new O.Turtle(cx + 7, base - 1, -Math.PI / 2 + 0.35);
      v.line(3).scroll(4.5, 1.1, 1, 0.45);
      o.stroke(O.mapPts(v.pts, f), O.wprof([[0, 1.8], [1, 0.6]]), "cast", "relief");
    }
    o.add(O.shell(cx, base + 1, -Math.PI / 2, 14, 9, 2.3));
    return o;
  }

  // ---------- images for the stylesheet ----------

  function setImg(name, w, h, orn, k) {
    const svg = O.svgDoc(w, h, O.renderOrn(orn, { light }), shadow(k));
    root.style.setProperty(name, O.cssURL(svg));
  }

  function parchment() {
    const N = 256,
      c = document.createElement("canvas");
    c.width = c.height = N;
    const g = c.getContext("2d"),
      img = g.createImageData(N, N),
      rnd = O.mulberry(11);
    const oct = [[4, 0.42], [8, 0.26], [16, 0.15], [32, 0.09], [64, 0.05], [128, 0.03]];
    const grids = oct.map(([n]) => Float32Array.from({ length: n * n }, () => rnd()));
    for (let y = 0; y < N; y++)
      for (let x = 0; x < N; x++) {
        let v = 0;
        oct.forEach(([n, amp], k) => {
          const fx = (x / N) * n,
            fy = (y / N) * n,
            x0 = Math.floor(fx),
            y0 = Math.floor(fy);
          let tx = fx - x0,
            ty = fy - y0;
          tx = tx * tx * (3 - 2 * tx);
          ty = ty * ty * (3 - 2 * ty);
          const G = grids[k],
            at = (i, j) => G[(j % n) * n + (i % n)];
          v += amp * ((at(x0, y0) * (1 - tx) + at(x0 + 1, y0) * tx) * (1 - ty) + (at(x0, y0 + 1) * (1 - tx) + at(x0 + 1, y0 + 1) * tx) * ty);
        });
        const t = Math.min(1, Math.max(0, (v - 0.32) * 1.5)) + (rnd() - 0.5) * 0.05;
        const i = (y * N + x) * 4;
        img.data[i] = 248 - 18 * t;
        img.data[i + 1] = 238 - 25 * t;
        img.data[i + 2] = 210 - 36 * t;
        img.data[i + 3] = 255;
      }
    g.putImageData(img, 0, 0);
    root.style.setProperty("--orn-parch", `url(${c.toDataURL()})`);
  }

  // Theme colours can be any CSS colour (oklch, color-mix); a canvas
  // resolves them to rgb so they are safe inside SVG images.
  const probe = document.createElement("canvas").getContext("2d", { willReadFrequently: true });
  function resolve(varName, fallback) {
    const el = document.createElement("span");
    el.style.color = `var(${varName}, ${fallback})`;
    el.style.display = "none";
    document.documentElement.appendChild(el);
    const c = getComputedStyle(el).color;
    el.remove();
    probe.clearRect(0, 0, 1, 1);
    probe.fillStyle = fallback;
    probe.fillStyle = c;
    probe.fillRect(0, 0, 1, 1);
    const [r, g, b, a] = probe.getImageData(0, 0, 1, 1).data;
    return `rgba(${r},${g},${b},${(a / 255).toFixed(3)})`;
  }

  function setURL(name, svg) {
    root.style.setProperty(name, O.cssURL(svg));
  }

  function images() {
    // frames for the slide pane and the sidebar, and boxes for panels: one
    // image each, sliced by border-image so corners and moldings never part
    const box = O.boxImage(20, light);
    setURL("--orn-box", box.svg);
    root.style.setProperty("--orn-box-slice", box.slice);
    // the old corner images still serve the sidebar headings' rule
    setImg("--orn-rule", 180, 14, O.rule(180, 14));
    setImg("--orn-fleuron", 16, 16, O.fleuron(8, 8, 13));
    setImg("--orn-pedestal", 230, 56, pedestal(230, 56), 1.3);
    cartouches();
  }

  // One cartouche image per sample state, tinted with the theme's colours.
  function cartouches() {
    const states = {
      neutral: ["--sample-neutral-bg", "#eee6d2"],
      focus: ["--exp-base", "#d9efd2"],
      "focus-pat": ["--pat-base", "#dcecf6"],
      before: ["--sample-above-bg", "#f6cdd2"],
      after: ["--sample-below-bg", "#cfe1ec"],
      off: ["--sample-off-bg", "#ebe4d2"],
    };
    const tints = Object.entries(states).map(([k, [v, fb]]) => [k, resolve(v, fb)]);
    const key = JSON.stringify(tints);
    if (key === tintKey) return; // our own writes also wake the observer below
    tintKey = key;
    let slice = 13;
    for (const [k, tint] of tints) {
      const c = O.cartImage(24, tint, light);
      setURL(`--orn-cart-${k}`, c.svg);
      slice = c.slice;
    }
    root.style.setProperty("--orn-cart-slice", slice);
  }
  let tintKey = "";

  // Hazel writes the colour scheme onto <html> after startup; retint then.
  let retint = 0;
  new MutationObserver(() => {
    clearTimeout(retint);
    retint = setTimeout(cartouches, 120);
  }).observe(root, { attributes: true, attributeFilter: ["style"] });

  // ---------- frames drawn at each pane's exact size ----------
  // The slide pane and the sidebar each get one frame image matching their
  // border box, used as border-image at slice = border width, so it is never
  // stretched or tiled and its bead runs are regrown to fit.
  const BAND = { main: 34, side: 26 };
  const panes = [
    { sel: "#main", var: "--orn-frame-main", band: BAND.main, el: null, key: "" },
    { sel: "#side-bar", var: "--orn-frame-side", band: BAND.side, el: null, key: "" },
  ];
  let framePending = 0;
  function drawFrames() {
    framePending = 0;
    for (const p of panes) {
      const el = p.el;
      if (!el || !el.isConnected) continue;
      const w = el.offsetWidth,
        h = el.offsetHeight;
      if (w < 2 * p.band + 10 || h < 2 * p.band + 10) continue;
      const key = `${w}x${h}`;
      if (key === p.key) continue;
      p.key = key;
      setURL(p.var, O.frameExact(w, h, p.band, light, p.band));
    }
  }
  const resizeObs = new ResizeObserver(() => {
    if (!framePending) framePending = requestAnimationFrame(drawFrames);
  });
  // Hazel can replace these elements when it re-renders; re-attach then
  function attachFrames() {
    for (const p of panes) {
      const el = document.querySelector(p.sel);
      if (el && el !== p.el) {
        if (p.el) resizeObs.unobserve(p.el);
        p.el = el;
        p.key = "";
        resizeObs.observe(el);
      }
    }
  }
  setInterval(attachFrames, 700);
  root.style.setProperty("--orn-band-main", BAND.main + "px");
  root.style.setProperty("--orn-band-side", BAND.side + "px");

  // ---------- switching ----------

  function apply() {
    root.classList.toggle("skin-baroque", on);
    window.HazelSkin.on = on;
    // decorations are drawn by the editor; a resize makes it redraw them
    window.dispatchEvent(new Event("resize"));
  }

  window.HazelSkin = {
    on,
    braceSVG,
    toggle() {
      on = !on;
      try {
        localStorage.setItem(KEY, on ? "baroque" : "off");
      } catch (e) {}
      apply();
    },
  };

  parchment();
  images();
  root.classList.toggle("skin-baroque", on);
  window.addEventListener("keydown", (e) => {
    if (e.ctrlKey && e.altKey && e.code === "KeyB") {
      e.preventDefault();
      e.stopPropagation();
      window.HazelSkin.toggle();
    }
  }, true);
})();
