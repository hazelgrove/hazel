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
  function braceSVG(x, y0, y1, R, C, cls) {
    const key = `${x}|${y0}|${y1}|${R}|${C}|${cls}`;
    const hit = braces.get(key);
    if (hit !== undefined) return hit;
    const rows = Math.round((y1 - y0) / R);
    const xa = x - 0.25 * C; // Hazel's arm runs a little left of the content column
    const yt = y0 + 0.12 * R,
      yb = y1 - 0.12 * R;
    const lod = rows >= 3 ? 3 : 2;
    const rnd = O.mulberry(O.hashStr(key));
    const orn = new O.Orn();
    const xs = xa - 0.55 * R;
    if (rows <= 4) orn.add(O.curlyBrace(xs, yt + 0.04 * R, yb - 0.04 * R, R, lod));
    else orn.add(O.rinceau(xs, yt, yb, R, lod, rnd));
    const kind = /indicated/.test(cls) ? " orn-indicated" : "";
    const svg = `<g class="orn-g${kind}">${O.renderOrn(orn, { light })}</g>`;
    if (braces.size > 600) braces.clear();
    braces.set(key, svg);
    return svg;
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

  function images() {
    // cartouche caps for probe samples; vector, so one size serves any zoom
    const h = 24,
      W = h * 0.56;
    const cap = O.capOrn(h, 3);
    setImg("--orn-cap-l", W, h, cap);
    setImg("--orn-cap-r", W, h, cap.map((x, y) => [W - x, y]));
    // small corner flourishes for framed boxes, each drawn in place so the
    // light stays put instead of being mirrored
    const S = 22,
      c = O.boxCorner(S, 3);
    setImg("--orn-box-tl", S, S, c);
    setImg("--orn-box-tr", S, S, c.map((x, y) => [S - x, y]));
    setImg("--orn-box-bl", S, S, c.map((x, y) => [x, S - y]));
    setImg("--orn-box-br", S, S, c.map((x, y) => [S - x, S - y]));
    // larger cast corners for the editor and sidebar frames
    const F = 58,
      f = O.cornerOrn(F, 3);
    setImg("--orn-frame-tl", F, F, f, 1.4);
    setImg("--orn-frame-tr", F, F, f.map((x, y) => [F - x, y]), 1.4);
    setImg("--orn-frame-bl", F, F, f.map((x, y) => [x, F - y]), 1.4);
    setImg("--orn-frame-br", F, F, f.map((x, y) => [F - x, F - y]), 1.4);
    // a fleuron rule for headings
    setImg("--orn-rule", 180, 14, O.rule(180, 14));
    setImg("--orn-fleuron", 16, 16, O.fleuron(8, 8, 13));
  }

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
