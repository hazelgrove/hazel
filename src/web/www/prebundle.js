
// This file is bundled into bundle.js as part of the build process.
import {NinjaKeys} from 'ninja-keys';
import hotkeys from 'hotkeys-js'
import Algebrite from 'algebrite';
window.Algebrite = Algebrite;

// D3 owns the DOM for the Chart projector: ChartProj.re marshals a chart spec
// to a plain object and drives window.HazelD3.render through an Attr hook. The
// spec is { kind: "bar"|"line"|"scatter"|"pie", ...fields }.
import * as d3Scale from 'd3-scale';
import * as d3Array from 'd3-array';
import * as d3Selection from 'd3-selection';
import * as d3Axis from 'd3-axis';
import * as d3Shape from 'd3-shape';
const d3 = Object.assign({}, d3Scale, d3Array, d3Selection, d3Axis, d3Shape);
window.d3 = d3;

const CHART = { W: 320, H: 220, M: { top: 12, right: 14, bottom: 30, left: 38 } };
const IW = CHART.W - CHART.M.left - CHART.M.right;
const IH = CHART.H - CHART.M.top - CHART.M.bottom;

// Series colors come from CSS custom properties (--chart-1..8) so charts track
// Hazel's theme; fall back to a fixed palette if they're unset.
const FALLBACK_PALETTE = [
  "#4e79a7", "#f28e2b", "#59a14f", "#e15759",
  "#edc948", "#b07aa1", "#76b7b2", "#ff9da7",
];
let COLORS = FALLBACK_PALETTE;
const color = i => COLORS[i % COLORS.length];
function chartColors(el) {
  const cs = getComputedStyle(el);
  const out = [];
  for (let i = 1; i <= 8; i++) {
    const v = cs.getPropertyValue(`--chart-${i}`).trim();
    if (v) out.push(v);
  }
  return out.length ? out : FALLBACK_PALETTE;
}

// A linear domain, padding a zero-width span so degenerate data still draws.
function domain(values) {
  let lo = Math.min(...values), hi = Math.max(...values);
  return lo === hi ? [lo - 1, hi + 1] : [lo, hi];
}
function fmt(v) {
  return Number.isInteger(v) ? String(v) : String(Math.round(v * 1e4) / 1e4);
}
function emptyChart(svg) {
  svg.append("text").attr("x", CHART.W / 2).attr("y", CHART.H / 2)
    .attr("class", "chart-empty").text("no data");
}

// Horizontal legend centered along the bottom edge (for multi-series bars).
function legendRow(svg, items) {
  const sw = 7, gap = 10, ly = CHART.H - 5;
  const widths = items.map(it => sw + 3 + it.name.length * 5.4 + gap);
  const total = widths.reduce((a, b) => a + b, 0) - gap;
  let lx = Math.max(2, (CHART.W - total) / 2);
  const g = svg.append("g");
  items.forEach((it, i) => {
    g.append("rect").attr("x", lx).attr("y", ly - 7)
      .attr("width", sw).attr("height", sw).attr("rx", 1.5).attr("fill", it.c);
    g.append("text").attr("x", lx + sw + 3).attr("y", ly)
      .attr("class", "chart-legend").text(it.name);
    lx += widths[i];
  });
}

function renderBar(svg, spec) {
  const cats = spec.categories || [], series = spec.series || [];
  if (!cats.length || !series.length) return emptyChart(svg);
  const x = d3.scaleBand().domain(cats.map((_, i) => i))
    .range([CHART.M.left, CHART.M.left + IW]).padding(0.2);
  const sub = d3.scaleBand().domain(series.map((_, i) => i))
    .range([0, x.bandwidth()]).padding(0.05);
  const all = [0];
  series.forEach(s => s.values.forEach(v => all.push(v)));
  const y = d3.scaleLinear().domain([Math.min(...all), Math.max(...all)]).nice()
    .range([CHART.M.top + IH, CHART.M.top]);
  const y0 = y(0);
  svg.append("g").attr("class", "d3-axis d3-grid")
    .attr("transform", `translate(${CHART.M.left},0)`)
    .call(d3.axisLeft(y).ticks(4).tickSize(-IW));
  svg.append("g").attr("class", "d3-axis")
    .attr("transform", `translate(0,${CHART.M.top + IH})`)
    .call(d3.axisBottom(x).tickFormat((_, i) => cats[i]).tickSize(0));
  const multi = series.length > 1;
  // Single series: color per category (decorative — the category is already
  // the x-axis). Multiple series: color per series, identified by a legend.
  const showLabels = !multi && cats.length <= 12; // else too cramped
  series.forEach((s, si) => {
    const g = svg.append("g");
    g.selectAll("rect").data(s.values).enter().append("rect")
      .attr("class", "chart-mark")
      .attr("x", (_, i) => x(i) + sub(si))
      .attr("y", d => Math.min(y(d), y0))
      .attr("width", sub.bandwidth())
      .attr("height", d => Math.abs(y(d) - y0))
      .attr("rx", 1.5)
      .attr("fill", (_, i) => color(multi ? si : i))
      .append("title")
      .text((d, i) => (multi ? `${s.name} · ` : "") + `${cats[i]}: ${fmt(d)}`);
    if (showLabels) {
      g.selectAll("text").data(s.values).enter().append("text")
        .attr("class", "chart-value")
        .attr("x", (_, i) => x(i) + sub(si) + sub.bandwidth() / 2)
        .attr("y", d => Math.min(y(d), y0) - 2)
        .text(d => fmt(d));
    }
  });
  if (multi) {
    legendRow(svg, series.map((s, i) => ({ name: s.name, c: color(i) })));
  }
}

function renderXY(svg, spec, connect) {
  const points = spec.points || [];
  if (!points.length) return emptyChart(svg);
  const x = d3.scaleLinear().domain(domain(points.map(p => p.x)))
    .range([CHART.M.left, CHART.M.left + IW]);
  const y = d3.scaleLinear().domain(domain(points.map(p => p.y))).nice()
    .range([CHART.M.top + IH, CHART.M.top]);
  svg.append("g").attr("class", "d3-axis d3-grid")
    .attr("transform", `translate(${CHART.M.left},0)`)
    .call(d3.axisLeft(y).ticks(4).tickSize(-IW));
  svg.append("g").attr("class", "d3-axis")
    .attr("transform", `translate(0,${CHART.M.top + IH})`)
    .call(d3.axisBottom(x).ticks(4));
  if (connect) {
    const line = d3.line().x(p => x(p.x)).y(p => y(p.y));
    svg.append("path").attr("class", "chart-line")
      .attr("d", line(points)).attr("fill", "none").attr("stroke", color(0));
  }
  svg.append("g").selectAll("circle").data(points).enter().append("circle")
    .attr("class", "chart-mark")
    .attr("cx", p => x(p.x)).attr("cy", p => y(p.y)).attr("r", 2.75)
    .attr("fill", color(0))
    .append("title").text(p => `(${fmt(p.x)}, ${fmt(p.y)})`);
}

function renderPie(svg, spec) {
  const slices = (spec.slices || []).map(s => ({ label: s.label, value: Math.max(0, s.value) }));
  const total = d3.sum(slices, s => s.value);
  if (total <= 0) return emptyChart(svg);
  const r = Math.min(IH / 2 - 2, 74);
  const cx = 8 + r, cy = CHART.M.top + IH / 2;
  const pie = d3.pie().sort(null).value(s => s.value);
  const arc = d3.arc().innerRadius(0).outerRadius(r);
  svg.append("g").attr("transform", `translate(${cx},${cy})`)
    .selectAll("path").data(pie(slices)).enter().append("path")
    .attr("class", "chart-mark")
    .attr("d", arc).attr("fill", (_, i) => color(i))
    .append("title")
    .text(d =>
      `${d.data.label}: ${fmt(d.data.value)} `
      + `(${Math.round(d.data.value / total * 100)}%)`);
  // Legend (the only place slice labels/values appear, since a pie has no
  // axis). Long entries are truncated to stay within the viewBox width.
  const lx = cx + r + 12, rowH = 14;
  const maxChars = Math.max(4, Math.floor((CHART.W - lx - 14) / 5.4));
  const truncate = str =>
    str.length > maxChars ? str.slice(0, maxChars - 1) + "…" : str;
  const y0 = cy - rowH * slices.length / 2 + rowH / 2;
  const legend = svg.append("g");
  slices.forEach((s, i) => {
    const ly = y0 + rowH * i;
    legend.append("rect").attr("x", lx).attr("y", ly - 6)
      .attr("width", 8).attr("height", 8).attr("rx", 1.5).attr("fill", color(i));
    legend.append("text").attr("x", lx + 12).attr("y", ly + 2)
      .attr("class", "chart-legend")
      .text(truncate(`${s.label}: ${fmt(s.value)}`))
      .append("title").text(`${s.label}: ${fmt(s.value)}`);
  });
}

window.HazelD3 = {
  render(el, spec) {
    COLORS = chartColors(el);
    const root = d3.select(el);
    root.selectAll("*").remove();
    const svg = root.append("svg")
      .attr("viewBox", `0 0 ${CHART.W} ${CHART.H}`)
      .attr("preserveAspectRatio", "xMidYMid meet")
      .attr("class", `chart-svg chart-${spec.kind}`);
    switch (spec.kind) {
      case "bar": return renderBar(svg, spec);
      case "line": return renderXY(svg, spec, true);
      case "scatter": return renderXY(svg, spec, false);
      case "pie": return renderPie(svg, spec);
      default: return emptyChart(svg);
    }
  },
};

// This is the default behavior for the hotkeys module but I'm overriding it for the
// clipboard-shim and the ninja-keys command palette (which lives inside a shadow DOM).
hotkeys.filter = event => {
  // composedPath() lets us see the original target even when the event has been
  // retargeted across a shadow DOM boundary (e.g. the <input> inside ninja-keys).
  const path = typeof event.composedPath === 'function' ? event.composedPath() : [];
  const target = event.target || event.srcElement;
  const { tagName, id } = target;

  // Override happening here
  if(id == "clipboard-shim") {
    return true;
  }

  // When the event originates inside the ninja-keys command palette, only let
  // its own navigation/close keys through. This stops globally-registered action
  // hotkeys (e.g. Cmd+A for "Select All") from firing while the user is typing
  // in the palette's search box, while still letting Esc close the palette and
  // the arrow/enter keys navigate it.
  const inNinjaKeys = path.some(el => el && el.tagName === 'NINJA-KEYS');
  if (inNinjaKeys) {
    return ['Escape', 'Enter', 'ArrowUp', 'ArrowDown', 'Backspace', 'Tab'].includes(event.key);
  }

  let flag = true;
  const isInput = tagName === 'INPUT' && !['checkbox', 'radio', 'range', 'button', 'file', 'reset', 'submit', 'color'].includes(target.type);
  // ignore: isContentEditable === 'true', <input> and <textarea> when readOnly state is false, <select>
  if (
    target.isContentEditable
    || ((isInput || tagName === 'TEXTAREA' || tagName === 'SELECT') && !target.readOnly)
  ) {
    flag = false;
  }
  return flag;
  };
