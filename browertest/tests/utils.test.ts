import { describe, it, expect } from 'vitest';
import { parseCodeString, findSpanByRowCol, findSpanByQuery } from '../src/utils';

describe('parseCodeString', () => {
  const codeString = `
    <span class="token Exp poly">let</span><span class="whitespace"> </span>
    <span class="token Pat mono">comb</span><span class="whitespace"> </span>
    <span class="token Exp poly">=</span><span class="whitespace"> </span>
    <span class="linebreak"></span>
    <span class="whitespace"> </span><span class="whitespace"> </span>
    <span class="token Exp poly">fun</span><span class="whitespace"> </span>
    <span class="token Pat mono">x</span><span class="token Pat mono">,</span>
    <span class="whitespace"> </span><span class="token Pat mono">y</span><span class="whitespace"> </span>
    <span class="token Exp poly">-&gt;</span><span class="linebreak"></span>
    <span class="whitespace"> </span><span class="whitespace"> </span>
    <span class="token Exp poly">if</span><span class="whitespace"> </span>
    <span class="token Exp poly">(</span><span class="token Exp mono">y</span><span class="whitespace"> </span>
    <span class="token Exp mono">&lt;</span><span class="whitespace"> </span><span class="token Exp mono">5</span>
    <span class="token Exp poly">)</span><span class="whitespace"> </span>
    <span class="token Exp poly">then</span><span class="whitespace"> </span>
    <span class="token Exp poly">(</span><span class="token Exp mono">x</span><span class="token Exp mono">,</span>
    <span class="whitespace"> </span><span class="token Exp mono">y</span><span class="whitespace"> </span>
    <span class="token Exp mono">+</span><span class="whitespace"> </span><span class="token Exp mono">1</span>
    <span class="token Exp poly">)</span><span class="whitespace"> </span><span class="token Exp mono">|&gt;</span>
    <span class="whitespace"> </span><span class="token Exp mono">x</span><span class="linebreak"></span>
    <span class="whitespace"> </span><span class="whitespace"> </span>
    <span class="token Exp poly">else</span><span class="whitespace"> </span>
    <span class="token string-lit mono">"done"</span><span class="whitespace"> </span>
    <span class="linebreak"></span>
    <span class="token Exp poly">in</span><span class="linebreak"></span>
    <span class="token Exp mono">comb</span><span class="token Exp poly">(</span>
    <span class="token Exp mono">comb</span><span class="token Exp mono">,</span>
    <span class="whitespace"> </span><span class="token Exp mono">0</span><span class="token Exp poly">)</span>`;

  it('should parse the text content correctly', () => {
    const result = parseCodeString(codeString);
    expect(result.text).toBe(`let comb = \n  fun x, y ->\n    if (y < 5) then (x, y + 1) |> x \n    else "done" \nin\ncomb(comb, 0)`);
  });

  it('should extract spans with correct offsets', () => {
    const result = parseCodeString(codeString);
    expect(result.spans.length).toBeGreaterThan(10); // Expect multiple spans due to the complex code

    // First span "let"
    expect(result.spans[0].startOffset).toBe(0);
    expect(result.spans[0].endOffset).toBe(3);

    // Second span "comb"
    expect(result.spans[1].startOffset).toBe(4);
    expect(result.spans[1].endOffset).toBe(8);

    // Ensure that the "fun" function and subsequent content are correctly parsed
    const funSpan = result.spans.find(span => span.element.textContent === 'fun');
    expect(funSpan).not.toBeUndefined();
  });
});

describe('findSpanByRowCol', () => {
  const codeString = `
    <span class="token Exp poly">let</span><span class="whitespace"> </span>
    <span class="token Pat mono">comb</span><span class="whitespace"> </span>
    <span class="token Exp poly">=</span><span class="whitespace"> </span>
    <span class="linebreak"></span>
    <span class="whitespace"> </span><span class="whitespace"> </span>
    <span class="token Exp poly">fun</span><span class="whitespace"> </span>
    <span class="token Pat mono">x</span><span class="token Pat mono">,</span>
    <span class="whitespace"> </span><span class="token Pat mono">y</span><span class="whitespace"> </span>
    <span class="token Exp poly">-&gt;</span><span class="linebreak"></span>
    <span class="whitespace"> </span><span class="whitespace"> </span>
    <span class="token Exp poly">if</span><span class="whitespace"> </span>
    <span class="token Exp poly">(</span><span class="token Exp mono">y</span><span class="whitespace"> </span>
    <span class="token Exp mono">&lt;</span><span class="whitespace"> </span><span class="token Exp mono">5</span>
    <span class="token Exp poly">)</span><span class="whitespace"> </span>
    <span class="token Exp poly">then</span><span class="whitespace"> </span>
    <span class="token Exp poly">(</span><span class="token Exp mono">x</span><span class="token Exp mono">,</span>
    <span class="whitespace"> </span><span class="token Exp mono">y</span><span class="whitespace"> </span>
    <span class="token Exp mono">+</span><span class="whitespace"> </span><span class="token Exp mono">1</span>
    <span class="token Exp poly">)</span><span class="whitespace"> </span><span class="token Exp mono">|&gt;</span>
    <span class="whitespace"> </span><span class="token Exp mono">x</span><span class="linebreak"></span>
    <span class="whitespace"> </span><span class="whitespace"> </span>
    <span class="token Exp poly">else</span><span class="whitespace"> </span>
    <span class="token string-lit mono">"done"</span><span class="whitespace"> </span>
    <span class="linebreak"></span>
    <span class="token Exp poly">in</span><span class="linebreak"></span>
    <span class="token Exp mono">comb</span><span class="token Exp poly">(</span>
    <span class="token Exp mono">comb</span><span class="token Exp mono">,</span>
    <span class="whitespace"> </span><span class="token Exp mono">0</span><span class="token Exp poly">)</span>`;

  it('should find the correct span at row 2, col 5', () => {
    const result = findSpanByRowCol(codeString, 2, 5);
    expect(result?.outerHTML).toBe('<span class="token Exp poly">fun</span>');
  });

  it('should return null for an out-of-bounds row', () => {
    const result = findSpanByRowCol(codeString, 10, 1);
    expect(result).toBeNull();
  });

  it('should return null for an out-of-bounds column', () => {
    const result = findSpanByRowCol(codeString, 1, 50);
    expect(result).toBeNull();
  });
});

describe('findSpanByQuery', () => {
  const codeString = `
    <span class="token Exp poly">let</span><span class="whitespace"> </span>
    <span class="token Pat mono">comb</span><span class="whitespace"> </span>
    <span class="token Exp poly">=</span><span class="whitespace"> </span>
    <span class="linebreak"></span>
    <span class="whitespace"> </span><span class="whitespace"> </span>
    <span class="token Exp poly">fun</span><span class="whitespace"> </span>
    <span class="token Pat mono">x</span><span class="token Pat mono">,</span>
    <span class="whitespace"> </span><span class="token Pat mono">y</span><span class="whitespace"> </span>
    <span class="token Exp poly">-&gt;</span><span class="linebreak"></span>
    <span class="whitespace"> </span><span class="whitespace"> </span>
    <span class="token Exp poly">if</span><span class="whitespace"> </span>
    <span class="token Exp poly">(</span><span class="token Exp mono">y</span><span class="whitespace"> </span>
    <span class="token Exp mono">&lt;</span><span class="whitespace"> </span><span class="token Exp mono">5</span>
    <span class="token Exp poly">)</span><span class="whitespace"> </span>
    <span class="token Exp poly">then</span><span class="whitespace"> </span>
    <span class="token Exp poly">(</span><span class="token Exp mono">x</span><span class="token Exp mono">,</span>
    <span class="whitespace"> </span><span class="token Exp mono">y</span><span class="whitespace"> </span>
    <span class="token Exp mono">+</span><span class="whitespace"> </span><span class="token Exp mono">1</span>
    <span class="token Exp poly">)</span><span class="whitespace"> </span><span class="token Exp mono">|&gt;</span>
    <span class="whitespace"> </span><span class="token Exp mono">x</span><span class="linebreak"></span>
    <span class="whitespace"> </span><span class="whitespace"> </span>
    <span class="token Exp poly">else</span><span class="whitespace"> </span>
    <span class="token string-lit mono">"done"</span><span class="whitespace"> </span>
    <span class="linebreak"></span>
    <span class="token Exp poly">in</span><span class="linebreak"></span>
    <span class="token Exp mono">comb</span><span class="token Exp poly">(</span>
    <span class="token Exp mono">comb</span><span class="token Exp mono">,</span>
    <span class="whitespace"> </span><span class="token Exp mono">0</span><span class="token Exp poly">)</span>`;

  it('should find the correct span for the query "fun x, y ->"', () => {
    const result = findSpanByQuery(codeString, 'fun x, y ->');
    expect(result?.outerHTML).toBe('<span class="token Exp poly">fun</span>');
  });

  it('should return null for a query that does not exist', () => {
    const result = findSpanByQuery(codeString, 'nonexistent');
    expect(result).toBeNull();
  });
});
