const fs=require('fs');
const S=JSON.parse(fs.readFileSync(process.argv[2],'utf8')).samples;
const med=xs=>{const s=[...xs].sort((a,b)=>a-b),n=s.length;return n%2?s[(n-1)/2]:(s[n/2-1]+s[n/2])/2;};
let seed=42; const rnd=()=>{seed=(seed*1664525+1013904223)>>>0; return seed/4294967296;};
const names=['reshape-source','add-metrics','fix-middle','tune-threshold','tariff-tune','split-summary'];
const tot={}; for(const s of S){ if(s.step===0)continue; const t=names.find(n=>s.trace.includes(n)); ((tot[t]??={})[s.calculus]??={})[s.rep]=(tot[t][s.calculus][s.rep]||0)+s.eval_ms; }
const B=20000;
function boot(t,num,den,alpha){ // ratio median(num)/median(den), paired by rep
  const reps=Object.keys(tot[t].a0).map(Number);
  const a=reps.map(r=>tot[t][num][r]), b=reps.map(r=>tot[t][den][r]);
  const pt=med(a)/med(b), bs=new Float64Array(B);
  for(let i=0;i<B;i++){const ia=[],ib=[];for(let k=0;k<reps.length;k++){const j=Math.floor(rnd()*reps.length);ia.push(a[j]);ib.push(b[j]);} bs[i]=med(ia)/med(ib);}
  bs.sort(); return [pt, bs[Math.floor(alpha/2*B)], bs[Math.ceil((1-alpha/2)*B)-1]];
}
const f=x=>x.toFixed(2);
console.log('== SPEEDUP vs a0 (ratio of medians), 95% paired-bootstrap CI, n =', Object.keys(tot[names[0]].a0).length);
for(const t of names){ console.log(t.padEnd(15), ['aPL','aM','a2','aStar'].map(c=>{const [p,l,h]=boot(t,'a0',c,0.05);return `${c} ${f(p)}x [${f(l)},${f(h)}]`;}).join('  ')); }
const m=18, alpha=0.05/m;
console.log(`\n== aStar time / X time  (>1 = aStar SLOWER). Bonferroni-adjusted ${(100*(1-alpha)).toFixed(2)}% CI over ${m} comparisons`);
for(const t of names) for(const c of ['aPL','aM','a2']){
  const [p,l,h]=boot(t,'aStar',c,alpha);
  const v = l>1?'aStar SLOWER': h<1?'aStar faster':'inconclusive';
  const eq = (c==='a2') ? ((l>=1/1.1 && h<=1.1)?'  equivalent within ±10%':'  NOT shown within ±10%') : '';
  console.log(`${t.padEnd(15)} vs ${c.padEnd(5)} ${p.toFixed(3)}  [${l.toFixed(3)}, ${h.toFixed(3)}]  ${v}${eq}`);
}
console.log('\n== drift: a0 median by fifth of reps');
for(const t of names){ const m0=tot[t].a0; const r=Object.keys(m0).map(Number).sort((a,b)=>a-b); const q=k=>med(r.filter(x=>Math.floor(x*5/r.length)===k).map(x=>m0[x])); console.log(t.padEnd(15),[0,1,2,3,4].map(k=>q(k).toFixed(0).padStart(6)).join('')); }
