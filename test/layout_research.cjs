// Run with node test/layout_research.cjs. Tests the actual packaged engines and
// async bridge, independently of js_of_ocaml's browser runtime.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const Module = require('node:module');
const path = require('node:path');
const output = require('esbuild').buildSync({entryPoints:[path.resolve(__dirname,'../src/web/www/canvas-layout-engines.js')],bundle:true,platform:'node',format:'cjs',write:false});
const mod=new Module(__filename);mod._compile(output.outputFiles[0].text,__filename);
const ELK=require('elkjs');const elk=new ELK();
const solve=data=>mod.exports.solve(data,{elk});
const item=(key,x,y,w=120,h=90)=>({key,x,y,w,h});
(async()=>{
 let tests=0;
 for(const kind of ['elk','elk-down','cola','cola-live']){
  const items=[item('a',100,100,240,110),item('b',100,100),item('c',100,100),item('island',3000,900)];
  const input={kind,items,links:[['a','b'],['b','c'],['c','a']],clusters:[['a','b','c']]};
  const result=await solve(input);
  assert.deepEqual(new Set(result.map(n=>n.key)),new Set(items.map(n=>n.key)));
  for(let i=0;i<result.length;i++)for(let j=i+1;j<result.length;j++){
   const a=result[i],b=result[j],na=items.find(n=>n.key===a.key),nb=items.find(n=>n.key===b.key);
   assert(Math.abs(a.x-b.x)>=(na.w+nb.w)/2-.01||Math.abs(a.y-b.y)>=(na.h+nb.h)/2-.01,`${kind}: node overlap ${a.key}/${b.key}`);
  }
  assert.deepEqual(await solve(input),result,`${kind}: deterministic result`); tests++;
 }
 const initial={kind:'cola-live',items:[item('a',100,100),item('b',400,100)],links:[['a','b']]};
 const old=await solve(initial);
 const growing=await solve({...initial,items:[...initial.items,item('c',500,400)],links:[...initial.links,['b','c']],old});
 for(const n of old){const p=growing.find(p=>p.key===n.key);assert(Math.hypot(p.x-n.x,p.y-n.y)<16,'Existing group moved excessively in anchored-growth mode');} tests++;
 const workers=[];
 class Worker{constructor(){this.messages=[];workers.push(this);}postMessage(m){this.messages.push(m);}terminate(){this.dead=true;}}
 const context={window:{},Worker,URL,document:{baseURI:'http://example.org/hazel/'},setTimeout,clearTimeout};
 vm.runInNewContext(fs.readFileSync(path.resolve(__dirname,'../src/web/www/canvas-layout-bridge.js'),'utf8'),context);
 const request=context.window.__canvasResearchLayout;
 let notifiedA=0,notifiedB=0;
 const a={...initial,scope:'scene/cola-live/0',clusters:[]};
 const b={...a,items:[...a.items,item('c',500,400)],links:[...a.links,['b','c']]};
 assert.equal(request(JSON.stringify(a),()=>notifiedA++),'[]');
 assert.equal(request(JSON.stringify(b),()=>notifiedB++),'[]');
 const [ma,mb]=workers[0].messages;
 workers[0].onmessage({data:{id:ma.id,positions:old,ms:10}});
 assert.equal(notifiedA,0,'Stale topology result triggered a repaint');
 workers[0].onmessage({data:{id:mb.id,positions:growing,ms:12}});
 assert.equal(notifiedB,1);assert.equal(context.window.__canvasResearchState.pending,0);
 assert.deepEqual(JSON.parse(request(JSON.stringify(b),()=>{})),growing);tests++;
 const expanded={...b,topology:[['a','b','c','new-terminal'],b.links]};
 request(JSON.stringify(expanded),()=>{});
 assert.equal(workers[0].messages.length,3,'A new terminal inside an existing host must invalidate placement');
 const mc=workers[0].messages.at(-1);
 workers[0].onmessage({data:{id:mc.id,positions:growing,ms:8}});tests++;
 console.log(`${tests} engine/bridge tests passed`);
})().catch(e=>{console.error(e);process.exitCode=1});
