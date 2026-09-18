// Lazy worker: the default Canvas pays no solver cost. Keep positions coherent
// until a complete result arrives; a stale result cannot replace a newer graph.
let worker, next=0, activeKey;
const jobs=new Map(), cache=new Map(), latest=new Map();
const observations=[];
const state={pending:0,status:'idle',runs:observations};
function finish(job, result) {
  clearTimeout(job.timer); jobs.delete(job.id); state.pending=jobs.size;
  cache.set(job.key,result); while(cache.size>48)cache.delete(cache.keys().next().value);
  observations.push({kind:job.data.kind,nodes:job.data.items.length,ms:result.ms,error:result.error||null});
  if(observations.length>150)observations.shift();
  if(activeKey===job.key && latest.get(job.scope)===job.key){state.status=result.error?'Layout failed; previous positions retained':`Ready · ${Math.round(result.ms)} ms`;job.done();}
}
function startWorker() {
  if(worker)return;
  worker=new Worker(new URL('canvas-layout-worker.js',document.baseURI));
  worker.onmessage=({data})=>{const job=jobs.get(data.id);if(job)finish(job,data);};
  worker.onerror=e=>failWorker('Layout worker failed: '+(e.message||'unknown error'));
}
function failWorker(error) {
  worker?.terminate();worker=null;
  for(const job of [...jobs.values()])finish(job,{error,ms:0});
}
window.__canvasResearchState=state;
window.__canvasResearchLayout=(encoded,done)=>{
  const data=JSON.parse(encoded),scope=data.scope;
  // Ignore changing seed coordinates and card hover state. Explicit Rearrange
  // changes scope; full graph topology (including terminals) changes the key. Drag pins are applied later.
  const key=JSON.stringify([scope,data.items.map(n=>n.key),data.links,data.clusters,data.topology]);
  latest.set(scope,key);activeKey=key;
  if(cache.has(key)){const r=cache.get(key);return JSON.stringify(r.positions||[]);}
  const existing=[...jobs.values()].find(j=>j.key===key);
  if(existing){existing.done=done;return '[]';}
  const id=++next,job={id,key,scope,data,done};
  jobs.set(id,job);state.pending=jobs.size;state.status='Arranging…';
  job.timer=setTimeout(()=>failWorker('Layout exceeded the 8 second limit'),8000);
  try{startWorker();worker.postMessage({...data,id});}catch(error){finish(job,{error:String(error),ms:0});}
  return '[]';
};
