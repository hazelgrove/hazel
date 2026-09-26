// Published engines used by the opt-in Canvas layout study. Geometry only.
import ELK from 'elkjs/lib/elk-api.js';
import { Layout } from 'webcola';
let browserElk;
const getElk = () => browserElk ||= new ELK({workerUrl:new URL('elk-worker.js',self.location.href).href});

export async function solve({kind, items, links, old = [], clusters = []}, engines = {}) {
  if (!items.length) return [];
  const index = new Map(items.map((n, i) => [n.key, i]));
  const valid = links.filter(([a,b]) => a !== b && index.has(a) && index.has(b));
  let result;
  if (kind.startsWith('elk')) {
    const graph = await (engines.elk || getElk()).layout({
      id: 'canvas', layoutOptions: {
        'elk.algorithm': 'layered', 'elk.direction': kind === 'elk-down' ? 'DOWN' : 'RIGHT',
        'elk.randomSeed': '42', 'elk.spacing.nodeNode': '48',
        'elk.layered.spacing.nodeNodeBetweenLayers': '100',
        'elk.spacing.componentComponent': '65', 'elk.aspectRatio': '1.3',
        'elk.layered.considerModelOrder.strategy': 'NODES_AND_EDGES',
        'elk.layered.crossingMinimization.strategy': 'LAYER_SWEEP',
        'elk.layered.nodePlacement.strategy': 'NETWORK_SIMPLEX',
        'elk.layered.thoroughness': '12', 'elk.separateConnectedComponents': 'true'
      },
      children: items.map((n,i) => ({id: String(i), width: n.w, height: n.h})),
      edges: valid.map(([a,b],i) => ({id:'e'+i,sources:[String(index.get(a))],targets:[String(index.get(b))]}))
    });
    result = graph.children.map(n => ({key:items[+n.id].key,x:n.x+n.width/2,y:n.y+n.height/2}));
  } else {
    const previous = new Map(old.map(n => [n.key, n]));
    const live = kind === 'cola-live' && previous.size > 0;
    const nodes = items.map(n => {
      const p = live && previous.get(n.key) || n;
      return {key:n.key,x:p.x,y:p.y,width:n.w+24,height:n.h+24,fixed:live && previous.has(n.key)?1:0};
    });
    const edges = valid.map(([a,b]) => ({source:index.get(a),target:index.get(b)}));
    const claimed = new Set();
    const groups = clusters.map(keys => ({leaves:keys.map(k=>index.get(k)).filter(i=>i!==undefined&&!claimed.has(i)).filter(i=>(claimed.add(i),true)),padding:32})).filter(g=>g.leaves.length>1);
    const layout = new Layout().size([1000,800]).nodes(nodes).links(edges)
      .groups(groups).avoidOverlaps(true).handleDisconnected(!live)
      .linkDistance(e => {
        const a = typeof e.source==='number'?nodes[e.source]:e.source;
        const b = typeof e.target==='number'?nodes[e.target]:e.target;
        return 90+(Math.hypot(a.width,a.height)+Math.hypot(b.width,b.height))/2;
      }).convergenceThreshold(0.001);
    // CoLa fixed nodes are high-weight anchors (small settling is possible).
    // Use them only in the explicit growth-preserving variant.
    // The other trial is free to untangle and compact the entire graph.
    layout.start(live?0:30,0,80,0,false,!live);
    result = nodes.map(n=>({key:n.key,x:n.x,y:n.y}));
  }
  // Use a stable positive world frame. Free recomputations are normalized;
  // anchored growth keeps that frame unless new geometry extends past it.
  const byKey=new Map(items.map(n=>[n.key,n]));
  const minX=Math.min(...result.map(n=>n.x-byKey.get(n.key).w/2));
  const minY=Math.min(...result.map(n=>n.y-byKey.get(n.key).h/2));
  const anchored=kind==='cola-live' && old.length>0;
  const dx=anchored?Math.max(0,26-minX):26-minX;
  const dy=anchored?Math.max(0,26-minY):26-minY;
  result=result.map(n=>({...n,x:n.x+dx,y:n.y+dy}));
  if (result.length !== items.length || result.some(n=>!Number.isFinite(n.x)||!Number.isFinite(n.y))) throw new Error('Layout returned incomplete or non-finite positions');
  return result;
}
