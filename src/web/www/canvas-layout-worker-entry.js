import { solve } from './canvas-layout-engines.js';
self.onmessage = async ({data}) => {
  const start=performance.now();
  try { self.postMessage({id:data.id,positions:await solve(data),ms:performance.now()-start}); }
  catch (error) { self.postMessage({id:data.id,error:String(error)}); }
};
