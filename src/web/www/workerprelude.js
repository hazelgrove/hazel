// The worker's own Algebrite, which the proof checker verifies algebra
// steps with. Bundled into worker-algebrite.js, which the worker loads.
import Algebrite from 'algebrite';
self.Algebrite = Algebrite;
