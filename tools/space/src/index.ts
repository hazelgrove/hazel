import type { Plugin, Tool } from '@inkandswitch/patchwork-plugins';

export const plugins: Plugin<any>[] = [
  {
    type: 'patchwork:tool',
    id: 'spaze',
    name: 'Spaze',
    supportedDatatypes: ['folder'],
    async load() {
      const { SpaceTool } = await import('./tool.tsx');
      return SpaceTool;
    },
  } satisfies Tool,
];

console.log('space plugin loaded');
