// Side-effect dynamic imports to coax esbuild into chunking large packages separately
import('react');
import('react-dom/client');
import('@tldraw/editor');

import {
  Tldraw,
  Editor,
  createShapeId,
  TLShapeId,
  TLUiComponents,
  TLRecord,
  TLAssetId,
  TLAsset,
  TLContent,
  createTLStore,
  defaultShapeUtils,
  getMediaAssetInfoPartial,
  getUserPreferences,
  setUserPreferences,
  defaultUserPreferences,
  createPresenceStateDerivation,
  InstancePresenceRecordType,
  computed,
  react,
  sortById,
  createBookmarkFromUrl,
  toRichText,
} from 'tldraw';
import type { VecLike } from 'tldraw';
import { DocHandle, type AutomergeUrl, type DocHandleChangePayload } from '@automerge/automerge-repo';
import { useDocument, useLocalAwareness, useRemoteAwareness, RepoContext } from '@automerge/automerge-repo-react-hooks';
import { createRoot } from 'react-dom/client';
import { useEffect, useRef, useMemo, useState, useCallback, memo } from 'react';
import { FolderDoc, DocLink } from '@inkandswitch/patchwork-filesystem';
import { automergeUrlToServiceWorkerUrl } from '@inkandswitch/patchwork-filesystem';
import type { ToolRender, ToolElement } from '@inkandswitch/patchwork-plugins';
import { PatchworkDocShapeUtil, PATCHWORK_DOC_SHAPE_TYPE } from './PatchworkDocShape';
import { applyTLStoreChangesToAutomerge, tldrawValueToAutomergeValue } from './TLStoreToAutomerge';
import type { TldrawDoc } from './TLStoreToAutomerge';
import { applyAutomergePatchesToTLStore } from './AutomergeToTLStore';
import {
  NewDocShapeTool,
  newDocUiOverrides,
  NewDocToolbar,
  setNewDocToolContext,
} from './NewDocTool';
import '@inkandswitch/patchwork-elements';

// ---- Logging ----------------------------------------------------------------

const LOG = '[space]';

// ---- Types ------------------------------------------------------------------

type SpaceDoc = FolderDoc & {
  /**
   * Automerge URL pointing at a dedicated tldraw document.
   * Legacy: may be a `{ [recordId: string]: any }` object — migrated on first load.
   */
  tldraw?: string | { [recordId: string]: any };
};

// ---- Constants --------------------------------------------------------------

const GRID_COLS = 3;
const DEFAULT_W = 640;
const DEFAULT_H = 480;
const GAP = 60;

const customShapeUtils = [PatchworkDocShapeUtil];
const customTools = [NewDocShapeTool];

// Keep everything except the page picker; use custom toolbar with new-doc tool.
const uiComponents: TLUiComponents = {
  PageMenu: null,
  Toolbar: NewDocToolbar,
};

// Memoized wrapper so parent re-renders (presence, useDocument, etc.)
// don't cause tldraw to re-render.  All props are stable references.
const StableTldraw = memo(function StableTldraw({
  onMount,
}: {
  onMount: (editor: Editor) => void;
}) {
  return (
    <Tldraw
      shapeUtils={customShapeUtils}
      tools={customTools}
      overrides={newDocUiOverrides}
      onMount={onMount}
      components={uiComponents}
      forceMobile
    />
  );
});

const MIME_TO_EXT: Record<string, string> = {
  'image/png': 'png',
  'image/jpeg': 'jpg',
  'image/gif': 'gif',
  'image/webp': 'webp',
  'image/svg+xml': 'svg',
  'image/bmp': 'bmp',
  'image/tiff': 'tiff',
  'image/avif': 'avif',
  'video/mp4': 'mp4',
  'video/webm': 'webm',
  'video/quicktime': 'mov',
};

function extensionForMimeType(mimeType: string): string {
  return MIME_TO_EXT[mimeType] || mimeType.split('/')[1] || 'bin';
}

// ---- Helpers ----------------------------------------------------------------

export function makeShapeId(docUrl: string): TLShapeId {
  return createShapeId(docUrl.replace(/[^a-zA-Z0-9]/g, '_'));
}

function defaultPosition(index: number) {
  const col = index % GRID_COLS;
  const row = Math.floor(index / GRID_COLS);
  return { x: col * (DEFAULT_W + GAP), y: row * (DEFAULT_H + GAP) };
}

async function filterTldrawDocs(repo: any, docLinks: DocLink[]): Promise<DocLink[]> {
  const filtered: DocLink[] = [];

  for (const docLink of docLinks) {
    try {
      const docHandle = await repo.find(docLink.url);
      const doc = docHandle.doc();

      // Skip documents with @patchwork.type === "tldraw"
      if (doc?.['@patchwork']?.type === 'tldraw') {
        console.log(LOG, 'filtering out tldraw doc:', docLink.name);
        continue;
      }

      filtered.push(docLink);
    } catch (error) {
      console.warn(LOG, 'error checking doc', docLink.url, error);
      // Include the doc if we can't check it
      filtered.push(docLink);
    }
  }

  return filtered;
}

// ---- Presence ---------------------------------------------------------------

interface ContactDoc {
  type: string;
  name?: string;
  color?: string;
}

function useContactInfo() {
  const [contactUrl, setContactUrl] = useState<AutomergeUrl | undefined>();

  useEffect(() => {
    const accountDocHandle = (
      window as any
    ).accountDocHandle as DocHandle<{ contactUrl: AutomergeUrl }> | undefined;
    if (!accountDocHandle) return;
    accountDocHandle.whenReady().then(() => {
      const doc = accountDocHandle.doc();
      if (doc?.contactUrl) {
        setContactUrl(doc.contactUrl);
      }
    });
  }, []);

  const [contactDoc] = useDocument<ContactDoc>(contactUrl);

  return {
    userId: contactUrl ?? (window as any).repo?.peerId ?? 'anonymous',
    name: contactDoc?.name ?? 'Anonymous',
    color: contactDoc?.color,
  };
}

function usePresence(
  handle: DocHandle<SpaceDoc>,
  editorRef: React.MutableRefObject<Editor | null>,
) {
  const { userId, name, color } = useContactInfo();

  const [, updateLocalState] = useLocalAwareness({
    handle: handle as DocHandle<any>,
    userId,
    initialState: {},
  });

  const [peerStates] = useRemoteAwareness({
    handle: handle as DocHandle<any>,
    localUserId: userId,
  });

  // Sync remote presence records into the editor's store
  useEffect(() => {
    const editor = editorRef.current;
    if (!editor) return;

    const toPut: TLRecord[] = Object.values(peerStates).filter(
      (record: any) => record && Object.keys(record).length !== 0,
    );

    const toRemove = editor.store.query
      .records('instance_presence')
      .get()
      .sort(sortById)
      .map((record) => record.id)
      .filter((id) => !toPut.find((record) => record.id === id));

    if (toRemove.length) editor.store.remove(toRemove);
    if (toPut.length) editor.store.put(toPut);
  }, [peerStates, editorRef]);

  // Broadcast local presence state derived from the editor
  useEffect(() => {
    const editor = editorRef.current;
    if (!editor) return;

    setUserPreferences({ id: userId, color, name });

    const userPreferences = computed<{
      id: string;
      color: string;
      name: string;
    }>('userPreferences', () => {
      const user = getUserPreferences();
      return {
        id: user.id,
        color: user.color ?? defaultUserPreferences.color,
        name: user.name ?? defaultUserPreferences.name,
      };
    });

    const presenceId = InstancePresenceRecordType.createId(userId);
    const presenceDerivation = createPresenceStateDerivation(
      userPreferences,
      presenceId,
    )(editor.store);

    return react('when presence changes', () => {
      const presence = presenceDerivation.get();
      requestAnimationFrame(() => {
        updateLocalState(presence);
      });
    });
  }, [editorRef.current, userId, updateLocalState, name, color]);
}

// ---- Tool entry point -------------------------------------------------------

export const SpaceTool: ToolRender = (handle, element) => {
  // Load tldraw CSS at runtime via <link>
  if (!document.querySelector('link[data-space-tldraw-css]')) {
    const link = document.createElement("link");
    link.rel = "stylesheet";
    link.href = new URL("./tldraw.css", import.meta.url).href;
    link.setAttribute('data-space-tldraw-css', '');
    document.head.appendChild(link);
  }

  const repo = element.repo;
  const root = createRoot(element);
  root.render(
    <RepoContext.Provider value={repo}>
      <SpaceCanvas handle={handle as DocHandle<SpaceDoc>} element={element} />
    </RepoContext.Provider>,
  );
  return () => {
    console.log(LOG, 'tool unmounting');
    root.unmount();
  };
};

// ---- React component --------------------------------------------------------

function SpaceCanvas({
  handle,
  element,
}: {
  handle: DocHandle<SpaceDoc>;
  element: ToolElement;
}) {
  // useDocument is ONLY used to derive the folder-doc list key and for
  // the loading overlay.  We never pass the reactive `doc` into any
  // tldraw-touching code-path — that caused the previous unmount/flicker bug.
  const [doc] = useDocument<SpaceDoc>(handle.url);

  const editorRef = useRef<Editor | null>(null);
  const initializedRef = useRef(false);
  const cleanupFnsRef = useRef<(() => void)[]>([]);

  // Guard to break the tldraw ↔ automerge feedback loop.
  const preventPatchApplicationsRef = useRef(false);
  const isReconcilingRef = useRef(false);

  // ---- Presence (cursors, selections visible to other users) ----
  usePresence(handle, editorRef);

  // ---- Tldraw mount callback (stable reference, never changes) ----
  const handleMountRef = useRef((editor: Editor) => {
    console.log(LOG, 'tldraw editor mounted, waiting for doc…');
    editorRef.current = editor;

    handle.whenReady().then(() => {
      if (initializedRef.current) {
        console.log(LOG, 'doc ready but already initialized — skipping');
        return;
      }
      initializedRef.current = true;
      console.log(LOG, 'doc ready — running initializeSync');
      setNewDocToolContext(element, handle, editor);
      initializeSync(
        editor,
        handle,
        element.repo,
        preventPatchApplicationsRef,
        isReconcilingRef,
        cleanupFnsRef,
      );
    });
  });

  // ---- Folder-doc-list reconciliation (add / remove patchwork-doc shapes) ---
  const docUrlsKey = useMemo(
    () => (doc?.docs ?? []).map((d) => `${d.url}|${d.name}|${d.type}`).join('\n'),
    [doc?.docs],
  );

  useEffect(() => {
    const editor = editorRef.current;
    if (!editor || !initializedRef.current) return;

    const currentDoc = handle.doc();
    if (!currentDoc?.docs) {
      console.log(LOG, 'reconcile effect: no doc yet');
      return;
    }

    console.log(
      LOG,
      'folder doc list changed (',
      currentDoc.docs.length,
      'docs) — reconciling patchwork-doc shapes',
    );

    // Synchronously exclude our own tldraw doc, then async-filter any others.
    const tldrawUrl = typeof currentDoc.tldraw === 'string' ? currentDoc.tldraw : null;
    const withoutOwn = tldrawUrl
      ? currentDoc.docs.filter((d) => d.url !== tldrawUrl)
      : currentDoc.docs;

    // Immediately reconcile with the synchronous filter so there's no
    // flash of a tldraw-doc shape on canvas.
    isReconcilingRef.current = true;
    reconcilePatchworkDocShapes(editor, withoutOwn);
    isReconcilingRef.current = false;

    // Then async-filter any remaining tldraw docs from other sources.
    // Use a stale flag so that if docUrlsKey changes before this resolves,
    // we don't reconcile with outdated data (which could delete valid shapes).
    let stale = false;
    filterTldrawDocs(element.repo, withoutOwn).then((filteredDocs) => {
      if (stale) return;
      isReconcilingRef.current = true;
      reconcilePatchworkDocShapes(editor, filteredDocs);
      isReconcilingRef.current = false;
    });

    return () => { stale = true; };
  }, [docUrlsKey, handle, element.repo]);

  // ---- patchwork:open-document → always stop, optionally zoom ---------------
  const sideboardRef = useRef<HTMLDivElement>(null);
  useEffect(() => {
    const handler = (e: Event) => {
      // Never let open-document events escape the space
      e.stopPropagation();

      // If the event came from inside the sideboard, zoom to the shape
      const sideboardEl = sideboardRef.current;
      if (!sideboardEl) return;
      const path = e.composedPath();
      if (!path.includes(sideboardEl)) return;

      const editor = editorRef.current;
      if (!editor) return;
      const detail = (e as CustomEvent).detail as { url?: string };
      if (!detail?.url) return;

      const targetId = makeShapeId(detail.url);
      const shape = editor.getShape(targetId);
      if (!shape) {
        console.log(LOG, 'open-document: no shape found for', detail.url);
        return;
      }

      editor.setSelectedShapes([targetId]);
      editor.zoomToSelection({ animation: { duration: 300 } });
      console.log(LOG, 'open-document: zoomed to shape for', detail.url);
    };

    element.addEventListener('patchwork:open-document', handler);
    return () => {
      element.removeEventListener('patchwork:open-document', handler);
    };
  }, [element]);

  // ---- Cleanup on unmount ---------------------------------------------------
  useEffect(() => {
    return () => {
      console.log(LOG, 'component unmounting — cleaning up', cleanupFnsRef.current.length, 'fns');
      for (const fn of cleanupFnsRef.current) {
        try {
          fn();
        } catch (e) {
          console.warn(LOG, 'cleanup error', e);
        }
      }
      cleanupFnsRef.current = [];
    };
  }, []);

  // ---- Render ---------------------------------------------------------------
  // IMPORTANT: <Tldraw> is ALWAYS rendered.  We never conditionally
  // switch it out for a loading indicator — that would destroy the
  // editor instance and all shapes.  Instead we overlay a translucent
  // loading screen until the doc arrives.

  return (
    <div
      className="pw-space-root"
      style={{ width: '100%', height: '100%', position: 'relative' }}
    >
      <style>{`
        .pw-space-root > .tl-container .tl-background {
          background: #e6ddf0 !important;
        }
        /* Make embed iframes (YouTube, etc.) always interactive */
        .pw-space-root .tl-embed-container iframe.tl-embed {
          pointer-events: auto !important;
          z-index: auto !important;
        }
        /* Titlebar for embed shapes so they can be dragged */
        .pw-space-root .tl-embed-container {
          position: relative;
        }
        .pw-space-root .tl-embed-container::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          height: 18px;
          background:
            linear-gradient(#fff, #fff),
            linear-gradient(#fff, #fff),
            repeating-linear-gradient(#d0d0d0 0px, #d0d0d0 1px, transparent 1px, transparent 3px);
          background-size: 100% 1px, 100% 1px, 100% 100%;
          background-position: top, bottom, center;
          background-repeat: no-repeat, no-repeat, repeat;
          background-color: #fff;
          border-bottom: 1px solid #808080;
          border-radius: 8px 8px 0 0;
          z-index: 1;
          pointer-events: auto;
          cursor: grab;
        }
        .pw-space-root .tl-embed-container iframe.tl-embed {
          margin-top: 18px !important;
          height: calc(100% - 18px) !important;
          border-radius: 0 0 8px 8px !important;
        }

        /* ---- Move all UI panels to the bottom ---- */

        /* Remove the top section from grid flow so it doesn't push the toolbar down */
        .pw-space-root .tlui-layout__top {
          position: absolute;
          width: 0;
          height: 0;
          overflow: visible;
          pointer-events: none;
        }

        /* Keep the toolbar centered by not sharing a grid row with __top */
        .pw-space-root .tlui-layout__bottom {
          grid-row: 5;
        }

        /* Fixed-position hamburger in the bottom-left */
        .pw-space-root .tlui-menu-zone {
          position: fixed;
          bottom: 12px;
          left: 12px;
          z-index: var(--tl-layer-panels);
          border-radius: var(--tl-radius-4);
          border: none;
          box-shadow: var(--tl-shadow-3);
        }

        /* ---- Replace hamburger icon with bold semicolon ---- */
        .pw-space-root .tlui-menu-zone [data-testid="main-menu.button"] .tlui-icon {
          mask: none !important;
          -webkit-mask: none !important;
          background-color: transparent !important;
          display: flex;
          align-items: center;
          justify-content: center;
        }
        .pw-space-root .tlui-menu-zone [data-testid="main-menu.button"] .tlui-icon::after {
          content: ";";
          font-weight: 900;
          font-size: 18px;
          line-height: 1;
          color: currentColor;
        }
      `}</style>
      <StableTldraw onMount={handleMountRef.current} />
      {!doc && (
        <div
          style={{
            position: 'absolute',
            inset: 0,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            background: 'rgba(255,255,255,0.85)',
            fontFamily: 'system-ui, -apple-system, sans-serif',
            color: '#888',
            fontSize: '16px',
            zIndex: 9999,
          }}
        >
          Loading…
        </div>
      )}
      <SideboardPanel containerRef={sideboardRef} docUrl={handle.url} title={doc?.title} />
    </div>
  );
}

// ---- Sideboard panel — fixed overlay, collapsible -------------------------

function SideboardPanel({ containerRef, docUrl, title }: { containerRef: React.RefObject<HTMLDivElement | null>; docUrl: string; title?: string }) {
  const [open, setOpen] = useState(false);

  return (
    <div
      ref={containerRef as any}
      style={{
        position: 'fixed',
        top: '12px',
        left: '12px',
        zIndex: 99,
        pointerEvents: 'auto',
      }}
    >
      {open ? (
        <div
          style={{
            width: '280px',
            height: '400px',
            display: 'flex',
            flexDirection: 'column',
            background: '#fff',
            border: '1px solid #ccc',
            borderRadius: '8px',
            boxShadow: '0 4px 16px rgba(0,0,0,0.12)',
            overflow: 'hidden',
          }}
        >
          {/* Header with collapse button on left, title on right */}
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              gap: '6px',
              padding: '6px 8px',
              borderBottom: '1px solid #eee',
              background: '#fafafa',
              flexShrink: 0,
            }}
          >
            <button
              type="button"
              onClick={() => setOpen(false)}
              style={{
                border: 'none',
                background: 'none',
                cursor: 'pointer',
                padding: '2px',
                lineHeight: 1,
                fontSize: '16px',
                color: '#999',
                flexShrink: 0,
              }}
            >
              <svg width="16" height="16" viewBox="0 0 16 16" fill="none">
                <line x1="3" y1="8" x2="13" y2="8" stroke="#999" strokeWidth="2" strokeLinecap="round" />
              </svg>
            </button>
            <span
              style={{
                fontSize: '12px',
                fontWeight: 600,
                fontFamily: 'system-ui, -apple-system, sans-serif',
                color: '#333',
                overflow: 'hidden',
                textOverflow: 'ellipsis',
                whiteSpace: 'nowrap',
              }}
            >
              {title || 'Sideboard'}
            </span>
          </div>
          {/* patchwork-view content */}
          <div style={{ flex: 1, minHeight: 0, overflow: 'auto' }}>
            {/* @ts-expect-error Custom element */}
            <patchwork-view
              doc-url={docUrl}
              tool-id="chee/sideboard"
              style={{ display: 'block', width: '100%', height: '100%' }}
            />
          </div>
        </div>
      ) : (
        <button
          type="button"
          onClick={() => setOpen(true)}
          title={title || 'Sideboard'}
          style={{
            width: '36px',
            height: '36px',
            border: '1px solid #ccc',
            borderRadius: '8px',
            background: '#fff',
            boxShadow: '0 2px 8px rgba(0,0,0,0.1)',
            cursor: 'pointer',
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            padding: 0,
          }}
        >
          <svg width="16" height="16" viewBox="0 0 16 16" fill="none">
            <line x1="3" y1="5" x2="13" y2="5" stroke="#666" strokeWidth="2" strokeLinecap="round" />
            <line x1="3" y1="8" x2="13" y2="8" stroke="#666" strokeWidth="2" strokeLinecap="round" />
            <line x1="3" y1="11" x2="13" y2="11" stroke="#666" strokeWidth="2" strokeLinecap="round" />
          </svg>
        </button>
      )}
    </div>
  );
}

// =============================================================================
//  resolveTldrawHandle — find, create, or migrate the dedicated tldraw doc
// =============================================================================

async function resolveTldrawHandle(
  handle: DocHandle<SpaceDoc>,
  repo: any,
): Promise<DocHandle<TldrawDoc>> {
  const doc = handle.doc();
  const existing = doc?.tldraw;

  if (typeof existing === 'string') {
    // Already an automerge URL.
    console.log(LOG, 'tldraw doc URL found:', existing);
    const tldrawHandle = (await repo.find(existing)) as DocHandle<TldrawDoc>;

    // Check if this is an old-format doc (records at root, no `store` key).
    const tldrawDoc = tldrawHandle.doc();
    if (tldrawDoc && !tldrawDoc.store) {
      console.log(LOG, 'migrating old root-level tldraw doc to store/schema format');
      const tlStore = createTLStore({
        shapeUtils: [PatchworkDocShapeUtil, ...defaultShapeUtils],
      });
      const currentSchema = tlStore.schema.serialize();

      tldrawHandle.change((d: any) => {
        const store: Record<string, any> = {};
        for (const [id, value] of Object.entries(d)) {
          if (id === '@patchwork') continue;
          store[id] = JSON.parse(JSON.stringify(value));
          delete d[id];
        }
        d.store = store;
        d.schema = currentSchema;
      });
      console.log(LOG, 'migration to store/schema format complete');
    }

    return tldrawHandle;
  }

  if (existing && typeof existing === 'object') {
    // Legacy: tldraw data stored inline — migrate to a new doc with store/schema.
    console.log(LOG, 'migrating inline tldraw data to dedicated doc');
    const tlStore = createTLStore({
      shapeUtils: [PatchworkDocShapeUtil, ...defaultShapeUtils],
    });
    const currentSchema = tlStore.schema.serialize();
    const store: Record<string, any> = {};
    for (const [id, value] of Object.entries(existing)) {
      store[id] = value;
    }
    const initial: TldrawDoc = {
      '@patchwork': { type: 'tldraw' },
      store,
      schema: currentSchema,
    };
    const tldrawHandle = (await repo.create2(initial)) as DocHandle<TldrawDoc>;

    handle.change((d: any) => {
      d.tldraw = tldrawHandle.url;
    });
    console.log(LOG, 'migration complete, tldraw doc:', tldrawHandle.url);
    return tldrawHandle;
  }

  // No tldraw data yet — create with a proper snapshot.
  console.log(LOG, 'creating new tldraw doc');
  const tlStore = createTLStore({
    shapeUtils: [PatchworkDocShapeUtil, ...defaultShapeUtils],
  });
  const snapshot = tlStore.getStoreSnapshot();

  const initial: TldrawDoc = {
    '@patchwork': { type: 'tldraw' },
    store: tldrawValueToAutomergeValue(snapshot.store),
    schema: snapshot.schema,
  };
  const tldrawHandle = (await repo.create2(initial)) as DocHandle<TldrawDoc>;

  handle.change((d: any) => {
    d.tldraw = tldrawHandle.url;
  });
  console.log(LOG, 'new tldraw doc created:', tldrawHandle.url);
  return tldrawHandle;
}

// =============================================================================
//  initializeSync — called exactly once when editor + doc are both ready
// =============================================================================

async function initializeSync(
  editor: Editor,
  handle: DocHandle<SpaceDoc>,
  repo: any,
  preventPatchApplicationsRef: React.MutableRefObject<boolean>,
  isReconcilingRef: React.MutableRefObject<boolean>,
  cleanupFnsRef: React.MutableRefObject<(() => void)[]>,
) {
  const currentDoc = handle.doc?.();
  if (!currentDoc) {
    console.error(LOG, 'initializeSync called but .doc() returned null!');
    return;
  }

  const folderDocs: DocLink[] = currentDoc.docs ?? [];

  // Resolve (or create/migrate) the dedicated tldraw document.
  const tldrawHandle = await resolveTldrawHandle(handle, repo);
  const tldrawDoc = tldrawHandle.doc();

  const storedCount = tldrawDoc?.store
    ? Object.keys(tldrawDoc.store).length
    : 0;

  console.log(
    LOG,
    'initializeSync',
    '| folder docs:',
    folderDocs.length,
    '| stored tldraw records:',
    storedCount,
    '| tldraw doc:',
    tldrawHandle.url,
  );

  // ------------------------------------------------------------------
  // 1.  Set up tldraw → automerge listener FIRST so that shapes created
  //     by reconcile (step 3) get persisted.
  // ------------------------------------------------------------------

  const unsubStore = editor.store.listen(
    ({ changes, source }) => {
      if (preventPatchApplicationsRef.current) {
        console.log(LOG, 'tldraw→am SKIP (preventPatchApplications=true, source=' + source + ')');
        return;
      }

      preventPatchApplicationsRef.current = true;
      try {
        // Filter out ephemeral shapes (e.g. transcript crawl) before persisting.
        const isEphemeral = (r: any) => r.meta?.ephemeral === true;
        const filteredChanges = {
          added: Object.fromEntries(Object.entries(changes.added).filter(([, r]) => !isEphemeral(r))),
          updated: Object.fromEntries(Object.entries(changes.updated).filter(([, [, after]]: any) => !isEphemeral(after))),
          removed: Object.fromEntries(Object.entries(changes.removed).filter(([, r]) => !isEphemeral(r))),
        };

        // Only persist if there are non-ephemeral changes.
        const hasChanges = Object.keys(filteredChanges.added).length > 0 ||
          Object.keys(filteredChanges.updated).length > 0 ||
          Object.keys(filteredChanges.removed).length > 0;

        if (hasChanges) {
          // Persist tldraw records to the dedicated tldraw doc.
          tldrawHandle.change((d: any) => {
            applyTLStoreChangesToAutomerge(d, filteredChanges);
          });
        }

        // When patchwork-doc shapes are deleted, remove from the folder doc list.
        // When patchwork-doc shapes are added (e.g. undo), re-insert into the folder doc list.
        // When patchwork-doc shapes are renamed, sync name to the folder doc list.
        // Skip during reconciliation (those changes reflect docs already gone/synced).
        if (!isReconcilingRef.current) {
          const added = Object.values(changes.added);
          const removed = Object.values(changes.removed);
          const updatedPairs = Object.values(changes.updated);
          const updated = updatedPairs.map(([, after]) => after);

          // Shapes that were just added with a docUrl (e.g. undo)
          // Exclude shapes pointing at the folder itself (e.g. sideboard)
          const docsToAdd = added.filter(
            (r: any) => r.type === PATCHWORK_DOC_SHAPE_TYPE && r.props?.docUrl && r.props.docUrl !== handle.url,
          );

          // Shapes whose docUrl went from empty to non-empty (NewDocTool flow:
          // shape is created with docUrl='', then updated once the doc is ready)
          const newlyLinked = updatedPairs
            .filter(([before, after]: any) =>
              after.type === PATCHWORK_DOC_SHAPE_TYPE &&
              after.props?.docUrl &&
              after.props.docUrl !== handle.url &&
              !before.props?.docUrl,
            )
            .map(([, after]) => after);

          // Combine both sources of new docs
          docsToAdd.push(...newlyLinked);

          const docsToRemove = removed.filter(
            (r: any) => r.type === PATCHWORK_DOC_SHAPE_TYPE && r.props?.docUrl && r.props.docUrl !== handle.url,
          );
          const docsRenamed = updated.filter(
            (r: any) => r.type === PATCHWORK_DOC_SHAPE_TYPE && r.props?.docUrl && r.props?.docName,
          );

          if (docsToAdd.length > 0 || docsToRemove.length > 0 || docsRenamed.length > 0) {
            // Collect URLs that still have shapes on canvas (after removals).
            // Only remove a URL from the folder list if NO shapes reference it.
            const remainingShapes = editor.getCurrentPageShapes();
            const urlsStillOnCanvas = new Set<string>();
            for (const s of remainingShapes) {
              if (s.type === PATCHWORK_DOC_SHAPE_TYPE) {
                urlsStillOnCanvas.add((s as any).props?.docUrl);
              }
            }

            handle.change((d) => {
              if (!d.docs) d.docs = [];
              for (const record of docsToRemove) {
                const r = record as any;
                const url = r.props.docUrl;
                // Don't remove from folder if another shape still uses this URL
                if (urlsStillOnCanvas.has(url)) continue;
                const idx = d.docs.findIndex((doc: any) => doc.url === url);
                if (idx >= 0) {
                  d.docs.splice(idx, 1);
                }
              }
              for (const record of docsToAdd) {
                const r = record as any;
                const alreadyExists = d.docs.some((doc: any) => doc.url === r.props.docUrl);
                if (!alreadyExists) {
                  d.docs.push({
                    name: r.props.docName || '',
                    type: r.props.docType || '',
                    url: r.props.docUrl,
                  });
                  console.log(LOG, 'reinserted doc into folder list:', r.props.docUrl);
                }
              }
              for (const record of docsRenamed) {
                const r = record as any;
                const entry = d.docs.find((doc: any) => doc.url === r.props.docUrl);
                if (entry && (entry as any).name !== r.props.docName) {
                  (entry as any).name = r.props.docName;
                  console.log(LOG, 'synced doc name to folder list:', r.props.docName);
                }
              }
            });
          }
        }

        console.log(LOG, 'tldraw→am handle.change() succeeded');
      } catch (e) {
        console.error(LOG, 'tldraw→am handle.change() THREW', e);
      }
      preventPatchApplicationsRef.current = false;
    },
    { source: 'user', scope: 'document' },
  );
  cleanupFnsRef.current.push(() => {
    console.log(LOG, 'unsubscribing store listener');
    unsubStore();
  });

  // ------------------------------------------------------------------
  // 2.  Set up automerge → tldraw listener (on the dedicated tldraw doc)
  // ------------------------------------------------------------------

  const handleTldrawDocChange = ({ patches }: DocHandleChangePayload<TldrawDoc>) => {
    if (preventPatchApplicationsRef.current) {
      return;
    }

    // applyAutomergePatchesToTLStore already filters for path[0] === "store"
    // and naturally skips @patchwork patches.
    if (patches.length === 0) return;

    console.log(LOG, 'am→tldraw (remote patches)', patches.length, 'patches');

    preventPatchApplicationsRef.current = true;
    try {
      applyAutomergePatchesToTLStore(patches, editor.store);
    } catch (e) {
      console.error(LOG, 'am→tldraw applyPatches THREW', e);
    }
    preventPatchApplicationsRef.current = false;
  };

  tldrawHandle.on('change', handleTldrawDocChange as any);
  cleanupFnsRef.current.push(() => {
    console.log(LOG, 'removing tldraw handle change listener');
    tldrawHandle.off('change', handleTldrawDocChange as any);
  });

  // ------------------------------------------------------------------
  // 3.  Load stored document-scoped records into the editor
  //     Skip session-scoped records (camera, instance, pointer, etc.)
  //     and page records (tldraw generates a fresh page on each mount).
  // ------------------------------------------------------------------

  const SESSION_RECORD_TYPES = new Set([
    'camera', 'instance', 'instance_page_state', 'instance_presence', 'pointer', 'page',
  ]);

  if (tldrawDoc?.store) {
    const storedRecords: TLRecord[] = [];
    for (const [id, value] of Object.entries(tldrawDoc.store)) {
      if (!value || typeof value !== 'object') continue;
      if (SESSION_RECORD_TYPES.has(value.typeName)) continue;
      storedRecords.push(value as TLRecord);
    }

    if (storedRecords.length > 0) {
      console.log(LOG, 'loading', storedRecords.length, 'stored records into tldraw store');
      preventPatchApplicationsRef.current = true;
      try {
        editor.store.mergeRemoteChanges(() => {
          editor.store.put(storedRecords);
        });
        console.log(LOG, 'stored records loaded OK');
      } catch (e) {
        console.error(LOG, 'loading stored records THREW', e);
      }
      preventPatchApplicationsRef.current = false;
    } else {
      console.log(LOG, 'no stored records to load');
    }
  }

  // ------------------------------------------------------------------
  // 3b. Register image/file and paste handlers
  // ------------------------------------------------------------------

  editor.registerExternalAssetHandler('file', async ({ file, assetId }) => {
    const isImage = file.type.startsWith('image/');
    const isVideo = file.type.startsWith('video/');

    const id = assetId ?? (`asset:${crypto.randomUUID()}` as TLAssetId);

    const bytes = new Uint8Array(await file.arrayBuffer());
    const ext = extensionForMimeType(file.type);
    const name =
      file.name && file.name !== 'image.png'
        ? file.name
        : `Pasted image on ${new Date().toLocaleDateString()}.${ext}`;

    const fileHandle = await repo.create2({
      content: bytes,
      extension: ext,
      mimeType: file.type,
      name,
    });

    const asset = await getMediaAssetInfoPartial(file, id, isImage, isVideo);
    asset.props.src = automergeUrlToServiceWorkerUrl(fileHandle.url);

    return asset as TLAsset;
  });

  editor.registerExternalContentHandler(
    'tldraw',
    ({ point, content }: { point?: VecLike; content: TLContent }) => {
      editor.run(() => {
        const selectionBoundsBefore = editor.getSelectionPageBounds();
        editor.markHistoryStoppingPoint('paste');

        for (const shape of content.shapes) {
          if (content.rootShapeIds.includes(shape.id)) {
            shape.isLocked = false;
          }
        }

        content.schema = editor.store.schema.serialize();

        editor.putContentOntoCurrentPage(content, {
          point,
          select: true,
        });

        const selectedBoundsAfter = editor.getSelectionPageBounds();
        if (
          selectionBoundsBefore &&
          selectedBoundsAfter &&
          selectionBoundsBefore.collides(selectedBoundsAfter)
        ) {
          editor.updateInstanceState({ isChangingStyle: true });
        }
      });
    },
  );

  // ------------------------------------------------------------------
  // 3b-2. Handle pasted/dropped YouTube URLs → create playable embed
  // ------------------------------------------------------------------

  editor.registerExternalContentHandler(
    'url',
    async ({ point, url }: { point?: VecLike; url: string }) => {
      // Handle patchwork tiny links (e.g. https://tiny.patchwork.inkandswitch.com/#doc=XXXX&title=...&type=...&tool=...&heads=...)
      try {
        const parsed = new URL(url);
        if (parsed.hostname === 'tiny.patchwork.inkandswitch.com' && parsed.hash) {
          const params = new URLSearchParams(parsed.hash.slice(1));
          const docId = params.get('doc');
          if (docId) {
            const heads = params.get('heads');
            const automergeUrl = `automerge:${docId}${heads ? `#${heads}` : ''}` as AutomergeUrl;
            const title = params.get('title') || '';
            const type = params.get('type') || '';
            const toolId = params.get('tool') || '';

            const position = point ?? editor.getViewportPageBounds().center;
            const shapeId = makeShapeId(automergeUrl);

            // Add to folder doc list
            handle.change((d) => {
              if (!d.docs) d.docs = [];
              const alreadyExists = d.docs.some((doc: any) => doc.url === automergeUrl);
              if (!alreadyExists) {
                d.docs.push({ name: title, type, url: automergeUrl });
              }
            });

            // Create shape on canvas if it doesn't already exist
            if (!editor.getShape(shapeId)) {
              editor.createShape({
                id: shapeId,
                type: PATCHWORK_DOC_SHAPE_TYPE,
                x: position.x - DEFAULT_W / 2,
                y: position.y - DEFAULT_H / 2,
                props: {
                  w: DEFAULT_W,
                  h: DEFAULT_H,
                  docUrl: automergeUrl,
                  docName: title,
                  docType: type,
                  toolId,
                },
              } as any);
            }

            console.log(LOG, 'patchwork link pasted:', automergeUrl, '(tool:', toolId, ')');
            return;
          }
        }
      } catch {
        // Not a valid URL or parsing failed, continue to other handlers
      }

      const ytMatch = url.match(
        /(?:youtube\.com\/(?:watch\?v=|embed\/|shorts\/)|youtu\.be\/)([a-zA-Z0-9_-]{11})/,
      );

      if (ytMatch) {
        const videoId = ytMatch[1];
        const embedUrl = `https://www.youtube.com/embed/${videoId}?enablejsapi=1`;
        const position = point ?? editor.getViewportPageBounds().center;
        const id = createShapeId();
        editor.createShape({
          id,
          type: 'embed',
          x: position.x - 240,
          y: position.y - 135,
          props: {
            w: 480,
            h: 270,
            url: embedUrl,
          },
        });
        console.log(LOG, 'YouTube embed created for', url);
        return;
      }

      // For non-YouTube URLs, fall back to tldraw's default bookmark shape
      const position = point ?? editor.getViewportPageBounds().center;
      await createBookmarkFromUrl(editor, { url, center: position });
    },
  );

  // ------------------------------------------------------------------
  // 3b-3. Handle drops with text/x-patchwork-urls
  // ------------------------------------------------------------------

  const container = editor.getContainer();

  const handlePatchworkDrop = async (e: DragEvent) => {
    const raw = e.dataTransfer?.getData('text/x-patchwork-urls');
    if (!raw) return;

    e.preventDefault();
    e.stopPropagation();

    let urls: string[];
    try {
      urls = JSON.parse(raw);
      if (!Array.isArray(urls)) return;
    } catch {
      return;
    }

    const dropPoint = editor.screenToPage({ x: e.clientX, y: e.clientY });

    for (let i = 0; i < urls.length; i++) {
      const url = urls[i] as AutomergeUrl;
      const shapeId = makeShapeId(url);

      let docName = '';
      let docType = '';
      try {
        const docHandle = await repo.find(url);
        const doc = docHandle.doc();
        if (doc) {
          docType = doc['@patchwork']?.type || '';
          docName = doc.title || doc.name || '';
        }
      } catch {
        // Proceed with empty metadata
      }

      handle.change((d: any) => {
        if (!d.docs) d.docs = [];
        d.docs.push({ name: docName, type: docType, url });
      });

      editor.createShape({
        id: shapeId,
        type: PATCHWORK_DOC_SHAPE_TYPE,
        x: dropPoint.x + i * 30 - DEFAULT_W / 2,
        y: dropPoint.y + i * 30 - DEFAULT_H / 2,
        props: {
          w: DEFAULT_W,
          h: DEFAULT_H,
          docUrl: url,
          docName,
          docType,
          toolId: '',
        },
      } as any);

      console.log(LOG, 'dropped patchwork doc:', url, docName);
    }
  };

  const handleDragOver = (e: DragEvent) => {
    if (e.dataTransfer?.types.includes('text/x-patchwork-urls')) {
      e.preventDefault();
    }
  };

  // Use capture phase so we intercept before tldraw's handlers
  container.addEventListener('drop', handlePatchworkDrop, true);
  container.addEventListener('dragover', handleDragOver, true);
  cleanupFnsRef.current.push(() => {
    container.removeEventListener('drop', handlePatchworkDrop, true);
    container.removeEventListener('dragover', handleDragOver, true);
  });

  // ------------------------------------------------------------------
  // 3b-4. Star Wars transcript crawl — chee:text-stream listener
  // ------------------------------------------------------------------

  const CRAWL_LINE_HEIGHT = 32;
  const CRAWL_MAX_LINES = 30;

  const handleTextStream = (e: Event) => {
    const { text, speaker, sourceUrl } = (e as CustomEvent).detail;
    if (!sourceUrl) return;

    // Find the patchwork-doc shape whose docUrl matches sourceUrl
    const allShapes = editor.getCurrentPageShapes();
    const sourceShape = allShapes.find(
      (s: any) => s.type === PATCHWORK_DOC_SHAPE_TYPE && s.props?.docUrl === sourceUrl,
    );
    if (!sourceShape) return;

    // Gather existing crawl shapes for this source
    const crawlShapes = allShapes
      .filter((s: any) => s.meta?.ephemeral && s.meta?.crawlSource === sourceUrl)
      .sort((a, b) => b.y - a.y); // newest (lowest y, closest to source) first

    // Shift all existing crawl shapes up by one line
    for (const shape of crawlShapes) {
      editor.updateShape({
        id: shape.id,
        type: shape.type,
        y: shape.y - CRAWL_LINE_HEIGHT,
      });
    }

    // Remove oldest shapes if over the cap
    if (crawlShapes.length >= CRAWL_MAX_LINES) {
      const toRemove = crawlShapes.slice(CRAWL_MAX_LINES - 1);
      editor.deleteShapes(toRemove.map((s) => s.id));
    }

    // Create a new text shape just above the source shape
    const sourceBounds = editor.getShapePageBounds(sourceShape);
    if (!sourceBounds) return;
    const sourceCenterX = sourceBounds.x + sourceBounds.w / 2;

    const label = speaker ? `${speaker}: ${text}` : text;

    editor.createShape({
      id: createShapeId(),
      type: 'text',
      x: sourceCenterX - 150,
      y: sourceBounds.y - CRAWL_LINE_HEIGHT - 8,
      props: {
        richText: toRichText(label),
        autoSize: true,
        size: 's',
        w: 300,
      },
      meta: {
        ephemeral: true,
        crawlSource: sourceUrl,
      },
    });
  };

  container.addEventListener('chee:text-stream', handleTextStream);
  cleanupFnsRef.current.push(() => {
    container.removeEventListener('chee:text-stream', handleTextStream);
  });

  // ------------------------------------------------------------------
  // 3c. Ensure patchwork-doc shapes exist for every folder item
  //     These are created as *user* changes so the store listener
  //     (step 1) persists them to automerge automatically.
  // ------------------------------------------------------------------

  const withoutOwn = folderDocs.filter((d) => d.url !== tldrawHandle.url);

  isReconcilingRef.current = true;
  reconcilePatchworkDocShapes(editor, withoutOwn);
  isReconcilingRef.current = false;

  filterTldrawDocs(repo, withoutOwn).then((filteredDocs) => {
    isReconcilingRef.current = true;
    reconcilePatchworkDocShapes(editor, filteredDocs);
    isReconcilingRef.current = false;
  });

  // ------------------------------------------------------------------
  // 5.  Restore camera from localStorage, or zoom to fit
  // ------------------------------------------------------------------

  const cameraKey = `space-camera:${handle.url}`;

  const shapeCount = editor.getCurrentPageShapes().length;
  const savedCamera = (() => {
    try {
      const raw = localStorage.getItem(cameraKey);
      return raw ? JSON.parse(raw) : null;
    } catch {
      return null;
    }
  })();

  if (savedCamera && typeof savedCamera.x === 'number' && typeof savedCamera.y === 'number' && typeof savedCamera.z === 'number') {
    console.log(LOG, 'restoring saved camera position');
    requestAnimationFrame(() => {
      try {
        editor.setCamera({ x: savedCamera.x, y: savedCamera.y, z: savedCamera.z });
      } catch {
        /* editor may have been disposed */
      }
    });
  } else if (shapeCount > 0) {
    console.log(LOG, 'zooming to fit', shapeCount, 'shapes');
    requestAnimationFrame(() => {
      try {
        editor.zoomToFit({ animation: { duration: 300 } });
      } catch {
        /* editor may have been disposed */
      }
    });
  }

  // Persist camera position to localStorage on changes (debounced).
  let cameraSaveTimer: ReturnType<typeof setTimeout> | null = null;
  const unsubCamera = editor.store.listen(
    () => {
      if (cameraSaveTimer) clearTimeout(cameraSaveTimer);
      cameraSaveTimer = setTimeout(() => {
        try {
          const camera = editor.getCamera();
          localStorage.setItem(cameraKey, JSON.stringify({ x: camera.x, y: camera.y, z: camera.z }));
        } catch {
          /* localStorage may be full or unavailable */
        }
      }, 500);
    },
    { source: 'user', scope: 'session' },
  );
  cleanupFnsRef.current.push(() => {
    unsubCamera();
    if (cameraSaveTimer) clearTimeout(cameraSaveTimer);
  });

  // ------------------------------------------------------------------
  // 6.  YouTube playback sync via broadcast channel
  // ------------------------------------------------------------------

  const ytCurrentTime = new Map<string, number>();
  let ytSuppressBroadcast = false;

  // Find YouTube iframes, ensure they have enablejsapi=1 in their src,
  // and send the "listening" handshake so they post state-change events.
  const initYTIframes = () => {
    const iframes = document.querySelectorAll(
      '.pw-space-root .tl-embed-container iframe.tl-embed',
    ) as NodeListOf<HTMLIFrameElement>;
    for (const iframe of iframes) {
      if (!iframe.src.includes('youtube.com/embed/')) continue;

      // Inject enablejsapi=1 if tldraw stripped it from the URL
      if (!iframe.src.includes('enablejsapi=1')) {
        const separator = iframe.src.includes('?') ? '&' : '?';
        iframe.src = `${iframe.src}${separator}enablejsapi=1`;
        // iframe will reload — skip handshake for now, next poll will catch it
        iframe.dataset.ytInitialized = '';
        continue;
      }

      if (iframe.dataset.ytInitialized) continue;
      iframe.dataset.ytInitialized = 'true';

      // Send the listening handshake once the iframe is ready
      const sendListening = () => {
        iframe.contentWindow?.postMessage(
          JSON.stringify({ event: 'listening', id: 1 }),
          '*',
        );
        console.log(LOG, 'youtube: sent listening handshake to', iframe.src);
      };

      if (iframe.contentDocument?.readyState === 'complete') {
        sendListening();
      } else {
        iframe.addEventListener('load', sendListening, { once: true });
      }
    }
  };

  const ytInitInterval = setInterval(initYTIframes, 2000);
  initYTIframes();
  cleanupFnsRef.current.push(() => clearInterval(ytInitInterval));

  // Listen for YouTube postMessage events (state changes + current time).
  const handleYTMessage = (event: MessageEvent) => {
    if (typeof event.data !== 'string') return;
    let data: any;
    try {
      data = JSON.parse(event.data);
    } catch {
      return;
    }

    // Find the videoId of the iframe that sent this message.
    const findVideoId = (): string | null => {
      const iframes = document.querySelectorAll(
        '.pw-space-root .tl-embed-container iframe.tl-embed',
      ) as NodeListOf<HTMLIFrameElement>;
      // Try matching by event.source first
      for (const iframe of iframes) {
        try {
          if (iframe.contentWindow === event.source) {
            const match = iframe.src.match(/embed\/([a-zA-Z0-9_-]{11})/);
            return match ? match[1] : null;
          }
        } catch {
          // contentWindow access may fail for sandboxed iframes
        }
      }
      // Fallback: if there's only one YouTube iframe, assume it sent the event
      const ytIframes = Array.from(iframes).filter((f) =>
        f.src.includes('youtube.com/embed/'),
      );
      if (ytIframes.length === 1) {
        const match = ytIframes[0].src.match(/embed\/([a-zA-Z0-9_-]{11})/);
        return match ? match[1] : null;
      }
      // If data includes info.videoData.video_id, use that
      if (data.info?.videoData?.video_id) {
        return data.info.videoData.video_id;
      }
      return null;
    };

    // Log all YouTube events for debugging
    if (data.event) {
      console.log(LOG, 'youtube event:', data.event, data.info);
    }

    // Track current time from periodic infoDelivery events.
    if (data.event === 'infoDelivery' && data.info?.currentTime !== undefined) {
      const vid = findVideoId();
      if (vid) ytCurrentTime.set(vid, data.info.currentTime);
      return;
    }

    // Broadcast play/pause state changes to peers.
    if (data.event === 'onStateChange' && !ytSuppressBroadcast) {
      const state = typeof data.info === 'number' ? data.info : data.info?.playerState;
      // 1 = playing, 2 = paused
      if (state !== 1 && state !== 2) return;
      const videoId = findVideoId();
      if (!videoId) return;

      handle.broadcast({
        type: 'youtube-sync',
        videoId,
        state,
        currentTime: ytCurrentTime.get(videoId) ?? 0,
      });
      console.log(LOG, 'youtube broadcast:', state === 1 ? 'play' : 'pause', videoId, 'at', ytCurrentTime.get(videoId));
    }
  };

  window.addEventListener('message', handleYTMessage);
  cleanupFnsRef.current.push(() => window.removeEventListener('message', handleYTMessage));

  // Receive YouTube sync broadcasts from peers.
  const handleYTBroadcast = (payload: any) => {
    const msg = payload.message;
    if (msg?.type !== 'youtube-sync') return;

    console.log(LOG, 'youtube received:', msg.state === 1 ? 'play' : 'pause', msg.videoId, '@', msg.currentTime);

    ytSuppressBroadcast = true;
    const iframes = document.querySelectorAll(
      '.pw-space-root .tl-embed-container iframe.tl-embed',
    ) as NodeListOf<HTMLIFrameElement>;

    for (const iframe of iframes) {
      if (!iframe.src.includes(msg.videoId)) continue;
      const win = iframe.contentWindow;
      if (!win) continue;

      // Seek first, then play or pause.
      win.postMessage(
        JSON.stringify({ event: 'command', func: 'seekTo', args: [msg.currentTime, true] }),
        '*',
      );

      if (msg.state === 1) {
        win.postMessage(
          JSON.stringify({ event: 'command', func: 'playVideo', args: '' }),
          '*',
        );
      } else if (msg.state === 2) {
        win.postMessage(
          JSON.stringify({ event: 'command', func: 'pauseVideo', args: '' }),
          '*',
        );
      }
    }

    // Re-enable broadcasting after a short delay to avoid echo.
    setTimeout(() => {
      ytSuppressBroadcast = false;
    }, 500);
  };

  handle.on('ephemeral-message', handleYTBroadcast as any);
  cleanupFnsRef.current.push(() => handle.off('ephemeral-message', handleYTBroadcast as any));

  console.log(LOG, 'initializeSync complete ✓');
}

// =============================================================================
//  reconcilePatchworkDocShapes — create / update / remove folder-item shapes
// =============================================================================

function reconcilePatchworkDocShapes(editor: Editor, folderDocs: DocLink[]) {
  // Collect ALL shapes per URL (there may be duplicates from sync races).
  const existing = new Map<string, TLShapeId[]>();

  for (const shape of editor.getCurrentPageShapes()) {
    if (shape.type === PATCHWORK_DOC_SHAPE_TYPE) {
      const url = (shape as any).props.docUrl;
      if (!url) continue;
      const ids = existing.get(url) ?? [];
      ids.push(shape.id);
      existing.set(url, ids);
    }
  }

  const folderUrls = new Set<string>(folderDocs.map((d) => d.url));
  let nextIdx = existing.size;

  console.log(
    LOG,
    'reconcile: existing patchwork shapes:',
    existing.size,
    'URLs |',
    'folder docs:',
    folderDocs.length,
  );

  for (const docLink of folderDocs) {
    const shapeIds = existing.get(docLink.url);
    if (shapeIds && shapeIds.length > 0) {
      // Already on canvas — update metadata on the first shape if stale
      const shapeId = shapeIds[0];
      const shape = editor.getShape(shapeId) as any;
      if (shape && (shape.props.docName !== docLink.name || shape.props.docType !== docLink.type)) {
        console.log(LOG, 'reconcile: updating metadata for', docLink.name);
        editor.updateShape({
          id: shapeId,
          type: PATCHWORK_DOC_SHAPE_TYPE,
          props: { docName: docLink.name, docType: docLink.type },
        } as any);
      }

      continue;
    }

    // New doc — create a shape
    const pos = defaultPosition(nextIdx++);
    console.log(LOG, 'reconcile: creating shape for', docLink.name, 'at', pos.x, pos.y);
    editor.createShape({
      id: makeShapeId(docLink.url),
      type: PATCHWORK_DOC_SHAPE_TYPE,
      x: pos.x,
      y: pos.y,
      rotation: 0,
      props: {
        w: DEFAULT_W,
        h: DEFAULT_H,
        docUrl: docLink.url,
        docName: docLink.name,
        docType: docLink.type,
        toolId: '',
      },
    } as any);
  }

  // Remove shapes whose doc was removed from the folder
  for (const [url, shapeIds] of existing) {
    if (!folderUrls.has(url)) {
      const toDelete = shapeIds.filter((id) => editor.getShape(id));
      if (toDelete.length > 0) {
        console.log(LOG, 'reconcile: removing', toDelete.length, 'shapes for deleted doc', url);
        editor.deleteShapes(toDelete);
      }
    }
  }
}
