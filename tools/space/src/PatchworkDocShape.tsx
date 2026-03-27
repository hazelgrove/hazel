import {
  Geometry2d,
  HTMLContainer,
  RecordProps,
  Rectangle2d,
  ShapeUtil,
  T,
  TLResizeInfo,
  TLShape,
  resizeBox,
  useEditor,
  useValue,
} from 'tldraw';
import { useMemo, useCallback, useRef, useEffect, useState, useContext } from 'react';
import {
  getSupportedToolsForType,
  getRegistry,
  type LoadedTool,
  type DatatypeDescription,
  type LoadedDatatype,
} from '@inkandswitch/patchwork-plugins';
import { openDocument } from '@inkandswitch/patchwork-elements';
import type { AutomergeUrl } from '@automerge/automerge-repo';
import { parseAutomergeUrl, encodeHeads, stringifyAutomergeUrl } from '@automerge/automerge-repo';
import { getHeads } from '@automerge/automerge';
import { RepoContext, useDocument } from '@automerge/automerge-repo-react-hooks';
import { automergeUrlToServiceWorkerUrl } from '@inkandswitch/patchwork-filesystem';

export const PATCHWORK_DOC_SHAPE_TYPE = 'patchwork-doc' as const;

// ---------------------------------------------------------------------------
// Module augmentation – register the shape props in tldraw's type system
// ---------------------------------------------------------------------------

declare module 'tldraw' {
  export interface TLGlobalShapePropsMap {
    [PATCHWORK_DOC_SHAPE_TYPE]: {
      w: number;
      h: number;
      docUrl: string;
      docName: string;
      docType: string;
      toolId: string;
    };
  }
}

export type PatchworkDocShape = TLShape<typeof PATCHWORK_DOC_SHAPE_TYPE>;

// ---------------------------------------------------------------------------
// useSupportedToolsForDatatype – React hook
// ---------------------------------------------------------------------------

export function useSupportedToolsForDatatype(datatype: string): LoadedTool[] {
  return useMemo(() => {
    if (!datatype) return [];
    try {
      return getSupportedToolsForType(datatype);
    } catch {
      return [];
    }
  }, [datatype]);
}

// ---------------------------------------------------------------------------
// ShapeUtil
// ---------------------------------------------------------------------------

export class PatchworkDocShapeUtil extends ShapeUtil<PatchworkDocShape> {
  static override type = PATCHWORK_DOC_SHAPE_TYPE;

  static override props: RecordProps<PatchworkDocShape> = {
    w: T.number,
    h: T.number,
    docUrl: T.string,
    docName: T.string,
    docType: T.string,
    toolId: T.string,
  };

  getDefaultProps(): PatchworkDocShape['props'] {
    return {
      w: 640,
      h: 480,
      docUrl: '',
      docName: 'Untitled',
      docType: '',
      toolId: '',
    };
  }

  getGeometry(shape: PatchworkDocShape): Geometry2d {
    return new Rectangle2d({
      width: shape.props.w,
      height: shape.props.h,
      isFilled: true,
    });
  }

  override canResize() {
    return true;
  }
  override canEdit() {
    return true;
  }
  override isAspectRatioLocked() {
    return false;
  }
  override hideRotateHandle() {
    return true;
  }

  override onResize(shape: any, info: TLResizeInfo<any>) {
    return resizeBox(shape, info);
  }

  component(shape: PatchworkDocShape) {
    return <PatchworkDocComponent shape={shape} />;
  }

  indicator(shape: PatchworkDocShape) {
    return <rect width={shape.props.w} height={shape.props.h} />;
  }
}

// ---------------------------------------------------------------------------
// Mac OS 7.5 titlebar styling
// ---------------------------------------------------------------------------

// Horizontal lines with clear 1px gaps at top and bottom of the titlebar
// Mac OS 7.5 used light grey stripes, not black
const TITLEBAR_STRIPES = [
  'linear-gradient(#fff, #fff)',        // top clear line
  'linear-gradient(#fff, #fff)',        // bottom clear line
  'repeating-linear-gradient(#d0d0d0 0px, #d0d0d0 1px, transparent 1px, transparent 3px)',
].join(', ');

const TITLEBAR_BG_SIZE = '100% 1px, 100% 1px, 100% 100%';
const TITLEBAR_BG_POS = 'top, bottom, center';
const TITLEBAR_BG_REPEAT = 'no-repeat, no-repeat, repeat';

// ---------------------------------------------------------------------------
// Inner React component (so we can use hooks like useEditor)
// ---------------------------------------------------------------------------

async function loadDatatype(id: string): Promise<LoadedDatatype | undefined> {
  try {
    const registry = getRegistry<DatatypeDescription>('patchwork:datatype');
    return (await registry.load(id)) as unknown as LoadedDatatype | undefined;
  } catch {
    return undefined;
  }
}

const CLICK_THRESHOLD = 5;

function ClickableTitle({
  docName,
  onClickTitle,
}: {
  docName: string;
  onClickTitle: () => void;
}) {
  const spanRef = useRef<HTMLSpanElement>(null);
  const downPos = useRef<{ x: number; y: number } | null>(null);

  useEffect(() => {
    const span = spanRef.current;
    if (!span) return;

    const onDown = (e: PointerEvent) => {
      downPos.current = { x: e.clientX, y: e.clientY };
    };

    // Listen on window so we still get the event even when tldraw
    // captures the pointer for dragging.
    const onUp = (e: PointerEvent) => {
      if (!downPos.current) return;
      const dx = e.clientX - downPos.current.x;
      const dy = e.clientY - downPos.current.y;
      downPos.current = null;
      if (Math.abs(dx) < CLICK_THRESHOLD && Math.abs(dy) < CLICK_THRESHOLD) {
        onClickTitle();
      }
    };

    span.addEventListener('pointerdown', onDown);
    window.addEventListener('pointerup', onUp);
    return () => {
      span.removeEventListener('pointerdown', onDown);
      window.removeEventListener('pointerup', onUp);
    };
  }, [onClickTitle]);

  return (
    <span
      ref={spanRef}
      style={{
        fontSize: '13px',
        fontWeight: 700,
        color: '#000',
        overflow: 'hidden',
        textOverflow: 'ellipsis',
        whiteSpace: 'nowrap',
        fontFamily: 'Geneva, "Lucida Grande", "Helvetica Neue", Helvetica, sans-serif',
        textAlign: 'center',
        padding: '0 6px',
        background: '#fff',
        cursor: 'text',
        display: 'block',
      }}
    >
      {docName}
    </span>
  );
}

const SPARKLE_POSITIONS = [
  { top: '-6px', left: '10%' },
  { top: '-6px', right: '20%' },
  { top: '15%', right: '-6px' },
  { top: '60%', right: '-6px' },
  { bottom: '-6px', right: '25%' },
  { bottom: '-6px', left: '15%' },
  { top: '40%', left: '-6px' },
  { top: '8%', left: '-6px' },
];

function Sparkles() {
  return (
    <>
      {SPARKLE_POSITIONS.map((pos, i) => (
        <span
          key={i}
          style={{
            position: 'absolute',
            ...pos,
            width: '10px',
            height: '10px',
            pointerEvents: 'none',
            zIndex: 10000,
            animation: `sparkle-pop 600ms ease-out ${i * 60}ms both`,
          }}
        >
          <svg viewBox="0 0 10 10" width="10" height="10">
            <path
              d="M5 0 L6 4 L10 5 L6 6 L5 10 L4 6 L0 5 L4 4 Z"
              fill="#ffd700"
            />
          </svg>
        </span>
      ))}
    </>
  );
}

function MenuItem({ label, onClick }: { label: string; onClick: () => void }) {
  return (
    <button
      onClick={(e) => { e.stopPropagation(); onClick(); }}
      onPointerDown={(e) => e.stopPropagation()}
      onPointerEnter={(e) => {
        (e.currentTarget as HTMLElement).style.background = '#000';
        (e.currentTarget as HTMLElement).style.color = '#fff';
      }}
      onPointerLeave={(e) => {
        (e.currentTarget as HTMLElement).style.background = 'transparent';
        (e.currentTarget as HTMLElement).style.color = '#000';
      }}
      style={{
        display: 'block',
        width: '100%',
        padding: '2px 12px',
        border: 'none',
        background: 'transparent',
        color: '#000',
        fontSize: '11px',
        fontFamily: 'inherit',
        textAlign: 'left',
        cursor: 'pointer',
        whiteSpace: 'nowrap',
      }}
    >
      {label}
    </button>
  );
}

function MenuSeparator() {
  return <div style={{ height: '1px', background: '#c0c0c0', margin: '2px 0' }} />;
}

function useIsImage(docUrl: string): boolean {
  const [doc] = useDocument<{ '@patchwork'?: { type?: string }; mimeType?: string }>(
    docUrl ? docUrl as AutomergeUrl : undefined,
  );
  return doc?.['@patchwork']?.type === 'file' && !!doc?.mimeType?.startsWith('image/');
}

function PatchworkDocComponent({ shape }: { shape: PatchworkDocShape }) {
  const { docUrl, docName, docType, toolId } = shape.props;
  const editor = useEditor();
  const repo = useContext(RepoContext);
  const isImage = useIsImage(docUrl);
  const tools = useSupportedToolsForDatatype(docType);
  const isSelectTool = useValue('is select tool', () => editor.getCurrentToolId() === 'select', [
    editor,
  ]);
  const isEditingShape = useValue(
    'is editing shape',
    () => editor.getEditingShapeId() === shape.id,
    [editor, shape.id],
  );

  const [isEditingName, setIsEditingName] = useState(false);
  const [isEditingTool, setIsEditingTool] = useState(false);
  const [menuOpen, setMenuOpen] = useState(false);
  const isFocused = isEditingShape;
  const [sparkling, setSparkling] = useState(false);
  const [copied, setCopied] = useState<string | null>(null);
  const nameInputRef = useRef<HTMLInputElement>(null);
  const toolInputRef = useRef<HTMLInputElement>(null);
  const containerRef = useRef<HTMLDivElement>(null);
  const contentRef = useRef<HTMLDivElement>(null);

  // Focus the name input when editing starts
  useEffect(() => {
    if (isEditingName && nameInputRef.current) {
      nameInputRef.current.focus();
      nameInputRef.current.select();
    }
  }, [isEditingName]);

  // Focus the tool input when editing starts
  useEffect(() => {
    if (isEditingTool && toolInputRef.current) {
      toolInputRef.current.focus();
    }
  }, [isEditingTool]);

  // Close menu on outside click
  useEffect(() => {
    if (!menuOpen) return;
    const handler = (e: PointerEvent) => {
      if (containerRef.current && !containerRef.current.contains(e.target as Node)) {
        setMenuOpen(false);
        setIsEditingTool(false);
      }
    };
    window.addEventListener('pointerdown', handler, true);
    return () => window.removeEventListener('pointerdown', handler, true);
  }, [menuOpen]);

  // Flash "Copied" feedback then clear
  useEffect(() => {
    if (!copied) return;
    const t = setTimeout(() => setCopied(null), 1200);
    return () => clearTimeout(t);
  }, [copied]);

  const copyToClipboard = useCallback(async (text: string, label: string) => {
    try {
      await navigator.clipboard.writeText(text);
      setCopied(label);
    } catch {
      setCopied('Failed');
    }
  }, []);

  const handleCopyAutomergeUrl = useCallback(() => {
    if (!docUrl) return;
    copyToClipboard(docUrl, 'URL');
    setMenuOpen(false);
  }, [docUrl, copyToClipboard]);

  const handleCopyUrlAtHeads = useCallback(async () => {
    if (!docUrl || !repo) return;
    try {
      const handle = await repo.find(docUrl as AutomergeUrl);
      const doc = handle.doc();
      if (!doc) { setCopied('No doc'); return; }
      const heads = getHeads(doc);
      const { documentId } = parseAutomergeUrl(docUrl as AutomergeUrl);
      const urlAtHeads = stringifyAutomergeUrl({ documentId, heads: encodeHeads(heads) });
      copyToClipboard(urlAtHeads, 'URL@heads');
    } catch {
      setCopied('Failed');
    }
    setMenuOpen(false);
  }, [docUrl, repo, copyToClipboard]);

  const handleCopyTinyUrl = useCallback(async () => {
    if (!docUrl || !repo) return;
    try {
      const handle = await repo.find(docUrl as AutomergeUrl);
      const doc = handle.doc();
      const { documentId } = parseAutomergeUrl(docUrl as AutomergeUrl);
      const params = new URLSearchParams();
      params.set('doc', documentId);
      if (docName) params.set('title', docName);
      if (docType) params.set('type', docType);
      if (toolId) params.set('tool', toolId);
      if (doc) {
        const heads = getHeads(doc);
        params.set('heads', encodeHeads(heads).join(','));
      }
      const tinyUrl = `https://tiny.patchwork.inkandswitch.com/#${params.toString()}`;
      copyToClipboard(tinyUrl, 'Tiny URL');
    } catch {
      setCopied('Failed');
    }
    setMenuOpen(false);
  }, [docUrl, docName, docType, toolId, repo, copyToClipboard]);

  // Listen for patchwork:mounted on the content area
  useEffect(() => {
    const el = contentRef.current;
    if (!el) return;
    const handler = () => {
      setSparkling(true);
      setTimeout(() => setSparkling(false), 800);
    };
    el.addEventListener('patchwork:mounted', handler);
    return () => el.removeEventListener('patchwork:mounted', handler);
  }, []);

  // Stop keyboard, wheel, and pointer events from reaching tldraw when
  // the shape content is focused.  This prevents tldraw keybindings from
  // activating, wheel-to-zoom from hijacking scroll, and pointer capture
  // from blocking text selection inside embedded tools.
  //
  // After stopping propagation we re-dispatch a clone of the event
  // directly on `document` so that frameworks with document-level event
  // delegation (Solid.js, etc.) still receive keyboard and pointer events.
  useEffect(() => {
    if (!isFocused) return;
    const el = contentRef.current;
    if (!el) return;

    // Re-dispatch an event on `document` preserving the original target
    // and composedPath so framework delegation walks the right DOM path.
    const REDISPATCHED = Symbol.for('patchwork-redispatched');
    const redispatchToDocument = (e: Event) => {
      const target = e.target;
      const path = e.composedPath();
      const clone = new (e.constructor as any)(e.type, e);
      (clone as any)[REDISPATCHED] = true;
      Object.defineProperty(clone, 'target', { value: target });
      clone.composedPath = () => path;
      document.dispatchEvent(clone);
    };

    const stopKey = (e: KeyboardEvent) => {
      if ((e as any)[REDISPATCHED]) return;
      e.stopPropagation();
      redispatchToDocument(e);
    };
    const stopWheel = (e: WheelEvent) => {
      // Let pinch-to-zoom (ctrlKey + wheel) pass through to tldraw
      if (e.ctrlKey) return;
      e.stopPropagation();
    };
    const stopPointer = (e: PointerEvent) => {
      if ((e as any)[REDISPATCHED]) return;
      e.stopPropagation();
      redispatchToDocument(e);
    };

    el.addEventListener('keydown', stopKey);
    el.addEventListener('keyup', stopKey);
    el.addEventListener('keypress', stopKey);
    el.addEventListener('wheel', stopWheel);
    el.addEventListener('pointerdown', stopPointer, true);
    el.addEventListener('pointermove', stopPointer, true);
    el.addEventListener('pointerup', stopPointer, true);

    return () => {
      el.removeEventListener('keydown', stopKey);
      el.removeEventListener('keyup', stopKey);
      el.removeEventListener('keypress', stopKey);
      el.removeEventListener('wheel', stopWheel);
      el.removeEventListener('pointerdown', stopPointer, true);
      el.removeEventListener('pointermove', stopPointer, true);
      el.removeEventListener('pointerup', stopPointer, true);
    };
  }, [isFocused]);

  // tldraw handles exiting editing mode when clicking outside the shape.

  const handleToolChange = useCallback(
    (newToolId: string) => {
      editor.updateShape({
        id: shape.id,
        type: PATCHWORK_DOC_SHAPE_TYPE,
        props: { toolId: newToolId },
      } as any);
    },
    [editor, shape.id],
  );

  const handleOpenDocument = useCallback(() => {
    const el = containerRef.current;
    if (!el || !docUrl) return;
    console.log('[space] patchwork:open-document', { url: docUrl, toolId: toolId || undefined });
    openDocument(el, docUrl as AutomergeUrl, toolId || undefined);
  }, [docUrl, toolId]);

  const handleRename = useCallback(
    async (newName: string) => {
      const trimmed = newName.trim();
      if (!trimmed || trimmed === docName || !repo || !docUrl || !docType) {
        setIsEditingName(false);
        return;
      }

      // Set title on the child doc via its datatype
      const datatype = await loadDatatype(docType);
      if (datatype?.module.setTitle) {
        const childHandle = await repo.find(docUrl as any);
        childHandle.change((d: any) => {
          datatype.module.setTitle!(d, trimmed);
        });

        // Read back the canonical title
        const childDoc = childHandle.doc();
        const canonicalName = childDoc ? datatype.module.getTitle(childDoc) : trimmed;

        editor.updateShape({
          id: shape.id,
          type: PATCHWORK_DOC_SHAPE_TYPE,
          props: { docName: canonicalName },
        } as any);
      }

      setIsEditingName(false);
    },
    [editor, shape.id, docName, docUrl, docType, repo],
  );

  const effectiveToolId = toolId || '';

  const selectedTool = tools.find((t) => t.id === effectiveToolId);
  const pillLabel = selectedTool ? selectedTool.name : docType || '';

  return (
    <HTMLContainer>
      <style>{`
        @keyframes tool-expand {
          from { width: 13px; opacity: 0.5; }
          to { width: 108px; opacity: 1; }
        }
        @keyframes sparkle-pop {
          0% { transform: scale(0) rotate(0deg); opacity: 1; }
          50% { transform: scale(1) rotate(180deg); opacity: 1; }
          100% { transform: scale(0) rotate(360deg); opacity: 0; }
        }
        @keyframes sparkle-border {
          0% { box-shadow: 0 0 0 0 rgba(255,215,0,0.6), inset 0 0 0 0 rgba(255,215,0,0.1); }
          30% { box-shadow: 0 0 12px 3px rgba(255,215,0,0.5), inset 0 0 8px 1px rgba(255,215,0,0.08); }
          100% { box-shadow: 0 0 0 0 rgba(255,215,0,0), inset 0 0 0 0 rgba(255,215,0,0); }
        }
      `}</style>
      <div
        ref={containerRef}
        style={{
          width: '100%',
          height: '100%',
          display: 'flex',
          flexDirection: 'column',
          overflow: 'visible',
          boxShadow: '1px 1px 0 rgba(0,0,0,0.4), 2px 2px 0 rgba(0,0,0,0.2), 0 2px 12px rgba(0,0,0,0.08)',
          border: '1px solid #888',
          borderTopColor: '#fff',
          borderLeftColor: '#fff',
          borderRadius: '2px',
          background: '#ffffff',
          pointerEvents: 'all',
          ...(sparkling ? { animation: 'sparkle-border 800ms ease-out' } : {}),
        }}
      >
      {sparkling && <Sparkles />}
      {/* ---- Mac OS 7.5 Titlebar ---- */}
      <div
        onPointerDown={() => {
          if (isEditingShape) {
            editor.setEditingShape(null);
          }
        }}
        style={{
          position: 'relative',
          display: 'flex',
          alignItems: 'center',
          padding: '3px 5px',
          backgroundImage: TITLEBAR_STRIPES,
          backgroundSize: TITLEBAR_BG_SIZE,
          backgroundPosition: TITLEBAR_BG_POS,
          backgroundRepeat: TITLEBAR_BG_REPEAT,
          backgroundColor: '#fff',
          borderBottom: '1px solid #808080',
          cursor: 'grab',
          userSelect: 'none',
          flexShrink: 0,
          minHeight: '22px',
        }}
      >
        {/* Open-document box (System 7.5 close-box style — sunken) */}
        <button
          title="Open document"
          onClick={(e) => {
            e.stopPropagation();
            handleOpenDocument();
          }}
          onPointerDown={(e) => e.stopPropagation()}
          style={{
            width: '13px',
            height: '13px',
            border: 'none',
            borderTop: '1px solid #808080',
            borderLeft: '1px solid #808080',
            borderRight: '1px solid #fff',
            borderBottom: '1px solid #fff',
            boxShadow: 'inset 1px 1px 0 #404040, inset -1px -1px 0 #dfdfdf',
            background: '#c0c0c0',
            padding: 0,
            cursor: 'pointer',
            flexShrink: 0,
            zIndex: 1,
          }}
        />

        {/* Document name — absolutely centered over the titlebar */}
        <div
          style={{
            position: 'absolute',
            left: '50%',
            top: '50%',
            transform: 'translate(-50%, -50%)',
            maxWidth: '70%',
            zIndex: 1,
          }}
        >
          {isEditingName ? (
            <input
              ref={nameInputRef}
              defaultValue={docName}
              onPointerDown={(e) => e.stopPropagation()}
              onClick={(e) => e.stopPropagation()}
              onKeyDown={(e) => {
                e.stopPropagation();
                if (e.key === 'Enter') handleRename((e.target as HTMLInputElement).value);
                if (e.key === 'Escape') setIsEditingName(false);
              }}
              onBlur={(e) => handleRename(e.target.value)}
              style={{
                fontSize: '13px',
                fontWeight: 700,
                fontFamily: 'Geneva, "Lucida Grande", "Helvetica Neue", Helvetica, sans-serif',
                color: '#000',
                background: '#fff',
                border: 'none',
                padding: '0 6px',
                outline: 'none',
                textAlign: 'center',
                width: '100%',
              }}
            />
          ) : (
            <ClickableTitle
              docName={docName}
              onClickTitle={() => setIsEditingName(true)}
            />
          )}
        </div>

        {/* Menu button — zoom box on right */}
        <div style={{ position: 'relative', flexShrink: 0, marginLeft: 'auto', zIndex: 1 }}>
          <button
            title="Menu"
            onClick={(e) => {
              e.stopPropagation();
              setMenuOpen((v) => !v);
              setIsEditingTool(false);
            }}
            onPointerDown={(e) => e.stopPropagation()}
            style={{
              width: '13px',
              height: '13px',
              border: 'none',
              borderTop: '1px solid #808080',
              borderLeft: '1px solid #808080',
              borderRight: '1px solid #fff',
              borderBottom: '1px solid #fff',
              boxShadow: 'inset 1px 1px 0 #404040, inset -1px -1px 0 #dfdfdf',
              background: '#c0c0c0',
              padding: 0,
              cursor: 'pointer',
              flexShrink: 0,
              position: 'relative',
            }}
          >
            <span style={{
              position: 'absolute',
              top: '1px',
              left: '1px',
              width: '5px',
              height: '5px',
              border: '1px solid #999',
              background: 'transparent',
            }} />
          </button>

          {/* Dropdown menu */}
          {menuOpen && (
            <div
              onPointerDown={(e) => e.stopPropagation()}
              style={{
                position: 'absolute',
                top: '100%',
                right: 0,
                marginTop: '2px',
                background: '#fff',
                border: '1px solid #808080',
                borderTopColor: '#fff',
                borderLeftColor: '#fff',
                borderRightColor: '#404040',
                borderBottomColor: '#404040',
                boxShadow: '1px 1px 0 #000',
                padding: '2px',
                minWidth: '160px',
                fontFamily: 'Geneva, "Lucida Grande", "Helvetica Neue", Helvetica, sans-serif',
                fontSize: '11px',
                zIndex: 10000,
              }}
            >
              {/* Tool selector row */}
              {isEditingTool ? (
                <div style={{ padding: '2px' }}>
                  <input
                    ref={toolInputRef}
                    list={`tool-list-${shape.id}`}
                    placeholder={pillLabel || docType || 'tool id'}
                    onPointerDown={(e) => e.stopPropagation()}
                    onClick={(e) => e.stopPropagation()}
                    onKeyDown={(e) => {
                      e.stopPropagation();
                      if (e.key === 'Enter') {
                        const val = (e.target as HTMLInputElement).value.trim();
                        if (val) handleToolChange(val);
                        setIsEditingTool(false);
                        setMenuOpen(false);
                      }
                      if (e.key === 'Escape') {
                        setIsEditingTool(false);
                      }
                    }}
                    onBlur={(e) => {
                      const val = e.target.value.trim();
                      if (val) handleToolChange(val);
                      setIsEditingTool(false);
                    }}
                    style={{
                      fontSize: '11px',
                      fontWeight: 400,
                      color: '#000',
                      padding: '2px 4px',
                      background: '#fff',
                      border: '1px solid #808080',
                      fontFamily: 'inherit',
                      outline: 'none',
                      width: '100%',
                      boxSizing: 'border-box',
                    }}
                  />
                  <datalist id={`tool-list-${shape.id}`}>
                    {tools.map((t) => (
                      <option key={t.id} value={t.id}>
                        {t.name}
                      </option>
                    ))}
                  </datalist>
                </div>
              ) : (
                <MenuItem
                  label={`Tool: ${pillLabel || 'default'}`}
                  onClick={() => setIsEditingTool(true)}
                />
              )}
              <MenuSeparator />
              <MenuItem label="Copy automerge URL" onClick={handleCopyAutomergeUrl} />
              <MenuItem label="Copy URL at heads" onClick={handleCopyUrlAtHeads} />
              <MenuItem label="Copy tiny.patchwork URL" onClick={handleCopyTinyUrl} />
              {copied && (
                <>
                  <MenuSeparator />
                  <div style={{
                    padding: '2px 12px',
                    color: '#808080',
                    fontSize: '10px',
                    textAlign: 'center',
                  }}>
                    Copied {copied}
                  </div>
                </>
              )}
            </div>
          )}
        </div>
      </div>

      {/* ---- Content area ---- */}
      <div
        ref={contentRef}
        style={{
          flex: 1,
          minHeight: 0,
          overflow: 'hidden',
          position: 'relative',
          pointerEvents: isSelectTool ? 'auto' : 'none',
          userSelect: isFocused ? 'text' : 'none',
          WebkitUserSelect: isFocused ? 'text' : 'none',
          cursor: isFocused ? 'auto' : undefined,
        }}
        onPointerDown={isSelectTool ? (e) => {
          e.stopPropagation();
          if (!isEditingShape) {
            editor.setEditingShape(shape.id);
          }
        } : undefined}
        onPointerUp={isSelectTool ? (e) => {
          e.stopPropagation();
          // Synthesize a click event so frameworks with document-level
          // event delegation (Solid.js) receive it. tldraw's preventDefault
          // on pointerdown suppresses the browser's native click event.
          const target = e.target as HTMLElement;
          if (target) {
            const click = new MouseEvent('click', {
              bubbles: true,
              cancelable: true,
              view: window,
              clientX: e.clientX,
              clientY: e.clientY,
            });
            target.dispatchEvent(click);
          }
        } : undefined}
      >
        {docUrl && isImage ? (
          <img
            src={automergeUrlToServiceWorkerUrl(docUrl as AutomergeUrl)}
            alt={docName}
            style={{
              width: '100%',
              height: '100%',
              objectFit: 'contain',
            }}
          />
        ) : docUrl ? (
          // @ts-expect-error Custom element from patchwork-elements
          <patchwork-view
            doc-url={docUrl}
            {...(effectiveToolId ? { 'tool-id': effectiveToolId } : {})}
            key={effectiveToolId || 'default'}
            style={{
              display: 'block',
              width: '100%',
              height: '100%',
            }}
          />
        ) : (
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
              height: '100%',
              color: '#000',
              fontSize: '12px',
              fontFamily: 'Geneva, "Lucida Grande", "Helvetica Neue", Helvetica, sans-serif',
            }}
          >
            No document
          </div>
        )}
      </div>
      </div>
    </HTMLContainer>
  );
}
