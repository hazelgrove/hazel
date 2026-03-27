/**
 * NewDocTool — a tldraw tool that lets users draw a box to create a new
 * patchwork document of a chosen datatype.
 *
 * Exports:
 *   - NewDocShapeTool        — pass in Tldraw `tools` prop
 *   - newDocUiOverrides      — spread into Tldraw `overrides` prop
 *   - NewDocToolbar          — pass as Tldraw `components.Toolbar`
 *   - setNewDocToolContext()  — call once during init
 */

import {
  StateNode,
  createShapeId,
  DefaultToolbarContent,
  type TLPointerEventInfo,
  type TLUiOverrides,
  type TLUiToolsContextType,
  type Editor,
  useEditor,
  useValue,
  DefaultToolbar,
} from 'tldraw';
import { useState, useEffect, useCallback, useRef, useMemo } from 'react';
import { createPortal } from 'react-dom';
import type { DocHandle } from '@automerge/automerge-repo';
import {
  getRegistry,
  createDocOfDatatype2,
  type DatatypeDescription,
  type LoadedDatatype,
  type ToolElement,
} from '@inkandswitch/patchwork-plugins';
import { PATCHWORK_DOC_SHAPE_TYPE } from './PatchworkDocShape';
import { makeShapeId as makeDeterministicShapeId } from './tool';

// ---------------------------------------------------------------------------
// Per-editor context — keyed by editor instance so nested spaces
// (subfolders rendered inside a parent) don't overwrite each other.
// ---------------------------------------------------------------------------

interface NewDocContext {
  element: ToolElement;
  handle: DocHandle<any>;
}

const _contextByEditor = new WeakMap<Editor, NewDocContext>();
let _selectedDatatypeId = '';

/** Call this once per editor when the editor + doc are ready. */
export function setNewDocToolContext(element: ToolElement, handle: DocHandle<any>, editor: Editor) {
  _contextByEditor.set(editor, { element, handle });
}

export function setSelectedDatatypeId(id: string) {
  _selectedDatatypeId = id;
}

export function getSelectedDatatypeId(): string {
  return _selectedDatatypeId;
}

// ---------------------------------------------------------------------------
// Datatype helpers
// ---------------------------------------------------------------------------

/** Get datatype descriptions (not necessarily loaded yet) that aren't unlisted. */
function getListedDatatypeDescriptions(): DatatypeDescription[] {
  try {
    const registry = getRegistry<DatatypeDescription>('patchwork:datatype');
    return registry.filter((d) => !d.unlisted) as unknown as DatatypeDescription[];
  } catch {
    return [];
  }
}

/** Load a specific datatype by id, returning a LoadedDatatype with its module. */
async function loadDatatype(id: string): Promise<LoadedDatatype | undefined> {
  try {
    const registry = getRegistry<DatatypeDescription>('patchwork:datatype');
    return (await registry.load(id)) as unknown as LoadedDatatype | undefined;
  } catch {
    return undefined;
  }
}

// ---------------------------------------------------------------------------
// NewDocShapeTool — draws a preview rectangle while dragging, then creates
// the patchwork-doc shape at the drawn bounds on pointer-up.
// ---------------------------------------------------------------------------

export class NewDocShapeTool extends StateNode {
  static override id = 'new-doc';

  private startPoint = { x: 0, y: 0 };
  private previewId: ReturnType<typeof createShapeId> | null = null;

  override onPointerDown(info: TLPointerEventInfo) {
    const { currentPagePoint } = this.editor.inputs;
    this.startPoint = { x: currentPagePoint.x, y: currentPagePoint.y };

    // Create a temporary dashed geo rectangle as a visual preview.
    // Explicitly parent to the current page so it doesn't get auto-parented
    // to a frame (which would offset it).
    this.previewId = createShapeId();
    this.editor.createShape({
      id: this.previewId,
      type: 'geo',
      x: currentPagePoint.x,
      y: currentPagePoint.y,
      parentId: this.editor.getCurrentPageId(),
      props: {
        w: 1,
        h: 1,
        geo: 'rectangle',
        dash: 'dashed',
        fill: 'none',
      },
    });
  }

  override onPointerMove(info: TLPointerEventInfo) {
    if (!this.previewId) return;

    const { currentPagePoint } = this.editor.inputs;
    const x = Math.min(this.startPoint.x, currentPagePoint.x);
    const y = Math.min(this.startPoint.y, currentPagePoint.y);
    const w = Math.max(1, Math.abs(currentPagePoint.x - this.startPoint.x));
    const h = Math.max(1, Math.abs(currentPagePoint.y - this.startPoint.y));

    this.editor.updateShape({
      id: this.previewId,
      type: 'geo',
      x,
      y,
      props: { w, h },
    });
  }

  override onCancel() {
    this.cleanup();
    this.editor.setCurrentTool('select');
  }

  override onInterrupt() {
    this.cleanup();
  }

  private cleanup() {
    if (this.previewId) {
      if (this.editor.getShape(this.previewId)) {
        this.editor.deleteShapes([this.previewId]);
      }
      this.previewId = null;
    }
  }

  override onPointerUp(info: TLPointerEventInfo) {
    if (!this.previewId) return;

    // Read final bounds from the preview shape
    const preview = this.editor.getShape(this.previewId);
    this.editor.deleteShapes([this.previewId]);
    this.previewId = null;

    if (!preview) {
      this.editor.setCurrentTool('select');
      return;
    }

    const px = (preview as any).x as number;
    const py = (preview as any).y as number;
    const pw = (preview as any).props.w as number;
    const ph = (preview as any).props.h as number;

    // Enforce minimum size
    const finalW = Math.max(pw, 200);
    const finalH = Math.max(ph, 150);

    const ctx = _contextByEditor.get(this.editor);
    if (!ctx) {
      console.warn('[space] NewDocTool: context not set for this editor');
      this.editor.setCurrentTool('select');
      return;
    }

    const datatypeId = _selectedDatatypeId;
    if (!datatypeId) {
      console.warn('[space] NewDocTool: no datatype selected');
      this.editor.setCurrentTool('select');
      return;
    }

    const editor = this.editor;
    const shapeId = createShapeId();
    const repo = ctx.element.repo;
    const hive = ctx.element.hive;

    // Create the patchwork-doc shape at the drawn bounds
    editor.createShape({
      id: shapeId,
      type: PATCHWORK_DOC_SHAPE_TYPE,
      x: px,
      y: py,
      rotation: 0,
      parentId: this.editor.getCurrentPageId(),
      props: {
        w: finalW,
        h: finalH,
        docUrl: '',
        docName: 'Creating\u2026',
        docType: datatypeId,
        toolId: '',
      },
    } as any);

    // Switch to select immediately
    editor.setCurrentTool('select');
    editor.setSelectedShapes([shapeId]);

    // Async: load the datatype module, create document, update shape, add to folder
    (async () => {
      try {
        const datatype = await loadDatatype(datatypeId);
        if (!datatype) {
          throw new Error(`Could not load datatype: ${datatypeId}`);
        }

        const docHandle = await (createDocOfDatatype2 as any)(datatype, repo, undefined, hive);
        const docUrl = docHandle.url;

        // Use the deterministic shape ID (same one reconcile would use) so
        // remote peers don't end up with duplicate shapes for the same doc.
        const deterministicId = makeDeterministicShapeId(docUrl);

        // Read the temp shape's position/size before removing it.
        const tempShape = editor.getShape(shapeId) as any;
        const sx = tempShape?.x ?? px;
        const sy = tempShape?.y ?? py;
        const sw = tempShape?.props?.w ?? finalW;
        const sh = tempShape?.props?.h ?? finalH;

        // Delete the temp shape (random ID).
        if (editor.getShape(shapeId)) {
          editor.deleteShapes([shapeId]);
        }

        // Create (or update) the shape with the deterministic ID.
        if (editor.getShape(deterministicId)) {
          editor.updateShape({
            id: deterministicId,
            type: PATCHWORK_DOC_SHAPE_TYPE,
            props: {
              docUrl,
              docName: datatype.name ?? datatypeId,
              docType: datatypeId,
              toolId: '',
            },
          } as any);
        } else {
          editor.createShape({
            id: deterministicId,
            type: PATCHWORK_DOC_SHAPE_TYPE,
            x: sx,
            y: sy,
            rotation: 0,
            parentId: editor.getCurrentPageId(),
            props: {
              w: sw,
              h: sh,
              docUrl,
              docName: datatype.name ?? datatypeId,
              docType: datatypeId,
              toolId: '',
            },
          } as any);
        }

        editor.setSelectedShapes([deterministicId]);
        console.log('[space] new doc created:', datatypeId, docUrl);
      } catch (err) {
        console.error('[space] new doc creation failed:', err);
        editor.updateShape({
          id: shapeId,
          type: PATCHWORK_DOC_SHAPE_TYPE,
          props: { docName: `Error creating ${datatypeId}` },
        } as any);
      }
    })();
  }
}

// ---------------------------------------------------------------------------
// Tldraw UI overrides — registers "new-doc" in the tools context
// ---------------------------------------------------------------------------

export const newDocUiOverrides: TLUiOverrides = {
  tools(_editor: Editor, tools: TLUiToolsContextType) {
    tools['new-doc'] = {
      id: 'new-doc',
      icon: 'plus' as any,
      label: 'New document' as any,
      kbd: 'c',
      onSelect(_source) {
        _editor.setCurrentTool('new-doc');
      },
    };
    // Remove 'd' keybinding for draw tool (conflicts with embedded tools)
    if (tools['draw']) {
      tools['draw'] = { ...tools['draw'], kbd: '' };
    }
    return tools;
  },
};

// ---------------------------------------------------------------------------
// Custom Toolbar — default tools + a "+" button with datatype select
// ---------------------------------------------------------------------------

export function NewDocToolbar() {
  const editor = useEditor();
  const isActive = useValue('new-doc active', () => editor.getCurrentToolId() === 'new-doc', [
    editor,
  ]);

  const [datatypes, setDatatypes] = useState<DatatypeDescription[]>([]);
  const [menuOpen, setMenuOpen] = useState(false);
  const [hovering, setHovering] = useState(false);
  const containerRef = useRef<HTMLDivElement>(null);
  const menuRef = useRef<HTMLDivElement>(null);
  const buttonRef = useRef<HTMLButtonElement>(null);
  const [menuPos, setMenuPos] = useState<{ x: number; y: number } | null>(null);

  // Load datatypes on mount
  useEffect(() => {
    const dt = getListedDatatypeDescriptions();
    setDatatypes(dt);
    if (dt.length > 0 && !_selectedDatatypeId) {
      _selectedDatatypeId = dt[0].id;
    }
  }, []);

  // Close menu on outside click
  useEffect(() => {
    if (!menuOpen) return;
    const handler = (e: MouseEvent) => {
      const target = e.target as Node;
      const inContainer = containerRef.current?.contains(target);
      const inMenu = menuRef.current?.contains(target);
      if (!inContainer && !inMenu) {
        setMenuOpen(false);
      }
    };
    document.addEventListener('pointerdown', handler, true);
    return () => document.removeEventListener('pointerdown', handler, true);
  }, [menuOpen]);

  // NOTE: we intentionally do NOT close the menu when isActive flips to false.
  // tldraw deactivates the tool on pointerdown before React can process the
  // click on a menu button.  The menu closes itself in handlePickDatatype
  // or via the outside-click handler.

  const activateTool = useCallback(() => {
    if (datatypes.length > 0 && !_selectedDatatypeId) {
      _selectedDatatypeId = datatypes[0].id;
    }
    editor.setCurrentTool('new-doc');
  }, [datatypes, editor]);

  const openMenu = useCallback(() => {
    if (buttonRef.current) {
      const rect = buttonRef.current.getBoundingClientRect();
      setMenuPos({ x: rect.left + rect.width / 2, y: rect.top });
    }
    setMenuOpen(true);
  }, []);

  const handlePlusClick = useCallback(() => {
    if (!isActive) {
      // First click: activate the tool and open the drawer
      activateTool();
      // openMenu needs the button rect, so defer until after render
      requestAnimationFrame(() => openMenu());
    } else {
      // Already active: toggle the menu
      if (menuOpen) {
        setMenuOpen(false);
      } else {
        openMenu();
      }
    }
  }, [isActive, activateTool, menuOpen, openMenu]);

  const handlePillClick = useCallback(() => {
    if (menuOpen) {
      setMenuOpen(false);
    } else {
      openMenu();
    }
  }, [menuOpen, openMenu]);

  const handlePickDatatype = useCallback(
    (id: string) => {
      setSelectedDatatypeId(id);
      setMenuOpen(false);
      editor.setCurrentTool('new-doc');
    },
    [editor],
  );

  if (datatypes.length === 0) {
    return (
      <DefaultToolbar>
        <DefaultToolbarContent />
      </DefaultToolbar>
    );
  }

  const selectedName = datatypes.find((d) => d.id === _selectedDatatypeId)?.name ?? 'document';

  return (
    <DefaultToolbar>
      <DefaultToolbarContent />

      {/* Separator */}
      <div
        style={{
          width: '1px',
          height: '20px',
          background: '#ddd',
          margin: '0 4px',
          flexShrink: 0,
        }}
      />

      {/* + button and pill/menu container */}
      <div
        ref={containerRef}
        style={{
          position: 'relative',
          display: 'inline-flex',
          alignItems: 'center',
          justifyContent: 'center',
          flexShrink: 0,
        }}
      >
        {/* Pill showing selected datatype — visible when hovering or active */}
        {(isActive || hovering) && !menuOpen && (
          <button
            type="button"
            onClick={handlePillClick}
            style={{
              position: 'absolute',
              bottom: '100%',
              left: '50%',
              transform: 'translateX(-50%)',
              marginBottom: '6px',
              display: 'inline-flex',
              alignItems: 'center',
              gap: '4px',
              padding: '3px 10px',
              background: '#e8f0fe',
              border: '1px solid #c4d7f2',
              borderRadius: '9999px',
              cursor: 'pointer',
              fontSize: '11px',
              fontFamily: 'system-ui, -apple-system, sans-serif',
              fontWeight: 500,
              color: '#2f80ed',
              whiteSpace: 'nowrap',
              lineHeight: '16px',
              zIndex: 10000,
            }}
          >
            {selectedName}
            {datatypes.length > 1 && (
              <svg width="8" height="8" viewBox="0 0 8 8" style={{ opacity: 0.6 }}>
                <path
                  d="M1 2.5L4 5.5L7 2.5"
                  stroke="currentColor"
                  strokeWidth="1.5"
                  fill="none"
                  strokeLinecap="round"
                  strokeLinejoin="round"
                />
              </svg>
            )}
          </button>
        )}

        {/* + button */}
        <button
          ref={buttonRef}
          type="button"
          onClick={handlePlusClick}
          onPointerEnter={() => setHovering(true)}
          onPointerLeave={() => setHovering(false)}
          title="Create a new document (C)"
          style={{
            display: 'inline-flex',
            alignItems: 'center',
            justifyContent: 'center',
            width: '32px',
            height: '32px',
            border: 'none',
            borderRadius: '6px',
            background: isActive ? 'var(--color-selected, #e8f0fe)' : 'transparent',
            cursor: 'pointer',
            color: isActive ? 'var(--color-selected-contrast, #2f80ed)' : 'currentColor',
            flexShrink: 0,
          }}
        >
          <svg width="16" height="16" viewBox="0 0 16 16" fill="none">
            {/* Drafting board */}
            <rect
              x="1.5" y="1.5" width="13" height="13" rx="0.5"
              stroke="currentColor"
              strokeWidth="1"
              fill="none"
            />
            {/* T-square crossbar */}
            <path
              d="M1.5 5.5h13"
              stroke="currentColor"
              strokeWidth="1"
            />
            {/* T-square stem */}
            <path
              d="M8 5.5v9"
              stroke="currentColor"
              strokeWidth="1"
            />
          </svg>
        </button>

        {/* Custom dropdown menu — portalled to body to escape overflow:hidden */}
        {menuOpen &&
          datatypes.length > 1 &&
          menuPos &&
          createPortal(
            <div
              ref={menuRef}
              style={{
                position: 'fixed',
                left: menuPos.x,
                top: menuPos.y,
                transform: 'translate(-50%, -100%)',
                marginTop: '-8px',
                background: '#fff',
                border: '1px solid #ddd',
                borderRadius: '8px',
                boxShadow: '0 4px 12px rgba(0,0,0,0.12)',
                padding: '4px',
                minWidth: '150px',
                zIndex: 100000,
              }}
            >
              {datatypes.map((dt) => (
                <button
                  key={dt.id}
                  type="button"
                  onPointerDown={(e) => {
                    // Handle selection on pointerdown so it fires before
                    // tldraw can process the event and deactivate the tool.
                    e.stopPropagation();
                    e.preventDefault();
                    handlePickDatatype(dt.id);
                  }}
                  style={{
                    display: 'block',
                    width: '100%',
                    padding: '6px 10px',
                    border: 'none',
                    borderRadius: '4px',
                    background: dt.id === _selectedDatatypeId ? '#e8f0fe' : 'transparent',
                    cursor: 'pointer',
                    fontSize: '13px',
                    fontFamily: 'system-ui, -apple-system, sans-serif',
                    color: '#333',
                    textAlign: 'left',
                    whiteSpace: 'nowrap',
                  }}
                  onPointerEnter={(e) => {
                    (e.currentTarget as HTMLElement).style.background =
                      dt.id === _selectedDatatypeId ? '#e8f0fe' : '#f5f5f5';
                  }}
                  onPointerLeave={(e) => {
                    (e.currentTarget as HTMLElement).style.background =
                      dt.id === _selectedDatatypeId ? '#e8f0fe' : 'transparent';
                  }}
                >
                  {dt.name}
                </button>
              ))}
            </div>,
            document.body,
          )}
      </div>
    </DefaultToolbar>
  );
}
