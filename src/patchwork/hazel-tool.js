const base = new URL(".", import.meta.url)

function resolve(path) {
	return new URL(path, base).href
}

// ============================================================================
// Granular Sync Helpers
// See docs/automerge-granular-sync.md for design rationale
// ============================================================================

function arraysEqual(a, b) {
	if (a.length !== b.length) return false
	for (let i = 0; i < a.length; i++) {
		if (a[i] !== b[i]) return false
	}
	return true
}

function moldsEqual(a, b) {
	return JSON.stringify(a) === JSON.stringify(b)
}

function computeLCS(a, b) {
	const n = a.length
	const m = b.length
	const dp = Array(n + 1)
		.fill(null)
		.map(() => Array(m + 1).fill(0))

	for (let i = 1; i <= n; i++) {
		for (let j = 1; j <= m; j++) {
			if (a[i - 1] === b[j - 1]) {
				dp[i][j] = dp[i - 1][j - 1] + 1
			} else {
				dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1])
			}
		}
	}

	const lcs = []
	let i = n,
		j = m
	while (i > 0 && j > 0) {
		if (a[i - 1] === b[j - 1]) {
			lcs.unshift(a[i - 1])
			i--
			j--
		} else if (dp[i - 1][j] > dp[i][j - 1]) {
			i--
		} else {
			j--
		}
	}
	return lcs
}

function computeListOps(before, after) {
	const ops = []
	const lcs = computeLCS(before, after)
	const lcsSet = new Set(lcs)

	before.forEach((id, i) => {
		if (!lcsSet.has(id)) {
			ops.push({type: "delete", index: i, id})
		}
	})

	after.forEach((id, i) => {
		if (!lcsSet.has(id)) {
			ops.push({type: "insert", index: i, id})
		}
	})

	return ops
}

function findParent(pieces, childId) {
	for (const [id, piece] of Object.entries(pieces)) {
		if (piece.t === "Tile") {
			for (const segment of piece.children) {
				if (segment.includes(childId)) {
					return id
				}
			}
		}
	}
	return null
}

function findAtomicIds(before, after, changedIds) {
	const atomicIds = new Set()

	for (const id of changedIds) {
		const b = before[id]
		const a = after[id]
		if (b?.t === "Tile" && a?.t === "Tile") {
			if (!arraysEqual(b.shards, a.shards)) {
				atomicIds.add(id)
				const parentId = findParent(after, id)
				if (parentId && changedIds.includes(parentId)) {
					atomicIds.add(parentId)
				}
			}
		}
	}

	return atomicIds
}

function applySegmentDiff(segment, before, after) {
	const ops = computeListOps(before, after)

	const deleteOps = ops.filter(o => o.type === "delete")
	deleteOps.sort((a, b) => b.index - a.index)
	for (const op of deleteOps) {
		segment.splice(op.index, 1)
	}

	const insertOps = ops.filter(o => o.type === "insert")
	insertOps.sort((a, b) => a.index - b.index)
	for (const op of insertOps) {
		segment.splice(op.index, 0, op.id)
	}
}

function applyTileDiff(tile, before, after) {
	if (!arraysEqual(before.label, after.label)) {
		tile.label = [...after.label]
	}
	if (!arraysEqual(before.shards, after.shards)) {
		tile.shards = [...after.shards]
	}
	if (!moldsEqual(before.mold, after.mold)) {
		tile.mold = JSON.parse(JSON.stringify(after.mold))
	}

	for (let i = 0; i < after.children.length; i++) {
		const oldSeg = before.children[i] || []
		const newSeg = after.children[i] || []
		if (!arraysEqual(oldSeg, newSeg)) {
			applySegmentDiff(tile.children[i], oldSeg, newSeg)
		}
	}

	while (tile.children.length > after.children.length) {
		tile.children.pop()
	}
	while (tile.children.length < after.children.length) {
		tile.children.push([])
	}
}

function applyGranularDelta(d, before, after, changedIds, addedIds, deletedIds) {
	for (const id of deletedIds) {
		delete d.pieces[id]
	}

	if (!before) {
		for (const id of changedIds) {
			d.pieces[id] = after[id]
		}
		for (const id of addedIds) {
			d.pieces[id] = after[id]
		}
		return
	}

	const atomicIds = findAtomicIds(before, after, changedIds)

	for (const id of changedIds) {
		const beforePiece = before[id]
		const afterPiece = after[id]

		if (atomicIds.has(id)) {
			d.pieces[id] = JSON.parse(JSON.stringify(afterPiece))
		} else if (beforePiece?.t === "Tile" && afterPiece?.t === "Tile") {
			applyTileDiff(d.pieces[id], beforePiece, afterPiece)
		} else {
			d.pieces[id] = JSON.parse(JSON.stringify(afterPiece))
		}
	}

	for (const id of addedIds) {
		d.pieces[id] = after[id]
	}
}

function doesStateEqualDoc(doc, state) {
	return JSON.stringify({pieces: doc.pieces}) === JSON.stringify({pieces: state.pieces})
}

// ============================================================================
// Tool implementation
// ============================================================================

export default function hazelTool(handle, element) {
	console.log("[spazel] initialized")
	const PORTS_EVENT = "patchwork:ports-changed"
	const EMPTY_PORTS = {inputs: [], outputs: []}

	// ---- Iframe-based isolation ----
	// Each Hazel instance runs in its own iframe so that the js_of_ocaml
	// runtime (globalThis.jsoo_runtime / caml_global_data) is fully isolated.
	// PatchworkComm.re already supports iframe mode (postMessage).

	const iframe = document.createElement("iframe")
	iframe.style.cssText = "width:100%;height:100%;border:none;display:block"
	iframe.srcdoc = `<!doctype html>
<html><head>
<meta charset="utf-8"/>
<link rel="stylesheet" href="${resolve("dist/style.css")}"/>
<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Material+Icons&display=swap"/>
<style>
#page { position:absolute;top:0;left:0;width:100%;height:100% }
</style>
<script type="module" src="${resolve("dist/bundled.js")}"></script>
</head><body spellcheck="false">
<div id="container"></div>
<script src="${resolve("dist/hazel.js")}"></script>
</body></html>`
	element.appendChild(iframe)

	// Inject the automerge repo and helpers into the iframe once it loads.
	// We must provide a parent-realm writeToDoc helper because JSON.parse
	// inside the iframe creates iframe-realm objects which automerge's
	// proxy rejects with "Cannot assign unknown object" (cross-realm
	// constructor identity check fails).
	iframe.addEventListener("load", () => {
		if (iframe.contentWindow) {
			const repo = element.repo ?? window.repo
			iframe.contentWindow.repo = repo

			// Write helper that runs entirely in the parent realm
			iframe.contentWindow.patchworkWriteToDoc = (url, jsonString) => {
				if (!repo) return
				repo.find(url).then(handle => {
					const parsed = JSON.parse(jsonString)
					handle.change(doc => {
						if (typeof parsed !== "object" || parsed === null) return
						// Delete keys in doc not in parsed
						for (const key of Object.keys(doc)) {
							if (!parsed.hasOwnProperty(key)) {
								delete doc[key]
							}
						}
						// Copy all keys from parsed into doc
						for (const key of Object.keys(parsed)) {
							doc[key] = parsed[key]
						}
					})
				})
			}
		}
	})

	let lastPortsSignature = ""
	let clearPortObservers = () => {}

	function getPatchworkViewHost() {
		if (element && element.tagName === "PATCHWORK-VIEW") {
			return element
		}
		const root = element.getRootNode?.()
		if (root && root.host && root.host.tagName === "PATCHWORK-VIEW") {
			return root.host
		}
		return element.closest?.("patchwork-view") ?? null
	}

	function publishPorts(ports) {
		const host = getPatchworkViewHost()
		const target = host ?? element
		target.__spazePorts = ports
		element.__spazePorts = ports
		const nextSignature = JSON.stringify(ports)
		if (nextSignature === lastPortsSignature) return
		lastPortsSignature = nextSignature
		target.dispatchEvent(
			new CustomEvent(PORTS_EVENT, {
				detail: ports,
				bubbles: true,
				composed: true,
			})
		)
	}

	function collectPortsFromIframe() {
		const doc = iframe.contentDocument
		if (!doc) return EMPTY_PORTS
		const viewportHeight =
			iframe.clientHeight ||
			iframe.getBoundingClientRect().height ||
			doc.documentElement?.clientHeight ||
			1
		const collect = selector => {
			const byLine = new Map()
			for (const node of doc.querySelectorAll(selector)) {
				const projectorId = node.getAttribute("data-projector-id")
				if (!projectorId) continue
				const rect = node.getBoundingClientRect()
				const centerPx = Math.round(rect.top + rect.height / 2)
				if (!Number.isFinite(centerPx) || byLine.has(centerPx)) continue
				const y = Math.max(0, Math.min(1, centerPx / viewportHeight))
				byLine.set(centerPx, {projectorId, y})
			}
			return [...byLine.entries()]
				.sort((a, b) => a[0] - b[0])
				.map(([, port]) => port)
		}
		return {
			inputs: collect(".projector.Automerge[data-projector-id]"),
			outputs: collect(".projector.AutomergeWriteBack[data-projector-id]"),
		}
	}

	function setupPortObservers() {
		clearPortObservers()
		const doc = iframe.contentDocument
		const win = iframe.contentWindow
		if (!doc || !win) {
			publishPorts(EMPTY_PORTS)
			return
		}

		let rafId = null
		const schedule = () => {
			if (rafId !== null) return
			rafId = win.requestAnimationFrame(() => {
				rafId = null
				publishPorts(collectPortsFromIframe())
			})
		}

		const mutationObserver = new MutationObserver(schedule)
		const root = doc.body ?? doc.documentElement
		if (root) {
			mutationObserver.observe(root, {
				subtree: true,
				childList: true,
				attributes: true,
				attributeFilter: ["class", "style", "data-projector-id"],
			})
		}

		const resizeObserver = new ResizeObserver(schedule)
		if (doc.documentElement) resizeObserver.observe(doc.documentElement)
		if (doc.body) resizeObserver.observe(doc.body)
		win.addEventListener("resize", schedule)

		schedule()
		clearPortObservers = () => {
			if (rafId !== null) {
				win.cancelAnimationFrame(rafId)
			}
			mutationObserver.disconnect()
			resizeObserver.disconnect()
			win.removeEventListener("resize", schedule)
		}
	}

	iframe.addEventListener("load", setupPortObservers)

	// Track state for delta computation
	let prevDoc = null
	let isUpdatingFromHazel = false

	// Send a message to Hazel via postMessage (iframe mode).
	// PatchworkComm.re listens for "message" events on window.
	function sendToHazel(message) {
		if (iframe.contentWindow) {
			iframe.contentWindow.postMessage(message, "*")
		}
	}

	// Listen for messages from Hazel (postMessage from iframe).
	function onMessage(event) {
		if (event.source !== iframe.contentWindow) return
		onHazelMessage(event.data)
	}
	window.addEventListener("message", onMessage)

	function onHazelMessage(msg) {
		switch (msg.t) {
			case "init": {
				const doc = handle.doc()
				sendToHazel({
					t: "state",
					state: {title: doc.title, pieces: doc.pieces},
				})
				prevDoc = doc
				break
			}
			case "ping": {
				sendToHazel({t: "pong", message: "Pong from Patchwork tool!"})
				break
			}
			case "pong": {
				break
			}
			case "state": {
				const doc = handle.doc()
				const newPiecesMap = msg.state.pieces
				const beforePiecesMap = msg.before?.pieces
				const deletedIds = msg.deleted ?? []

				if (
					deletedIds.length === 0 &&
					doesStateEqualDoc(doc, msg.state)
				) {
					return
				}

				const oldPieces = doc.pieces
				const changedIds = []
				const addedIds = []

				for (const [id, newPiece] of Object.entries(newPiecesMap)) {
					const oldPiece = oldPieces[id]
					if (!oldPiece) {
						addedIds.push(id)
					} else if (
						JSON.stringify(oldPiece) !== JSON.stringify(newPiece)
					) {
						changedIds.push(id)
					}
				}

				isUpdatingFromHazel = true

				handle.change(d => {
					d.title = msg.state.title

					applyGranularDelta(
						d,
						beforePiecesMap,
						newPiecesMap,
						changedIds,
						addedIds,
						deletedIds,
					)
				})

				queueMicrotask(() => {
					isUpdatingFromHazel = false
				})

				break
			}
			case "caret": {
				// Broadcast caret position to other peers via handle.broadcast
				try {
					handle.broadcast([
						"anonymous",
						{
							pieceId: msg.pieceId,
							shardIdx: msg.shardIdx ?? null,
							offset: msg.caretOffset,
							shape: msg.shape ?? null,
							side: msg.side ?? null,
						},
					])
				} catch (e) {
					// Broadcast not supported or failed
				}
				break
			}
		}
	}

	// Listen for Automerge doc changes (from other peers) and forward to Hazel
	const onDocChange = () => {
		if (isUpdatingFromHazel) {
			isUpdatingFromHazel = false
			prevDoc = handle.doc()
			return
		}

		const doc = handle.doc()
		if (prevDoc === null) {
			prevDoc = doc
			return
		}

		const affectedIds = new Set()
		for (const [id, piece] of Object.entries(doc.pieces)) {
			const prevPiece = prevDoc.pieces[id]
			if (
				!prevPiece ||
				JSON.stringify(piece) !== JSON.stringify(prevPiece)
			) {
				affectedIds.add(id)
			}
		}

		const pieces = {}
		for (const id of affectedIds) {
			pieces[id] = doc.pieces[id]
		}

		const titleChanged = doc.title !== prevDoc.title

		if (Object.keys(pieces).length > 0 || titleChanged) {
			sendToHazel({t: "state", state: {title: doc.title, pieces}})
		}

		prevDoc = doc
	}

	handle.on("change", onDocChange)

	// Listen for ephemeral messages (remote carets) from other peers
	const onEphemeral = event => {
		const msg = event.message
		if (Array.isArray(msg) && msg.length === 2) {
			const [remoteUserId, caretState] = msg
			if (
				caretState &&
				typeof remoteUserId === "string" &&
				caretState.pieceId &&
				typeof caretState.offset === "number"
			) {
				const colors = [
					"#E53935",
					"#1E88E5",
					"#43A047",
					"#FB8C00",
					"#8E24AA",
					"#00ACC1",
				]
				let hash = 0
				for (let i = 0; i < remoteUserId.length; i++) {
					hash = (hash << 5) - hash + remoteUserId.charCodeAt(i)
					hash |= 0
				}
				const color = colors[Math.abs(hash) % colors.length]

				sendToHazel({
					t: "remote-caret",
					userId: remoteUserId,
					userName: caretState.name ?? undefined,
					color,
					pieceId: caretState.pieceId,
					shardIdx: caretState.shardIdx ?? null,
					caretOffset: caretState.offset,
					shape: caretState.shape ?? null,
					side: caretState.side ?? null,
				})
			}
		}
	}

	try {
		handle.on("ephemeral-message", onEphemeral)
	} catch (e) {
		// Ephemeral messages not supported
	}

	// Listen for arrow connect/disconnect events from the spatial canvas
	element.addEventListener("patchwork:connect-arrow", event => {
		const {url, direction, projectorId} = event.detail
		console.log("[spazel] connect-arrow", direction, url, projectorId ?? null)
		sendToHazel({t: "connect", url, direction, projectorId})
	})
	element.addEventListener("patchwork:disconnect-arrow", event => {
		const {url, direction, projectorId} = event.detail
		console.log("[spazel] disconnect-arrow", direction, url, projectorId ?? null)
		sendToHazel({t: "disconnect", url, direction, projectorId})
	})

	// Cleanup
	return () => {
		clearPortObservers()
		publishPorts(EMPTY_PORTS)
		window.removeEventListener("message", onMessage)
		handle.off("change", onDocChange)
		try {
			handle.off("ephemeral-message", onEphemeral)
		} catch (e) {}
		iframe.remove()
	}
}
