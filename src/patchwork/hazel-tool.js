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
	const container = document.createElement("div")
	container.id = "container"
	element.appendChild(container)

	const style = document.createElement("link")
	style.rel = "stylesheet"
	style.href = resolve("dist/style.css")
	element.appendChild(style)

	const fonts = document.createElement("link")
	fonts.rel = "stylesheet"
	fonts.href =
		"https://fonts.googleapis.com/css?family=Material+Icons&display=swap"
	element.appendChild(fonts)

	const overrides = document.createElement("style")
	overrides.textContent = `
		#page {
			position: absolute;
			top: 0;
			left: 0;
			width: 100%;
			height: 100%;
		}
	`
	element.appendChild(overrides)

	// Track state for delta computation
	let prevDoc = null
	let isUpdatingFromHazel = false

	// Send a message to Hazel by dispatching a CustomEvent on the element.
	// Hazel's PatchworkComm.re listens for "patchwork-to-hazel" events.
	function sendToHazel(message) {
		element.dispatchEvent(
			new CustomEvent("patchwork-to-hazel", {detail: message}),
		)
	}

	// Listen for messages from Hazel dispatched as "hazel-to-patchwork" events.
	element.addEventListener("hazel-to-patchwork", event => {
		onHazelMessage(event.detail)
	})

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

	// Listen for arrow-connect events from the spatial canvas
	element.addEventListener("patchwork:arrow", event => {
		const {url, direction} = event.detail
		sendToHazel({t: "connect", url, direction})
	})

	// Load Hazel scripts
	const bundled = document.createElement("script")
	bundled.type = "module"
	bundled.src = resolve("dist/bundled.js")
	element.appendChild(bundled)

	const hazel = document.createElement("script")
	hazel.src = resolve("dist/hazel.js")
	bundled.addEventListener("load", () => {
		element.appendChild(hazel)
	})

	// Cleanup
	return () => {
		handle.off("change", onDocChange)
		try {
			handle.off("ephemeral-message", onEphemeral)
		} catch (e) {}
		style.remove()
		fonts.remove()
		overrides.remove()
		bundled.remove()
		hazel.remove()
		container.remove()
	}
}
