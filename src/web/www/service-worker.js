/// <reference types="service-worker-types" />
let cachename = "default"
let debugging = false
function log(...args) {
	if (!debugging) return
	console.log.call(
		console,
		`%cpatchwork:serviceworker%c\n`,
		`color: #00ffcc; font-weight: bold`,
		"color: inherit",
		...args
	)
}
self.addEventListener("install", () => self.skipWaiting())
async function clearOldCaches() {
	const cacheWhitelist = [cachename]
	const cacheNames = await caches.keys()
	const deletePromises = cacheNames.map(cacheName => {
		if (!cacheWhitelist.includes(cacheName)) {
			return caches.delete(cacheName)
		}
	})
	await Promise.all(deletePromises)
}
self.addEventListener("activate", async () => {
	await clearOldCaches()
	clients.claim()
})
// a map of response promises by their id
const responseResolvers = new Map()
function accept(message) {
	const responseItem = responseResolvers.get(message.id)
	if (!responseItem) {
		return console.warn(`No read response found for id ${message.id}`)
	}
	return responseItem.resolve(message.response)
}
const bc = new BroadcastChannel("@patchwork/handoff")
bc.addEventListener("message", event => {
	if (event.data.type == "response") accept(event.data)
})
// when we receive a `response` req, we resolve the promise with that id
self.addEventListener("message", async event => {
	if (event.data.type == "response") {
		accept(event.data)
	} else if (event.data.type == "port") {
		log("recieved messagechannel")
		const [port] = event.ports
		port.addEventListener("message", event => {
			if (event.data.type == "response") {
				accept(event.data)
			}
		})
	} else if (event.data.type == "cachename") {
		const nextCachename = event.data.cachename
		if (cachename == nextCachename) {
			return
		}
		console.info(
			`deleting ${cachename} and setting cache name to ${nextCachename}`
		)
		caches.delete(cachename)
		cachename = nextCachename
	} else if (event.data.type == "debug") {
		debugging = event.data.debug
		log("serviceworker debugging enabled")
	}
})
// request ids are kept in a counter
let reqcount = 0
self.addEventListener("fetch", async fetchEvent => {
	const request = fetchEvent.request
	if (request.method !== "GET") return fetchEvent.respondWith(fetch(request))
	const url = new URL(fetchEvent.request.url)
	let handoffURL
	if (
		url.hostname == self.location.hostname &&
		url.port == self.location.port &&
		url.protocol == self.location.protocol
	) {
		try {
			// trap any request like /url e.g. /automerge%3Awhatever or /http%3A%2F%2Fsomething.com
			// URI encoded so we can include hashes etc
			handoffURL = new URL(decodeURIComponent(url.pathname.slice(1)))
			log(`received handoff request ${handoffURL}`)
		} catch {}
	}
	fetchEvent.respondWith(
		(async () => {
			const cache = await caches.open(cachename)
			const match = await cache.match(request)
			try {
				if (handoffURL) {
					// cache-first strategy for handoff requests
					if (match) return match
					const client = await self.clients.get(fetchEvent.clientId)
					// set up a request id
					const reqid = reqcount++
					// create a place for the response event handler to put the response
					const resolvers = Promise.withResolvers()
					responseResolvers.set(reqid, resolvers)
					// i don't think this can happen
					if (!client) {
						throw new Error(
							`the client has gone missing!!! ${fetchEvent.clientId}. i have NO IDEA what to do`
						)
					}
					const message = {
						id: reqid,
						type: "request",
						cache: cachename,
						request: {
							url: handoffURL.href,
							headers: Object.fromEntries(request.headers.entries()),
							method: request.method,
							destination: request.destination,
							referrer: request.referrer,
						},
					}
					log("sending handoff request", message)
					// send request event to main thread to ask them how to handle it
					client.postMessage(message)
					// this'll finish when the main thread gets back to us
					fetchEvent.waitUntil(resolvers.promise)
					const handoffResponse = await resolvers.promise
					log("received handoff response", handoffResponse)
					if (handoffResponse) {
						const response = new Response(handoffResponse.body, {
							status: handoffResponse.status,
							headers: handoffResponse.headers,
						})
						if (handoffResponse.cache !== false) {
							log(`caching ${handoffURL}`)
							await cache.put(request, response.clone())
						} else {
							log(`caching disabled on ${handoffURL}`)
						}
						return response
					}
					// no idea what's going on now i'm a teapot i'm a teapot
					return new Response("handler returned nothing", {
						status: 418,
					})
				} else {
					// network first strategy for external requests
					const response = await fetch(request)
					if (response) {
						if (response.ok && response.url.match(/^https?\:/)) {
							await cache.put(request, response.clone())
						}
						return response
					}
					if (match) return match
					return new Response("couldnt fetch and no stale", {status: 503})
				}
			} catch (error) {
				if (match) return match
				// if something fucked up happens, serve a stale thing if there is one
				// probably can do better error messaging here based on what was caught
				return new Response(`yikes: ${error}`, {status: 555})
			}
		})()
	)
})
