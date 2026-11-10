import {
	IndexedDBStorageAdapter,
	MessageChannelNetworkAdapter,
	Repo,
} from "@automerge/vanillajs"
async function installServiceWorker() {
	const sw = await navigator.serviceWorker
		.register("service-worker.js", {type: "classic"})
		.then(registration => {
			const installing = registration.installing
			if (installing) {
				console.log("%c spawing new service worker", "color: pink")
				return new Promise(resolve => {
					installing.onstatechange = event => {
						const serviceWorker = event.target
						if (serviceWorker.state === "activated") {
							resolve(serviceWorker)
						}
					}
				})
			}
			return registration.active
		})
	setInterval(() => {
		sw.postMessage({type: "PING"})
	}, 5e3)
	return sw
}
async function createRepo(storage) {
	const peerIdSuffix = `patchwork-${Math.random().toString(36).slice(2)}`
	const peerId = peerIdSuffix
	const repo = new Repo({
		network: [],
		storage,
		peerId,
		enableRemoteHeadsGossiping: true,
	})
	self.repo = repo
	repo.subscribeToRemotes(["3760df37-a4c6-4f66-9ecd-732039a9385d"])
	return {repo}
}
let globalMessageChannelAdapter
function connectServiceWorkerToRepo(serviceWorker, repo) {
	const messageChannel = new MessageChannel()
	if (globalMessageChannelAdapter) {
		repo.networkSubsystem.removeNetworkAdapter(globalMessageChannelAdapter)
	}
	globalMessageChannelAdapter = new MessageChannelNetworkAdapter(
		messageChannel.port1
	)
	repo.networkSubsystem.addNetworkAdapter(globalMessageChannelAdapter)
	serviceWorker.postMessage({type: "INIT"}, [messageChannel.port2])
	console.log("%c Connected to service worker", "color: blue")
}
async function bootstrap() {
	let sw = await installServiceWorker()
	const storage = new IndexedDBStorageAdapter()
	const {repo, hive} = await createRepo(storage)
	const {promise: serviceWorkerInitEcho, resolve} = Promise.withResolvers()
	if (!hive) {
		navigator.serviceWorker.addEventListener("message", event => {
			switch (event.data.type) {
				case "SERVICE_WORKER_RESTARTED":
					console.log(
						"establishMessageChannel: SERVICE_WORKER_RESTARTED message"
					)
					connectServiceWorkerToRepo(sw, repo)
					break
				case "SERVICE_WORKER_READY":
					resolve()
					break
			}
		})
		navigator.serviceWorker.addEventListener("controllerchange", event => {
			const newServiceWorker = event.target.controller
			if (newServiceWorker !== sw) {
				console.log(
					"establishMessageChannel: controllerchange to new service worker"
				)
				sw = newServiceWorker
				connectServiceWorkerToRepo(newServiceWorker, repo)
			}
		})
		connectServiceWorkerToRepo(sw, repo)
		await serviceWorkerInitEcho
	}
	return {repo, hive}
}
export {
	connectServiceWorkerToRepo,
	createRepo,
	bootstrap as default,
	installServiceWorker,
}
