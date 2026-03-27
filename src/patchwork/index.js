export const plugins = [
	{
		type: "patchwork:datatype",
		id: "@hazelgrove/hazel",
		name: "Hazel Program",
		async load() {
			return import("./hazel-program.js").then(mod => mod.default)
		},
	},
	{
		type: "patchwork:tool",
		id: "@hazelgrove/spazel",
		name: "Spazel Editor",
		supportedDatatypes: ["@hazelgrove/hazel"],
		async load() {
			return import("./hazel-tool.js").then(mod => mod.default)
		},
	},
]
