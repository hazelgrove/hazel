// Where Hazel keeps its state. null: in this browser's IndexedDB, as on
// hazel.org/build. A canister deploy overwrites this file with the backend
// canister's address (see ic/deploy-local.sh), and HazelDB then reads and
// writes there instead.
window.hazelBackend = null;
