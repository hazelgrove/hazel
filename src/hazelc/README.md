# hazelc

Hazel compiler source code.

See [PHI 14](https://github.com/hazelgrove/phi/blob/14-compiler/14-compiler/14-compiler.md) for
details.

## Development

All shell commands should be run the repository root directory.

### Testing

Tests can be found in [`test/test`](./test/test). To run:

``` shell
make test # or
dune runtest
```

### Benchmarking

Benchmarks can be found in [`test/bench`](./test/bench). To run:

``` shell
dune exec src/hazelc/test/bench/main.exe --profile=release
```
