These instructions assume you want a local opam switch.
If you don't want that, replace "." with the name of your switch.

From project root folder:

1. Replace the current opam switch with a fresh one for OCaml 4.14.0.

``` shell
opam switch remove .
opam switch create . 4.14.0
```

2. Make sure this switch remains selected when you leave the project folder.

``` shell
eval $(opam env --switch=. --set-switch)
```

3. From somewhere outside the Hazel source tree, clone the following opam package repos:

``` shell
cd ..
git clone 'git@github.com:janestreet/async_js.git'
git clone 'git+https://github.com/janestreet/incr_dom.git'
git clone 'git+https://github.com/janestreet/sexplib.git'
git clone 'git@github.com:dedbox/virtual_dom.git'
```

4. Install the patched virtual_dom first.

``` shell
cd virtual_dom
opam pin .
```

5. Install the other packages (they are the direct dependents of virtual_dom)

``` shell
cd ../sexplib
opam pin .
cd ../async_js
opam pin .
cd ../incr_dom
opam pin .
```

6. Install the remaining dependencies and compile Hazel.

``` shell
cd ../hazel
make deps
make
```

That's it!
