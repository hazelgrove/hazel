# esy-solve-cudf

[![Build Status](https://travis-ci.com/andreypopp/esy-solve-cudf.svg?branch=master)](https://travis-ci.com/andreypopp/esy-solve-cudf)

CUDF solver used by [esy][]. Based on [mccs][].

## Release

Tag & push a new version:

```
% npm version {patch,minor,major}
% git push & git push --tags
```

Wait till built artifacts (https://github.com/andreypopp/esy-solve-cudf/releases) appear and:

```
% make release
% cd _release
% npm publish
```

[esy]: https://github.com/esy/esy
[mccs]: http://www.i3s.unice.fr/~cpjm/misc/mccs.html
