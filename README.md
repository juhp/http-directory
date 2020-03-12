# http-directory

[![Hackage](https://img.shields.io/hackage/v/http-directory.svg)](https://hackage.haskell.org/package/http-directory)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/http-directory/badge/lts)](http://stackage.org/lts/package/http-directory)
[![Stackage Nightly](http://stackage.org/package/http-directory/badge/nightly)](http://stackage.org/nightly/package/http-directory)
[![Build status](https://secure.travis-ci.org/juhp/http-directory.svg)](https://travis-ci.org/juhp/http-directory)

A simple library for reading http directories.

It uses http-client for http transport, and
html-conduit and xml-conduit to parse the html for links.

The library is intended for listing the files in http file directories,
but since http directories are just html pages it can also be used
to list the links (href's) on any html webpage.

## Usage Example

```
*Network.HTTP.Directory> httpDirectory' "https://hackage.haskell.org/package/base/src/System"
["CPUTime.hsc","Environment.hs","Exit.hs","IO.hs","Info.hs","Mem.hs","Timeout.hs","CPUTime","Console","Environment","IO","Mem","Posix"]
```

See more [examples](https://github.com/juhp/http-directory/blob/master/example/) and the haddock documentation for usage.
