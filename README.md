# haskell-ssi

Server Side Includes (SSI) processor written in Haskell

# Background

My website has accumulated cruft over the years.  In the beginning, one of the
technologies I used, since Apache Httpd was everywhere, was SSI.

The most recent incarnation of my site is now served by Caddy, so I needed a
way to parse the few old `.shtml` files I still cared about.

# Features

Features that are planned or implemented.

- [x] Terrible code organization (initial version in single file).
- [x] Horrible assumptions ([See: Bugs](#bugs)).
- [ ] Better-organized code (names, splits).
- [ ] FastCGI env setup (currently just a cli script w/ config via env vars).
- [ ] Rename executable from `ssi` to `haskell-ssi`. `(maybe)`

# SSI Directives

I only really care about "old style" SSI, and even then, only really about the
few directives I used back in ~2005.

- [x] `<!--#if expr="..."--><!--#else--><!--#endif-->`
- [x] `<!--#config timefmt="..."-->`
- [x] `<!--#echo var="..."-->`
- [x] `<!--#set var="..." value="..."-->`
- [x] `<!--#include file="..."-->`
- [ ] `<!--#include virtual="..." -->` (dependent on fixing initial bug).

# Bugs

I wrote the initial version thinking that SSI directives would be found as
comments in HTML.  Turns out, the files are parsed as text (so, embedding an
SSI directive within an HTML comment is fine).

# License

Copyright © 2017 Benjamin R. Haskell

Distributed under the MIT License (included in file: [LICENSE](LICENSE)).
