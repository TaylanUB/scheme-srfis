R7RS SRFI implementations
=========================

<http://srfi.schemers.org/>

Overview
--------

Our intention is to bridge gaps between credible Scheme platforms, as
well as explore what is possible to implement *sanely* in R7RS.
Unlike a lot of legacy "portable" Scheme code, this is not a freak
show.  Therefore:

- Feel free to assume sane platform properties like a full numeric
  tower (excluding exact complex numbers), Unicode, etc. and keep
  `cond-expand` usage to a minimum.  (Moving to an R7RS-large base
  will make this point moot.)

- Feel free to use other SRFIs in your SRFI implementation, even if
  they're not in this repository, if the alternative is using horrid
  hacks.

- Write test suites in SRFI-64: <http://srfi.schemers.org/srfi-64/>.
  It's also fine to use any ad-hoc test suites found within reference
  implementations though, like in SRFI-2.

- For those SRFIs which cannot be implemented in pure R7RS, it's fine
  to write libraries that just wrap features of specific platforms via
  `cond-expand` and thus at least work across several platforms even
  if not all R7RS implementations.  However, don't try to implement
  functionality via library code when it's clearly intended to be
  implemented at the platform level.

- The preferred license is the LGPL version 3.  Refrain from GPL and
  the like; we'd like it if Scheme platforms shipped straight with
  these implementations or platform-tuned versions of them, and
  forcing whole Scheme programs to be GPL'ed just for using these SRFI
  implementations would also be overboard.

Platform expectations
---------------------

As explained above, we expect some basic maturity and feature richness
from platforms.  So far this is not documented in any more detail, but
it should be something along the lines of:

<http://trac.sacrideo.us/wg/wiki/MandatoryDocket>

If a library in this repository doesn't work on your preferred Scheme
platform, file a bug report.  If it contains a working patch it will
most likely be accepted; otherwise it might get rejected on the
grounds that the mentioned Scheme platform is not suitable.

The author of this repository uses Chibi to test the libraries.
Creating a symlink `n.sld` to any file `n.scm` and *prepending* the
path to the *parent* directory of this repository to the load-path of
Chibi (via the -I flag) will make the implementations here override
any native SRFI implementations of Chibi.

Other than Chibi, the following platforms are explicitly intended to
be supported once they implement R7RS:

- Racket
- Guile
- Chicken
- Gauche
- Gambit
- Larceny
- Bigloo
- Kawa
- Chez

The following do not intend to support R7RS, for various reasons:

- MIT/GNU Scheme
- Scheme48
- Stalin
- TinyScheme

Source:
<http://lists.scheme-reports.org/pipermail/scheme-reports/2011-October/001595.html>

Concrete conventions
--------------------

- No withdrawn SRFIs.

- All implementations should be valid R7RS-small libraries for now.
  We will move to an R7RS-large core/base when possible.

- Libraries go into a file named `n.scm` where `n` is the SRFI number.
  The library is correspondingly named `(srfi n)`.

- The order of things in a library declaration is:

  - exports
  - imports
  - auxiliary code in a `begin` if the main body is in an `include`
  - the main body as an `include` or `begin`

- If an export or import list doesn't fit in one line, then put a
  newline directly after the `export` or `import` keyword, i.e., don't
  put any export or import specs on the same line as the keyword.

- When there are many exports, put the closing parenthesis of the
  export form on its own line.  Don't do this for imports.

- If there is a substantial amount of code such that the indentation
  of the library form is an annoyance, split the main body of the
  library into a file named `n.body.scm` and `include` that.

- When using a reference implementation with a substantial amount of
  code, put the original source code in a file `n.upstream.scm`, and
  do not edit that file.  `include` directly that if no changes are
  needed; otherwise copy it to `n.body.scm` and make changes there.
  This is because some reference implementations change ad-hoc without
  version control; we want to know what version we forked.

- Put test suites into a file named `n.test.scm`.  These should be
  R7RS programs, but not libraries.

- Follow Riastradh's Lisp Style Rules for new code:
  <http://mumble.net/~campbell/scheme/style.txt>

Using reference implementations
-------------------------------

- You can probably often just wrap a reference implementation in an
  R7RS library, but do modify them to make good use of R7RS features,
  to avoid dirty hacky things like redefining type predicates (it's
  mostly illegal anyway now), liberal use of global variables (use
  parameters instead), etc.

- Be careful about the copyright and licensing.  When putting source
  code in an `n.upstream.scm` file, add suitable legal boilerplate
  even if the original code didn't have any (usually the SRFI HTML
  page will have a copyright).  The same boilerplate goes into
  `n.body.scm` of course.

SRFI-specific notes
===================

For some SRFIs, setting the `debug-mode` parameter from the library
`(srfi aux)` to true while loading the library will enable improved
diagnostics, usually at the expense of performance.  Note that a
library might be loaded only a single time for the whole run-time of a
system, so a restart might be necessary to reload a library with a
different `debug-mode` setting.

SRFI-2
------

- `and-let*`: The author of this SRFI thought that `let*` is supposed
  to error when the same identifier is bound multiple times in
  sequential clauses, and specified `and-let*` to do the same; it was
  in fact an ambiguity prior to R6RS what `let*` should do, and has
  been clarified since R6RS that it should allow sequential repeated
  bindings.  Racket's, Guile's, and Chibi's SRFI-2 implementations do
  allow it, and it's a lot easier to implement too, hence our
  implementation allows it as well.

SRFI-13
-------

- `string-titlecase`: The module `(srfi aux)` contains the parameters
  `char-cased?-proc` and `char-titlecase-proc` which you can set while
  loading this library (and only while loading it, not later) to
  correct the behavior of this procedure.  The default `char-cased?`
  compares the upcased and downcased versions of a character to decide
  whether it's cased, and `char-titlecase` does a mere upcase.

SRFI-64
-------

- `test-read-eval-string`: This now takes an optional `env` argument
  to specify in what environment to evaluate.  Passing `#f` will
  explicitly attempt to call `eval` with a single argument, which is
  an error in R7RS.  Leaving the argument out will try to use a sane
  platform-specific default but falls back to `#f`.

Progress
========

All SRFI are listed here, and marked with one of the following:

- (empty): Has yet to be looked into.
- withdrawn: It's a withdrawn SRFI.
- deprecated: Deprecated by another SRFI.
- platform: Ought to be implemented at the platform level.
- r7rs: Subsumed by R7RS in some way.
- UNTESTED: Yet lacks a test-suite.
- DRAFT: Still in draft status.
- check: Implemented and passes its test-suite.

UNTESTED and DRAFT are capitalized to emphasize that we're not done
with them yet; all other non-empty tags mean we're done with it.

The r7rs tag can be vague.  In the simplest case the SRFI is adopted
as-is in R7RS.  Sometimes there are minor tweaks.  Sometimes the same
essential functionality is provided in a different way in R7RS; a good
example might be SRFI-7, though it's equivalent to `define-library` if
you pay attention.

- SRFI-0: platform
- SRFI-1: UNTESTED
- SRFI-2: check
- SRFI-3: withdrawn
- SRFI-4: platform
- SRFI-5: UNTESTED
- SRFI-6: r7rs
- SRFI-7: r7rs (define-library)
- SRFI-8: UNTESTED
- SRFI-9: r7rs
- SRFI-10: platform
- SRFI-11: r7rs
- SRFI-12: withdrawn
- SRFI-13: UNTESTED
- SRFI-14:
- SRFI-15: withdrawn
- SRFI-16: r7rs
- SRFI-17: UNTESTED
- SRFI-18:
- SRFI-19:
- SRFI-20: withdrawn
- SRFI-21:
- SRFI-22: platform
- SRFI-23: r7rs
- SRFI-24: withdrawn
- SRFI-25:
- SRFI-26: check
- SRFI-27:
- SRFI-28: UNTESTED
- SRFI-29:
- SRFI-30: r7rs
- SRFI-31: check
- SRFI-32: withdrawn
- SRFI-33: withdrawn
- SRFI-34: r7rs
- SRFI-35: UNTESTED
- SRFI-36:
- SRFI-37:
- SRFI-38: r7rs
- SRFI-39: r7rs
- SRFI-40: deprecated
- SRFI-41:
- SRFI-42:
- SRFI-43:
- SRFI-44:
- SRFI-45: r7rs
- SRFI-46: r7rs
- SRFI-47:
- SRFI-48:
- SRFI-49:
- SRFI-50: withdrawn
- SRFI-51:
- SRFI-52: withdrawn
- SRFI-53: withdrawn
- SRFI-54:
- SRFI-55:
- SRFI-56: withdrawn
- SRFI-57:
- SRFI-58:
- SRFI-59:
- SRFI-60: UNTESTED
- SRFI-61:
- SRFI-62:
- SRFI-63:
- SRFI-64: UNTESTED
- SRFI-65: withdrawn
- SRFI-66:
- SRFI-67:
- SRFI-68: withdrawn
- SRFI-69:
- SRFI-70:
- SRFI-71:
- SRFI-72:
- SRFI-73: withdrawn
- SRFI-74:
- SRFI-75: withdrawn
- SRFI-76: withdrawn
- SRFI-77: withdrawn
- SRFI-78:
- SRFI-79: withdrawn
- SRFI-80: withdrawn
- SRFI-81: withdrawn
- SRFI-82: withdrawn
- SRFI-83: withdrawn
- SRFI-84: withdrawn
- SRFI-85: withdrawn
- SRFI-86:
- SRFI-87:
- SRFI-88:
- SRFI-89:
- SRFI-90:
- SRFI-91: withdrawn
- SRFI-92: withdrawn
- SRFI-93: withdrawn
- SRFI-94:
- SRFI-95:
- SRFI-96:
- SRFI-97:
- SRFI-98:
- SRFI-99:
- SRFI-100:
- SRFI-101:
- SRFI-102: withdrawn
- SRFI-103: withdrawn
- SRFI-104: withdrawn
- SRFI-105:
- SRFI-106:
- SRFI-107:
- SRFI-108:
- SRFI-109:
- SRFI-110:
- SRFI-111:
- SRFI-112: DRAFT
- SRFI-113: DRAFT
- SRFI-114:
- SRFI-115:
