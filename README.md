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

- Feel free to use R6RS libraries in your SRFI implementation
  (imported via the R7RS library system), if the alternative is using
  horrid hacks.  In particular, the Larceny project hosts a number of
  R7RS libraries named `(r6rs *)`, under `tools/R6RS/` in its source
  tree, which implement the APIs of the corresponding `(rnrs *)`
  libraries of R6RS; feel free to use those.

- Test suites are written in a strict format using SRFI-64 for human
  and machine consumption; see below.

- For those SRFIs which cannot be implemented in pure R7RS, it's fine
  to write libraries that just wrap features of specific platforms via
  `cond-expand` and thus at least work across several platforms even
  if not all R7RS implementations.  However, don't try to implement
  functionality via library code when it's clearly intended to be
  implemented at the platform level.

- Withdrawn SRFIs are no priority since we assume they've been
  withdrawn for good reasons, but if there's any which you find useful
  then feel free to make a request or (better) contribute it.

- Please use the standard SRFI license, found at the bottom of most
  SRFI pages.

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

The author of this repository uses Chibi to test the libraries.  See
the next section for instructions.

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

Using Chibi to test the implementations
---------------------------------------

Chibi Scheme is a clean, small, portable, and pretty much fully R7RS
compliant Scheme platform, so it's a good choice for testing R7RS
libraries.

- Clone Chibi from its git repository and build it by running `make`.
- Create the directory `lib/needed-for-init` in the source tree.
- Copy the directory `lib/srfi` to `lib/needed-for-init/srfi`.
- Remove all files and directories except `69` and `69.sld` under
  `lib/needed-for-init/srfi`.
- Run Chibi as follows:

    LD_LIBRARY_PATH=$chibi_repo \
    CHIBI_MODULE_PATH=$chibi_repo/lib \
    $chibi_repo/chibi-scheme \
      -I $srfi_repo \
      -I $chibi_repo/lib/needed-for-init \
      remaining arguments ...

Concrete conventions
--------------------

- All implementations should be valid R7RS-small libraries for now.
  We will move to an R7RS-large core/base when possible.

- Libraries go into a file named `n.sld` under the `srfi` directory,
  where `n` is the SRFI number.  The library is correspondingly named
  `(srfi n)`.

- Test suites are libraries too, and go into `n.sld` under the
  `srfi-tests` directory, and are named accordingly.  They must export
  a procedure named `run-tests` defined via `define-tests` from
  `(srfi-tests aux)`.  The first argument to this macro must be the
  identifier `run-tests`, and the second must be the string `"SRFI-n"`
  for every SRFI-`n`.  The rest of the arguments make up the body of
  an SRFI-64 test suite; the initial `test-begin` and final `test-end`
  forms must not appear in this body, since `define-tests` handles
  that.

- The following rules about libraries apply to all libraries in this
  project, including SRFI implementations and test suites.

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
diagnostics, usually at the expense of performance.  Note that there
is no portable and declarative way to do this; it relies on the
platform supporting an imperative and sequential method for loading
modules and executing code in-between.  It is most likely to work in a
REPL, if at all.  A library might also be loaded only a single time
for the whole run-time of a platform, so a restart might be necessary
to load a library with a different `debug-mode` setting.  A number of
SRFIs also support other settings through the `(srfi aux)` library;
the same limitations apply to those.

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

SRFI-41
-------

The SRFI proposes an R6RS library `(streams)` with sub-libraries
`(streams primitive)` and `(streams derived)`.  We provide these as
`(srfi 41)`, `(srfi 41 primitive)`, and `(srfi 41 derived)`,
respectively.

SRFI-63
-------

All identifiers have been lowercased.  If you have source code using
identifiers with uppercase letters, you might want to use the
`#!fold-case` directive.

SRFI-64
-------

- `test-runner-simple`: In line with the specification (and contrary
  to the reference implementation), this returns a runner that does no
  logging and does not use its "aux value" field.  However, it takes
  an optional first argument that may be a file name, which tells the
  test runner to log to that file.

*Note:* The default test runner is created by calling this procedure
with a default file name for logging.  To explicitly disable logging,
you can call: `(test-runner-current (test-runner-simple))`

- The reference implementation calls the on-group-begin handler before
  it modifies the group stack, and before it sets the group name, so
  calling `test-runner-group-stack`, `test-runner-group-path`, or
  `test-runner-test-name` will return values corresponding to *before*
  the new group begins. Our implementation calls the handler *after*
  these values are changed. The specification is unclear about which
  behavior is correct, but includes the following wording in the
  definition of `test-runner-test-name`: "During execution of
  test-begin this is the name of the test group."  It seems logical
  that the on-group-begin handler would be called "during" test-begin,
  so our behavior is arguably more in line with the spec. And the
  group stack should intuitively always have the current name as the
  topmost value.

- `test-read-eval-string`: This now takes an optional `env` argument
  to specify in what environment to evaluate.  Passing `#f` will
  explicitly attempt to call `eval` with a single argument, which is
  an error in R7RS.  Leaving the argument out will try to use a sane
  platform-specific default but falls back to `#f`.

- `test-exit`: A non-standard extension: exits with a non-zero exit
   status if any tests failed or unexpectedly passed.

SRFI-66
-------

The "u8vector" API in this SRFI is almost equivalent to the bytevector
API in R7RS.  The only incompatibilities are that `u8vector-copy!`
takes a different order of arguments, and that there is no equivalent
of `u8vector-compare` in R7RS.  I see neither of these reason enough
to implement this SRFI, considering it to be subsumed by R7RS instead.

That being said, I augmented SRFI-67 with `bytevector-compare`.

SRFI-67
-------

- `bytevector-compare`, `bytevector-compare-as-list`: These are
  natural additions to this SRFI since R7RS has bytevectors as an
  additional sequence type.

SRFI-70
-------

This is an odd one.  It's not an SRFI in the strict sense, because it
requests a change to the standard and not to implementations.  In any
case I marked it as subsumed by R7RS below because it seems that R7RS
improved on the section which this SRFI requests improvements in,
although not exactly in the way this SRFI asks for.


Progress
========

All SRFI are listed here, and marked with one of the following:

- (empty): Has yet to be looked into.
- withdrawn: It's a withdrawn SRFI.
- deprecated: Deprecated by another SRFI.
- platform: Ought to be implemented at the platform level.
- r6rs: Subsumed by R6RS in some way.
- r7rs: Subsumed by R7RS in some way.
- UNTESTED: Yet lacks a test-suite.
- DRAFT: Still in draft status.
- check: Implemented and passes its test-suite.

UNTESTED and DRAFT are capitalized to emphasize that we're not done
with them yet; all other non-empty tags mean we're done with it.

Don't let the UNTESTED tag scare you off; I minimally test any
implementation in Chibi, and some are really trivial, but so long as
an SRFI has no corresponding `n.test.scm`, it's UNTESTED, period.

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
- SRFI-13:
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
- SRFI-25: UNTESTED
- SRFI-26: check
- SRFI-27: UNTESTED
- SRFI-28: UNTESTED
- SRFI-29:
- SRFI-30: r7rs
- SRFI-31: check
- SRFI-32: withdrawn
- SRFI-33: withdrawn
- SRFI-34: r7rs
- SRFI-35: UNTESTED
- SRFI-36: platform
- SRFI-37: UNTESTED
- SRFI-38: r7rs
- SRFI-39: r7rs
- SRFI-40: deprecated (SRFI-41)
- SRFI-41: UNTESTED
- SRFI-42: UNTESTED
- SRFI-43: UNTESTED
- SRFI-44:
- SRFI-45: r7rs
- SRFI-46: r7rs
- SRFI-47: deprecated (SRFI-63)
- SRFI-48: UNTESTED
- SRFI-49: platform
- SRFI-50: withdrawn
- SRFI-51: UNTESTED
- SRFI-52: withdrawn
- SRFI-53: withdrawn
- SRFI-54: check
- SRFI-55: r7rs (import)
- SRFI-56: withdrawn
- SRFI-57: UNTESTED
- SRFI-58: platform
- SRFI-59:
- SRFI-60: UNTESTED
- SRFI-61: UNTESTED
- SRFI-62: r7rs
- SRFI-63: UNTESTED
- SRFI-64: check
- SRFI-65: withdrawn
- SRFI-66: r7rs (bytevectors)
- SRFI-67: UNTESTED
- SRFI-68: withdrawn
- SRFI-69: UNTESTED
- SRFI-70: r7rs (see remarks above)
- SRFI-71: UNTESTED
- SRFI-72: platform
- SRFI-73: withdrawn
- SRFI-74: r6rs
- SRFI-75: withdrawn
- SRFI-76: withdrawn
- SRFI-77: withdrawn
- SRFI-78: UNTESTED
- SRFI-79: withdrawn
- SRFI-80: withdrawn
- SRFI-81: withdrawn
- SRFI-82: withdrawn
- SRFI-83: withdrawn
- SRFI-84: withdrawn
- SRFI-85: withdrawn
- SRFI-86:
- SRFI-87: UNTESTED
- SRFI-88: platform
- SRFI-89:
- SRFI-90:
- SRFI-91: withdrawn
- SRFI-92: withdrawn
- SRFI-93: withdrawn
- SRFI-94:
- SRFI-95: UNTESTED
- SRFI-96:
- SRFI-97:
- SRFI-98: r7rs
- SRFI-99:
- SRFI-100:
- SRFI-101:
- SRFI-102: withdrawn
- SRFI-103: withdrawn
- SRFI-104: withdrawn
- SRFI-105: platform
- SRFI-106: DRAFT
- SRFI-107: platform
- SRFI-108: platform
- SRFI-109: platform
- SRFI-110: platform
- SRFI-111: UNTESTED
- SRFI-112:
- SRFI-113:
- SRFI-114:
- SRFI-115:
- SRFI-116:
- SRFI-117: DRAFT
- SRFI-118:
- SRFI-119: platform
- SRFI-120:
- SRFI-121: DRAFT
- SRFI-122: DRAFT
- SRFI-123: DRAFT
