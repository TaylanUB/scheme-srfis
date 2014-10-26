R7RS SRFI implementations
=========================

<http://srfi.schemers.org/>

Overview
--------

- All implementations should be valid R7RS-small libraries for now.
  We will move to an R7RS-large core/base when possible.

- Feel free to assume sane platform properties like a full numeric
  tower (excluding exact complex numbers), Unicode, etc. and keep
  `cond-expand` usage to a minimum.  (Moving to an R7RS-large base
  will make this point moot.)

  - Rationale: We aren't primarily interested in every tiny Scheme
    implementation; we want to fill gaps between the credible ones.
    If you care about maximal portability, patches welcome; code won't
    be rejected due to liberal use of `cond-expand` etc.

- Feel free to use other SRFIs in your SRFI implementation, even if
  they're not in this repository, if the alternative is using horrid
  hacks.

- Write test suites in SRFI-64: <http://srfi.schemers.org/srfi-64/>

- For those SRFIs which cannot be implemented in pure R7RS, it's fine
  to write libraries that just wrap features of specific platforms via
  `cond-expand` and thus at least work across several platforms even
  if not all R7RS implementations.

- The preferred license is the LGPL version 3.  Refrain from GPL and
  the like; we'd like it if Scheme platforms shipped straight with
  these implementations or platform-tuned versions of them, and
  forcing whole Scheme programs to be GPL'ed just for using these SRFI
  implementations would also be overboard.

Concrete conventions
--------------------

- Libraries go into a file named `n.scm` where `n` is the SRFI number.
  The library is correspondingly named `(srfi n)`.

- The order of things in a library declaration, after the name, is:

  - exports
  - imports
  - optional auxiliary code in a `begin`
  - the main body as an `include` or `begin`

- If an export or import list doesn't fit in one line, then put a
  newline directly after the `export` or `import` keyword, i.e., don't
  leave any expression on the same line as the keyword.

- Unless there is only a very small amount of code, split the main
  body of the library into a file named `n.body.scm` and include that
  from `n.scm`.

- When using a reference implementation, put the original source code
  in a file `n.upstream.scm`, and copy it to `n.body.scm` if you will
  make modifications.  This is because some reference implementations
  change ad-hoc without version control; we want to know what version
  we forked.

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

SRFI-64
-------

- `test-read-eval-string`: This now takes an optional `env` argument
  to specify in what environment to evaluate.  Passing `#f` will
  explicitly attempt to call `eval` with a single argument, which is
  an error in R7RS.  Leaving the argument out will try to use a sane
  platform-specific default but falls back to `#f`.
