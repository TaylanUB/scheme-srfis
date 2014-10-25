R7RS SRFI implementations
=========================

- SRFI are at: <http://srfi.schemers.org/>

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

- Feel free to use other SRFI in your SRFI implementation; though of
  course it's best if that SRFI is found in this repository as well.
  Still, rely on an SRFI if the alternative is using horrid hacks.

- Write test suites in SRFI-64: <http://srfi.schemers.org/srfi-64/>

- For those SRFIs which cannot be implemented in pure R7RS, it's fine
  to write libraries that just wrap features of specific platforms via
  `cond-expand` and thus at least work across several platforms even
  if not all R7RS implementations.

- Don't mix library declarations and code; put library declarations in
  an srfi-x.scm file, and include the main code from a corresponding
  srfi-x.body.scm or srfi-x.upstream.scm file.

- The preferred license is the LGPL version 3.  Refrain from GPL and
  the like; we'd like it if Scheme platforms shipped straight with
  these implementations or platform-tuned versions of them, and
  forcing whole Scheme programs to be GPL'ed just for using these SRFI
  implementations would also be overboard.

Using reference implementations
-------------------------------

- You can probably often just wrap a reference implementation in an
  R7RS library, but do modify them to make good use of R7RS features,
  to avoid dirty hacky things like redefining type predicates (it's
  mostly illegal anyway now), liberal use of global variables (use
  parameters instead), etc. etc.

- Include the upstream source code you started with in a file named
  srfi-x.upstream.scm, with copyright etc. added as comments at the
  top if it wasn't there already.  For modifications, copy the file to
  srfi-x.body.scm and use that.  This is because some SRFI reference
  implementations change ad-hoc without version control; we want to
  know what version we forked.

- Be careful about the copyright and licensing.

SRFI-specific notes
===================

SRFI-64
-------

- `test-read-eval-string`: This now takes an optional `env` argument
  to specify in what environment to evaluate.  Passing `#f` will
  explicitly attempt to call `eval` with a single argument, which is
  an error in R7RS.  Leaving the argument out will try to use a sane
  platform-specific default but falls back to `#f`.
