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

- Write test suites in SRFI-64: <http://srfi.schemers.org/srfi-64/>

- You can probably often just wrap a reference implementation in an
  R7RS library, but pay attention to the following point:

- Do make good use of R7RS features, as well as having SRFIs rely on
  each other, to avoid dirty hacky things like redefining type
  predicates (it's mostly illegal anyway now), liberal use of global
  variables (use parameters instead), etc. etc.

- For those SRFIs which cannot be implemented in pure R7RS, it's fine
  to write libraries that just wrap features of specific platforms via
  `cond-expand` and thus at least work across several platforms even
  if not all R7RS implementations.

- The preferred license is the LGPL version 3.  Refrain from GPL and
  the like; we'd like it if Scheme platforms shipped straight with
  these implementations or platform-tuned versions of them, and
  forcing whole Scheme programs to be GPL'ed just for using these SRFI
  implementations would also be overboard.

- Be careful about the copyright and licensing of SRFI reference
  implementations when you plan to use them.

- When using a reference implementation, include the exact source code
  you started with in a file srfi-x.upstream.scm, with copyright etc.
  added as comments at the top if it wasn't there already.  For
  modifications, copy the file to srfi-x.body.scm and use that.  This
  is because some SRFI reference implementations change ad-hoc without
  version control; we want to know what version we forked.

- Don't mix library declarations and code; put library declarations in
  an srfi-x.scm file, and include the main code from a corresponding
  srfi-x.body.scm or srfi-x.upstream.scm file.
