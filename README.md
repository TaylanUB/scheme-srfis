R7RS SRFI implementations
=========================

- SRFI are at: <http://srfi.schemers.org/>

- All implementations should be valid R7RS-small libraries for now.
  When it's finished, we will move to an R7RS-large core/base.

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
