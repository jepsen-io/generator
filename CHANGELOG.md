# 0.3.13

This is a small release. It makes expensive generators more efficent with a
cache, allows rw and append tests a final chance to detect data loss at the end
of tests, and lets file snapshot nemeses run when `/tmp` is on a different
kind of filesystem.

## New Features

- `generator/cache`: caches operations from an expensive generator to reduce
  (e.g.) IO costs.
- `tests.cycle.rw` and `append` now come with `:final-generator` and
  `:wrap-generator` which, during the final phase of a test, can issue final
   reads of all keys.
- `snapshot-file-chunks` now works across filesystem types.

## Minor Changes

`core/prepare-test`: Don't double-wrap generators in `Forgettable` when run as a part of `test-all`. This didn't break anything, but it was unnecesary.

