# mpl-demo
Demonstration of Parallel ML with `mpl`.

## Directories
- `shared/` is a library.
- `arrays/` has considers various parallel array primitives and experiments
with different implementation and granularity control techniques.
- `counter/` demonstrates race conditions and atomic operations
(compare-and-swap, fetch-and-add) on a concurrent counter.
- `print/` demonstrates race conditions with printing to stdout, and shows how
to implement a mutex in terms of compare-and-swap to fix this problem.
- `dedup/` shows an example of deduplicating integers with parallel hashing.
