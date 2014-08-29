# Parsnip

A parsing library which is the spiritual successor of [parsley](http://github.com/cgrand/parsley) and technical descendant of [seqexp](http://github.com/cgrand/seqexp).

## Implementation details

Parsnip uses a VM with 5 opcodes (`PRED`, `JUMP`, `FORK`, `CALL` and `RET`).

The VM is multithreaded (these are not real threads) and all threads are run in lockstep.

The state of a thread consists only of its PC (program counter) and its stack. At any time there can't be more than one thread with the same PC and same stack. Each threads also has an error count and a priority; they are both used when deduplicating threads with identical PC and stack.

The threads are stored in a structure resembling a lazy [grap-structured stack](http://en.wikipedia.org/wiki/Graph-structured_stack).


## License

Copyright © 2014 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
