# Parsnip

A parsing library which is the spiritual successor of [parsley](http://github.com/cgrand/parsley) and technical descendant of [seqexp](http://github.com/cgrand/seqexp).

## Design goals

* General: accepts any CFG.
* Deterministic: always select the same tree out of the parse forrest.
* Total: for unacceptable inputs, yields a parse for one of the closest acceptable input (distance being character deleted from the original input).
* Scannerless
* Incremental

## Implementation details

Parsnip uses a VM with 5 opcodes (`PRED`, `JUMP`, `FORK`, `CALL` and `RET`).

The VM is multithreaded (these are not real threads) and all threads are run in lockstep.

The state of a thread consists only of its PC (program counter) and its stack. At any time there can't be more than one thread with the same PC and same stack. Each threads also has an error count and a priority; they are both used when deduplicating threads with identical PC and stack.

The threads are stored in a structure resembling a lazy [graph-structured stack](http://en.wikipedia.org/wiki/Graph-structured_stack).

### PRED pred
`pred` is a predicate whose type is determined by the VM (the naive VM expectes functions).

The predicate is applied to the current element.

If the predicate succeeds, the thread continues to the next instruction and to the next element of the input sequence. When the predicate fails the thread stays at the same PC (thus waits for the next character), increments its error count.

### JUMP address
Performs a jump, `address` is an absolute address.

### FORK address
Forks the current thread in two threads, one thread will continue to the next instruction while the other will performs a relative jump to the specified `address`.

Priority is given to the continuing thread.

It should be noted that the only effect of this *priority* is parse-tree selection: selecting one parse tree out of the parse forrest.

### CALL address
Pushes the return address on the stack and jump to `address`.

### RET tag
Pops an address from the stack and jumps to it. (`tag` is an arbitrary value)

## License

Copyright © 2014 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
