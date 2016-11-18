## version 0.11

In this version we fixed a bug ([commit](https://github.com/s3team/uroboros/commit/45f018af9a322e114b6e4d365a897115b934e3b3)) and provide instructions to re-produce the experiment results in our USENIX paper.


### The bug:

Note that 64-bit program binaries require the data sections to be 16B aligned
and SSE instructions will do a align checking on this. If the memory access through SSE
instructions are not 16B aligned, the memory access through SSE instructions will crash immediately.
 
We used to have some routines to add the ".align 16" macro to force this 16B alignment. However, they 
are errorly excluded during our internal code merge of 32-bit and 64-bit codebases.

### Instruction to re-produce the experiment results in our USENIX paper:

Please find the instructions [here](https://github.com/s3team/uroboros/blob/master/instruction_to_reproduce_experimenttal_results_reported_in_Wang_et_al._2015.md).
