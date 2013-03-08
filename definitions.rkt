#lang racket

(provide (struct-out Statement))

#| Draw an tree that corresponds to the symbolic execution of a program. 
   Each node in the tree is a Statement. |#

#| A Statement contains:
     Next instruction to execute, i.e. the program counter.
     Current state, i.e. values of variables.
     Path constraints collected. A list representing a 
     sequence of conjunctions. |#
(struct Statement
  (#;field-names:
   instruction-to-execute
   state
   pc)
  #:transparent)

#| 

A program is a sequence of one or more «body-expr». 

«body-expr» ::= (define «id» «symbolic-expr»)
             |  (set! «id» «symbolic-expr») 
             |  (block «body-expr» ...)
             |  (unless «symbolic-expression» «body-expr» ...)
             |  (until «symbolic-expression» «body-expr» ...)
             |  (if «symbolic-expression» (block «body-expr» ...) (block «body-expr» ...))
             | (ERROR)

«symbolic-expr»::= «id»
                | number
                | (= «symbolic-expr» ...)
                | (> «symbolic-expr» ...)
                | (>= «symbolic-expr» ...)
                | (< «symbolic-expr» ...)
                | (<= «symbolic-expr» ...)
                | (+ «symbolic-expr» ...)
                | (- «symbolic-expr» ...)
                | (* «symbolic-expr» ...)
                | (/ «symbolic-expr» ...)     
  
You can observe the symbolic execution tree by calling

(symbolic-execution-tree (parameters «p» ...) (parameter-values «v» ...) (body «body-expr» ...))

where (body «body-expr» ...) is the sequence of «body-expr» that 
correspond to a program, (parameters «p» ...) is the sequence of 
parameters of these «body-exprs» and (parameter-values «v» ...) 
correspond to the values of each of these parameters, which can 
either be numbers or symbols.

A set of examples to run can be found in tests.rkt.

|#