#lang racket


#| Symbolic Tree Model: Tree of Statements. |#
(struct Tree (#;field-names:
              parent
              [children #:mutable])
  #:transparent)

#| A Statement contains:
     String representation of the statement
     The current assingments to symbolic values and variables
     A path condition that must hold for the execution to reach
     that location (conditions on the input to reach that location)
  It also contains the preceding Statement and its children statements.|#
(struct Statement
  Tree ; super-type/class, i.e. has a Tree's fields and also:
  (#;field-names:
   state
   assignments
   pc)
  #:transparent)

#| A Tree of Statements corresponding to the symbolic execution of program.
   This will be used to print the tree afterwards. 
   Or should this receive the actual *call* to program? |#
(define (symbolic-interpret program))

#;
(define (test x y)
  (if (> x y)
      ))

#| Wait: how do we differentiate symbolic values from normal values?
 Do we use capital letters, i.e. Y?
 Or we could use a special character, λ or Γ, appended with a number generated
 with uid! ?
 (string->symbol (string-append "λ" (number->string uid)))
 The user most likely would like to choose which
 parameters are concrete and which are symbolic. Maybe we could identify
 a symbolic parameter through the tag symbolic: . For example:

 (test 1 (symbolic: y))

 or (test 1 'y)|#

#| Cases of symbolic interpret:
   (define «id» «expr»): Create a new Statement, link it back to its parent (current Statement)
                         with the same information except with for the field assignments, which
                         now contains the value «expr» bound to «id», and the description for "define".
   (set! «id» «expr»): If «id» is in current statement, create a new Statement with the same information
                       as the current statement except for the field assignments that now contains the value 
                       of «expr» bound to the «id», and the description for "set!"
   «literal» : Create a new Statement, with the same information as the current Statement with a new description,
               which is «literal».
   (zero? «expr»): Create a new Statement, with the same information as the current Statement with a new description,
               which is «literal».
   («prefix-nary-operator» . ,«exprs») :??????????????????
   |#