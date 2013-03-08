#lang racket
#|
    Symbolic execution.
    
    The most naive way of testing a program is to provide concrete inputs
    to the program and observing its behavior and output.

    Is there a better way than this? Yes!

    Idea, mid 70's: Instead of providing concrete inputs, why not provide
                    symbols representing arbitrary values? 
   
    This idea is known as symbolic execution.

    Symbolic execution follows the same line-by-line execution of normal
    execution, but with a few modifications:
    
    - Values in the program may be symbolic formulas over the input
      symbols. For example:

        input y = α
        int x = y*y + 1
   
        The value of x is thus α² + 1.

    - With each line executed we associate an object containing:
          - The next instruction to be executed (if there isn't any, END
            is the value), i.e. the program counter.
          - The state, this is, the current assignments to symbolic values
            and variables.
          - The path condition: an accumulator of properties which the
            inputs must satisfy for the execution to reach the current 
            location. A conjunction of conditions. 
    
    How is this path condition calculated?
        1) It is initialized to 'true'.
        2) Nothing happens until we arrive at a branch ('if' 
           'unless' 'while' 'until' «condition»). At this point, we 
           'fork' the current path condition, resulting in two new 
            path conditions, and hence two more objects:
               - True branch: new-pc := current-pc ∧ «condition»
               - False branch: new-pc := current-pc ∧ ¬(«condition»)
           
            This is not completely blind, however. If the pc is false,
            we halt execution, i.e. the path will never occur in a 
            normal execution. A computer can detect by using
            decision procedures!
   
    From this description it is natural to think of symbolic execution as
    a tree.

    This is a lot to chew. Let's see a few examples, shall we?

|#

(require "symbolic-model.rkt")

#| Program in pseudo-C:
      int x,y;
      if (x > y){
          x = x + y;
          y = x - y;
          x = x - y;
          if (x > y)
              assert(false);
      }
|#
(symbolic-execution-tree (parameters x y) (parameter-values 'X 'Y)  
                         (body

                          (unless (>= y x)
                            (set! x (+ x y))
                            (set! y (- x y))
                            (set! x (- x y))
                            (unless (> y x)
                              (ERROR)))))

#| Program in pseudo-C:

       int multiply(x,y)
       {
            z := 0;
            while (x > 0) {
                x := x - 1;
                z := z + y;
            }
            if (z != x * y){
                ERROR;
            }
            return z;
       }

       w := multiply(2, Y);
|#
#;(symbolic-execution-tree (parameters x y) (parameter-values 2 'Y) 
                         (body
                          (define z 0)
                          (until (<= x 0)
                                 (set! x (- x 1))
                                 (set! z (+ z y)))
                          (unless (= z (* x y))
                            (ERROR))
                          z))

#| Great! We can see all paths a program will take, and notice
   interesting things about the program by just observing the leaves
   of the tree, and using a constraint solver over these leaves.

   Essentially, symbolic execution is a static analysis tool, since
   we really do not **run** the code.

   As we can observe, symbolic execution aims at path coverage of
   the source code. 

   All honky dory until now, but symbolic execution does have some
   major negatives:

       - Function calls: In the above example, let's suppose we didn't
         access to the code of multiply. What do we do?!

       - Termination: What if the exit condition of a loop depended on
                      one of the symbolic inputs? We will end up with
                      an infinite tree!

       - Aliasing: A problem mainly present with arrays + structs in symbolic
                   execution. [Still a bit fuzzy on this point.]
  |#