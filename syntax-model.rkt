#lang slideshow

#;(require "definitions.rkt")
(provide (struct-out Statement) syntax-model)

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

#| Program that repeatedly asks user for source code, displaying it
    as a tree based solely on grouping.
 
Compound components are represented by the first element circled,
 the remaining elements its children. There's no special handling
 of components where the first element isn't special, such as a
 sequence of parameters for 'lambda'/'λ' [which as mentioned in
 the lecture notes was why I put 'parameters' in 'make-function':
 uniformity and explicitness].
 
The syntax of Racket code will also be used for other structured data,
 so this "blind" diagramming will be generally useful. It might be an
 exercise later for you to add specialized handling for Racket code,
 and/or other domains.
 
Exercise for now: identify and understand every use of a language
 feature covered in "Quick" sections 1-9. |#


#| Definitions for the "look" of the tree.
   Helpers separated out from the general tree drawing algorithm. |#
(require slideshow/code)

(define (label token)
  (inset (apply vl-append 
                (map (λ (s) (text s 'modern)) 
                     (match token
                       [(Statement ins state pc) 
                        (append (list "Instruction to execute:")
                                (list (pretty-format ins 50))
                                (list "\n")
                                (list "State:")
                                (hash-map state (λ (f s) 
                                                  (string-append
                                                   (symbol->string f)
                                                   " = "
                                                   (cond [(list? s) (pretty-format s 50)]
                                                         [(number? s) (number->string s)]
                                                         [(symbol? s) (symbol->string s)]))))
                                (list "\n")
                                (list "Path constraints:")
                                (list (pretty-format pc 50)))]))) 10 0))

#;(define (label token) (inset (apply vl-append 
                                      (map (λ (s) (text s 'modern)) 
                                           (regexp-split "\n" (pretty-format token 25)))) 10 0))
(define (ellipsify picture)
  (cc-superimpose picture (rectangle (+ 15 (pict-width picture))
                                     (+ 20 (pict-height picture)))))
; Combiners.
(define (layout-siblings siblings) (apply ht-append 20 siblings))
(define (layout-parent-children parent children) (vc-append 50 parent children))
(define (join! tree parent child)
  (pin-line tree
            parent cb-find
            child ct-find
            #:end-pull 1/10
            #:end-angle (- (/ pi 2))))

#| Picture of the tree of source code 's', where 's' represents code
    as a list of lists of .... |#
(define (syntax-model s)
  (cond [(empty? s) (ellipsify (label '||))]
        [(list? s) (let* ([root (ellipsify (label (first s)))]
                          [children (map syntax-model (rest s))]
                          [arranged (layout-parent-children
                                     root (layout-siblings children))])
                     (for/fold ([tree arranged])
                       ((child children))
                       (join! tree root child)))]
        [else (ellipsify (label s))]))

#| Main. |#
(define (read-draw-forever)
  (displayln
   "Please enter some source code into the box, or click EOF to end.")
  (define s (read))
  (unless (eof-object? s)
    (print (syntax-model s))
    (newline)
    (read-draw-forever)))
#;(read-draw-forever)