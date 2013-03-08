#lang racket

(require "syntax-model.rkt")
(provide symbolic-execution-tree)

#| Graphical representation of the tree resulting from
   the symbolic execution of s-expression 'function' with 
   zero or more 'input's. 
   'function' must be an s-expression denoting a λ-function. |#

(define-syntax-rule 
  (symbolic-execution-tree (parameters «p» ...) (parameter-values «v» ...) (body «body-expr» ...))
  (syntax-model (symbolic-call (quote (λ («p» ...) «body-expr» ...)) «v» ...)))

#| Tree resulting from the symbolic execution of λ-function 
  'function' with list of inputs 'input'|#
(define (symbolic-call function . input)
  (match function
    [`(λ ,«args» . ,«body-exprs») 
     (let ([root  
            (Statement (first «body-exprs») 
                       (make-immutable-hash 
                        (map cons «args» input)) (list #t))]) 
       (symbolic-interpret root (rest «body-exprs»)))])) 

#| A list of lists of Statements corresponding to the tree resulting
   from the symbolic execution of s-expressions 'body-exprs' starting 
   with Statement 'root'. |#
(define (symbolic-interpret root body-exprs)
  #| Whether the Statement-pc of the first element of stmt-expr-pair 
     contains only non-false values. |#
  (define (consistent-pc? stmt-expr-pair) (andmap 
                                           (λ (e)
                                             (match e
                                               [(or (list 'not #t) #f) #f]
                                               [else #t])) (Statement-pc (car stmt-expr-pair))))
  (let*-values ([(leaves not-leaves) 
                 (partition (λ (p) 
                              (equal? (Statement-instruction-to-execute (car p)) 'END)) 
                            (execute-statement root body-exprs))]
                [(not-dead-ends dead-ends)
                 (partition consistent-pc? not-leaves)])
    (cons root (append (map (λ (p) 
                              (symbolic-interpret (car p) (cdr p))) 
                            not-dead-ends) (map first dead-ends) (map first leaves)))))

#| A list of pairs. Each pair consists of a Statement and a list of s-expressions
   that will follow the execution of "instruction-to-execute" of that Statement.
   Each pair results from the *execution* of "instruction-to-execute" of Statement 
   'stmt', and from the list of s-expressions that follow the execution of 'stmt',
   'exprs-to-evaluate'. 
  You can think of this as the *children* of Statement 'stmt'. |#
(define (execute-statement stmt exprs-to-evaluate)
  (define-values (next-ins next-exprs-to-evaluate) 
    (if (empty? exprs-to-evaluate) 
        (values 'END '())
        (values (first exprs-to-evaluate) (rest exprs-to-evaluate))))
  (define instruction-to-execute (Statement-instruction-to-execute stmt))
  (define state (Statement-state stmt))
  (define pc (Statement-pc stmt))
  (match instruction-to-execute
    [(or `(define ,id ,expr) 
         `(set! ,id ,expr))
     (let ([updated-state 
            (hash-set state id (evaluate-symbolic expr stmt))])
       (list  (cons (Statement next-ins
                               updated-state 
                               pc)
                    next-exprs-to-evaluate)))]
    [`(block . ,block-body-exprs) (list (cons (Statement (first block-body-exprs)
                                                         state
                                                         pc)
                                              (append (rest block-body-exprs) exprs-to-evaluate)))]
    [`(unless ,condition 
        . ,unless-body-exprs)  
     (let ([evaluated-condition (evaluate-symbolic condition stmt)])
       (list (cons (Statement (first unless-body-exprs)
                              state
                              (cons (list 'not evaluated-condition) pc))
                   (append (rest unless-body-exprs) exprs-to-evaluate))
             (cons (Statement next-ins
                              state
                              (cons evaluated-condition pc))
                   next-exprs-to-evaluate)))]
    [`(until ,condition 
             . ,until-body-exprs)  
     (let ([state (Statement-state stmt)]
           [pc (Statement-pc stmt)])
       (if (evaluate-symbolic condition stmt)
           (execute-statement  (Statement (append 
                                           (list 'unless condition)
                                           until-body-exprs) 
                                          state pc) exprs-to-evaluate)
           (execute-statement (Statement (append 
                                          (list 'unless condition)
                                          until-body-exprs
                                          (list (append
                                                 (list 'until condition)
                                                 until-body-exprs)))
                                         state pc) exprs-to-evaluate)))]
    [`(if ,condition ,then-expr ,else-expr)
     (let ([evaluated-condition (evaluate-symbolic condition stmt)])
       (list (cons (Statement then-expr
                              state
                              (cons evaluated-condition pc))
                   exprs-to-evaluate)
             (cons (Statement else-expr
                              state
                              (cons (list 'not evaluated-condition) pc))
                   exprs-to-evaluate)))]
    ['(ERROR) (list (cons (Statement 'END state pc) '()))]
    [_ (list (cons (Statement next-ins state pc) next-exprs-to-evaluate))]))

#| The symbolic value of s-expression 'quoted-expr', as determined by the
   bindings in "Statement-state" of Statement 'current'. 
   Note that this value may be a concrete value in the case that 'quoted-expr'
   does not contain variables that |#
(define (evaluate-symbolic quoted-expr current)
  #| Whether s-expression 'quoted-expr' contains a symbolic value. |#
  (define (contains-symbolic-value? quoted-expr)
    (match quoted-expr
      [`(,n-ary-operator . ,operands) 
       (not (andmap (λ (e) (not (contains-symbolic-value? e))) operands))]
      [_ (symbol? quoted-expr)]))
  #| 'quoted-expr' with variables substituted with their value as determined
     by the bindings in "Statement-state" of Statement 'current'. |#
  (define (substitute-values quoted-expr current) 
    (match quoted-expr
      [`(,n-ary-operator . ,operands) 
       (cons n-ary-operator (map (λ (e) (evaluate-symbolic e current)) operands))]
      [_ (if (number? quoted-expr) quoted-expr 
             (hash-ref (Statement-state current) quoted-expr))]))
  #| The concrete value of s-expression 'quoted-expr'.|#
  (define (evaluate-concrete quoted-expr)
    (match quoted-expr
      [`(not ,condition) (not (evaluate-concrete condition))]
      [`(= . ,operands) 
       (apply = (map (λ (e) (evaluate-concrete e)) operands))]
      [`(> . ,operands) 
       (apply > (map (λ (e) (evaluate-concrete e)) operands))]
      [`(>= . ,operands) 
       (apply >= (map (λ (e) (evaluate-concrete e)) operands))]
      [`(< . ,operands) 
       (apply < (map (λ (e) (evaluate-concrete e)) operands))]
      [`(<= . ,operands) 
       (apply <= (map (λ (e) (evaluate-concrete e)) operands))]
      [`(+ . ,operands) 
       (apply + (map (λ (e) (evaluate-concrete e)) operands))]
      [`(- . ,operands) 
       (apply - (map (λ (e) (evaluate-concrete e)) operands))]
      [`(* . ,operands) 
       (apply * (map (λ (e) (evaluate-concrete e)) operands))]
      [`(/ . ,operands) 
       (apply / (map (λ (e) (evaluate-concrete e)) operands))]
      [_ quoted-expr]))
  
  (define substituted-expr (substitute-values quoted-expr current))
  (if (contains-symbolic-value? substituted-expr)
      substituted-expr
      (evaluate-concrete substituted-expr)))
