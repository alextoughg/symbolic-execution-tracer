#lang racket

(require "symbolic-model.rkt")

(symbolic-execution-tree (parameters x y) (parameter-values 2 'α₁) 
                         (body 
                          (unless (< x 3) 1 0)))

(symbolic-execution-tree (parameters x y) (parameter-values 'α₁ 'α₂) 
                         (body 
                          (block 
                           (define z 1)
                           (set! x 1)
                           (set! y 5))
                          (define f z)))

(symbolic-execution-tree (parameters x y) (parameter-values 2 'α₁) 
                         (body 
                          (until (> x 2) 
                                 (set! x (+ x 1)))))

(symbolic-execution-tree (parameters x y) (parameter-values 'α₁ 'α₂) 
                         (body 
                          (if (< y 3) 
                              (block
                               (define z 1)
                               (set! x (+ x (- y 1)))
                               (if (> x 1)
                                   (block
                                    (define w 2)
                                    (set! y w))
                                   (block
                                    (define j 5))))
                              (block 
                               (set! y 4)))
                          (define f 3)))


(symbolic-execution-tree (parameters x y) (parameter-values 4 'Y) 
                         (body 
                          (unless (< x 3) 
                            (block
                             (define z 1)
                             (set! x (- y 1))
                             (if (< y 2)
                                 (block 
                                  (set! x 1))
                                 (block
                                  (set! y 4)))))
                          (define w 3)))



(symbolic-execution-tree (parameters x y) (parameter-values 2 'Y) 
                         (body
                          (define z 0)
                          (until (<= x 0)
                                 (set! x (- x 1))
                                 (set! z (+ z y)))
                          (unless (= z (* x y))
                            (ERROR))
                          z))