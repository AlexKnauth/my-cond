#lang typed/racket/base

(provide my-cond
         cond/local-def
         for/cond-clause
         for*/cond-clause
         )

(require "../my-cond/my-cond.rkt"
         "my-cond/for-cond-clause.rkt"
         )

(module* test typed/racket/base
  (require (submod "..")
           typed/rackunit)
  (check-equal? (my-cond) (cond))
  (let ([test-sym (gensym 'test-sym)])
    (check-equal? (my-cond [#true test-sym]) test-sym)
    (check-equal? (my-cond [else test-sym]) test-sym)
    
    (check-equal? (my-cond #:defs [(define b #t) (define x test-sym)]
                           [b x])
                  test-sym)
    (check-equal? (my-cond #:let ([b #t] [x test-sym])
                           [b x])
                  test-sym)
    
    (check-equal? (my-cond (for/cond-clause ()
                             [#true test-sym]))
                  test-sym)
    (check-equal? (my-cond (for/cond-clause ([var (in-list '())])
                             [#true 'ntaaoeux]))
                  (void))
    
    (check-equal? (my-cond (for/cond-clause ([var (in-list '(#t))])
                            [var test-sym]))
                  test-sym)
    )
  
  (check-equal? (my-cond (for/cond-clause ([var (in-range 0 5)])
                           [(<= 3 var) var]
                           [(<= 2 var) (number->string var)]))
                "2")
  (check-equal? (my-cond (for/cond-clause ([var (in-range 0 5)])
                           [(<= 3 var) (define x var) x]
                           [(<= 2 var) (define x (number->string var)) x]))
                "2")
  (check-equal? (my-cond (for/cond-clause ([var (in-range 0 5)])
                           [(<= 3 var) (define x var) x]
                           [(<= 2 var) (define x (number->string var)) x]))
                "2")
  (check-equal? (my-cond #:defs [(define b : Boolean #f)]
                         (for/cond-clause ([i : Integer (in-range 0 5)]) : Integer
                           [b i])
                         [(not b) "this-thing"])
                "this-thing")
  
  )
