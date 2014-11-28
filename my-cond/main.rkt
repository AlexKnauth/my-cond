#lang racket/base
(require "my-cond.rkt"
         "for-cond-clause.rkt"
         (for-syntax (only-in "cond-expander.rkt"
                              prop:cond-expander
                              cond-expander
                              cond-expander?
                              syntax-local-cond-introduce
                              )))
(provide (all-from-out
          "my-cond.rkt"
          "for-cond-clause.rkt")
         (for-syntax (all-from-out
                      "cond-expander.rkt"
                      )))

(module+ test
  (require rackunit))

(module+ test
  (check-equal? (my-cond) (cond))
  (let ([test-sym (gensym 'test-sym)])
    (check-equal? (my-cond [#true test-sym]) test-sym)
    (check-equal? (my-cond [else test-sym]) test-sym)
    
    (check-equal? (my-cond #:defs [(define b #t) (define x test-sym)]
                           [b x])
                  test-sym)
    (check-equal? (my-cond #:let ([b 5])
                           #:let ([b #t] [x (if (= b 5) test-sym #f)])
                           [b x])
                  test-sym)
    (check-equal? (my-cond #:let* ([b 5] [b (if b #t #f)])
                           [else b])
                  #t)
    (check-equal? (my-cond #:letrec ([f (λ () g)] [g (λ () f)])
                           [else #t])
                  #t)
    
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
  (check-equal? (my-cond #:defs [(define b #f)]
                         (for/cond-clause ([i (in-range 0 5)])
                           [b i])
                         [(not b) "this-thing"])
                "this-thing")
  
  )