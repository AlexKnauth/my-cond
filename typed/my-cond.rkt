#lang typed/racket/base

(provide my-cond
         my-cond/defs
         my-cond/local
         my-cond/begin
         my-cond/let
         my-cond/let*
         my-cond/letrec
         my-cond/letrec-syntaxes+values
         my-cond/parameterize
         my-cond/with-handlers
         for/cond-clause
         for*/cond-clause
         cond/local-def
         )

(require "../my-cond/main.rkt"
         )

(module* test typed/racket/base
  (require (submod "..")
           typed/rackunit)
  (check-equal? (my-cond) (cond))
  (let ([test-sym (gensym 'test-sym)])
    (check-equal? (my-cond [#true test-sym]) test-sym)
    (check-equal? (my-cond [else test-sym]) test-sym)
    
    (check-equal? (my-cond #:defs [(define b : Boolean #t) (define x test-sym)]
                           [b x])
                  test-sym)
    (check-equal? (my-cond #:let ([b : 5 5])
                           #:let ([b #t] [x : Symbol (if (eq? b 5) test-sym '|#f|)])
                           [b x])
                  test-sym)
    (check-equal? (my-cond #:let* ([b 5] [b (if b #t #f)])
                           [else b])
                  #t)
    (check-equal? (my-cond #:letrec ([f : (-> False) (λ () g #f)] [g : (-> False) (λ () f #f)])
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
  
  (check-equal? (my-cond (for/cond-clause ([var (in-range 0 5)]) : (U Integer String)
                           [(<= 3 var) var]
                           [(<= 2 var) (number->string var)]))
                "2")
  (check-equal? (my-cond (for/cond-clause : (U Integer String) ([var (in-range 0 5)])
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
