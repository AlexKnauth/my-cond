#lang racket/base

(provide for/cond-clause
         for*/cond-clause
         )

(require (for-syntax racket/base
                     syntax/parse
                     "cond-expander.rkt"
                     ))

(define failure-sym
  (gensym 'failure))

(define-syntax for/cond-clause
  (cond-expander
   (lambda (stx)
     (syntax-parse stx #:literals (for/cond-clause)
       [(my-cond (for/cond-clause ~! (for-clause ...) looped-cond-clause ...)
                 clause ...)
        (syntax/loc stx
          (let/cc return
            (for (for-clause ...)
              (define maybe-result
                (my-cond looped-cond-clause ...
                         [else failure-sym]))
              (if (not (eq? maybe-result failure-sym))
                  (return maybe-result)
                  #f))
            (my-cond clause ...)))]))))

(define-syntax for*/cond-clause
  (cond-expander
   (lambda (stx)
     (syntax-parse stx #:literals (for*/cond-clause)
       [(my-cond (for*/cond-clause ~! (for-clause ...) looped-cond-clause ...)
                 clause ...)
        (syntax/loc stx
          (let/cc return
            (for* (for-clause ...)
              (define maybe-result
                (my-cond looped-cond-clause ...
                         [else failure-sym]))
              (if (not (eq? maybe-result failure-sym))
                  (return maybe-result)
                  #f))
            (my-cond clause ...)))]))))
