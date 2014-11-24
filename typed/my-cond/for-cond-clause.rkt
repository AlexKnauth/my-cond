#lang typed/racket/base

(provide for/cond-clause
         for*/cond-clause
         )

(require (for-syntax racket/base
                     syntax/parse
                     "../../my-cond/cond-expander.rkt"
                     ))

(define-syntax define-failure-sym
  (lambda (stx)
    (syntax-parse stx
      [(define-failure-sym failure-sym:id)
       (with-syntax ([sym (gensym 'failure)])
         #'(begin
             (define-type failure-sym 'sym #:omit-define-syntaxes)
             (define failure-sym : failure-sym 'sym)))])))
(define-failure-sym failure-sym)

(define-syntax for/cond-clause
  (cond-expander
   (lambda (stx)
     (syntax-parse stx #:datum-literals (for/cond-clause) #:literals (:)
       [(my-cond (for/cond-clause ~!
                   (~or (~seq (for-clause ...)
                              (~optional (~seq : type) #:defaults ([type #'Any])))
                        (~seq (~optional (~seq : type) #:defaults ([type #'Any]))
                              (for-clause ...)))
                   looped-cond-clause ...)
                 clause ...)
        (syntax/loc stx
          (call/cc
           (lambda ([return : (type -> Nothing)])
             (for (for-clause ...)
               (: maybe-result : (U type failure-sym))
               (define maybe-result
                 (my-cond looped-cond-clause ...
                          [else failure-sym]))
               (if (not (eq? maybe-result failure-sym))
                   (return (ann maybe-result type))
                   #f))
             (my-cond clause ...))))]))))

(define-syntax for*/cond-clause
  (cond-expander
   (lambda (stx)
     (syntax-parse stx #:datum-literals (for/cond-clause) #:literals (:)
       [(my-cond (for*/cond-clause ~!
                   (~or (~seq (for-clause ...)
                              (~optional (~seq : type) #:defaults ([type #'Any])))
                        (~seq (~optional (~seq : type) #:defaults ([type #'Any]))
                              (for-clause ...)))
                   looped-cond-clause ...)
                 clause ...)
        (syntax/loc stx
          (call/cc
           (lambda ([return : (type -> Nothing)])
             (for* (for-clause ...)
               (: maybe-result : (U type failure-sym))
               (define maybe-result
                 (my-cond looped-cond-clause ...
                          [else failure-sym]))
               (if (not (eq? maybe-result failure-sym))
                   (return (ann maybe-result type))
                   #f))
             (my-cond clause ...))))]))))
