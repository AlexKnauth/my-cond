#lang racket/base

(provide my-cond
         cond/local-def
         )

(require racket/local
         (prefix-in tr: typed/racket/base)
         (for-syntax racket/base
                     racket/local
                     syntax/parse
                     racket/syntax
                     (only-in typed/untyped-utils syntax-local-typed-context?)
                     "cond-expander.rkt"
                     ))


(begin-for-syntax
  (define-syntax-class cond-exp
    [pattern id:id
             #:attr exp (syntax-local-value #'id (λ () #f))
             #:when (cond-expander? (attribute exp))
             #:attr proc (cond-expander-proc (attribute exp))])
  (define (t/u id)
    (cond [(syntax-local-typed-context?)
           (format-id id "tr:~a" id #:source id)]
          [else id])))

(define-syntax my-cond
  (lambda (stx)
    (syntax-parse stx #:literals (else)
      [(my-cond)
       (syntax/loc stx (cond))]
      [(my-cond [id:cond-exp . stuff] clause ...)
       (let* ([proc (attribute id.proc)]
              [intro (make-syntax-introducer)]
              [form (intro (syntax-local-introduce stx))]
              [new-form
               (parameterize ([current-cond-introducer intro])
                 (proc form))])
         (syntax-property
          (syntax-local-introduce (intro new-form))
          'disappeared-use (list (syntax-local-introduce #'id))))]
      [(my-cond [else ~! body:expr ...+])
       (syntax/loc stx (cond [else body ...]))]
      [(my-cond #:defs ~! [def ...] clause ...)
       (syntax/loc stx (local [def ...] (my-cond clause ...)))]
      [(my-cond #:let ~! (binding ...) clause ...)
       #:with let-id (t/u #'let)
       (syntax/loc stx (let-id (binding ...) (my-cond clause ...)))]
      [(my-cond #:begin [expr:expr ...] clause ...)
       (syntax/loc stx (begin expr ... (my-cond clause ...)))]
      [(my-cond clause1
                clause ...)
       (syntax/loc stx
         (cond clause1
               [else
                (my-cond clause ...)]))]
      )))

(define-syntax cond/local-def
  (lambda (stx)
    (syntax-parse stx
      [(cond/local) #'(cond)]
      [(cond/local #:defs [def ...] clause ...)
       #'(local [def ...]
           (cond/local clause ...))]
      [(cond/local #:local [def ...] clause ...)
       #'(local [def ...]
           (cond/local clause ...))]
      [(cond/local #:let (binding ...) clause ...)
       #:with let-id (t/u #'let)
       #'(let-id (binding ...)
           (cond/local clause ...))]
      [(cond/local #:let* (binding ...) clause ...)
       #:with let*-id (t/u #'let*)
       #'(let*-id (binding ...)
           (cond/local clause ...))]
      [(cond/local #:letrec (binding ...) clause ...)
       #:with letrec-id (t/u #'letrec)
       #'(letrec-id (binding ...)
           (cond/local clause ...))]
      [(cond/local #:letrec-syntaxes+values (trans-binding  ...) (binding ...)
                   clause ...)
       #:with letrec-syntaxes+values-id (t/u #'letrec-syntaxes+values)
       #'(letrec-syntaxes+values-id (trans-binding ...) (binding ...)
           (cond/local clause ...))]
      [(cond/local #:parameterize ([parameter val] ...) clause ...)
       #:with parameterize-id (t/u #'parameterize)
       #'(parameterize-id ([parameter val] ...)
           (cond/local clause ...))]
      [(cond/local #:with-handlers ([pred handler] ...) clause ...)
       #:with with-handlers-id (t/u #'with-handlers)
       #'(with-handlers-id ([pred handler] ...)
           (cond/local clause ...))]
      [(cond/local kw:keyword stuff clause ...)
       (with-syntax ([let-id (datum->syntax stx (string->symbol (keyword->string (syntax->datum #'kw))))])
         #'(let-id stuff
                   (cond/local clause ...)))]
      [(cond/local clause)
       #'(cond clause)]
      [(cond/local clause0 clause ...)
       #'(cond clause0
               [else
                (cond/local clause ...)])]
      )))


