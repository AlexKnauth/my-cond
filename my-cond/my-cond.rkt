#lang racket/base

(provide my-cond
         cond/local-def
         )

(require racket/local
         (for-syntax racket/base
                     racket/local
                     syntax/parse
                     "cond-expander.rkt"
                     ))


(begin-for-syntax
  (define-syntax-class cond-exp
    [pattern id:id
             #:attr exp (syntax-local-value #'id (λ () #f))
             #:when (cond-expander? (attribute exp))
             #:attr proc (cond-expander-proc (attribute exp))]))

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
       (syntax/loc stx (let (binding ...) (my-cond clause ...)))]
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
       #'(let (binding ...)
           (cond/local clause ...))]
      [(cond/local #:let* (binding ...) clause ...)
       #'(let* (binding ...)
           (cond/local clause ...))]
      [(cond/local #:letrec (binding ...) clause ...)
       #'(letrec (binding ...)
           (cond/local clause ...))]
      [(cond/local #:letrec-syntaxes+values (trans-binding  ...) (binding ...)
                   clause ...)
       #'(letrec-syntaxes+values (trans-binding ...) (binding ...)
           (cond/local clause ...))]
      [(cond/local #:parameterize ([parameter val] ...) clause ...)
       #'(parameterize ([parameter val] ...)
           (cond/local clause ...))]
      [(cond/local #:with-handlers ([pred handler] ...) clause ...)
       #'(with-handlers ([pred handler] ...)
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


