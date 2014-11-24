#lang racket/base
(provide prop:cond-expander
         cond-expander?
         cond-expander-proc
         current-cond-introducer
         syntax-local-cond-introduce
         )

(define-values (prop:cond-expander cond-expander? get-proc-getter)
  (make-struct-type-property 'cond-expander))

(define (cond-expander-proc cond-expander)
  (define get-proc (get-proc-getter cond-expander))
  (get-proc cond-expander))

(define current-cond-introducer
  (make-parameter
   (lambda (stx)
     (error 'syntax-local-cond-introduce "not expanding a my-cond clause"))))

(define (syntax-local-cond-introduce stx)
  ((current-cond-introducer) stx))
