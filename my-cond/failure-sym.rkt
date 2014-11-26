#lang racket/base

(provide failure-sym
         failure-sym?
         )

(define failure-sym
  (gensym 'failure))

(define (failure-sym? x)
  (eq? x failure-sym))

