#lang racket/base
(require (only-in "cond-expander-prop.rkt" prop:cond-expander))
(provide cond-expander)

(struct cond-expander (proc) #:transparent
  #:property prop:cond-expander
  (λ (this) (cond-expander-proc this))) ; needs to be wrapped in (λ (this) (_ this))
