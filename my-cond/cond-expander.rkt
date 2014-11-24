#lang racket/base

(require racket/contract/base)
(provide (contract-out
          [prop:cond-expander
           (struct-type-property/c [cond-expander? . -> . (syntax? . -> . syntax?)])]
          [cond-expander [(syntax? . -> . syntax?) . -> . cond-expander?]]
          [cond-expander? [any/c . -> . boolean?]]
          [cond-expander-proc [cond-expander? . -> . (syntax? . -> . syntax?)]]
          [current-cond-introducer (parameter/c [syntax? . -> . syntax?])]
          [syntax-local-cond-introduce [syntax? . -> . syntax?]]
          ))

(require "cond-expander-prop.rkt"
         "cond-expander-struct.rkt"
         )

