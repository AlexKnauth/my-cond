#lang typed/racket/base
(require/typed/provide "../../my-cond/failure-sym.rkt"
                       [#:opaque Failure-Sym failure-sym?]
                       [failure-sym Failure-Sym])
