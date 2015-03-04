#lang scribble/manual

@(require (for-label my-cond my-cond/iffy (rename-in racket/base [if rkt:if])))

@title{my-cond/iffy with sweet-exp}

@defmodule[my-cond/iffy]

@codeblock{
#lang sweet-exp racket
require my-cond/iffy
my-cond
  if condition-expr
    body-expr
    ...
  else-if condition-expr
    body-expr
    ...
  ...
  else
    body-expr
    ...

}

The @racketmodname[my-cond/iffy] module provides @racket[my-cond], @racket[if],
@racket[else-if], and @racket[else], which work together to allow if expressions
like the above. 

@defidform[if]{
Defined as a @racket[cond-expander] to work with @racket[my-cond] (see above).
When not used within a @racket[my-cond] expression, acts like normal @racket[rkt:if].
}

@defidform[else-if]{
Defined as a @racket[cond-expander] to work with @racket[my-cond] (see above).
}

