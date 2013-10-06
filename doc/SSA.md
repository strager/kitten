# Kitten SSA form

## By example

Naming conventions:

* `c#` is a lambda extracted to be top-level.
* `l#_x` is a local named `x` in the source code.
* `p#` is a function parameter.
* `s#` is a data stack variable.

Instructions:

* `act` creates an activation (lambda + locals closure) with
  the given top-level function and local values.
* `call` calls a function with zero or more arguments,
  yielding zero or more results.
* `copy` renames a value.  The original value and the new
  name are synonymous.
* `int` constructs a integer from a literal.
* `return` yields zero or more values to the caller and
  terminates execution of the current function.
* `string` constructs a string from a literal.
* `vector` constructs a vector from zero or more values.

Kitten source:

    \if 2 2 =:
      "true" say

SSA form:

    top:
      s0 <- int 2
      s1 <- int 2
      s2 <- call = s1 s0
      s3 <- act c0
      call if s3 s2
      return

    c0:
      s0 <- string "true"
      call say s0
      return

Kitten source:

    def map ([a] (a -> b +e) -> [b] +e):
      -> { xs f }
      \option xs head:
        f@ vector
        (xs tail) f map
        cat
      else:
        []

SSA form:

    map (p0 p1):
      s0 <- copy p0
      s1 <- copy p1
      l0_f <- copy s1
      l1_xs <- copy s0
      s2 <- copy l1_xs
      s3 <- call head s2
      s4 <- act c0 l0_f l1_xs
      s5 <- act c1 l0_f l1_xs
      s6 <- call option_else s5 s4 s3
      return s6

    c0 (p0) {l0_f l1_xs}:
      s0 <- copy p0
      s1 <- copy l0_f
      s2 <- call s1 s0
      s3 <- call vector s2
      s4 <- copy l1_xs
      s5 <- call tail s4
      s6 <- copy l0_f
      s7 <- call map s6 s5
      s8 <- call cat s7 s3
      return s8

    c1 {l0_f l1_xs}:
      s0 <- vector
      return s0

Kitten source:

    def dup (a -> a a):
      -> x
      x x

    def drop (a ->):
      -> x

    def swap (a b -> b a):
      -> { a b }
      b a

SSA form:

    dup (p0):
      s0 <- copy p0
      l0_x <- copy s0
      s1 <- copy l0_x
      s2 <- copy l0_x
      return s2 s1

    drop (p0):
      s0 <- copy p0
      l0_x <- copy s0
      return

    swap (p0 p1):
      s0 <- copy p0
      s1 <- copy p1
      l0_b <- copy s1
      l1_a <- copy s0
      s2 <- copy l0_b
      s3 <- copy l0_a
      return s3 s2

Optimization of `dup`, `drop`, and `swap` is trivial, as
`copy` is aliasing:

    dup (p0):
      return p0 p0

    drop (p0):
      return

    swap (p0 p1):
      return p1 p0
