(define (hypotenuse a b)
  (sqrt (+ (* a a) (* b b)))
)

(displayln (hypotenuse 3 4))

(define (if-test x)
  (if (> x 0)
      (displayln "positive")
      (displayln "non-positive")
  )
)

(if-test 3)
(if-test -3)

(define (cond-test x)
  (cond ((> x 0) (displayln "positive"))
        ((== x 0) (displayln "zero"))
        ((< x 0) (displayln "non-positive"))
  )
)

(cond-test 1)
(cond-test 0)
(cond-test -1)

(displayln ((lambda (x) (+ x 1)) 2))