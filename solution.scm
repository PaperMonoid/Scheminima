(define (mini/solution S quality)
  (vector S quality))


(define (mini/solution->data x)
  (vector-ref x 0))


(define (mini/solution->quality x)
  (vector-ref x 1))
