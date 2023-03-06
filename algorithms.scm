(define (norm2 x)
  (sqrt (map (lambda (xi) (expt xi 2)) x)))

(define (make-rng-uniform seed)
  (let ((a 25214903917)
	(c 11)
	(m (expt 2 48))
	(x seed))
    (lambda ()
      (begin
	(set! x (mod (+ (* a x) c) m))
	(inexact (/ x m))))))


(define (bounded-uniform-convolution rng v p v-min v-max)
  (let* ((P (map (lambda (vi) (rng)) v)))
    (letrec
	((sample-r
	  (lambda (vi)
	    (let* ((n (- (rng) 0.5))
		   (v* (+ vi n)))
	      (if (and (<= v* v-max) (>= v* v-min)) v* (sample-r vi))))))
      (map (lambda (vi pi) (if (>= p pi) (sample-r vi) vi)) v P))))


(define (tabu-search S tweak objective l n)
  (letrec
      ((is-element
	(lambda (x L epsilon)
	  (> epsilon
	     (min (map (lambda (Li) (norm2 (map - x Li))) L)))))
       (sample-gradient
	(lambda (N R L)
	  (if (> N 0)
	      (let* ((W (tweak S)))
		(if (and (is-element W L 0.05)
			 (or (> (objective W) (objective R))
			     (is-element R  L 0.05)))
		    (sample-gradient (- N 1) W L)))
	      R)))
       (optimization-loop
	(lambda (L)
	  (if (> (length L) l)
	      (optimization-loop (cdr L))
	      (let* ((R (tweak S)))
		(sample-gradient n R L))))))))

;;(bounded-uniform-convolution (make-rng-uniform 0) '(0.5 0.5 0.5) 1 0 1)
