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


(define (steepest-ascent-hill-climbing S tweak objective logger max-iterations n)
  (letrec
      ((climb-loop
	(lambda (S R iterations)
	  (if (> iterations 0)
	      (let* ((W (tweak S)))
		(if (> (objective W) (objective R))
		    (climb-loop S W (- iterations 1))
		    (climb-loop S R (- iterations 1))))
	      R)))
       (optimization-loop
	(lambda (S iterations)
	  (if (> iterations 0)
	      (begin
		(logger iterations S (objective S))
		(let* ((R (tweak S))
		       (R* (climb-loop S R n)))
		  (if (> (objective R*) (objective S))
		      (optimization-loop R* (- iterations 1))
		      (optimization-loop S (- iterations 1)))))
	      S))))
    (optimization-loop S max-iterations)))


(define (tabu-search S tweak objective logger max-iterations l n)
  (letrec
      ((pick-best
	(lambda (a b)
	  (if (< (objective a) (objective b)) a b)))
       (is-element
	(lambda (x L epsilon)
	  (> epsilon
	     (min (map (lambda (Li) (norm2 (map - x Li))) L)))))
       (sample-gradient
	(lambda (iterations R L)
	  (if (> iterations 0)
	      (let* ((W (tweak S)))
		(if (and (is-element W L 0.05)
			 (or (> (objective W) (objective R))
			     (is-element R  L 0.05)))
		    (sample-gradient (- iterations 1) W L)))
	      R)))
       (optimization-loop
	(lambda (S Best L iterations)
	  (if (> iterations 0)
	      (begin
		(logger iterations S (objective S))
		(if (> (length L) l)
		    (optimization-loop S Best (cdr L) iterations)
		    (let* ((R (tweak S))
			   (R* (sample-gradient n R L)))
		      (if (not (is-element R L 0.05))
			  (optimization-loop R* (pick-best Best R*) (append R* L) (- iterations 1))
			  (optimization-loop S (pick-best Best S) (append R L) (- iterations 1))))))
	      Best))))
    (optimization-loop S S '() max-iterations)))

;;(bounded-uniform-convolution (make-rng-uniform 0) '(0.5 0.5 0.5) 1 0 1)
