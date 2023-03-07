(define (norm2 x)
  (sqrt (apply + (map (lambda (xi) (expt xi 2)) x))))


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
	((in-range
	  (lambda (x-min x x-max)
	    (and (>= x x-min) (<= x x-max))))
	 (half-range-rng
	  (lambda ()
	    (- (rng) 0.5)))
	 (sample-r
	  (lambda (vi)
	    (let* ((n (half-range-rng))
		   (v* (+ vi n)))
	      (if (in-range v-min v* v-max) v* (sample-r vi))))))
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
	  (if (not (null? L))
	      (> epsilon
		 (apply min (map (lambda (Li) (norm2 (map - x Li))) L))) #f)))
       (sample-gradient
	(lambda (R L iterations)
	  (if (> iterations 0)
	      (let* ((W (tweak S)))
		(if (and (is-element W L 0.05)
			 (or (> (objective W) (objective R))
			     (is-element R L 0.05)))
		    (sample-gradient W L (- iterations 1))
		    R))
	      R)))
       (optimization-loop
	(lambda (S Best L iterations)
	  (if (> iterations 0)
	      (if (> (length L) l)
		  (optimization-loop S Best (cdr L) iterations)
		  (let* ((R (tweak S))
			 (R* (sample-gradient R L n)))
		    (begin
		      (logger iterations S (objective S))
		      (if (not (is-element R L 0.05))
			  (optimization-loop R* (pick-best Best R*) (append L (list R*)) (- iterations 1))
			  (optimization-loop S (pick-best Best S) (append L (list R)) (- iterations 1))))))
	      Best))))
    (optimization-loop S S '() max-iterations)))
