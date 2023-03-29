(define opt/pi
  (* 4 (atan 1)))


(define (opt/norm2 x)
  (sqrt (apply + (map (lambda (xi) (expt xi 2)) x))))


(define (opt/box-muller-transform mu sigma u1 u2)
  (let ((magnitude (* sigma (sqrt (* -2 (log u1))))))
    (list
     (+ (* magnitude (cos (* 2 opt/pi u2))) mu)
     (+ (* magnitude (sin (* 2 opt/pi u2))) mu))))


(define (opt/make-rng-uniform seed)
  (let ((a 25214903917)
	(c 11)
	(m (expt 2 48))
	(x seed))
    (lambda ()
      (begin
	(set! x (mod (+ (* a x) c) m))
	(inexact (/ x m))))))


(define (opt/make-rng-normal seed mu sigma)
  (let ((rng (opt/make-rng-uniform seed)) (z '()))
    (lambda ()
      (begin
	(when (null? z)
	  (set! z (opt/box-muller-transform mu sigma (rng) (rng))))
	(let ((z0 (car z)) (z1 (cdr z)))
	  (begin (set! z z1) z0))))))


(define (opt/bounded-uniform-convolution rng v p v-min v-max)
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


(define (opt/solution S quality)
  (vector S quality))


(define (opt/solution->data x)
  (vector-ref x 0))


(define (opt/solution->quality x)
  (vector-ref x 1))


(define (opt/steepest-ascent-hill-climbing S tweak objective stop-criteria logger max-iterations n)
  (letrec
      ((candidate
	(lambda (S)
	  (opt/solution S (objective S))))
       (candidate-tweak
	(lambda (S)
	  (candidate (tweak (opt/solution->data S)))))
       (sample-gradient-loop
	(lambda (S R iterations)
	  (if (> iterations 0)
	      (let* ((W (candidate-tweak S)))
		(if (> (opt/solution->quality W) (opt/solution->quality R))
		    (sample-gradient-loop S W (- iterations 1))
		    (sample-gradient-loop S R (- iterations 1))))
	      R)))
       (optimization-loop
	(lambda (S iterations)
	  (if (and (> iterations 0) (stop-criteria))
	      (begin
		(logger iterations (opt/solution->data S) (opt/solution->quality S))
		(let* ((R (candidate-tweak S))
		       (R* (sample-gradient-loop S R n)))
		  (if (> (opt/solution->quality R*) (opt/solution->quality S))
		      (optimization-loop R* (- iterations 1))
		      (optimization-loop S (- iterations 1)))))
	      S))))
    (optimization-loop (candidate S) max-iterations)))


(define (opt/tabu-search S tweak objective stop-criteria logger max-iterations l n)
  (letrec
      ((candidate
	(lambda (S)
	  (opt/solution S (objective S))))
       (candidate-tweak
	(lambda (S)
	  (candidate (tweak (opt/solution->data S)))))
       (pick-best
	(lambda (a b)
	  (if (> (opt/solution->quality a) (opt/solution->quality b)) a b)))
       (is-element
	(lambda (x L epsilon)
	  (if (not (null? L))
	      (> epsilon
		 (apply min (map (lambda (Li) (opt/norm2 (map - x Li))) L))) #f)))
       (sample-gradient-loop
	(lambda (S R L iterations)
	  (if (> iterations 0)
	      (let* ((W (candidate-tweak S)))
		(if (and (is-element (opt/solution->data W) L 0.05)
			 (or (> (opt/solution->quality W) (opt/solution->quality R))
			     (is-element (opt/solution->data R) L 0.05)))
		    (sample-gradient-loop S W L (- iterations 1))
		    R))
	      R)))
       (optimization-loop
	(lambda (S Best L iterations)
	  (if (and (> iterations 0) (stop-criteria))
	      (if (> (length L) l)
		  (optimization-loop S Best (cdr L) iterations)
		  (let* ((R (candidate-tweak S))
			 (R* (sample-gradient-loop S R L n)))
		    (begin
		      (logger iterations (opt/solution->data Best) (opt/solution->quality Best))
		      (if (not (is-element (opt/solution->data R) L 0.05))
			  (optimization-loop R* (pick-best Best R*) (append L (list (opt/solution->data R*))) (- iterations 1))
			  (optimization-loop S (pick-best Best S) (append L (list (opt/solution->data R))) (- iterations 1))))))
	      Best))))
    (let ((S (candidate S)))
      (optimization-loop S S '() max-iterations))))


;; (define (opt/genetic-algorithm sample fitness population-size)
;;   (letrec
;;       ((population
;; 	(map (lambda () (sample)) (opt/range 0 population-size)))
;;        (optimization-loop
;; 	(lambda (population best)
;; 	  (map fitness population))))))
