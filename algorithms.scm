(define mini/pi
  (* 4 (atan 1)))


(define (mini/norm2 x)
  (sqrt (apply + (map (lambda (xi) (expt xi 2)) x))))


(define (mini/box-muller-transform mu sigma u1 u2)
  (let ((magnitude (* sigma (sqrt (* -2 (log u1))))))
    (list
     (+ (* magnitude (cos (* 2 mini/pi u2))) mu)
     (+ (* magnitude (sin (* 2 mini/pi u2))) mu))))


(define (mini/make-rng-uniform seed)
  (let ((a 25214903917)
	(c 11)
	(m (expt 2 48))
	(x seed))
    (lambda ()
      (begin
	(set! x (mod (+ (* a x) c) m))
	(inexact (/ x m))))))


(define (mini/make-rng-normal seed mu sigma)
  (let ((rng (mini/make-rng-uniform seed)) (z '()))
    (lambda ()
      (begin
	(when (null? z)
	  (set! z (mini/box-muller-transform mu sigma (rng) (rng))))
	(let ((z0 (car z)) (z1 (cdr z)))
	  (begin (set! z z1) z0))))))


(define (mini/bounded-uniform-convolution rng v p v-min v-max)
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


(define (mini/solution S quality)
  (vector S quality))


(define (mini/solution->data x)
  (vector-ref x 0))


(define (mini/solution->quality x)
  (vector-ref x 1))


(define (mini/steepest-ascent-hill-climbing S tweak objective stop-criteria logger max-iterations n)
  (letrec
      ((candidate
	(lambda (S)
	  (mini/solution S (objective S))))
       (candidate-tweak
	(lambda (S)
	  (candidate (tweak (mini/solution->data S)))))
       (sample-gradient-loop
	(lambda (S R iterations)
	  (if (> iterations 0)
	      (let* ((W (candidate-tweak S)))
		(if (> (mini/solution->quality W) (mini/solution->quality R))
		    (sample-gradient-loop S W (- iterations 1))
		    (sample-gradient-loop S R (- iterations 1))))
	      R)))
       (optimization-loop
	(lambda (S iterations)
	  (if (and (> iterations 0) (stop-criteria))
	      (begin
		(logger iterations (mini/solution->data S) (mini/solution->quality S))
		(let* ((R (candidate-tweak S))
		       (R* (sample-gradient-loop S R n)))
		  (if (> (mini/solution->quality R*) (mini/solution->quality S))
		      (optimization-loop R* (- iterations 1))
		      (optimization-loop S (- iterations 1)))))
	      S))))
    (optimization-loop (candidate S) max-iterations)))


(define (mini/tabu-search S tweak objective stop-criteria logger max-iterations l n)
  (letrec
      ((candidate
	(lambda (S)
	  (mini/solution S (objective S))))
       (candidate-tweak
	(lambda (S)
	  (candidate (tweak (mini/solution->data S)))))
       (pick-best
	(lambda (a b)
	  (if (> (mini/solution->quality a) (mini/solution->quality b)) a b)))
       (is-element
	(lambda (x L epsilon)
	  (if (not (null? L))
	      (> epsilon
		 (apply min (map (lambda (Li) (mini/norm2 (map - x Li))) L))) #f)))
       (sample-gradient-loop
	(lambda (S R L iterations)
	  (if (> iterations 0)
	      (let* ((W (candidate-tweak S)))
		(if (and (is-element (mini/solution->data W) L 0.05)
			 (or (> (mini/solution->quality W) (mini/solution->quality R))
			     (is-element (mini/solution->data R) L 0.05)))
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
		      (logger iterations (mini/solution->data Best) (mini/solution->quality Best))
		      (if (not (is-element (mini/solution->data R) L 0.05))
			  (optimization-loop R* (pick-best Best R*) (append L (list (mini/solution->data R*))) (- iterations 1))
			  (optimization-loop S (pick-best Best S) (append L (list (mini/solution->data R))) (- iterations 1))))))
	      Best))))
    (let ((S (candidate S)))
      (optimization-loop S S '() max-iterations))))


;; (define (mini/genetic-algorithm sample fitness population-size)
;;   (letrec
;;       ((population
;; 	(map (lambda () (sample)) (mini/range 0 population-size)))
;;        (optimization-loop
;; 	(lambda (population best)
;; 	  (map fitness population))))))
