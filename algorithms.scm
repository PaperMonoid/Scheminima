(load "sflow/sflow.scm")
(load "solution.scm")


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


(define (mini/steepest-ascent-hill-climbing solution tweak objective n)
  (define (candidate S) (mini/solution S (objective S)))
  (define (candidate-tweak S) (candidate (tweak (mini/solution->data S))))
  (define S (candidate solution))
  (define (sample-gradient S R iterations)
    (if (> iterations 0)
	(let* ((W (candidate-tweak S)))
	  (if (> (mini/solution->quality W) (mini/solution->quality R))
	      (sample-gradient S W (- iterations 1))
	      (sample-gradient S R (- iterations 1))))
	R))
  (define (optimization-step)
    (let* ((R (candidate-tweak S))
	   (R* (sample-gradient S R n)))
      (when (> (mini/solution->quality R*) (mini/solution->quality S))
	(set! S R*))
      S))
  (sflow/make-stream optimization-step))


(define (mini/tabu-search solution tweak objective l n)
  (define (candidate S) (mini/solution S (objective S)))
  (define (candidate-tweak S) (candidate (tweak (mini/solution->data S))))
  (define S (candidate solution))
  (define Best S)
  (define L '())
  (define (pick-best a b)
    (if (> (mini/solution->quality a) (mini/solution->quality b)) a b))
  (define (is-element x L epsilon)
    (if (not (null? L))
	(> epsilon
	   (apply min (map (lambda (Li) (mini/norm2 (map - x Li))) L))) #f))
  (define (sample-gradient S R L iterations)
    (if (> iterations 0)
	(let* ((W (candidate-tweak S)))
	  (if (and (is-element (mini/solution->data W) L 0.05)
		   (or (> (mini/solution->quality W) (mini/solution->quality R))
		       (is-element (mini/solution->data R) L 0.05)))
	      (sample-gradient S W L (- iterations 1))
	      R))
	R))
  (define (optimization-step)
    (if (> (length L) l)
	(set! L (cdr L))
	(let* ((R (candidate-tweak S))
	       (R* (sample-gradient S R L n)))
	  (if (not (is-element (mini/solution->data R) L 0.05))
	      (begin
		(set! S R*)
		(set! Best (pick-best Best R*))
		(set! L (append L (list (mini/solution->data R*)))))
	      (begin
		(set! Best (pick-best Best S))
		(set! L (append L (list (mini/solution->data R))))))))
    Best)
  (sflow/make-stream optimization-step))
