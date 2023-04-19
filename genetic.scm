(load "sflow/sflow.scm")
(load "solution.scm")

;; (define (mini/bounded-uniform-convolution rng v p v-min v-max)
;;   (let* ((P (map (lambda (vi) (rng)) v)))
;;     (letrec
;; 	((in-range
;; 	  (lambda (x-min x x-max)
;; 	    (and (>= x x-min) (<= x x-max))))
;; 	 (half-range-rng
;; 	  (lambda ()
;; 	    (- (rng) 0.5)))
;; 	 (sample-r
;; 	  (lambda (vi)
;; 	    (let* ((n (half-range-rng))
;; 		   (v* (+ vi n)))
;; 	      (if (in-range v-min v* v-max) v* (sample-r vi))))))
;;       (map (lambda (vi pi) (if (>= p pi) (sample-r vi) vi)) v P))))


;; (define (non-uniform-mutation rng x)
;;   (mini/bounded-uniform-convolution rng x 1.0 -5.0 5.0))


;; maybe do high % to mutate, low % it per gene
(define (non-uniform-mutation rng x)
  (map (lambda (xi) (+ xi (rng))) x))


;; add elitism (survive best parent/child keep best)
;; add crossover probability (allow clonning)
;; tournament per parent
(define (mini/tournament rng population k)
  (let* ((N (length population))
	 (samples
	  (sflow/make-stream
	   (lambda ()
	     (list-ref population (inexact->exact (round (* (rng) (- N 1)))))))))
    (map
     mini/solution->data
     (sflow/foldl
      '()
      (lambda (parents individual)
	(if (null? parents)
	    (cons individual '())
	    (let ((x0 (car parents))
		  (x1 (cdr parents)))
	      (if (null? x1)
		  (if (> (mini/solution->quality individual) (mini/solution->quality x0))
		      (list individual x0)
		      (list x0 individual))
		  (if (> (mini/solution->quality individual) (mini/solution->quality x0))
		      (list individual x0)
		      (if (> (mini/solution->quality individual) (mini/solution->quality (car x1)))
			  (list x0 individual)
			  parents))))))
      (sflow/take k samples)))))


(define (mini/blend-crossover rng alpha x y)
  (let* ((u (rng))
	 (gamma (- (* (- 1 (* 2 alpha)) u) alpha)))
    (map (lambda (xi yi) (+ (* (- 1 gamma) xi) (* gamma yi))) x y)))


(define (mini/genetic-algorithm sample fitness population-size uniform-rng rng)
  (define (candidate S)
      (mini/solution S (fitness S)))
  (define (sample-candidate)
    (candidate (sample)))
  (define population
    (sflow/stream->list
     (sflow/take population-size
		 (sflow/make-stream sample-candidate))))
  (define (optimization-step)
    (define parents
      (sflow/make-stream
       (lambda ()
	 (mini/tournament uniform-rng population 10))))
    (define children
      (sflow/map
       (lambda (xs)
	 (mini/blend-crossover uniform-rng 0.5 (car xs) (car (cdr xs)))) parents))
    (define evaluated-children
      (sflow/map
       (lambda (x)
	 (candidate (non-uniform-mutation rng x)))
       children))
    (set! population
	  (sflow/stream->list
	   (sflow/take population-size evaluated-children)))
    (sflow/foldl
     (mini/solution '() -inf.0)
     (lambda (x y)
       (if (> (mini/solution->quality x) (mini/solution->quality y)) x y))
     (apply sflow/list->stream population)))
  (sflow/make-stream optimization-step))
