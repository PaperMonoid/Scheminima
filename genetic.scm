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
;; done
(define (non-uniform-mutation uniform-rng rng x)
  (let ((mutate-chance 0.66)
	(mutate-gene-chance 0.33))
    (if (< (uniform-rng) mutate-chance)
	(map
	 (lambda (xi)
	   (if (< (uniform-rng) mutate-gene-chance)
	       (+ xi (rng))
	       xi))
	 x)
	x)))


;; add elitism (survive best parent/child keep best)
;; add crossover probability (allow clonning) ;; done
;; tournament per parent ;; done
(define (mini/tournament rng population k)
  (let* ((N (length population))
	 (samples
	  (sflow/make-stream
	   (lambda ()
	     (list-ref population (inexact->exact (round (* (rng) (- N 1)))))))))
    (mini/solution->data
     (sflow/foldl
      '()
      (lambda (best-individual individual)
	(if (null? best-individual)
	    individual
	    (if (< (mini/solution->quality individual)
		   (mini/solution->quality best-individual))
		individual
		best-individual)))
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
  (define best-individual (car population))
  (define (find-best)
    (sflow/foldl
     best-individual
     (lambda (best individual)
       (if (> (mini/solution->quality individual)
	      (mini/solution->quality best))
	   individual
	   best))
     (apply sflow/list->stream population)))
  (define (find-worst)
    (sflow/foldl
     best-individual
     (lambda (worst individual)
       (if (< (mini/solution->quality individual)
	      (mini/solution->quality best))
	   individual
	   worst))
     (apply sflow/list->stream population)))
  (define (optimization-step)
    (define parents
      (sflow/make-stream
       (lambda ()
	 (list (mini/tournament uniform-rng population 5)
	       (mini/tournament uniform-rng population 5)))))
    (define children
      (sflow/map
       (lambda (xs)
	 (mini/blend-crossover uniform-rng 0.5 (car xs) (car (cdr xs))))
       parents))
    (define evaluated-children
      (sflow/map
       (lambda (x)
	 (candidate (non-uniform-mutation uniform-rng rng x)))
       children))

    (set! best-individual (find-best))
    (set! population
	  (sflow/stream->list
	   (sflow/take population-size evaluated-children)))

    ;; elitism
    (let ((new-best-individual (find-best)))
      (if (> (mini/solution->quality best-individual)
	     (mini/solution->quality new-best-individual))
	  (let ((worst (find-worst)))
	    (set! population (map (lambda (x) (if (eq? x worst) best-individual x)) population))
	    best-individual)
	  (begin
	    (set! best-individual new-best-individual)
	    best-individual))))

  (sflow/make-stream optimization-step))
