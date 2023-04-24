(load "sflow/sflow.scm")
(load "solution.scm")

(define (non-uniform-mutation uniform-rng rng x)
  (let ((mutate-chance 0.50)
	(mutate-gene-chance 0.10))
    (if (< (uniform-rng) mutate-chance)
	(map
	 (lambda (xi)
	   (if (< (uniform-rng) mutate-gene-chance)
	       (+ xi (rng))
	       xi))
	 x)
	x)))


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
	    (if (> (mini/solution->quality individual)
		   (mini/solution->quality best-individual))
		individual
		best-individual)))
      (sflow/take k samples)))))


(define (mini/blend-crossover rng alpha x y)
  (let* ((u (rng))
	 (gamma (- (* (- 1 (* 2 alpha)) u) alpha)))
    (map (lambda (xi yi) (+ (* (- 1 gamma) xi) (* gamma yi))) x y)))


(define (mini/generate-population sample population-size)
  (sflow/stream->list
   (sflow/take
    population-size
    (sflow/make-stream sample))))


(define (mini/find-best-individual best-individual population)
  (sflow/foldl
   best-individual
   (lambda (best individual)
     (if (> (mini/solution->quality individual)
	    (mini/solution->quality best))
	 individual
	 best))
   (apply sflow/list->stream population)))


(define (mini/find-worst-individual best-individual population)
  (sflow/foldl
   best-individual
   (lambda (worst individual)
     (if (< (mini/solution->quality individual)
	    (mini/solution->quality best))
	 individual
	 worst))
   (sflow/list->stream population)))


(define (mini/genetic-algorithm sample fitness population-size uniform-rng normal-rng)
  (define (candidate S)
      (mini/solution S (fitness S)))
  (define (sample-candidate)
    (candidate (sample)))
  (define population
    (mini/generate-population sample-candidate population-size))
  (define best-individual
    (mini/find-best-individual (car population) population))
  (define (optimization-step)
    (define parents
      (sflow/make-stream
       (lambda ()
	 (list (mini/tournament uniform-rng population 5)
	       (mini/tournament uniform-rng population 5)))))
    (define children
      (sflow/map
       (lambda (xs)
	 (if (< (uniform-rng) 0.10)
	     (if (< (uniform-rng) 0.5)
		 (car xs)
		 (car (cdr xs)))
	     (mini/blend-crossover uniform-rng 0.5 (car xs) (car (cdr xs)))))
       parents))
    (define evaluated-children
      (sflow/map
       (lambda (x)
	 (candidate (non-uniform-mutation uniform-rng normal-rng x)))
       children))
    (set! best-individual
	  (mini/find-best-individual best-individual population))
    (set! population
	  (sflow/stream->list
	   (sflow/take population-size evaluated-children)))

    (let ((new-best-individual
	   (mini/find-best-individual best-individual population)))
      (if (> (mini/solution->quality best-individual)
	     (mini/solution->quality new-best-individual))
	  (let ((worst (mini/find-worst-individual best-individual population)))
	    (set! population (map (lambda (x) (if (eq? x worst) best-individual x)) population))
	    best-individual)
	  (begin
	    (set! best-individual new-best-individual)
	    best-individual))))
  (sflow/make-stream optimization-step))
