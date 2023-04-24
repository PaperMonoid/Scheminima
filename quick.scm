(load "algorithms.scm")
(load "benchmarks.scm")
(load "genetic.scm")

(define rng (mini/make-rng-normal 3482 0 2))
(define uniform-rng (mini/make-rng-uniform 3482))
(define normal-rng (mini/make-rng-normal 3482 0 1))

(define (csv-logger filename)
  (begin
    (delete-file (format "./executions/~a.csv" filename))
    (lambda (iteration count S quality)
      (let ((port (open-output-file (format "./executions/~a.csv" filename) 'append)))
	(display (format "~d,~d,~s,~f\n" iteration count S quality) port)
	(close-port port)))))


(define (random-list s)
  (map (lambda (_) (max (min (rng) 5) -5)) (mini/range 0 s)))


(define (mini/objective function max-evaluation-count)
  (let ((evaluation-count 0))
    (list
     (lambda ()
       (< evaluation-count max-evaluation-count))
     (lambda (S)
       (begin
	 (set! evaluation-count (+ evaluation-count 1))
	 (function S)))
     (lambda () evaluation-count))))


(define (tweak x)
  (mini/bounded-uniform-convolution rng x 1.0 -5.0 5.0))


(define (zip-index stream)
  (define i 0)
  (define (zip-value value)
    (let ((j i))
      (set! i (+ i 1))
      (list i value)))
  (sflow/map zip-value stream))


(define (ga objective-function D)
  (let* ((objective (mini/objective objective-function 40000))
	 (stop-criteria (car objective))
	 (quality (car (cdr objective)))
	 (count (car (cdr (cdr objective)))))
    (zip-index
     (sflow/until
      (lambda (value) (not (stop-criteria)))
      (mini/genetic-algorithm
       (lambda () (random-list D))
       quality
       33
       uniform-rng
       normal-rng)))))


(let* ((D 10)
       (F mini/sphere-function)
       (S* (random-list D))
       (S (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "ga-sphere-n2-test2")))
  (letrec
      ((loop
	(lambda (i)
	  (when (< i 2)
	    (display "Solution: ") (display S*) (newline)
	    (display "Evaluated: ") (display (- (F S* S*))) (newline)
	    (sflow/stream->list
	     (sflow/map
	      (lambda (x)
		(let* ((Iteration i)
		       (Count (car x))
		       (Solution (car (cdr x)))
		       (Best (mini/solution->data Solution))
		       (Quality (mini/solution->quality Solution)))
		  (display Iteration) (display ", ")
		  (display Count) (display ", ")
		  (display Solution) (display ", ")
		  (display Quality)
		  (newline)))
	      (ga objective-function D)))
	    (loop (+ i 1))))))
    (loop 1)))
