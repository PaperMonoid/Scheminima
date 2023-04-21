(load "algorithms.scm")
(load "benchmarks.scm")
(load "genetic.scm")

(define uniform-rng (mini/make-rng-uniform 3482))
(define mutation-rng (mini/make-rng-normal 3482 0 0.05))
(define rng (mini/make-rng-normal 3482 0 2))

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


;; (let* ((D 2)
;;        (F mini/sphere-function)
;;        (S* (random-list D))
;;        (objective-function (lambda (x) (- (F x S*))))
;;        (S (random-list D)))
;;   (display "Solution: ") (display S*) (newline)
;;   (sflow/stream->list (sflow/take 10 (mini/steepest-ascent-hill-climbing S tweak objective-function 30))))

;; (let* ((D 2)
;;        (F mini/sphere-function)
;;        (S* (random-list D))
;;        (objective-function (lambda (x) (- (F x S*))))
;;        (S (random-list D)))
;;   (display "Solution: ") (display S*) (newline)
;;   (sflow/stream->list (sflow/take 50 (mini/tabu-search-v2 S tweak objective-function 30 3))))

(define (ga objective-function D)
  (let* ((objective (mini/objective objective-function 1000))
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
       mutation-rng)))))


(let* ((D 2)
       (F mini/sphere-function)
       (S* (random-list D))
       (S (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "ga-sphere-n2")))
  (letrec
      ((loop
	(lambda (i)
	  (when (< i 11)
	    (display "Solution: ") (display S*) (newline)
	    (sflow/stream->list
	     (sflow/map
	      (lambda (x)
		(let* ((Iteration i)
		       (Count (car x))
		       (Solution (car (cdr x)))
		       (Best (mini/solution->data Solution))
		       (Quality (mini/solution->quality Solution)))
		  (logger Iteration Count Solution Quality)))
	      (ga objective-function D)))
	    (loop (+ i 1))))))
    (loop 1)))


(let* ((D 5)
       (F mini/sphere-function)
       (S* (random-list D))
       (S (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "ga-sphere-n5")))
  (letrec
      ((loop
	(lambda (i)
	  (when (< i 11)
	    (display "Solution: ") (display S*) (newline)
	    (sflow/stream->list
	     (sflow/map
	      (lambda (x)
		(let* ((Iteration i)
		       (Count (car x))
		       (Solution (car (cdr x)))
		       (Best (mini/solution->data Solution))
		       (Quality (mini/solution->quality Solution)))
		  (logger Iteration Count Solution Quality)))
	      (ga objective-function D)))
	    (loop (+ i 1))))))
    (loop 1)))


(let* ((D 10)
       (F mini/sphere-function)
       (S* (random-list D))
       (S (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "ga-sphere-n10")))
  (letrec
      ((loop
	(lambda (i)
	  (when (< i 11)
	    (display "Solution: ") (display S*) (newline)
	    (sflow/stream->list
	     (sflow/map
	      (lambda (x)
		(let* ((Iteration i)
		       (Count (car x))
		       (Solution (car (cdr x)))
		       (Best (mini/solution->data Solution))
		       (Quality (mini/solution->quality Solution)))
		  (logger Iteration Count Solution Quality)))
	      (ga objective-function D)))
	    (loop (+ i 1))))))
    (loop 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((D 2)
       (F mini/buche-rastringin-function)
       (S* (random-list D))
       (S (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "ga-buche-rastringin-n2")))
  (letrec
      ((loop
	(lambda (i)
	  (when (< i 11)
	    (display "Solution: ") (display S*) (newline)
	    (sflow/stream->list
	     (sflow/map
	      (lambda (x)
		(let* ((Iteration i)
		       (Count (car x))
		       (Solution (car (cdr x)))
		       (Best (mini/solution->data Solution))
		       (Quality (mini/solution->quality Solution)))
		  (logger Iteration Count Solution Quality)))
	      (ga objective-function D)))
	    (loop (+ i 1))))))
    (loop 1)))


(let* ((D 5)
       (F mini/buche-rastringin-function)
       (S* (random-list D))
       (S (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "ga-buche-rastringin-n5")))
  (letrec
      ((loop
	(lambda (i)
	  (when (< i 11)
	    (display "Solution: ") (display S*) (newline)
	    (sflow/stream->list
	     (sflow/map
	      (lambda (x)
		(let* ((Iteration i)
		       (Count (car x))
		       (Solution (car (cdr x)))
		       (Best (mini/solution->data Solution))
		       (Quality (mini/solution->quality Solution)))
		  (logger Iteration Count Solution Quality)))
	      (ga objective-function D)))
	    (loop (+ i 1))))))
    (loop 1)))


(let* ((D 10)
       (F mini/buche-rastringin-function)
       (S* (random-list D))
       (S (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "ga-buche-rastringin-n10")))
  (letrec
      ((loop
	(lambda (i)
	  (when (< i 11)
	    (display "Solution: ") (display S*) (newline)
	    (sflow/stream->list
	     (sflow/map
	      (lambda (x)
		(let* ((Iteration i)
		       (Count (car x))
		       (Solution (car (cdr x)))
		       (Best (mini/solution->data Solution))
		       (Quality (mini/solution->quality Solution)))
		  (logger Iteration Count Solution Quality)))
	      (ga objective-function D)))
	    (loop (+ i 1))))))
    (loop 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((D 2)
       (F mini/rosenbrock-function)
       (S* (random-list D))
       (S (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "ga-rosenbrock-n2")))
  (letrec
      ((loop
	(lambda (i)
	  (when (< i 11)
	    (display "Solution: ") (display S*) (newline)
	    (sflow/stream->list
	     (sflow/map
	      (lambda (x)
		(let* ((Iteration i)
		       (Count (car x))
		       (Solution (car (cdr x)))
		       (Best (mini/solution->data Solution))
		       (Quality (mini/solution->quality Solution)))
		  (logger Iteration Count Solution Quality)))
	      (ga objective-function D)))
	    (loop (+ i 1))))))
    (loop 1)))


(let* ((D 5)
       (F mini/rosenbrock-function)
       (S* (random-list D))
       (S (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "ga-rosenbrock-n5")))
  (letrec
      ((loop
	(lambda (i)
	  (when (< i 11)
	    (display "Solution: ") (display S*) (newline)
	    (sflow/stream->list
	     (sflow/map
	      (lambda (x)
		(let* ((Iteration i)
		       (Count (car x))
		       (Solution (car (cdr x)))
		       (Best (mini/solution->data Solution))
		       (Quality (mini/solution->quality Solution)))
		  (logger Iteration Count Solution Quality)))
	      (ga objective-function D)))
	    (loop (+ i 1))))))
    (loop 1)))


(let* ((D 10)
       (F mini/rosenbrock-function)
       (S* (random-list D))
       (S (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "ga-rosenbrock-n10")))
  (letrec
      ((loop
	(lambda (i)
	  (when (< i 11)
	    (display "Solution: ") (display S*) (newline)
	    (sflow/stream->list
	     (sflow/map
	      (lambda (x)
		(let* ((Iteration i)
		       (Count (car x))
		       (Solution (car (cdr x)))
		       (Best (mini/solution->data Solution))
		       (Quality (mini/solution->quality Solution)))
		  (logger Iteration Count Solution Quality)))
	      (ga objective-function D)))
	    (loop (+ i 1))))))
    (loop 1)))


;; (let* ((D 10)
;;        (F mini/sphere-function)
;;        (S* (random-list D))
;;        (S (random-list D))
;;        (objective-function (lambda (x) (- (F x S*))))
;;        (objective (mini/objective objective-function 5000))
;;        (stop-criteria (car objective))
;;        (quality (car (cdr objective)))
;;        (count (car (cdr (cdr objective)))))
;;   (display "Solution: ") (display S*) (newline)
;;   (sflow/stream->list
;;    (zip-index
;;     (sflow/until
;;      (lambda (value) (not (stop-criteria)))
;;      (mini/genetic-algorithm
;;       (lambda ()
;; 	(random-list D))
;;       quality
;;       33
;;       uniform-rng
;;       mutation-rng)))))

  ;; (letrec
  ;;     ((test-loop
  ;; 	(lambda (n)
  ;; 	  (when (> n 0)
  ;; 	    (let* ((objective (mini/objective objective-function 1000))
  ;; 		   (stop-criteria (car objective))
  ;; 		   (quality (car (cdr objective)))
  ;; 		   (count (car (cdr (cdr objective))))
  ;; 		   (S (random-list D)))
  ;; 	      (begin
  ;; 		(display (format "Solving ~d for ~s..." n S*))
  ;; 		(mini/steepest-ascent-hill-climbing S tweak quality stop-criteria log-function 30 30)
  ;; 		(display " Done!")
  ;; 		(newline)
  ;; 		(test-loop (- n 1))))))))
  ;;   (begin
  ;;     (test-loop 10)
  ;;     (evaluate-plane objective-function plane-logger))))
