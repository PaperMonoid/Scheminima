(load "benchmarks.scm")
(load "algorithms.scm")


(define (csv-logger filename)
  (begin
    (delete-file (format "./executions/~a.csv" filename))
    (lambda (iteration count S quality)
      (let ((port (open-output-file (format "./executions/~a.csv" filename) 'append)))
	(display (format "~d,~d,~s,~f\n" iteration count S quality) port)
	(close-port port)))))


(define (log-plane filename)
  (let ((filepath (format "./planes/~a.csv" filename)))
    (delete-file filepath)
    (lambda (data)
      (let ((port (open-output-file filepath 'append)))
	(display (apply format  (cons "~s,~s,~s\n" data)) port)
	(close-port port)))))


;; DEFINE RNG
;;(define rng (opt/make-rng-uniform 3482))
;;(define rng (opt/make-rng-normal 3482 0 0.33))
(define rng (opt/make-rng-normal 3482 0 2))


(define (random-list s)
  (map (lambda (_) (max (min (rng) 5) -5)) (opt/range 0 s)))

;;(random-list 10)


(define (opt/objective function max-evaluation-count)
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
  (opt/bounded-uniform-convolution rng x 1.0 -5.0 5.0))


(define (evaluate-plane objective logger)
  (letrec
      ((loop-y
	(lambda (x y delta)
	  (when (<= y 5)
	    (begin
	      (logger (list x y (objective (list x y))))
	      (loop-y x (+ y delta) delta)))))
       (loop-x
	(lambda (x delta)
	  (when (<= x 5)
	    (begin
	      (loop-y x -5 delta)
	      (loop-x (+ x delta) delta))))))
    (loop-x -5 0.05)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((D 2)
       (F opt/sphere-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "steepest-ascent-hill-climbing-sphere-n2"))
       (plane-logger (log-plane "steepest-ascent-hill-climbing-sphere-n2-plane")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/steepest-ascent-hill-climbing S tweak quality stop-criteria log-function 30 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (begin
      (test-loop 10)
      (evaluate-plane objective-function plane-logger))))


(let* ((D 5)
       (F opt/sphere-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "steepest-ascent-hill-climbing-sphere-n5")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/steepest-ascent-hill-climbing S tweak quality stop-criteria log-function 30 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (test-loop 10)))


(let* ((D 10)
       (F opt/sphere-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "steepest-ascent-hill-climbing-sphere-n10")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/steepest-ascent-hill-climbing S tweak quality stop-criteria log-function 30 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (test-loop 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((D 2)
       (F opt/buche-rastringin-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "steepest-ascent-hill-climbing-buche-rastringin-n2"))
       (plane-logger (log-plane "steepest-ascent-hill-climbing-buche-rastringin-n2-plane")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/steepest-ascent-hill-climbing S tweak quality stop-criteria log-function 30 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (begin
      (test-loop 10)
      (evaluate-plane objective-function plane-logger))))


(let* ((D 5)
       (F opt/buche-rastringin-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "steepest-ascent-hill-climbing-buche-rastringin-n5")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/steepest-ascent-hill-climbing S tweak quality stop-criteria log-function 30 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (test-loop 10)))


(let* ((D 10)
       (F opt/buche-rastringin-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "steepest-ascent-hill-climbing-buche-rastringin-n10")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/steepest-ascent-hill-climbing S tweak quality stop-criteria log-function 30 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (test-loop 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((D 2)
       (F opt/rosenbrock-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "steepest-ascent-hill-climbing-rosenbrock-n2"))
       (plane-logger (log-plane "steepest-ascent-hill-climbing-rosenbrock-n2-plane")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/steepest-ascent-hill-climbing S tweak quality stop-criteria log-function 30 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (begin
      (test-loop 10)
      (evaluate-plane objective-function plane-logger))))


(let* ((D 5)
       (F opt/rosenbrock-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "steepest-ascent-hill-climbing-rosenbrock-n5")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/steepest-ascent-hill-climbing S tweak quality stop-criteria log-function 30 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (test-loop 10)))


(let* ((D 10)
       (F opt/rosenbrock-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "steepest-ascent-hill-climbing-rosenbrock-n10")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/steepest-ascent-hill-climbing S tweak quality stop-criteria log-function 30 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (test-loop 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((D 2)
       (F opt/sphere-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "tabu-search-sphere-n2"))
       (plane-logger (log-plane "tabu-search-sphere-n2-plane")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/tabu-search S tweak quality stop-criteria log-function 30 3 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (begin
      (test-loop 10)
      (evaluate-plane objective-function plane-logger))))


(let* ((D 5)
       (F opt/sphere-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "tabu-search-sphere-n5")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/tabu-search S tweak quality stop-criteria log-function 30 3 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (test-loop 10)))


(let* ((D 10)
       (F opt/sphere-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "tabu-search-sphere-n10")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/tabu-search S tweak quality stop-criteria log-function 30 3 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (test-loop 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((D 2)
       (F opt/buche-rastringin-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "tabu-search-buche-rastringin-n2"))
       (plane-logger (log-plane "tabu-search-buche-rastringin-n2-plane")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/tabu-search S tweak quality stop-criteria log-function 30 3 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (begin
      (test-loop 10)
      (evaluate-plane objective-function plane-logger))))


(let* ((D 5)
       (F opt/buche-rastringin-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "tabu-search-buche-rastringin-n5")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/tabu-search S tweak quality stop-criteria log-function 30 3 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (test-loop 10)))


(let* ((D 10)
       (F opt/buche-rastringin-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "tabu-search-buche-rastringin-n10")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/tabu-search S tweak quality stop-criteria log-function 30 3 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (test-loop 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((D 2)
       (F opt/rosenbrock-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "tabu-search-rosenbrock-n2"))
       (plane-logger (log-plane "tabu-search-rosenbrock-n2-plane")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/tabu-search S tweak quality stop-criteria log-function 30 3 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (begin
      (test-loop 10)
      (evaluate-plane objective-function plane-logger))))


(let* ((D 5)
       (F opt/rosenbrock-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "tabu-search-rosenbrock-n5")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/tabu-search S tweak quality stop-criteria log-function 30 3 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (test-loop 10)))


(let* ((D 10)
       (F opt/rosenbrock-function)
       (S* (random-list D))
       (objective-function (lambda (x) (- (F x S*))))
       (logger (csv-logger "tabu-search-rosenbrock-n10")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/tabu-search S tweak quality stop-criteria log-function 30 3 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (test-loop 10)))
