(load "benchmarks.scm")
(load "algorithms.scm")


;;(define (logger iteration count S quality)
;;  (display (format "~d\t~d\t~s\t~f \n" iteration count S quality)))


(define (csv-logger filename)
  (begin
    (delete-file (format "./executions/~a.csv" filename))
    (lambda (iteration count S quality)
      (let ((port (open-output-file (format "./executions/~a.csv" filename) 'append)))
	(display (format "~d,~d,~s,~f\n" iteration count S quality) port)
	(close-port port)))))


(define rng (opt/make-rng-uniform 3482))


(define (random-list s)
  (map (lambda (_) (rng)) (opt/range 0 s)))


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
  (opt/bounded-uniform-convolution rng x 1.0 0.0 1.0))


(let* ((D 2)
       (F opt/sphere-function)
       (S* (random-list D))
       (logger (csv-logger "steepest-ascent-hill-climbing-sphere-n2")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective-function (lambda (x) (- (F x S*))))
		   (objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function
		    (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/steepest-ascent-hill-climbing S tweak quality stop-criteria log-function 30 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (test-loop 10)))


(let* ((D 5)
       (F opt/sphere-function)
       (S* (random-list D))
       (logger (csv-logger "steepest-ascent-hill-climbing-sphere-n5")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective-function (lambda (x) (- (F x S*))))
		   (objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function
		    (lambda (iteration S quality) (logger n (count) S quality)))
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
       (logger (csv-logger "steepest-ascent-hill-climbing-sphere-n10")))
  (letrec
      ((test-loop
	(lambda (n)
	  (when (> n 0)
	    (let* ((objective-function (lambda (x) (- (F x S*))))
		   (objective (opt/objective objective-function 1000))
		   (stop-criteria (car objective))
		   (quality (car (cdr objective)))
		   (count (car (cdr (cdr objective))))
		   (log-function
		    (lambda (iteration S quality) (logger n (count) S quality)))
		   (S (random-list D)))
	      (begin
		(display (format "Solving ~d for ~s..." n S*))
		(opt/steepest-ascent-hill-climbing S tweak quality stop-criteria log-function 30 30)
		(display " Done!")
		(newline)
		(test-loop (- n 1))))))))
    (test-loop 10)))


;; (let ((D 2)
;;       (F opt/rosenbrock-function))
;;   (letrec
;;       ((test-loop
;; 	(lambda (n)
;; 	  (when (> n 0)
;; 	    (let* ((S* (random-list D))
;; 		   (objective-function (lambda (x) (- (F x S*))))
;; 		   (objective (opt/objective objective-function 1000))
;; 		   (stop-criteria (car objective))
;; 		   (quality (car (cdr objective)))
;; 		   (S (random-list D)))
;; 	      (begin
;; 		(display "------------------------------------- ")
;; 		(display S*)
;; 		(display " -------------------------------------")
;; 		(newline)
;; 		(opt/steepest-ascent-hill-climbing S tweak quality stop-criteria logger 30 30)
;; 		(test-loop (- n 1))))))))
;;     (test-loop 10)))


;; (let ((D 2)
;;       (F opt/buche-rastringin-function))
;;   (letrec
;;       ((test-loop
;; 	(lambda (n)
;; 	  (when (> n 0)
;; 	    (let* ((S* (random-list D))
;; 		   (objective-function (lambda (x) (- (F x S*))))
;; 		   (objective (opt/objective objective-function 1000))
;; 		   (stop-criteria (car objective))
;; 		   (quality (car (cdr objective)))
;; 		   (S (random-list D)))
;; 	      (begin
;; 		(display "------------------------------------- ")
;; 		(display S*)
;; 		(display " -------------------------------------")
;; 		(newline)
;; 		(opt/steepest-ascent-hill-climbing S tweak quality stop-criteria logger 30 30)
;; 		(test-loop (- n 1))))))))
;;     (test-loop 10)))


;; (define (tweak x) (opt/bounded-uniform-convolution rng x 1.0 -5.0 5.0))
;; (define (objective-2 x) (- (opt/linear-slope-function x '(-5 5))))
;; (opt/steepest-ascent-hill-climbing '(0.5 0.5) tweak objective-2 logger 30 30)


;; (define (objective-1 x) (- (opt/sphere-function x '(0.75 0.25))))
;; (define (tweak x) (opt/bounded-uniform-convolution rng x 1.0 0.0 1.0))
;; (opt/tabu-search '(0.5 0.5) tweak objective-1 logger 30 3 30)

;; (call-with-output-file "b.txt"
;;   (lambda (output-port)
;;     (display "hello, world" output-port)))

;; (letrec
;;     ((loop-1
;;       (lambda (i j)
;; 	(when (< i 1)
;; 	  (loop-2 i j)
;; 	  (loop-1 (+ i 0.05) j))))
;;      (loop-2
;;       (lambda (i j)
;; 	(when (< j 1)
;; 	  (display (format "F(~1,2f, ~1,2f) = ~1,2f`\n" i j (f (list i j))))
;; 	  (loop-2 i (+ j 0.05))))))
;;   (loop-1 0 0))
