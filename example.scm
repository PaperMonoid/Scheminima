(load "benchmarks.scm")
(load "algorithms.scm")

(define (logger iteration S quality)
  (display (format "~d\t~s\t~1,4f \n" iteration S quality)))

(define rng (opt/make-rng-uniform 0))

(define (objective-1 x) (- (opt/sphere-function x '(0.75 0.25))))
(define (tweak x) (opt/bounded-uniform-convolution rng x 1.0 0.0 1.0))
(opt/steepest-ascent-hill-climbing '(0.5 0.5) tweak objective-1 logger 30 30)

(define (tweak x) (opt/bounded-uniform-convolution rng x 1.0 -5.0 5.0))
(define (objective-2 x) (- (opt/linear-slope-function x '(-5 5))))
(opt/steepest-ascent-hill-climbing '(0.5 0.5) tweak objective-2 logger 30 30)

(define (objective-1 x) (- (opt/sphere-function x '(0.75 0.25))))
(define (tweak x) (opt/bounded-uniform-convolution rng x 1.0 0.0 1.0))
(opt/tabu-search '(0.5 0.5) tweak objective-1 logger 30 3 30)

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
