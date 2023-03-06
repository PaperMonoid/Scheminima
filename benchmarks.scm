;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions. ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (range a b)
  (letrec
      ((loop
	(lambda (l a b)
	  (if (< a b)
	      (loop (cons a l) (+ a 1) b)
	      l))))
    (when (< a b)
      (reverse (loop '() a b)))))


(define (sign x)
  (cond
   ((> x 0) 1)
   ((< x 0) -1)
   (else 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objetive functions to optimize. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sphere-function x x*)
  (let* ((z (map (lambda (xi xi*) (- xi xi*)) x x*))
	 (f (lambda (z) (apply + (map (lambda (zi) (expt zi 2)) z)))))
    (+ (f z) (f x*))))


(define (linear-slope-function x x*)
  (let* ((D (length x))
	 (I (range 1 (+ D 1)))
	 (s (map (lambda (xi* i) (* (sign xi*) (expt 10 (/ (- i 1) (- D 1))))) x I))
	 (z (map (lambda (xi xi*) (if (< (* xi* xi) (expt 5 2)) xi xi*)) x x*))
	 (f (lambda (x) (apply + (map (lambda (zi si) (- (* 5 (abs si)) (* si zi))) z s)))))
    (+ (f z) (f x*))))


(define (different-powers-function x x*)
  (let* ((D (length x))
	 (I (range 1 (+ D 1)))
	 (z (map (lambda (xi xi*) (- xi xi*)) x x*))
	 (f (lambda (x) (sqrt (apply + (map (lambda (zi) (expt (abs zi) (+ 2 (* 4 (/ (- i 1) (- D 1)))))) z I))))))
    (+ (f z) (f x*))))


;; (define (sphere-function x x*)
;;   (letrec
;;       ((loop
;; 	(lambda (value x x*)
;; 	  (if (not (null? x))
;; 	      (let* ((xn (car x))
;; 		     (xn* (car x*))
;; 		     (zn (- xn xn*)))
;; 		(loop (+ value (expt zn 2) (expt xn* 2)) (cdr x) (cdr x*)))
;; 	      value))))
;;     (loop 0 x x*)))

(define (f x) (sphere-function x '(0 0.25)))
(f '(0 0.25))

;;(define (f x) (linear-slope-function x '(-5 5)))
;;(f '(-5 5))
