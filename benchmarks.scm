;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions. ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define opt/pi
  (* 4 (atan 1)))


(define (opt/range a b)
  (letrec
      ((loop
	(lambda (l a b)
	  (if (< a b)
	      (loop (cons a l) (+ a 1) b)
	      l))))
    (when (< a b)
      (reverse (loop '() a b)))))


(define (opt/sign x)
  (cond
   ((> x 0) 1)
   ((< x 0) -1)
   (else 0)))


(define (opt/Toz x)
  (let* ((x^ (if (not (equal? x 0)) (log (abs x)) 0))
	 (c1 (if (> x 0) 10 5.5))
	 (c2 (if (> x 0) 7.9 3.1)))
    (* (opt/sign x) (exp (+ x^ (* 0.049 (+ (sin (* c1 x^)) (sin (* c2 x^)))))))))


(define (opt/fpen x)
  (apply + (map (lambda (xi) (expt (max 0 (- (abs xi) 5)) 2)) x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objetive functions to optimize. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (opt/sphere-function x x*)
  (let* ((z (map (lambda (xi xi*) (- xi xi*)) x x*))
	 (f (lambda (z) (apply + (map (lambda (zi) (expt zi 2)) z)))))
    (+ (f z) (f x*))))


(define (opt/rosenbrock-function x x*)
  (let* ((D (length x))
	 (a (max 1 (/ (sqrt D) 8)))
	 (Z (lambda (xi) (+ (* a xi) 1)))
	 (z (map Z (map - x x*))))
    (letrec
	((butlast
	  (lambda (x)
	    (reverse (cdr (reverse x)))))
	 (g
	  (lambda (zi zi+1)
	    (+ (* 100 (expt (- (expt zi 2) zi+1) 2)) (expt (- zi 1) 2))))
	 (f
	  (lambda (x)
	    (apply + (map g (butlast x) (cdr x))))))
      (+ (f z) (f x*)))))


(define (opt/buche-rastringin-function x x*)
  (let* ((D (length x))
	 (Ds (range 1 (+ D 1)))
	 (s
	  (map
	   (lambda (i)
	     (if (equal? (mod i 2) 1)
		 (* 10 (expt 10 (* (/ 1 2) (/ (- i 1) (- D 1)))))
		 (expt 10 (* (/ 1 2) (/ (- i 1) (- D 1))))))
	   Ds))
	 (z
	  (map
	   (lambda (si xi xi*)
	     (* si (opt/Toz (- xi xi*)))) s x x*))
	 (f
	  (lambda (x)
	    (+ (* 10 (- D (apply + (map (lambda (zi) (cos (* 2 opt/pi zi))) z))))
	       (apply + (map (lambda (zi) (expt zi 2)) z))
	       (* 100 (opt/fpen x))))))
    (+ (f z) (f x*))))


(define (opt/linear-slope-function x x*)
  (let* ((D (length x))
	 (I (range 1 (+ D 1)))
	 (s (map (lambda (xi* i) (* (opt/sign xi*) (expt 10 (/ (- i 1) (- D 1))))) x I))
	 (z (map (lambda (xi xi*) (if (< (* xi* xi) (expt 5 2)) xi xi*)) x x*))
	 (f (lambda (x) (apply + (map (lambda (zi si) (- (* 5 (abs si)) (* si zi))) z s)))))
    (+ (f z) (f x*))))


(define (opt/different-powers-function x x*)
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

;; (define (f x) (sphere-function x '(0 0.25)))
;; (f '(0 0.25))

;; (define (f x) (linear-slope-function x '(-5 5)))
;; (f '(-5 5))
