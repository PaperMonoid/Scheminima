;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions. ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define mini/pi
  (* 4 (atan 1)))


(define (mini/range a b)
  (letrec
      ((loop
	(lambda (l a b)
	  (if (< a b)
	      (loop (cons a l) (+ a 1) b)
	      l))))
    (when (< a b)
      (reverse (loop '() a b)))))


(define (mini/sign x)
  (cond
   ((> x 0) 1)
   ((< x 0) -1)
   (else 0)))


(define (mini/Toz x)
  (let* ((x^ (if (not (equal? x 0)) (log (abs x)) 0))
	 (c1 (if (> x 0) 10 5.5))
	 (c2 (if (> x 0) 7.9 3.1)))
    (* (mini/sign x) (exp (+ x^ (* 0.049 (+ (sin (* c1 x^)) (sin (* c2 x^)))))))))


(define (mini/fpen x)
  (apply + (map (lambda (xi) (expt (max 0 (- (abs xi) 5)) 2)) x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objetive functions to optimize. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mini/sphere-function x x*)
  (let* ((z (map (lambda (xi xi*) (- xi xi*)) x x*))
	 (f (lambda (z) (apply + (map (lambda (zi) (expt zi 2)) z)))))
    (+ (f z) (f x*))))


(define (mini/rosenbrock-function x x*)
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


(define (mini/buche-rastringin-function x x*)
  (let* ((D (length x))
	 (Ds (mini/range 1 (+ D 1)))
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
	     (* si (mini/Toz (- xi xi*)))) s x x*))
	 (f
	  (lambda (x)
	    (+ (* 10 (- D (apply + (map (lambda (zi) (cos (* 2 mini/pi zi))) z))))
	       (apply + (map (lambda (zi) (expt zi 2)) z))
	       (* 100 (mini/fpen x))))))
    (+ (f z) (f x*))))
