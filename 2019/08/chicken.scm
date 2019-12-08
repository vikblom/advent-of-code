(import (chicken io) (chicken port) (chicken string))

(define (make-countdown n)
  (lambda (#!rest args) (if (>= n 0) (set! n (- n 1))) (>= n 0)))

(define (split-every-n lst n)
  (unfold null?
          (lambda (l) (take-while (make-countdown n) l))
          (lambda (l) (drop-while (make-countdown n) l))
          lst))

(define (argmin fn args)
  (cdr
   (fold
    (lambda (val arg bestp) (if (< val (car bestp))
                                (cons val arg)
                                bestp))
    '(+inf.0 . void)
    (map fn args)
    args)))

(define (collect! fn stop)
  (if (stop) '()
      (cons (fn) (collect! fn stop))))

(define (read-bytes) (collect! read-byte (compose eof-object? peek-char)))

(define input
  (map (λ (b) (- b 48))
       (with-input-from-string
           (with-input-from-file "input.txt" read-line)
         read-bytes)))

(define layers (split-every-n input (* 25 6)))

;; PART 1
(define (part1 layers)
  (let ((best-layer
         (argmin (λ (l) (count zero? l)) layers)))
    (* (count (λ (x) (eq? x 1)) best-layer)
       (count (λ (x) (eq? x 2)) best-layer))))

(part1 layers) ;; 1474

;; PART 2
(define (stack-all layers)
  (define (merge back front)
    (map (λ (f b) (if (= 2 f) b f)) front back))
  (fold merge
        (make-list (length (car layers)) 2)
        layers))


(define (nice-print image)
  (if (list? (car image))
      (map nice-print image)
      (apply print (map (λ (p) (if (zero? p) "⬛" " "))
                        image))))

(nice-print (split-every-n (stack-all layers) 25))
