(import srfi-1 srfi-69 (chicken io) (chicken string))

(define (argmin fn args)
  (cdr
   (fold
    (lambda (val arg bestp) (if (< val (car bestp))
                                (cons val arg)
                                bestp))
    '(+inf.0 . void)
    (map fn args)
    args)))

(define (argmax fn args)
  (cdr
   (fold
    (lambda (val arg bestp) (if (> val (car bestp))
                                (cons val arg)
                                bestp))
    '(-inf.0 . void)
    (map fn args)
    args)))


(define (collect! fn stop)
  (if (stop) '()
      (cons (fn) (collect! fn stop))))

(define (chain f seed n)
  (let loop ((n n))
    (if (zero? n)
        seed
        (f (chain f seed (- n 1))))))


(define (read-bytes) (collect! read-byte (compose eof-object? peek-char)))


(define (nbrs pos)
  (let ((x (car pos))
        (y (cadr pos)))
    (list (list (+ x 1) y)
          (list x (+ y 1))
          (list (- x 1) y)
          (list x (- y 1)))))
