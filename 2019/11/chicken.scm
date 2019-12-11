(import arrays srfi-69)
(load "../advent.scm")
(load "../intcode.scm")


;; d 0..3 and t 0,1
(define (next-heading d t)
  (remainder (+ 4 ((if (zero? t) - +) d 1)) 4))


(define (next-pos p d)
  (case d
    ((0) (cons (+ 1 (car p)) (cdr p)))
    ((1) (cons (car p) (+ 1 (cdr p))))
    ((2) (cons (- (car p) 1) (cdr p)))
    ((3) (cons (car p) (- (cdr p) 1)))))

;; 0 is black and also LEFT
;; 1 is white and also RIGHT

(define input (with-input-from-file "input.txt" read-line))

(define (move-n-paint paint pos dir instr)
  (if (null? instr)
      (list pos dir)
      (let* ((ndir (next-heading dir (cadr instr)))
             (npos (next-pos pos ndir)))
        ;; Paint
        (hash-table-set! paint pos (car instr))
        ;; Then move
        (move-n-paint paint npos ndir (cddr instr)))))

(define (run-robot src paint start)
  (let loop ((ic (intcode-run src))
             (pos start)
             (dir 0))
    (define-values (npos ndir)
      (apply values
             (move-n-paint paint pos dir (intcode-pop-outputs! ic))))
    (if (intcode-halted? ic)
        paint
        (loop (intcode-run ic (hash-table-ref/default paint npos 0))
              npos ndir))))

;; PART 1
(print (run-robot input (make-hash-table) '(0 . 0)))


(begin
  (define two (make-hash-table))
  (define start '(0 . 0))
  (hash-table-set! two start 1)
  (run-robot input two start)
  (define painted-ps (hash-table-keys two))
  (define xs (map car painted-ps))
  (define ys (map cdr painted-ps))

  (define low-x (apply min xs))
  (define high-x (apply max xs))
  (define low-y (apply min ys))
  (define high-y (apply max ys))

  (for-each (lambda (i)
              (for-each (lambda (j)
                          (print* (if (zero? (hash-table-ref/default two (cons i j) 0))
                                      " "
                                      "X")))
                        (iota (+ 1 (- high-y low-y)) low-y))
              (newline))
            (reverse (iota (+ 1 (- high-x low-x)) low-x))))
