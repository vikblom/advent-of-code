(load "../advent.scm")
(load "../intcode.scm")
(import (chicken io) srfi-1 srfi-18 srfi-69)

(define input (with-input-from-file "input.txt" read-line))


(define out (intcode-pop-outputs! (intcode-run input)))



(define (count-blocks outputs)
  (define tiles (make-hash-table))
  (let loop ((lst outputs))
    (if (not (null? lst))
        (begin
          (hash-table-set! tiles (take lst 2) (caddr lst))
          (loop (drop lst 3)))))
  (count (lambda (x) (= x 2))
         (hash-table-values tiles)))



(count-blocks out)


;; PART 2
(define input2 (with-input-from-file "input2.txt" read-line))


(define (make-score)
  (define n 0)
  (lambda (#!optional x) (if x (set! n x) n)))


(define symbols (alist->hash-table
                 '((0 . " ")
                   (1 . "X")
                   (2 . "#")
                   (3 . "_")
                   (4 . "o"))))

(define (update-state tiles eyes score op)
  (if (not (null? op))
      (let loop ((xy (take op 2))
                 (val (caddr op)))
        (if (equal? xy '(-1 0))
            (score val)
            (begin
              (hash-table-set! tiles
                               xy
                               (hash-table-ref symbols val))
              (hash-table-set! eyes
                               val
                               (car xy))))
        (update-state tiles eyes score (drop op 3)))))

(define (draw-state tiles score)
  (let* ((xs (map car (hash-table-keys tiles)))
         (ys (map cadr (hash-table-keys tiles)))
         (low-x (apply min xs))
         (high-x (apply max xs))
         (low-y (apply min ys))
         (high-y (apply max ys)))

    (for-each (lambda (j)
                (for-each (lambda (i)
                            (print* (hash-table-ref/default tiles (list i j) " ")))
                          (iota (+ 1 (- high-x low-x)) low-x))
                (newline))
              (iota (+ 1 (- high-y low-y)) low-y))
    (print (score))))

(define (sign a b)
  (cond ((< a b) -1)
        ((> a b) 1)
        (else 0)))

(define (paddle eyes)
  (sign (hash-table-ref eyes 4)
        (hash-table-ref eyes 3)))

(define (game-loop)
  (define tiles (make-hash-table))
  (define eyes (make-hash-table))
  (define score (make-score))
  (let loop ((ic (intcode-run input2)))
    (update-state tiles eyes score (intcode-pop-outputs! ic))
    (if (intcode-halted? ic)
        (score)
        (loop (intcode-run ic (paddle eyes)))))
  (draw-state tiles score)
  )
