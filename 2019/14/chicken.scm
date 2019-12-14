(load "../advent.scm")

(define (maybe-number foo)
  (if (string->number foo)
      (string->number foo)
      foo))

(define (read-chem str)
  (let ((tmp (string-split str)))
    (cons (cadr tmp) (string->number (car tmp)))))

(define (read-reac line)
  (map maybe-number (reverse (string-split line " ,=>"))))

(define (read-reac-table f)
  (alist->hash-table
   (map read-reac
        (with-input-from-file f read-lines))))

(define (make-how-many? slask type asked)
  (let ((stored (hash-table-ref/default slask type 0)))
    (if (zero? stored)
        asked
        (let ((taking (min asked stored)))
          (hash-table-set! slask type (- stored taking))
          (- asked taking)))))



(define (cost reacts type n)
  (define slask (make-hash-table))
  (define (inner type n)
    (if (equal? type "ORE")
        n
        (let* ((recipe (hash-table-ref reacts type))
               (prod (car recipe))
               (reagents (cdr recipe))
               (make-n (make-how-many? slask type n))
               (batches (ceiling (/ make-n prod)))
               (surplus (- (* prod batches) make-n)))
          (if (zero? batches)
              0
              (begin
                (hash-table-update!/default slask
                                            type
                                            (lambda (x) (+ x surplus))
                                            0)
                (apply + (pair-map inner
                                   (map (lambda (x) (if (number? x)
                                                        (* x batches)
                                                        x))
                                        reagents)))))
          )))
  (inner type n))



(define (pair-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst) (cadr lst))
            (pair-map f (cddr lst)))))


;; PART 1
(define (part1 f) (cost (read-reac-table f) "FUEL" 1))


;; PART 2
(define trillion 1000000000000)


(define (part2 f)
  (define reac (read-reac-table f))
  (define init (floor (/ trillion (cost reac "FUEL" 1))))

  (let loop ((guess init)
             (step (floor (/ init 2))))
    (if (zero? step)
        guess
        (loop (if (< (cost reac "FUEL" guess) trillion)
                  (+ guess step)
                  (- guess step))
              (floor (/ step 2))))))
