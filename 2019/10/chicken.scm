(import arrays srfi-69)
(load "../advent.scm")


(define (read-asteroids f)
  (let* ((asteroids (make-hash-table))
         (rows (with-input-from-file f read-lines))
         (lines (map string->list rows)))
    (map (lambda (line i)
           (map (lambda (elem j)
                  (if (eq? elem #\#)
                      (hash-table-set! asteroids
                                       (cons i j)
                                       0)))
                line (iota (length line))))
         lines (iota (length lines)))
    asteroids))

(define (map-read f)
  (let* ((rows (with-input-from-file f read-lines))
         (lines (map string->list rows))
         (rows (length lines))
         (cols (length (car lines))))
    (cons (cons rows cols) (list->array (apply append lines)))))

(define map-rows (compose car car))
(define map-cols (compose cdr car))
(define map-arr cdr)

(define (map-ref map x y)
  (array-item (+ x (* y (map-cols map)))
              (map-arr map)))

(define (asteroid? map p)
  (eq? (map-ref map (car p) (cdr p)) #\#))

(define (asteroid-coords m)
  (filter
   (lambda (p) (asteroid? m p))
   (let ((linear (iota (* (map-rows m) (map-cols m))))
         (rower (lambda (i) (remainder i (map-cols m))))
         (coler (lambda (i) (quotient i (map-cols m)))))
     (map cons
          (map rower linear)
          (map coler linear)))))


(define (pos-between p1 p2)
  (let* ((dx (- (car p2) (car p1)))
         (dy (- (cdr p2) (cdr p1)))
         (div (gcd dx dy)))
    (if (< div 2) '()
        (cdr (map cons
                  (iota div (car p1) (quotient dx div))
                  (iota div (cdr p1) (quotient dy div)))))))

(define (los? m p1 p2)
  (let ((candidates (pos-between p1 p2)))
    (if (null? candidates)
        #t
        (not (any (lambda (c) (asteroid? m c)) candidates)))))

(define (count-los m ast asteroids)
  (- (count (lambda (p) (los? m ast p)) asteroids)
     1))


;; PART 1
(define (part-1 f)
  (let* ((m (map-read f))
         (asteroids (asteroid-coords m))
         (counts (map (lambda (ast) (count-los m ast asteroids))
                      asteroids)))
    (cons
     (apply max counts)
     (argmax (lambda (ast) (count-los m ast asteroids)) asteroids))))

(part-1 "first.txt")

(part-1 "input.txt")
