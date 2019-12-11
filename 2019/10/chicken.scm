(import arrays srfi-69)
(load "../advent.scm")


(define (read-asteroids f)
  (let* ((asteroids (make-hash-table))
         (rows (with-input-from-file f read-lines))
         (lines (map string->list rows)))
    (map (lambda (line j)
           (map (lambda (elem i)
                  (if (eq? elem #\#)
                      (hash-table-set! asteroids
                                       (cons i j)
                                       0)))
                line (iota (length line))))
         lines (iota (length lines)))
    asteroids))

(define (pos-between p1 p2)
  (let* ((dx (- (car p2) (car p1)))
         (dy (- (cdr p2) (cdr p1)))
         (div (gcd dx dy)))
    (if (< div 2) '()
        (cdr (map cons
                  (iota div (car p1) (quotient dx div))
                  (iota div (cdr p1) (quotient dy div)))))))

(define (los? table p1 p2)
  (not (any (lambda (c) (hash-table-exists? table c))
            (pos-between p1 p2))))

(define (count-los table ast)
  (- (count (lambda (p) (los? table ast p))
            (hash-table-keys table)) 1))

(define (most-los table)
  (apply max (map (lambda (ast) (count-los table ast))
                  (hash-table-keys table))))


;; PART 1
(most-los (read-asteroids "first.txt"))
(most-los (read-asteroids "input.txt"))

;; PART 2
(define (best-pos table)
  (argmax (lambda (ast) (count-los table ast))
          (hash-table-keys table)))

(best-pos (read-asteroids "input.txt")) ;; (8 . 16)

(define (angle-n-dist p1 p2)
  (let ((dx (- (car p2) (car p1)))
        (dy (- (cdr p2) (cdr p1))))
    (cons (if (< dy 0)
              (+ (* 2 pi) (atan dy dx))
              (atan dy dx))
          (+ (* dx dx) (* dy dy)))))

(define (first-target asteroids best)
  (define (up pos) (cons (car pos) (- (cdr pos) 1)))
  (let loop ((cand (up best)))
    (if (hash-table-exists? asteroids cand)
        cand
        (loop (up cand)))))


(define pi (* 2 (acos 0)))

(define (next-target asteroids alpha)
  (define (angle-from-alpha a)
    (if (< alpha a) (- a alpha) (+ (* 2 pi) (- a alpha))))

  (define (less? a b)
    (if (= (angle-from-alpha (car a))
           (angle-from-alpha (car b)))
        (< (cdr a) (cdr b))
        (< (angle-from-alpha (car a))
           (angle-from-alpha (car b)))))

  (let loop ((ast (hash-table-keys asteroids))
             (min-k '())
             (min-v '(+inf.0 . +inf.0)))
    (if (null? ast)
        min-k
        (if (less? (hash-table-ref asteroids (car ast)) min-v)
            (loop (cdr ast) (car ast) (hash-table-ref asteroids (car ast)))
            (loop (cdr ast) min-k min-v)
            ))))

(begin
  (define asteroids (read-asteroids "input.txt"))
  (define best (best-pos asteroids))
  (hash-table-delete! asteroids best)
  ;;(hash-table-delete! asteroids '(11 . 12))
  (hash-table-for-each
   asteroids
   (lambda (p v) (hash-table-set! asteroids
                                  p
                                  (angle-n-dist best p)))))

(define (shoot-asteroids asteroids)
  (let loop ((target (first-target asteroids best))
             (n 1))
    (if (not (zero? (hash-table-size asteroids)))
    (let ((beta (car (hash-table-ref asteroids target))))
      (hash-table-delete! asteroids target)
      (print n " vaporized " target " at " beta)
      (loop (next-target asteroids beta) (+ n 1))
      ))))
