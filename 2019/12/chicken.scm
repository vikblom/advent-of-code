(include "../advent.scm")
(import regex srfi-69)

(define (vec-diff a b) (map - a b))
(define (vec-add a b) (map + a b))
(define (vec-sum lst) (reduce vec-add '(0 0 0) lst))
(define (vec-abs a) (map abs a))
(define (energy a) (apply + (map abs a)))

(define (vec-accel a b)
  (map (lambda (x y) (if (= x y)
                         0
                         (if (< x y) 1 -1)))
       a b))

(define (total-acc pos)
  (map (lambda (a)
         (vec-sum
          (map (lambda (b) (vec-accel a b)) pos)))
       pos))

(define pos car)
(define vel cdr)

(define (parse-pos str)
  (map string->number
       (string-split-fields (regexp "-?\\d+") str)))


(define (step moons)
  (let* ((old-pos (map pos moons))
         (old-vel (map vel moons))
         (new-vel (map vec-add old-vel (total-acc old-pos)))
         (new-pos (map vec-add old-pos new-vel)))

    (map cons new-pos new-vel)))

(define (chain f seed n)
  (let loop ((n n))
    (if (zero? n)
        seed
        (f (chain f seed (- n 1))))))

(define (total-energy moons)
  (let ((pos (map pos moons))
        (vel (map vel moons)))
    (apply + (map * (map energy pos) (map energy vel)))))

(begin
  (define moons (map cons
                     (map parse-pos
                          (with-input-from-file "input.txt" read-lines))
                     (circular-list '(0 0 0))))
  (define x0 (slice moons car))
  (define y0 (slice moons cadr))
  (define z0 (slice moons caddr)))


;; PART 1
(print "Total energy at step 1000: "
       (total-energy (chain step moons 1000)))


(define (slice lst selector)
  (map (compose selector car) moons))


(define (cmp a b)
  (if (= a b)
      0
      (if (< a b) 1 -1)))

(define (acc-single pos)
  (map (lambda (a)
         (apply + (map (lambda (b) (cmp a b))
                       pos)))
       pos))


(define (step-single state)
  (let* ((old-pos (car state))
         (old-vel (cadr state))
         (new-vel (map + old-vel (acc-single old-pos)))
         (new-pos (map + old-pos new-vel)))
    (list new-pos new-vel)))


(define (find-repetition p0)
  (define seen (make-hash-table))
  (define init (list p0 (make-list (length p0) 0)))
  (hash-table-set! seen init 0)
  (let loop ((state (step-single init))
             (n 1))
    (if (hash-table-exists? seen state)
        n
        (loop (step-single state) (+ n 1)))))


(print (find-repetition x0))
(print (find-repetition y0))
(print (find-repetition z0))


(print "Repeating state after: "
       (lcm (find-repetition x0)
            (find-repetition y0)
            (find-repetition z0)))
