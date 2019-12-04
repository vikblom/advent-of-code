



(define (number->list number)
  (let loop ((n number) (acc '()))
    (if (< n 10)
        (cons n acc)
        (loop (quotient n 10)
              (cons (remainder n 10) acc)))))

(define (sorted? lst)
  (let loop ((bar 0) (lst lst))
    (cond ((null? lst) #t)
          ((< (car lst) bar) #f)
          (else (loop (max bar (car lst)) (cdr lst))))))

(define (twice? lst)
  (if (null? lst) #f)
  (let loop ((pin (car lst)) (rem (cdr lst)))
    (if (null? rem)
        #f
        (if (eq? pin (car rem))
            #t
            (loop (car rem) (cdr rem))))))


;; PART 1
(length (filter twice? (filter sorted? (map number->list range))))



;; PART 2
(define (exactly-twice? lst)
  (if (null? lst) #f)
  (let loop ((pin (car lst))
             (rem (cdr lst))
             (repeat 1))
    (if (null? rem)
        (eq? repeat 2)
        (if (eq? pin (car rem))
            (loop (car rem) (cdr rem) (+ repeat 1))
            (if (eq? repeat 2)
                #t
                (loop (car rem) (cdr rem) 1))))))

(length (filter exactly-twice? (filter sorted? (map number->list range))))
