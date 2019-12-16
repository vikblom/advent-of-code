(import srfi-1 srfi-69 (chicken io) (chicken port))
(load "../advent.scm")

(define base '(0 1 0 -1))

(define (repeat lst n)
  (apply append (map (lambda (e) (make-list n e)) lst)))

(define (pattern n)
  (cdr (apply circular-list (repeat base n))))


(define memory (make-hash-table))
(define (pattern-mem n)
  (if (hash-table-exists? memory n)
      (hash-table-ref memory n)
      (let ((new (pattern n)))
        (hash-table-set! memory n new)
        new)))


(define (decima n) (remainder (if (< n 0) (- n) n) 10))

(define (conv a b)
  (decima
   (fold (lambda (a b acc) (+ acc (* a b))) 0 a b)))


(define (phase signal patterns)
  (map
   (lambda (pat) (conv signal pat))
   patterns))

(define (phase-vanishing signal patterns)
  (if (null? signal)
      '()
      (cons (conv signal (car patterns))
            (phase-vanishing (cdr signal) (cdr patterns)))))

(define (multi-phase signal n)
  (define patterns (map (lambda (n) (drop (pattern n) (- n 1)))
                        (iota (length signal) 1)))
  (let loop ((n n) (signal signal))
    (if (zero? n)
        signal
        (loop (- n 1) (phase-vanishing signal patterns)))))

(define (chain f seed n)
  (let loop ((n n))
    (if (zero? n)
        seed
        (f (chain f seed (- n 1))))))


(define (str->lst str)
  (map (lambda (n) (- n 48))
       (with-input-from-string str read-bytes)))

(take (multi-phase (str->lst "80871224585914546619083218645595") 100) 8)
;;(take (multi-phase (str->lst "19617804207202209144916044189917") 100) 8)
;;(take (multi-phase (str->lst "69317163492948606335995924319873") 100) 8)

(define input (with-input-from-file "input.txt" read-line))
;;(print (take (multi-phase (str->lst input) 100) 8))

;; PART 2

(define (digits->number lst)
  (fold
   (lambda (d pow acc) (+ acc (* d (expt 10 pow))))
   0
   lst
   (reverse (iota (length lst)))))


(define (acc-sum-overflowed lst)
  (let loop ((sum 0) (rev-acc '()) (lst lst))
    (if (null? lst)
        (reverse rev-acc)
        (let ((s (remainder (+ sum (car lst)) 10)))
          (loop s (cons s rev-acc) (cdr lst))))))



(define (part2 signal)
  ;; For offset > N/2, we only have an upper diagonal matrix with ones.
  (define offset (digits->number (take signal 7)))
  (if (< offset (* (length signal) 5000))
      (error "NOT IN UPPER HALF"))
  (define tail-rev (reverse (drop (apply append (make-list 10000 signal))
                                  offset)))
  (take (reverse (chain acc-sum-overflowed tail-rev 100)) 8))



(part2 (str->lst "03036732577212944063491565474664"))
(part2 (str->lst "02935109699940807407585447034323"))
(part2 (str->lst "03081770884921959731165446850517"))
(print (part2 (str->lst input)))
