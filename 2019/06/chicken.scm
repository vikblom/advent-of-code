(import (chicken io) (chicken string) srfi-1 srfi-69)

(define input
  (call-with-input-file "input.txt"
    (lambda (port) (read-lines port))))

;; Sets up a hashmap center -> orbiter
(define (parse-orbits input)
  (let ((v (make-hash-table))
        (in (map (lambda (s) (string-split s ")")) input)))
    (map (lambda (pair) (add-orbit! v (car pair) (cadr pair))) in)
    v))


(define (add-orbit! table center orbiter)
  (hash-table-update!/default table
                              center
                              (lambda (r) (cons orbiter r)) '()))


(define (sum-orbits orbits)
  (let loop ((key "COM") (ring 0))
    (if (not (hash-table-exists? orbits key))
        ring
        (+ ring (apply + (map loop
                              (hash-table-ref orbits key)
                              (circular-list (+ ring 1))))))))

;; PART 1
(sum-orbits (parse-orbits input))



;; PART 2


;; Sets up a hashmap orbiter -> center
(define (parse-orbiteers input)
  (let ((v (make-hash-table))
        (in (map (lambda (s) (string-split s ")")) input)))
    (map (lambda (pair) (add-orbit! v (cadr pair) (car pair))) in)
    v))


(define orbiteers (parse-orbiteers input))

(define (get-to-com orbiteers key)
  (unfold-right (lambda (k) (equal? k "COM"))
                identity
                (lambda (k) (car (hash-table-ref orbiteers k)))
                key))

(define (steps-to-santa orbiteers)
  (let loop ((you-to-com (get-to-com orbiteers "YOU"))
        (san-to-com (get-to-com orbiteers "SAN")))
    (if (equal? (car you-to-com) (car san-to-com))
        (loop (cdr you-to-com) (cdr san-to-com))
        (+ (length you-to-com) (length san-to-com) -2))))

;; PART 2
(steps-to-santa (parse-orbiteers input))
