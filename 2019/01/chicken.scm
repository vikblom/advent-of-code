;; Input
(define masses
  (map string->number
       (call-with-input-file "input.txt"
         (lambda (port) (read-lines port)))))

;; Part 1
(define (fuel-req mass) (- (floor (/ mass 3)) 2))

(fold + 0 (map (lambda (mass) (fuel-req mass)) masses))


;; Part 2
(define (total-fuel mass)
  (define (fuel-fuel acc fuel)
    (if (<= fuel 0)
        acc
        (fuel-fuel (+ acc fuel) (fuel-req fuel))))
  (fuel-fuel 0 (fuel-req mass)))

(fold + 0 (map (lambda (mass) (total-fuel mass)) masses))
