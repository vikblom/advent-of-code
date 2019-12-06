(import (chicken io) (chicken string) srfi-1)

(define vref vector-ref)

(define (rem10 x) (remainder x 10))
(define (qt10 x) (quotient x 10))

(define (stop-after n)
  (lambda (#!rest args) (if (>= n 0) (set! n (- n 1))) (< n 0)))


(define (ic-params code p n)
  (let* ((modekey (quotient (vref code p) 100))
         (modes (unfold (stop-after n) rem10 qt10 modekey)))
    ;; TODO first grab immediate, then maybe replace with pos?
    (map (lambda (mode p)
           (if (eq? mode 0)
               (vref code (vref code p))
               (vref code p)))
         modes (iota n (+ p 1)))))

(define (ic-add code p)
  (vector-set! code (vref code (+ p 3))
               (apply + (ic-params code p 2)))
  code)

(define (ic-mul code p)
  (vector-set! code (vref code (+ p 3))
               (apply * (ic-params code p 2)))
  code)

(define (ic-input code p in)
  (vector-set! code (vref code (+ p 1)) in)
  code)

(define (ic-output code p)
  (apply print (ic-params code p 1))
  code)

(define (ic-jump code p)
  (let-values (((pred dest)
                (apply values (ic-params code p 2))))
    (if (zero? pred) (+ p 3) dest)))

(define (ic-njump code p)
  (let-values (((pred dest)
                (apply values (ic-params code p 2))))
    (if (zero? pred) dest (+ p 3))))

(define (ic-less code p)
  (let-values (((lhs rhs)
                (apply values (ic-params code p 2)))
               ((dest) (vref code (+ p 3))))
    (vector-set! code
                 dest
                 (if (< lhs rhs) 1 0))))

(define (ic-equal code p)
  (let-values (((lhs rhs)
                (apply values (ic-params code p 2)))
               ((dest) (vref code (+ p 3))))
    (vector-set! code
                 dest
                 (if (eq? lhs rhs) 1 0))))

(define (ic-run src . input)
  (let ic-step ((code (ic-parse src)) (p 0))
    (case (remainder (vref code p) 100)
      ((1) (ic-add code p) (ic-step code (+ p 4)))
      ((2) (ic-mul code p) (ic-step code (+ p 4)))
      ((3) (ic-input code p (car input)) (ic-step code (+ p 2)))
      ((4) (ic-output code p) (ic-step code (+ p 2)))
      ((5) (ic-step code (ic-jump code p)))
      ((6) (ic-step code (ic-njump code p)))
      ((7) (ic-less code p) (ic-step code (+ p 4)))
      ((8) (ic-equal code p) (ic-step code (+ p 4)))
      ((99) code)
      (else (error "UNKOWN OPCODE")))))

(define (ic-parse src)
  (list->vector (map string->number (string-split src ","))))

(ic-run (ic-parse "2,3,0,3,99")) ;; 2,3,0,6,99
(ic-run (ic-parse "2,4,4,5,99,0")) ;; 2,4,4,5,99,9801
(ic-run (ic-parse "1,1,1,4,99,5,6,0,99")) ;; 30,1,1,4,2,5,6,0,99

(ic-run (ic-parse "3,0,4,0,99") 13)
(ic-run (ic-parse "1101,100,-1,4,0"))



;; PART 1
(define inputs
  (call-with-input-file "input.txt"
    (lambda (port) (read-line port))))
(ic-run (ic-parse inputs) 1)

;; PART 2
(ic-run (ic-parse "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") 1) ;; -> 1
(ic-run (ic-parse "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") 0) ;; -> 0
(ic-run (ic-parse "3,9,8,9,10,9,4,9,99,-1,8") 6) ;; -> (in == 8)
(ic-run (ic-parse "3,9,7,9,10,9,4,9,99,-1,8") 7) ;; -> (in < 8)
(ic-run (ic-parse "3,3,1108,-1,8,3,4,3,99") -123) ;; -> (in == 8)
(ic-run (ic-parse "3,3,1107,-1,8,3,4,3,99") -11230) ;; -> (in < 8)

(ic-run (ic-parse inputs) 5) ;; -> 11956381
