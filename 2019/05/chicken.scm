(import (chicken io) (chicken string) srfi-1)

(define vref vector-ref)

(define (rem10 x) (remainder x 10))
(define (qt10 x) (quotient x 10))

(define (stop-after n)
  (lambda (#!rest args) (if (>= n 0) (set! n (- n 1))) (< n 0)))


(define (intcode-params code p n)
  (let* ((modekey (quotient (vref code p) 100))
         (modes (unfold (stop-after n) rem10 qt10 modekey)))
    ;; TODO first grab immediate, then maybe replace with pos?
    (map (lambda (mode p)
           (if (eq? mode 0)
               (vref code (vref code p))
               (vref code p)))
         modes (iota n (+ p 1)))))

(define (intcode-add code p)
  (vector-set! code (vref code (+ p 3))
               (apply + (intcode-params code p 2)))
  code)

(define (intcode-mul code p)
  (vector-set! code (vref code (+ p 3))
               (apply * (intcode-params code p 2)))
  code)

(define (intcode-input code p in)
  (vector-set! code (vref code (+ p 1)) in)
  code)

(define (intcode-output code p)
  (apply print (intcode-params code p 1))
  code)

(define (intcode-jump code p)
  (let-values (((pred dest)
                (apply values (intcode-params code p 2))))
    (if (zero? pred) (+ p 3) dest)))

(define (intcode-njump code p)
  (let-values (((pred dest)
                (apply values (intcode-params code p 2))))
    (if (zero? pred) dest (+ p 3))))

(define (intcode-less code p)
  (let-values (((lhs rhs)
                (apply values (intcode-params code p 2)))
               ((dest) (vref code (+ p 3))))
    (vector-set! code
                 dest
                 (if (< lhs rhs) 1 0))))

(define (intcode-equal code p)
  (let-values (((lhs rhs)
                (apply values (intcode-params code p 2)))
               ((dest) (vref code (+ p 3))))
    (vector-set! code
                 dest
                 (if (eq? lhs rhs) 1 0))))

(define (intcode-run code . input)
  (let intcode-step ((code code) (p 0))
    (case (remainder (vref code p) 100)
      ((1) (intcode-step (intcode-add code p) (+ p 4)))
      ((2) (intcode-step (intcode-mul code p) (+ p 4)))
      ((3) (intcode-step (intcode-input code p (car input)) (+ p 2)))
      ((4) (intcode-step (intcode-output code p) (+ p 2)))
      ((5) (intcode-step code (intcode-jump code p)))
      ((6) (intcode-step code (intcode-njump code p)))
      ((7) (intcode-less code p) (intcode-step code (+ p 4)))
      ((8) (intcode-equal code p) (intcode-step code (+ p 4)))
      ((99) code)
      (else (error "UNKOWN OPCODE")))))

(define (intcode-parse src)
  (list->vector (map string->number (string-split src ","))))

(intcode-run (intcode-parse "2,3,0,3,99")) ;; 2,3,0,6,99
(intcode-run (intcode-parse "2,4,4,5,99,0")) ;; 2,4,4,5,99,9801
(intcode-run (intcode-parse "1,1,1,4,99,5,6,0,99")) ;; 30,1,1,4,2,5,6,0,99

(intcode-run (intcode-parse "3,0,4,0,99") 13)
(intcode-run (intcode-parse "1101,100,-1,4,0"))



;; PART 1
(define inputs
  (call-with-input-file "input.txt"
    (lambda (port) (read-line port))))
(intcode-run (intcode-parse inputs) 1)

;; PART 2
(intcode-run (intcode-parse "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") 1) ;; -> 1
(intcode-run (intcode-parse "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") 0) ;; -> 0
(intcode-run (intcode-parse "3,9,8,9,10,9,4,9,99,-1,8") 8) ;; -> (in == 8)
(intcode-run (intcode-parse "3,9,7,9,10,9,4,9,99,-1,8") 9) ;; -> (in < 8)
(intcode-run (intcode-parse "3,3,1108,-1,8,3,4,3,99") -123) ;; -> (in == 8)
(intcode-run (intcode-parse "3,3,1107,-1,8,3,4,3,99") -11230) ;; -> (in < 8)

(intcode-run (intcode-parse inputs) 5)
