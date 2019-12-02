
(define example-1 (vector 1 0 0 0 99))

(define vref vector-ref)
(define (vector-copy vec)
  (list->vector (vector->list vec)))

(define (intcode-add code p)
  (vector-set! code (vref code (+ p 3))
               (+
                (vref code (vref code (+ p 1)))
                (vref code (vref code (+ p 2)))))
  code)

(define (intcode-mul code p)
  (vector-set! code (vref code (+ p 3))
               (*
                (vref code (vref code (+ p 1)))
                (vref code (vref code (+ p 2)))))
  code)

(define (eval-intcode code p)
  (define opcode (vref code p))
  (case opcode
    ((1) (eval-intcode (intcode-add code p) (+ p 4)))
    ((2) (eval-intcode (intcode-mul code p) (+ p 4)))
    ((99) code)
    (else (error "UNKOWN OPCODE" opcode))))


(import (chicken io) (chicken string))
(define inputs
  (map string->number (string-split
                       (call-with-input-file "input.txt"
                         (lambda (port) (read-line port)))
                       ",")))

(begin
  (define program1 (list->vector inputs))
  (vector-set! program1 1 12)
  (vector-set! program1 2 2)
  (vector-ref (eval-intcode program1 0) 0))



(define (mutate-and-run program foo bar)
  (vector-set! program 1 foo)
  (vector-set! program 2 bar)
  (vector-ref (eval-intcode program 0) 0))

(import srfi-1)

(mutate-and-run (list->vector inputs) 12 2)

(for-each
 (lambda (noun)
   (for-each (lambda (verb)
               (if (eq? 19690720 (mutate-and-run (list->vector inputs) noun verb))
                   (print "FOUND A HIT " (+ verb (* 100 noun)))))
             (iota 100)))
 (iota 100))
