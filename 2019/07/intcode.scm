(import (chicken string) srfi-1 srfi-133 defstruct)

;; INTCODE
(defstruct intcode state p in out)

(define (intcode-print ic)
  (print "Intcode at: " (+ 1 (intcode-p ic)))
  (print "In: " (intcode-in ic))
  (print "Out: " (intcode-out ic))
  (print "State: " (intcode-state ic)))

(define (intcode-copy ic)
  (make-intcode state: (vector-copy (intcode-state ic))
                p: (intcode-p ic)
                in: (intcode-in ic)
                out: (intcode-out ic)))

(define (intcode-current ic)
  (vector-ref (intcode-state ic) (intcode-p ic)))

(define (intcode-read ic n)
  (vector-ref (intcode-state ic) n))

(define (intcode-next! ic)
  (intcode-p-set! ic (+ 1 (intcode-p ic)))
  (intcode-current ic))

(define (intcode-write! ic n x)
  (vector-set! (intcode-state ic) n x))

(define (intcode-jump! ic n)
  (intcode-p-set! ic n))

(define (stop-after n)
  (lambda (#!rest args) (if (>= n 0) (set! n (- n 1))) (< n 0)))

(define (rem10 x) (remainder x 10))
(define (qt10 x) (quotient x 10))

;; Uses current cursor position value as mode mask.
;; Moves cursor n positions.
(define (intcode-gobble! ic n)
  (let* ((modekey (quotient (intcode-current ic) 100))
         (modes (unfold (stop-after n) rem10 qt10 modekey))
         (vals (map (lambda (e) (intcode-next! ic)) (iota n))))
    (map (lambda (mode val) (if (eq? mode 0)
                                (intcode-read ic val)
                                val))
         modes vals)))

(define (intcode-add ic)
  (let ((sum (apply + (intcode-gobble! ic 2))))
    (intcode-write! ic (intcode-next! ic) sum)))

(define (intcode-mul ic)
  (let ((sum (apply * (intcode-gobble! ic 2))))
    (intcode-write! ic (intcode-next! ic) sum)))

;; INPUT OUTPUT
(define (intcode-input ic)
  (let-values (((in . rest) (apply values (intcode-in ic))))
    (intcode-write! ic (intcode-next! ic) in)
    (intcode-in-set! ic rest)))

(define (intcode-push-inputs! ic inputs)
  (intcode-in-set! ic (append (intcode-in ic) inputs)))

(define (intcode-output ic)
  (intcode-out-set! ic (append (intcode-out ic) (intcode-gobble! ic 1))))

(define (intcode-pop-outputs! ic)
  (let ((out (intcode-out ic)))
    (intcode-out-set! ic '())
    out))

(define (intcode-non-zero-jump ic)
  (let-values (((pred dest) (apply values (intcode-gobble! ic 2))))
    (if (not (zero? pred))
        (intcode-jump! ic (- dest 1)))))

(define (intcode-zero-jump ic)
  (let-values (((pred dest) (apply values (intcode-gobble! ic 2))))
    (if (zero? pred) (intcode-jump! ic (- dest 1)))))

(define (intcode-less ic)
  (let-values (((lhs rhs) (apply values (intcode-gobble! ic 2)))
               ((dest) (intcode-next! ic)))
    (intcode-write! ic dest (if (< lhs rhs) 1 0))))


(define (intcode-equal ic)
  (let-values (((lhs rhs) (apply values (intcode-gobble! ic 2)))
               ((dest) (intcode-next! ic)))
    (intcode-write! ic dest (if (eq? lhs rhs) 1 0))))

(define (intcode-read-op ic)
  (case (remainder (intcode-current ic) 100)
    ((1) intcode-add)
    ((2) intcode-mul)
    ((3)  (if (null? (intcode-in ic))
              ic
              intcode-input))
    ((4) intcode-output)
    ((5) intcode-non-zero-jump)
    ((6) intcode-zero-jump)
    ((7) intcode-less)
    ((8) intcode-equal)
    ((99) '())
    (else (error "UNKOWN OPCODE: " (intcode-current ic)
                 (intcode-state ic)))))

(define (intcode-setup src)
  (make-intcode state: (intcode-parse src)
                p: 0
                in: '()
                out: '()))

(define (intcode-parse src)
  (list->vector (map string->number (string-split src ","))))
;;(map (lambda (e) (array-add! e program)) (reverse inputs))

(define (intcode-run ic . inputs)
  ;; If first time around we should parse the string
  (if (string? ic) (set! ic (intcode-setup ic)))
  ;; Each call can freely push on more inputs
  (intcode-push-inputs! ic inputs)
  ;; Eval codes until halt or out-of-input
  (let loop ()
    (let ((op (intcode-read-op ic)))
      (if (procedure? op)
          (begin (op ic) (intcode-next! ic) (loop))
          ic))))

(define (intcode-halted? ic)
  (eq? (intcode-current ic) 99))

;; TESTS
(if #f
    (begin
      (intcode-run "2,3,0,3,99") ;; 2,3,0,6,99
      (intcode-run "2,4,4,5,99,0") ;; 2,4,4,5,99,9801
      (intcode-run "1,1,1,4,99,5,6,0,99") ;; 30,1,1,4,2,5,6,0,99

      ;; Position mode test
      (intcode-run "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" 1) ;; -> 1
      (intcode-run "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" 0) ;; -> 0

      ;; Immediate mode test
      (intcode-run  "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" 1) ;; -> 1
      (intcode-run  "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" 0) ;; -> 0

      (intcode-run "3,9,8,9,10,9,4,9,99,-1,8" 9) ;; -> (in == 8)
      (intcode-run "3,9,7,9,10,9,4,9,99,-1,8" 10) ;; -> (in < 8)
      (intcode-run "3,3,1108,-1,8,3,4,3,99" 8) ;; -> (in == 8)
      (intcode-run "3,3,1107,-1,8,3,4,3,99" 9) ;; -> (in < 8)


      ;;(car (intcode-run (read-lines "day05.txt") 1)) ;; -> 0 ... 5821753
      ;;(car (intcode-run (read-lines "day05.txt") 5)) ;; -> 0 ... 11956381

      (intcode-run inputs 5))) ;; -> 11956381
