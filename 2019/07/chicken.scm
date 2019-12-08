(import (chicken io))

(load "intcode.scm")

(define (read-a-line f)
  (call-with-input-file f
    (lambda (port) (read-line port))))


(define ic1 "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
(define ic2 "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0")
(define ic3 "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")


(define (run-amp-series src phases input)
  (fold (lambda (ic sig)
          (intcode-run ic sig)
          (car (intcode-pop-outputs! ic)))
        input
        (map (lambda (p) (intcode-run src p)) phases)))

(run-amp-series ic1 '(4 3 2 1 0) 0) ;; -> 43210
(run-amp-series ic2 '(0 1 2 3 4) 0) ;; -> 54321
(run-amp-series ic3 '(1 0 4 3 2) 0) ;; -> 65210

(define (insert lst e)
  (let loop ((rev-head '())
             (tail lst)
             (acc '()))
    (if (null? tail)
        (cons (reverse (cons e rev-head)) acc)
        (loop (cons (car tail) rev-head)
              (cdr tail)
              (cons (append-reverse rev-head (cons e tail)) acc)))))

(define (permute lst)
  (if (equal? 1 (length lst))
      (list lst)
      (append-map (lambda (sub) (insert sub (car lst)))
                  (permute (cdr lst)))))

;; PART 1
;; (apply max (map (lambda (p) (run-acs-seq ic1 p)) (permute '(0 1 2 3 4))))
;; (apply max (map (lambda (p) (run-acs-seq ic2 p)) (permute '(0 1 2 3 4))))
;; (apply max (map (lambda (p) (run-acs-seq ic3 p)) (permute '(0 1 2 3 4))))
;; (apply max (map (lambda (p) (run-acs-seq (read-a-line "input.txt") p))
;;                 (permute '(0 1 2 3 4))))

;; PART 2
(define (sweep-amp-series amps passed)
  (fold
   (lambda (ic sigs)
     (intcode-push-inputs! ic sigs)
     (intcode-run ic)
     (intcode-pop-outputs! ic))
   passed amps))


(define (exhaust-feedback src phases)
  (define amps (map (lambda (p) (intcode-run src p)) phases))
  (let loop ((state '(0)))
    (if (intcode-halted? (last amps))
        state
        (loop (sweep-amp-series amps state)))))

(define ic4 "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
(define ic5 "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")


(exhaust-feedback ic4 '(9 8 7 6 5)) ;; -> 139629729
(exhaust-feedback ic5 '(9 7 8 5 6)) ;; -> 18216


(define (maximize-feedback src)
  (apply max (map (lambda (p) (car (exhaust-feedback src p)))
                  (permute '(5 6 7 8 9)))))

(maximize-feedback ic4)
(maximize-feedback ic5)
(maximize-feedback (read-a-line "input.txt"))


(apply max (map (lambda (p) (exhaust-feedback (read-a-line "input.txt") p))
                (permute '(5 6 7 8 9))))
