(load "advent.scm")
(load "intcode.scm")






(define input (with-input-from-file "./09/input.txt" read-line))

;; PART 1
(intcode-print (intcode-run input 1))


;; PART 2
(intcode-print (intcode-run input 2))
