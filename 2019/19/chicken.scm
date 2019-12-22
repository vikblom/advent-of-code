(import srfi-1 srfi-69)
(load "../advent.scm")
(load "../intcode.scm")


(define orig (intcode-file "input.txt"))

(define (tractord? x y)
  (car (intcode-pop-outputs!
        (intcode-run (intcode-copy orig) x y))))



(define (draw-tractor-beam width height)
  (for-each
   (lambda (y)
     (for-each
      (lambda (x) (print* (if (not (zero? (tractord? x y))) #\# #\.)))
      (iota width))
     (newline))
   (iota height)))


;; PART 1
(define (tractor-beam-size width height)
  (reduce + 0 (apply append
                     (map
                      (lambda (y)
                        (map
                         (lambda (x) (tractord? x y))
                         (iota width)))
                      (iota height)))))


;; PART 2
(define memory (make-hash-table))
(define (beamd-memo? pos)
  (if (hash-table-exists? memory pos)
      (hash-table-ref memory pos)
      (let ((ans (not (zero? (car (intcode-pop-outputs!
                                   (intcode-run (intcode-copy orig)
                                                (car pos)
                                                (cadr pos))))))))
        (hash-table-set! memory pos ans)
        ans)))

(define (beamd? x y)
  (not (zero? (car (intcode-pop-outputs!
                    (intcode-run (intcode-copy orig)
                                 x
                                 y))))))


(define (left-edge? x y)
  (let ((this (beamd? x y))
        (prev (beamd? (- x 1) y)))
    (and this (not prev))))


;; TODO start from best guess based on previous biggest left edge?

(define size 99)

(define (find-left-edge y)
  (let search ((x (floor (* y 1.2)))) ;; heuristic
    (if (beamd? x y)
        (list x y)
        (search (+ x 1)))))

(define (santa-fits? low-left-corn)
  (and (apply beamd? low-left-corn)
       (apply beamd? (map + low-left-corn (list size (- size))))))


(define (santa-on-row? n)
  (and (santa-fits? (find-left-edge n))
       (not (santa-fits? (find-left-edge (- n 1))))))

(define (part2)
  (let loop ((needle (floor (/ 2048 2)))
             (step (floor (/ 2048 4))))
    (print needle)
    (if (santa-fits? (find-left-edge needle))
        (if (santa-fits? (find-left-edge (- needle 1)))
            (loop (- needle step) (floor (/ step 2)))
            needle
            )
        (loop (+ needle step) (floor (/ step 2))))))

(print (reduce + 0 (map * (part2) '(10000 1))))

;; Linear search finds 1121
;; Low corner is then 1523 1121
;; Top rocrner is then 1523 1022
;; Answer is 15231022
