;; INPUT
(import (chicken io) (chicken string) srfi-1 srfi-69)

(define input
  (call-with-input-file "input.txt"
    (lambda (port) (read-lines port))))

(define (parse-move raw)
  (let ((in  (string-split raw " ")))
    (cons (car in) (string->number (cadr in)))))

(define (parse-moves raw)
  (map parse-move
       (string-split
        (string-translate* raw
                           '(("U" . "up ")
                             ("R" . "right ")
                             ("D" . "down ")
                             ("L" . "left ")))
        ",")))

(define (right pos) (cons (+ (car pos) 1) (cdr pos)))
(define (left pos) (cons (- (car pos) 1) (cdr pos)))
(define (up pos) (cons (car pos) (+ (cdr pos) 1)))
(define (down pos) (cons (car pos) (- (cdr pos) 1)))

(define (stop-after n)
  (lambda (#!rest args) (if (>= n 0) (set! n (- n 1))) (< n 0)))

(define (trail pos dir len)
  (define mover (eval (string->symbol dir)))
  (unfold-right (stop-after len) identity mover (mover pos)))

(define (trailer instr sofar)
  (append (trail (car sofar) (car instr) (cdr instr)) sofar))

(define (manhattan pos) (+ (abs (car pos)) (abs (cdr pos))))

(define (str-to-pos input)
  ((compose cdr reverse fold) trailer '((0 . 0)) (parse-moves input)))

(define (xs pos1 pos2)
  (let ((pmap (make-hash-table)))
    (begin
      (map (lambda (p) (hash-table-set! pmap p 1))
           pos1)
      (filter (lambda (p) (hash-table-exists? pmap p))
              pos2))))

(define (closest-cross in1 in2)
  (apply min (map manhattan (xs in1 in2))))


(define (part-one str1 str2)
  (let ((pos1 (str-to-pos str1))
        (pos2 (str-to-pos str2)))
    (closest-cross pos1 pos2)))


(part-one "R8,U5,L5,D3" "U7,R6,D4,L4") ;; 6

(part-one "R75,D30,R83,U83,L12,D49,R71,U7,L72"
          "U62,R66,U55,R34,D71,R55,D58,R83") ;; 159

(part-one "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
          "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") ;; 135

(print (part-one (car input) (cadr input)))


;; PART 2
;; We get one step less for each wire because (0 . 0) is
;; not in the path. So result + 2 is the true answer

(define (best-cross crosses in1 in2)
  (define (dist-to-cross cross path)
    (list-index (lambda (x) (equal? x cross)) path))
  (define (dist-to-crosses path)
    (map dist-to-cross crosses (circular-list path)))
  (apply min
         (map + (dist-to-crosses in1) (dist-to-crosses in2))))

(define (part-two str1 str2)
  (let ((pos1 (str-to-pos str1))
        (pos2 (str-to-pos str2)))
    (+ 2 (best-cross (xs pos1 pos2) pos1 pos2))))

(part-two "R8,U5,L5,D3" "U7,R6,D4,L4") ;; 30

(part-two "R75,D30,R83,U83,L12,D49,R71,U7,L72"
          "U62,R66,U55,R34,D71,R55,D58,R83") ;; 610

(part-two "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
          "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") ;; 410

(part-two (car input) (cadr input)) ;; 14228
