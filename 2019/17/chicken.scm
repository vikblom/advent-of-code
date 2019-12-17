(import srfi-69 (chicken port))
(load "../advent.scm")
(load "../intcode.scm")


(define input (with-input-from-file "input.txt" read-line))

(define ascii (intcode-out (intcode-run input)))

(print (map integer->char ascii))


(define (scaffolds ascii)
  (define scaff (make-hash-table))
  (let loop ((row 0)
             (col 0)
             (lst ascii))
    (cond ((null? lst) scaff)
          ((equal? 10 (car lst)) (loop (+ row 1) 0 (cdr lst)))
          (else (if (equal? 35 (car lst))
                    (hash-table-set! scaff (list row col) #t))
                (loop row (+ col 1) (cdr lst))))))

(define scaff (scaffolds ascii))

(define (all lst)
  (if (null? lst)
      #t
      (and (car lst) (all (cdr lst)))))


(define (intersects scaff)
  (define (cross? pos)
    (all (map (lambda (p) (hash-table-exists? scaff p))
              (nbrs pos))))
  (filter cross? (hash-table-keys scaff)))

(reduce + 0 (map (lambda (p) (apply * p)) (intersects scaff)))


;; PART 2

(define (asciify str)
  (append
   (with-input-from-string str read-bytes) '(10)))

(begin
  (define ic (intcode-setup input))
  (intcode-write! ic 0 2)

  (intcode-push-inputs! ic
                        (append (asciify "A,B,B,A,C,B,C,C,B,A")
                                (asciify "R,10,R,8,L,10,L,10")
                                (asciify "R,8,L,6,L,6")
                                (asciify "L,10,R,10,L,6")
                                (asciify "n")))

  (intcode-run ic)
  (print (last (intcode-out ic))))
