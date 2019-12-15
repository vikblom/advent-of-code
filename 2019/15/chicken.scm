(import srfi-1 srfi-69 (chicken io))

(load "../advent.scm")
(load "../intcode.scm")

(define translator
  (alist->hash-table '((0 . #\#)
                       (1 . #\.)
                       (2 . #\X)
                       (10 . #\z)
                       (20 . #\O))))

(define (sketch-table raw)
  (define (translate r) (hash-table-ref translator r))
  (define new (make-hash-table))
  (hash-table-walk
   raw
   (lambda (k v)
     (hash-table-set! new k (translate v))))
  new)

(define (draw-map karta)
  (hash-table-set! karta '(0 0) #\D)
  (let* ((xs (map car (hash-table-keys karta)))
         (ys (map cadr (hash-table-keys karta)))
         (low-x (apply min xs))
         (high-x (apply max xs))
         (low-y (apply min ys))
         (high-y (apply max ys)))
    (for-each
     (lambda (j)
       (for-each
        (lambda (i)
          (print* (hash-table-ref/default karta (list i j) " ")))
        (iota (+ 1 (- high-x low-x)) low-x))
       (newline))
     (iota (+ 1 (- high-y low-y)) low-y))))



(define (nbrs pos)
  (let ((x (car pos))
        (y (cadr pos)))
    (list (list (+ x 1) y)
          (list x (+ y 1))
          (list (- x 1) y)
          (list x (- y 1)))))

(define (rev dir)
  (case dir
    ((1) 2)
    ((4) 3)
    ((2) 1)
    ((3) 4)))

(define (mv pos dir)
  (let ((x (car pos))
        (y (cadr pos)))
    (case dir
      ((1) (list x (- y 1)))
      ((2) (list x (+ y 1)))
      ((3) (list (- x 1) y))
      ((4) (list (+ x 1) y)))))

(define (instruct ic dir)
  (car (intcode-pop-outputs! (intcode-run ic dir))))


(define (explore ic)
  (define goal '(0 0))
  (define karta (make-hash-table))
  (define nearest (make-hash-table)) ;; (x y) -> (d x y)
  ;;(hash-table-set! nearest '(0 0) '(0 0 0))

  (define (update-nbr pos dist from)
    (if (< dist (car (hash-table-ref nearest pos)))
        (hash-table-set! nearest pos (cons dist from))))

  (let loop ((pos '(0 0)) (dist 0))
    (let-values (((visited-dir unseen-dir)
                  (partition (lambda (d) (hash-table-exists?
                                          nearest
                                          (mv pos d)))
                             (iota 4 1))))
      ;; See if we can reach already visited faster from here
      (map (lambda (d) (update-nbr (mv pos d) (+ dist 1) pos))
           visited-dir)
      ;; Otherwise try to visit and record
      (map (lambda (d)
             (let ((res (instruct ic d)))
               (hash-table-set! karta (mv pos d) res)
               (if (not (zero? res))
                   (begin
                     (if (equal? res 2) (set! goal (mv pos d)))
                     (hash-table-set! nearest (mv pos d) (cons dist pos))
                     (loop (mv pos d) (+ dist 1))
                     (instruct ic (rev d))))))
           unseen-dir)))
  (values karta nearest goal))


;; PART 1

(begin
  (define ic (intcode-run (with-input-from-file "input.txt" read-line)))

  (define-values (karta crumbs goal) (explore ic))

  (draw-map (sketch-table karta))

  (define (backtrack crumbs goal)
    (let loop ((pos goal)
               (dist 0))
      (if (equal? pos '(0 0))
          dist
          (loop (cdr (hash-table-ref crumbs pos)) (+ dist 1)))))

  (print "Steps from oxygen machine: "(backtrack crumbs goal)))

;; PART 2

(begin
  (define ic (intcode-run (with-input-from-file "input.txt" read-line)))
  (define-values (karta crumbs goal) (explore ic)))

(define (spread oxygen pos)
  (map (lambda (p)
         (if (= 1 (hash-table-ref/default oxygen p 0))
             (hash-table-set! oxygen p 10)))
       (nbrs pos)))

(define (mark oxygen)
  (hash-table-walk
   oxygen
   (lambda (pos type)
     (if (equal? type 20)
         (begin
           (spread oxygen pos))
         ))))

(define (sweep oxygen)
  (hash-table-walk
   oxygen
   (lambda (k v) (if (= v 10)
                     (hash-table-set! oxygen k 20)))))

(define (part2 karta goal)
  (define oxygenated (hash-table-copy karta))
  (hash-table-set! oxygenated goal 20)

  (let loop ((minutes 0))
    (if
     (any (lambda (x) (equal? x 1)) (hash-table-values oxygenated))
     (begin (mark oxygenated)
            (sweep oxygenated)
            (loop (+ minutes 1)))
     minutes)))

(print "Minutes to spread all oxygen: "(part2 karta goal))
