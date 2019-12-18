(include "../advent.scm")


(define (read-maze f)
  (define maze (make-hash-table))
  (let loop ((row 0) (col 0) (lst (with-input-from-file f read-chars)))
    (if (null? lst)
        maze
        (cond ((equal? (car lst) #\newline) (loop (+ row 1) 0 (cdr lst)))
              (else (hash-table-set! maze (list col row) (car lst))
                    (loop row (+ col 1) (cdr lst)))))))

(define (draw-map karta)
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




(define (pop-entrance! maze)
  (define pos '())
  (hash-table-walk maze (lambda (k v) (if (equal? v #\@)
                                          (set! pos k))))
  (hash-table-set! maze pos #\.)
  pos)

(define (find-keys maze)
  (define keys (make-hash-table))
  (hash-table-walk maze
                   (lambda (k v) (if (char-lower-case? v)
                                     (hash-table-set! keys v k))))
  keys)

(define (find-gates maze)
  (define gates (make-hash-table))
  (hash-table-walk maze
                   (lambda (k v) (if (char-upper-case? v)
                                     (hash-table-set! gates v k))))
  gates)


(define (open? char) (or (equal? char #\.)
                         (char-lower-case? char)))


;; Return a hashmap pos -> n where n is the
;; shortest number of steps to get to pos
(define (stepmap maze start)
  (define steps (make-hash-table))
  (hash-table-set! steps start 0)

  (define (update-nbrs pos n)
    (map (lambda (p) (if (and (open? (hash-table-ref/default maze p #\#))
                              (not (hash-table-exists? steps p)))
                         (hash-table-set! steps p n)))
         (nbrs pos)))

  (let loop ((n 1) (size (hash-table-size steps)))
    (map (lambda (pos) (update-nbrs pos n))
         (hash-table-keys steps))
    (if (equal? size (hash-table-size steps))
        steps
        (loop (+ n 1) (hash-table-size steps)))))



;; Return a hashmap pos -> #t indicating which positions we can reach
(define (explore maze reach)
  ;; Helper operating on each node
  (define (update-nbrs pos)
    (map (lambda (p) (if (and (open? (hash-table-ref/default maze p #\#))
                              (not (hash-table-exists? reach p)))
                         (hash-table-set! reach p #t)))
         (nbrs pos)))
  ;; Add frontier to reach
  (let loop ((size (hash-table-size steps)))
    (map update-nbrs (hash-table-keys reach))
    (if (equal? size (hash-table-size reach))
        reach
        (loop (hash-table-size reach)))))


;; List of positions where there are keys we can reach
(define (collectable keys reachable)
  (hash-table-fold
   keys
   (lambda (char pos knil) (if (hash-table-exists? reachable pos)
                               (cons pos knil)
                               knil))
   '()))

(define (upcase c)
  (integer->char (- (char->integer c) 32)))

(define (part1 maze)
  (define start (pop-entrance! maze))
  (define keys (find-keys maze))
  (define gates (find-gates maze))

  (define (unlock mz pos)
    (let ((C (upcase (hash-table-ref mz pos))))
      (if (hash-table-exists? gates C)
          (hash-table-set! mz (hash-table-ref gates C) #\.)))
    mz)

  (let recur ((pos start) (dist 0) (maze maze) (keys keys))
    (hash-table-delete! keys (hash-table-ref maze pos))
    (hash-table-set! maze pos #\.)

    (let* ((steps (stepmap maze pos))
           (candidates (collectable keys steps)))
      (if (null? candidates)
          dist
          (apply min
                 (map (lambda (next) (recur next
                                            (+ dist (hash-table-ref steps next))
                                            (unlock (hash-table-copy maze) next)
                                            (hash-table-copy keys)))
                      candidates))))))


;;(print (part1 (read-maze "one.txt")))
;;(print (part1 (read-maze "two.txt")))
(print (part1 (read-maze "three.txt")))
;;(print (part1 (read-maze "input.txt")))
