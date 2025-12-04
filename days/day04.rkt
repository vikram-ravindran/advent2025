#lang racket

(provide part1
         part2)

(struct 2d-vector (nrows ncols [vec #:mutable]) #:transparent)

(define make-2d-vector-from-initval
  (lambda (rows cols initval) (2d-vector rows cols (make-vector (* rows cols) initval))))

(define make-2d-vector-from-vector
  (lambda (rows cols initvec) (2d-vector rows cols (vector-copy initvec))))

(define get-2d-vector-element
  (lambda (2dvec row col) (vector-ref (2d-vector-vec 2dvec) (+ (* row (2d-vector-nrows 2dvec)) col))))

(define set-2d-vector-element!
  (lambda (2dvec row col newval)
    (vector-set! (2d-vector-vec 2dvec) (+ (* row (2d-vector-nrows 2dvec)) col) newval)))

(define compact-print-2d-vector
  (lambda (2dvec)
    (for ([i (range 0 (2d-vector-nrows 2dvec))])
      (for ([j (range 0 (2d-vector-ncols 2dvec))])
        (display (get-2d-vector-element 2dvec i j)))
      (newline))))

(define roll-count (lambda (str) (if (equal? str "@") 1 0)))

(define count-surrounding-rolls
  (lambda (2dvec row col)
    (let ([nrows (2d-vector-nrows 2dvec)]
          [ncols (2d-vector-ncols 2dvec)])
      (+ (if (not (or (= row 0) (= col 0)))
             (roll-count (get-2d-vector-element 2dvec (- row 1) (- col 1)))
             0) ; NW
         (if (not (= row 0))
             (roll-count (get-2d-vector-element 2dvec (- row 1) col))
             0) ; N
         (if (not (or (= row 0) (= col (- ncols 1))))
             (roll-count (get-2d-vector-element 2dvec (- row 1) (+ col 1)))
             0) ; N
         (if (not (= col 0))
             (roll-count (get-2d-vector-element 2dvec row (- col 1)))
             0) ; W
         (if (not (= col (- ncols 1)))
             (roll-count (get-2d-vector-element 2dvec row (+ col 1)))
             0) ; E
         (if (not (or (= row (- nrows 1)) (= col 0)))
             (roll-count (get-2d-vector-element 2dvec (+ row 1) (- col 1)))
             0) ; SW
         (if (not (= row (- nrows 1)))
             (roll-count (get-2d-vector-element 2dvec (+ row 1) col))
             0) ; S
         (if (not (or (= row (- nrows 1)) (= col (- ncols 1))))
             (roll-count (get-2d-vector-element 2dvec (+ row 1) (+ col 1)))
             0) ; SE
         ))))

(define process-file
  (lambda (filename)
    (let ([filelines (map string->list (file->lines filename))])
      (make-2d-vector-from-vector
       (length filelines)
       (length (car filelines))
       (for/fold ([totalvector (vector)]) ([currentline filelines])
         (values (vector-append totalvector (list->vector (map string currentline)))))))))

(define number-of-rolls-with-fewer-than-n-surrounding
  (lambda (2dvec n)
    (filter (lambda (nrolls) (and (< nrolls n) (> nrolls -1)))
            (for*/list ([row (range 0 (2d-vector-nrows 2dvec))]
                        [col (range 0 (2d-vector-ncols 2dvec))])
              (if (equal? (get-2d-vector-element 2dvec row col) "@")
                  (count-surrounding-rolls 2dvec row col)
                  -1)))))

(define coordinates-of-rolls-with-fewer-than-n-surrounding
  (lambda (2dvec n)
    (filter (lambda (coords) (not (empty? coords)))
            (for*/list ([row (range 0 (2d-vector-nrows 2dvec))]
                        [col (range 0 (2d-vector-ncols 2dvec))])
              (if (and (equal? (get-2d-vector-element 2dvec row col) "@")
                       (< (count-surrounding-rolls 2dvec row col) n))
                  (cons row col)
                  '())))))

(define count-number-of-removable-rolls
  (lambda (2dvec n)
    (let* ([coords (coordinates-of-rolls-with-fewer-than-n-surrounding 2dvec n)]
           [numcoords (length coords)])
      (cond
        [(= numcoords 0) 0]
        [else
         (for ([coord coords])
           (set-2d-vector-element! 2dvec (car coord) (cdr coord) "."))
         (+ numcoords (count-number-of-removable-rolls 2dvec n))]))))

(define part1
  (lambda (filename)
    (let ([2dvec (process-file filename)])
      (length (number-of-rolls-with-fewer-than-n-surrounding 2dvec 4)))))

(define part2
  (lambda (filename)
    (let ([2dvec (process-file filename)]) (count-number-of-removable-rolls 2dvec 4))))
