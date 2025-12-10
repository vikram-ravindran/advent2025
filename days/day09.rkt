#lang racket

(provide part1
         part2)

(require data/bit-vector)

;(define MAX-BYTES (* 1024 1024 1024))
;(custodian-limit-memory (current-custodian) MAX-BYTES)

(struct tile (x y) #:transparent)

#|

; Wow! This eats up all the memory on my system! Better to find another way
(struct 2d-vector (nrows ncols [vec #:mutable]) #:transparent)

(define make-2d-vector-from-initval
  (lambda (rows cols initval) (2d-vector rows cols (make-bit-vector (* rows cols) initval))))

(define get-2d-vector-element
  (lambda (2dvec row col) (bit-vector-ref (2d-vector-vec 2dvec) (+ (* row (2d-vector-nrows 2dvec)) col))))

(define set-2d-vector-element!
  (lambda (2dvec row col newval)
    (bit-vector-set! (2d-vector-vec 2dvec) (+ (* row (2d-vector-nrows 2dvec)) col) newval)))

(define initialize-valid-tile-grid
  (lambda (tlist)
    (let* ([nrows (+ (apply max (map (lambda (t) (tile-x t)) tlist)) 1)]
           [ncols (+ (apply max (map (lambda (t) (tile-y t)) tlist)) 1)]
           [valid-tile-grid (make-2d-vector-from-initval nrows ncols #f)])
      (set-2d-vector-element! valid-tile-grid (tile-x (car tlist)) (tile-y (car tlist)) #t)
      (let loop ([previous-tile (car tlist)]
                 [remaining-tiles (cdr tlist)])
        (let ([current-tile (car remaining-tiles)])
          (cond
            [(= (tile-x current-tile) (tile-x previous-tile))
             (for ([y (range (min (tile-y current-tile) (tile-y previous-tile))
                             (max (tile-y current-tile) (tile-y previous-tile)))])
               (set-2d-vector-element! valid-tile-grid (tile-x current-tile) y #t))]
            [(= (tile-y current-tile) (tile-y previous-tile))
             (for ([x (range (min (tile-x current-tile) (tile-x previous-tile))
                             (max (tile-x current-tile) (tile-x previous-tile)))])
               (set-2d-vector-element! valid-tile-grid x (tile-y current-tile) #t))])
          (unless (empty? (cdr remaining-tiles))
            (loop current-tile (cdr remaining-tiles))))))))
|#

(define initialize-valid-tile-boundary-list
  (lambda (tlist)
    (let* ([valid-tile-boundary-list (list (car tlist))])
      (let loop ([previous-tile (car tlist)]
                 [remaining-tiles (append (cdr tlist) (list (car tlist)))])
        (let ([current-tile (car remaining-tiles)])
          (cond
            [(= (tile-x current-tile) (tile-x previous-tile))
             (for ([y (range (min (tile-y current-tile) (tile-y previous-tile))
                             (+ (max (tile-y current-tile) (tile-y previous-tile)) 1))])
               (set! valid-tile-boundary-list
                     (cons (tile (tile-x current-tile) y) valid-tile-boundary-list)))]
            [(= (tile-y current-tile) (tile-y previous-tile))
             (for ([x (range (min (tile-x current-tile) (tile-x previous-tile))
                             (+ (max (tile-x current-tile) (tile-x previous-tile)) 1))])
               (set! valid-tile-boundary-list
                     (cons (tile x (tile-y current-tile)) valid-tile-boundary-list)))])
          (unless (empty? (cdr remaining-tiles))
            (loop current-tile (cdr remaining-tiles)))))
      valid-tile-boundary-list)))

(define area
  (lambda (t1 t2)
    (* (+ 1 (abs (- (tile-x t1) (tile-x t2)))) (+ 1 (abs (- (tile-y t1) (tile-y t2)))))))

(define is-tile-in-area-interior?
  (lambda (t1 t2 test-tile)
    (let ([min-x (min (tile-x t1) (tile-x t2))]
          [min-y (min (tile-y t1) (tile-y t2))]
          [max-x (max (tile-x t1) (tile-x t2))]
          [max-y (max (tile-y t1) (tile-y t2))])
      (and (< min-x (tile-x test-tile) max-x) (< min-y (tile-y test-tile) max-y)))))

(define is-area-embedded-in-valid-zone?
  (lambda (t1 t2 valid-tile-boundary-list)
    (for/and ([valid-tile valid-tile-boundary-list])
      (not (is-tile-in-area-interior? t1 t2 valid-tile)))))

(define find-maximum-area
  (lambda (tlist)
    (for*/fold ([max-area 0])
               ([t1 tlist]
                [t2 tlist])
      (max max-area (area t1 t2)))))

(define find-maximum-area-in-valid-zone
  (lambda (tlist valid-tile-boundary-list)
    ;(define i 0)
    ;(write (format "Areas to check: ~a" (* (length tlist) (length tlist)))) (newline)
    (for*/fold ([max-area 0])
               ([t1 tlist]
                [t2 tlist])
      ;              (set! i (+ i 1))
      ;     (when (= (remainder i 1000) 0)
      ;     (write (format "Checking area ~a of ~a" i (* (length tlist) (length tlist)))) (newline))

      (cond
        [[is-area-embedded-in-valid-zone? t1 t2 valid-tile-boundary-list] (max max-area (area t1 t2))]
        [else max-area]))))

(define process-file
  (lambda (filename)
    (let ([lines (file->lines filename)])
      (map (lambda (line) (apply tile (map string->number (string-split line ",")))) lines))))

(define renormalize-tile-coordinates
  (lambda (tlist)
    (let ([min-x (apply min (map (lambda (t) (tile-x t)) tlist))]
          [min-y (apply min (map (lambda (t) (tile-y t)) tlist))])
      (map (lambda (t) (tile (- (tile-x t) min-x) (- (tile-y t) min-y))) tlist))))

(define part1 (lambda (filename) (find-maximum-area (process-file filename))))

(define part2
  (lambda (filename)
    (let ([tile-coordinates (process-file filename)])
      (find-maximum-area-in-valid-zone tile-coordinates
                                       (initialize-valid-tile-boundary-list tile-coordinates)))))
