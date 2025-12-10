#lang racket

(provide part1
         part2)

(struct junction-box (x y z) #:transparent)

(define N '())

(define distance
  (lambda (j1 j2)
    (sqrt (+ (expt (- (junction-box-x j1) (junction-box-x j2)) 2)
             (expt (- (junction-box-y j1) (junction-box-y j2)) 2)
             (expt (- (junction-box-z j1) (junction-box-z j2)) 2)))))

(define initialize-junction-vector (lambda (jlist) (list->vector jlist)))

(define initialize-circuit-table
  (lambda (jvector)
    (let ([circuit-table (make-hash)])
      (for ([junction jvector])
        (hash-set! circuit-table junction junction))
      circuit-table)))

(define initialize-circuit-size-table
  (lambda (jvector)
    (let ([circuit-size-table (make-hash)])
      (for ([junction jvector])
        (hash-set! circuit-size-table junction 1))
      circuit-size-table)))

(define find-first-circuit-junction
  (lambda (jvector circuit-table)
    (cond
      [(equal? (hash-ref circuit-table jvector) jvector) jvector]
      [else (find-first-circuit-junction (hash-ref circuit-table jvector) circuit-table)])))

(define combine-junctions
  (lambda (j1 j2 circuit-table circuit-size-table)
    (let ([j1-first (find-first-circuit-junction j1 circuit-table)]
          [j2-first (find-first-circuit-junction j2 circuit-table)])
      (cond
        [(equal? j1-first j2-first) #f]
        [(< (hash-ref circuit-size-table j1-first) (hash-ref circuit-size-table j2-first))
         (hash-set! circuit-table j1-first j2-first)
         (hash-set! circuit-size-table
                    j2-first
                    (+ (hash-ref circuit-size-table j1-first) (hash-ref circuit-size-table j2-first)))
         #t]
        [(>= (hash-ref circuit-size-table j1-first) (hash-ref circuit-size-table j2-first))
         (hash-set! circuit-table j2-first j1-first)
         (hash-set! circuit-size-table
                    j1-first
                    (+ (hash-ref circuit-size-table j2-first) (hash-ref circuit-size-table j1-first)))
         #t]))))

(define initialize-junction-distance-list
  (lambda (jvector)
    (sort (for*/list ([x (range 0 (vector-length jvector))]
                      [y (range (+ x 1) (vector-length jvector))])
            (let ([j1 (vector-ref jvector x)]
                  [j2 (vector-ref jvector y)])
              (list j1 j2 (distance j1 j2))))
          (lambda (j1 j2) (< (third j1) (third j2))))))

(define update-circuit-tables-with-junctions
  (lambda (junction-distance-list circuit-table circuit-size-table n)
    (for ([junction-distance junction-distance-list]
          [i (range 0 n)])
      (combine-junctions (first junction-distance)
                         (second junction-distance)
                         circuit-table
                         circuit-size-table))))

(define update-circuit-tables-with-junctions-part2
  (lambda (junction-distance-list circuit-table circuit-size-table)
    (let loop ([unconnected-boxes (hash-count circuit-size-table)]
               [remaining-junction-distances junction-distance-list]
               [final-product 123])
      (cond
        [(or (= unconnected-boxes 0) (empty? remaining-junction-distances)) final-product]
        [(combine-junctions (first (car remaining-junction-distances))
                            (second (car remaining-junction-distances))
                            circuit-table
                            circuit-size-table)
         (loop (- unconnected-boxes 1)
               (cdr remaining-junction-distances)
               (* (junction-box-x (first (car remaining-junction-distances)))
                  (junction-box-x (second (car remaining-junction-distances)))))]
        [else (loop unconnected-boxes (cdr remaining-junction-distances) final-product)]))))

(define get-n-largest-circuits
  (lambda (circuit-table circuit-size-table n)
    (let ([all-first-junctions (remove-duplicates (hash-map circuit-table (lambda (key val) val)))])
      (take (sort (for/list ([first-junction all-first-junctions])
                    (hash-ref circuit-size-table first-junction))
                  >)
            n))))

(define process-file
  (lambda (filename)
    (let ([lines (file->lines filename)])
      (map (lambda (line) (apply junction-box (map string->number (string-split line ",")))) lines))))

(define part1
  (lambda (filename)
    (let* ([jvector (initialize-junction-vector (process-file filename))]
           [junction-distance-list (initialize-junction-distance-list jvector)]
           [circuit-table (initialize-circuit-table jvector)]
           [circuit-size-table (initialize-circuit-size-table jvector)])
      (set! N (vector-length jvector))
      (update-circuit-tables-with-junctions junction-distance-list
                                            circuit-table
                                            circuit-size-table
                                            1000)
      (apply * (get-n-largest-circuits circuit-table circuit-size-table 3)))))

(define part2
  (lambda (filename)
    (let* ([jvector (initialize-junction-vector (process-file filename))]
           [junction-distance-list (initialize-junction-distance-list jvector)]
           [circuit-table (initialize-circuit-table jvector)]
           [circuit-size-table (initialize-circuit-size-table jvector)])
      (update-circuit-tables-with-junctions-part2 junction-distance-list
                                                  circuit-table
                                                  circuit-size-table))))
