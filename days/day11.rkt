#lang racket

(provide part1
         part2)

(define process-file
  (lambda (filename)
    (map (lambda (line)
           (map string->symbol
                (string-split (list->string (filter (lambda (c) (not (char=? c #\:)))
                                                    (string->list line))))))
         (file->lines filename))))

(define get-neighbours
  (lambda (graph node)
    (let ([node-specifier (filter (lambda (n) (equal? (car n) node)) graph)])
      (cond
        [(empty? node-specifier) '()]
        [else (cdar node-specifier)]))))

(define find-all-paths
  (lambda (graph start end)
    (let ([paths '()])
      (define find-all-paths-helper
        (lambda (graph start end path)
          (set! path (append path (list start)))
          (cond
            [(equal? start end) (set! paths (cons path paths))]
            [else
             (map (lambda (neighbour) (find-all-paths-helper graph neighbour end path))
                  (get-neighbours graph start))])))
      (find-all-paths-helper graph start end '())
      paths)))

(define pathcount-hash (make-hash))

(define count-paths
  (lambda (graph start end)
    (define count-paths-helper
      (lambda (graph start end)
        (cond
          [(equal? start end) 1]
          [(hash-has-key? pathcount-hash (list start end)) (hash-ref pathcount-hash (list start end))]
          [else
           (let ([subcount (apply +
                                  (map (lambda (neighbour) (count-paths-helper graph neighbour end))
                                       (get-neighbours graph start)))])
             (hash-set! pathcount-hash (list start end) subcount)
             subcount)])))
    (count-paths-helper graph start end)))

(define count-paths-with-dac-and-fft
  (lambda (graph start end)
    (+
     (* (count-paths graph start 'fft) (count-paths graph 'fft 'dac) (count-paths graph 'dac end))
     (* (count-paths graph start 'dac) (count-paths graph 'dac 'fft) (count-paths graph 'fft end)))))

(define part1 (lambda (filename) (length (find-all-paths (process-file filename) 'you 'out))))
(define part2 (lambda (filename) (count-paths-with-dac-and-fft (process-file filename) 'svr 'out)))
