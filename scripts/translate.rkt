#lang racket
(require csv-reading)
(require json)

(define tidsplan (csv->list (open-input-file "tidsplan.csv")))

(define-values (lokaler plan) (match tidsplan
                                [(list lokaler plan ...) (values (rest lokaler) plan)]))

(define (translate-time tid)
  (if (eq? tid "")
      ""
      (string-append (substring tid 0 2) ":00")))

(define p (filter
           (lambda (item) (not (string=? (string-trim (hash-ref item 'title)) "")))
           (second
                   (foldl (lambda (tidspunkt acc)
                            (match tidspunkt
                              [(list dag "" ...) #:when (member (string-trim dag) (list "Fredag" "Lørdag" "Søndag"))
                                                 (define dato (cond
                                                                [(string=? dag "Fredag") "2026-06-05"]
                                                                [(string=? dag "Lørdag") "2026-06-06"]
                                                             [else "2026-06-07"]))
                                                 (list dato (second acc))]
                              [(list tid program ...)
                               (list (first acc)
                                     (append (map (lambda (p r)
                                                    (hasheq
                                                     'id (~v (random 100000))
                                                     'date (first acc)
                                                     'time (translate-time tid)
                                                     'mins "45"
                                                     'title p
                                                     'loc (list r)
                                                     'tags (list)
                                                     'people (list)))
                                                  program
                                                  lokaler)
                                             (second acc)))]))
                          '("" ())
                          plan))))


(with-output-to-file "../public/2026/program.js" #:exists 'replace
  (lambda () (write-json p #:indent 2)))
