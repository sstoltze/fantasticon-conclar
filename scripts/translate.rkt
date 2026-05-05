#lang racket
(require csv-reading)

(define tidsplan (csv->list (open-input-file "tidsplan.csv")))

(define-values (lokaler plan) (match tidsplan
                                [(list lokaler plan ...) (values (rest lokaler) plan)]))

(foldl (lambda (tidspunkt acc)
            (match tidspunkt
              [(list dag "" ...) #:when (member dag (list "Lørdag" "Søndag")) (list dag (second acc))]
              [(list tid program ...)
               (list (first acc)
                     (append (map (lambda (p r) (list (first acc) tid r p)) program lokaler) (second acc)))]))
       '("" ())
       plan)
