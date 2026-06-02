#lang racket
(require csv-reading)
(require json)

(define tidsplan (csv->list (open-input-file "tidsplan.csv")))
(define beskrivelser (csv->list (open-input-file "beskrivelser.csv")))

(define-values (lokaler plan) (match tidsplan
                                [(list lokaler plan ...) (values (rest lokaler) plan)]))

(define (translate-time tid)
  (match tid
    ["" (values "" "45")]
    [(pregexp #px"(\\d+) \\((\\d+)\\)")
     (match (regexp-match #px"(\\d+) \\((\\d+)\\)" tid)
       [(list _ hour duration) (values (string-append hour ":00") duration)])]
    [(pregexp #px"(\\d+)-\\1:30") (values (string-append (substring tid 0 2) ":00") "30")]
    [(pregexp #px"(\\d+):30-.*") (values (string-append (substring tid 0 2) ":30") "30")]
    [_ (values (string-append (substring tid 0 2) ":00") (if (string-contains? tid ":30")
                                                                    "90"
                                                                    "45"))]))

(define programme-info (foldl (lambda (item acc)
                                (match item
                                  [(list "Arbejdes aktivt på" _ ...)
                                   acc]
                                  [(list _ _ _ title desc type _ _ people language _ ...)
                                   (hash-set acc (string-trim title) (hasheq 'desc desc
                                                                             'type type
                                                                             'people (map (lambda (p)
                                                                                            (hasheq 'id (string-trim p)
                                                                                                    'name (string-trim p)))
                                                                                          (string-split people ","))
                                                                             'tags (map (lambda (l) (hasheq 'value (string-trim l)
                                                                                                            'category "language"
                                                                                                            'label (string-trim l)))
                                                                                        (string-split language ","))))]
                                  [_ acc]))
                              (hash)
                              beskrivelser))

(define programme
  (sort
   (filter
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
                       (append (map (lambda (title room)
                                      (define trimmed-title (string-trim title))
                                      (define info (hash-ref programme-info trimmed-title (hash)))
                                      (define-values (start duration) (translate-time tid))

                                      (hasheq
                                       'id trimmed-title
                                       'date (first acc)
                                       'format (hash-ref info 'type "")
                                       'time start
                                       'mins duration
                                       'title trimmed-title
                                       'loc (list room)
                                       'tags (hash-ref info 'tags (list))
                                       'people (hash-ref info 'people (list))
                                       'desc (hash-ref info 'desc "")))
                                    program
                                    lokaler)
                               (second acc)))]))
            '("" ())
            plan)))
   string<=?
   #:key (lambda (h) (hash-ref h 'id))))

(define people (remove-duplicates (sort (foldl (lambda (p acc) (append (hash-ref p 'people (list))
                                                                       acc))
                                               (list)
                                               programme)
                                        string<=?
                                              #:key (lambda (h)
                                                (hash-ref h 'id)))
                                        #:key (lambda (h)
                                                (hash-ref h 'id))))

(with-output-to-file "../public/2026/program.js" #:exists 'replace
  (lambda () (write-json programme #:indent 2)))

(with-output-to-file "../public/2026/people.js" #:exists 'replace
  (lambda () (write-json people #:indent 2)))
