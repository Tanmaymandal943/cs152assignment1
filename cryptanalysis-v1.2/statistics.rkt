#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))


(require "list-comprehension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; Ciphertext Statistics                                                           ;;
;; =====================                                                           ;;
;;                                                                                 ;;
;; The module should provide a bunch of functions that do statistical analysis of  ;;
;; ciphertext. The output is almost always just an ordered list (counts are        ;;
;; omitted).                                                                       ;;
;;                                                                                 ;;
;; Fill in the body for the skeletons and do not change the arguments. You can     ;;
;; define as many functions as you require, there's no special credit for          ;;
;; implementing/using all of them.                                                 ;;
;;                                                                                 ;;
;; CAUTION:                                                                        ;;
;; 1. Be mindful that bi, tri and quadgrams do not cross word boundaries. Hence,   ;;
;; you must process each word separately.                                          ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Analyses
(provide cipher-monograms
         cipher-bigrams
         cipher-unique-neighbourhood
         cipher-neighbourhood
         cipher-trigrams
         cipher-quadgrams
         cipher-common-words-single
         cipher-common-words-double
         cipher-common-words-triple
         cipher-common-words-quadruple
         cipher-common-initial-letters
         cipher-common-final-letters
         cipher-common-double-letters
         ;; any other functions of your design come below:

         ;; my-fundoo-analysis
         )


;; Adds element x to frequency-list lst 

(define (addtolist x lst)
  (if (null? lst)
      (list (cons x 1))
      (if (equal? (caar lst) x) (cons (cons x (+ 1 (cdar lst))) (cdr lst))
          (cons (car lst) (addtolist x (cdr lst))))))


;; Takes ciphertext and produces a list of cipher chars sorted in decreasing
;; order of frequency.


(define (cipher-monograms ciphertext)
  (let* ([st (string->list ciphertext)]
         [frel (cipher-mono st '())]
         [sorfrel (sorted-cipherlst frel)]
         )
    (retlst sorfrel '())
         ))


;; takes a characterlist and '() returns frequency list

(define (cipher-mono cipherlst lst)  
  (if (null? cipherlst)
      lst
      (if (char-alphabetic? (car cipherlst)) (cipher-mono (cdr cipherlst)(addtolist (car cipherlst) lst))
          (cipher-mono (cdr cipherlst) lst)
          )))


;; Compares two elements of frequency list and returns is-greater?

(define (compcons a b)
  (> (cdr a) (cdr b)))

;; sorts frequency list

(define (sorted-cipherlst lst)
  (sort lst compcons))


;; takes a frequency list and returns a list with the elements present in the list (not the frequency)

(define (retlst lst l)
  (if (null? lst) l
      (retlst (cdr lst) (append l (list (caar lst))))))






;; first n terms of the list

(define (n-terms-list lst n)
  (if (= n 0) '()
      (cons (car lst) (n-terms-list (cdr lst) (- n 1)))))

;; return frequency list of n letter string (n-gram) of cipher-word list(cwl) taking lst as '() 

(define (ngrams cwl lst n)
  (cond [(null? cwl) lst]
        [(> n (string-length (car cwl))) (ngrams (cdr cwl) lst n)]
        [else (let* ([word (car cwl)]
                     [listword (string->list word)]
                     [nlet (list->string (n-terms-list listword n))]
                     [dellet (list->string (cdr listword))])
                (ngrams (cons dellet (cdr cwl)) (addtolist nlet lst) n))]))


(define (cipher-bigrams-list-freq cipher-word-list)
  (let* ([sorl (sort (ngrams cipher-word-list '() 2) compcons)])
    sorl))

;; Takes the cipher-word-list and produces a list of 2-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!


(define (cipher-bigrams cipher-word-list)
  (retlst (cipher-bigrams-list-freq cipher-word-list) '() ))









;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter. Only unique
;; neighbours are to be counted.
;; Consider the character #\o.
;;
;; Takes an argument `mode`:
;; 1. (cipher-unique-neighbourhood cipher-bigrams 'predecessor)
;;    - Count only those unique occurrences where the (given) char preceeds
;;      other char.
;;    - Patterns of the form: "o?"
;; 2. (cipher-unique-neighbourhood cipher-bigrams 'successor)
;;    - Count only those unique occurrences where the (given) char succeeds
;;      other char.
;;    - Patterns of the form: "?o"
;; 3. (cipher-unique-neighbourhood cipher-bigrams 'both)
;;    - Count all unique occurrences where the (given) char neighbours the other
;;      char.
;;    - Patterns of the form "?o" and "o?". Note that this is not sum of (1) and
;;    (2) always.
;;
;; The output is a list of pairs of cipher char and the count of it's
;; neighbours. The list must be in decreasing order of the neighbourhood count.


      




(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  (cond [(equal? 'predecessor mode) (sort (une cipher-bigrams-list ciphunne1 cadr) compcons)]
        [(equal? 'successor mode) (sort (une cipher-bigrams-list ciphunne1 car) compcons)]
        [else (sort (une cipher-bigrams-list ciphunne2 x) compcons)])
  )


;; takes a bigram frequency list and a character l and 0 and position (car for successor cadr for predecessor)
;; and returns number of unique neighbours of l

(define (ciphunne1 lst l c ad)
 (if (null? lst) c
     (if (eq? l (ad (string->list (car lst))))
       (ciphunne1 (cdr lst) l (+ c 1) ad)
       (ciphunne1 (cdr lst) l c ad))))

;; takes a bigram frequency list and a character l and 0 and anything (doesn't matter) and returns
;; and returns number of 'both unique neighbours

(define (ciphunne2 lst l c ad)
 (if (null? lst) c
     (if (or (eq? l (cadr (string->list (car lst)))) (eq? l (car (string->list (car lst)))))
       (ciphunne2 (cdr lst) l (+ c 1) ad)
       (ciphunne2 (cdr lst) l c ad))))

;; Higher order function for (bigram-list (f)ciphunne(1/2) and function (car/cadr) and
;; returns frequency list of unique neighbours

(define (une lst func ad) (lc (cons x (func lst x 0 ad)) : x <- (string->list "abcefghijklmnopqrstuvwxyz")))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter, but counts each
;; occurrence distinctly. This comment contains 6 bigrams with "w", all with "i" or "h".
;; So "w" has:
;; when mode is 'both,        6 neighbours
;; when mode is 'predecessor, 6 neighbours
;; when mode is 'successor,   0 neighbours
(define (cipher-neighbourhood cipher-word-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  (define cipher-bigrams-list (cipher-bigrams-list-freq cipher-word-list))
(cond [(equal? 'predecessor mode) (sort (une cipher-bigrams-list fciphunne1 cadr) compcons)]
        [(equal? 'successor mode) (sort (une cipher-bigrams-list fciphunne1 car) compcons)]
        [else (sort (une cipher-bigrams-list fciphunne2 x) compcons)])
  )


;; takes a bigram frequency list and a character l and 0 and position (car for successor cadr for predecessor)
;; and returns number of neighbours of l

(define (fciphunne1 lst l c ad)
 (if (null? lst) c
     (if (eq? l (ad (string->list (caar lst))))
       (fciphunne1 (cdr lst) l (+ c (cdar lst)) ad)
       (fciphunne1 (cdr lst) l c ad))))

;; takes a bigram frequency list and a character l and 0 and anything (doesn't matter) and returns
;; and returns number of 'both neigghbours

(define (fciphunne2 lst l c ad)
 (if (null? lst) c
     (if (or (eq? l (cadr (string->list (caar lst)))) (eq? l (car (string->list (caar lst)))))
       (fciphunne2 (cdr lst) l (+ c (cdar lst)) ad)
       (fciphunne2 (cdr lst) l c ad))))

;; Takes the cipher-word-list and produces a list of 3-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-trigrams cipher-word-list)
  (sort (ngrams cipher-word-list '() 3) compcons))

;; Takes the cipher-word-list and produces a list of 4-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-quadgrams cipher-word-list)
  (sort (ngrams cipher-word-list '() 4) compcons))

;; Takes the cipher word list and produces a list of single letter words, sorted
;; in decreasing order of frequency. Each element must be a string!


;; returns a frequency list of n-lettered word in cipher-word-list (cwl)  (list = '()

(define (nword cwl n lst)
(if (null? cwl) lst
    (if (= (string-length (car cwl)) n)
        (nword (cdr cwl) n (addtolist (car cwl) lst))
        (nword (cdr cwl) n lst))))

(define (cipher-common-words-single cipher-word-list)
    (retlst (sort (nword cipher-word-list 1 '()) compcons) '()))

;; Takes the cipher word list and produces a list of double letter words, sorted
;; in decreasing order of frequency.
 

(define (cipher-common-words-double cipher-word-list)
   (retlst (sort (nword cipher-word-list 2 '()) compcons) '()))

;; Takes the cipher word list and produces a list of triple letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-triple cipher-word-list)
   (retlst (sort (nword cipher-word-list 3 '()) compcons) '()))

;; Takes the cipher word list and produces a list of four letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-quadruple cipher-word-list)
   (retlst (sort (nword cipher-word-list 4 '()) compcons) '()))

;; Takes the cipher word list and produces a list of chars that appear at the
;; start of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-initial-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; end of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-final-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear as
;; consecutive double letters in some word, sorted in decreasing order of
;; frequency. Each element must thus be a char!
(define (cipher-common-double-letters cipher-word-list)
  '())
