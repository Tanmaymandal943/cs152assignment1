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

;; Takes ciphertext and produces a list of cipher chars sorted in decreasing
;; order of frequency.
(define (cipher-monograms ciphertext)
  (let* ([st (string->list ciphertext)]
         [frel (cipher-mono st '())]
         [sorfrel (sorted-cipherlst frel)]
         )
    (retlst sorfrel '())
         ))

;;(define freq (lc (cons x 0) : x <- (string->list "abcdefghijklmnopqrstuvwxyz")))

(define (addlet l lst)  
  (addtolist l lst))


(define (cipher-mono cipherlst lst)  ;; returns a list of (cons char freq) call with freq
  (if (null? cipherlst)
      lst
      (cipher-mono (cdr cipherlst)(addlet (car cipherlst) lst))))

(define (compcons a b)
  (> (cdr a) (cdr b)))

(define (sorted-cipherlst lst)
  (sort lst compcons))

(define (retlst lst l)
  (if (null? lst) l
      (retlst (cdr lst) (append l (list (caar lst))))))


;; Takes the cipher-word-list and produces a list of 2-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-bigrams cipher-word-list)
  (let* ([sorl (sort (cipbi cipher-word-list '()) compcons)])
     sorl))


(define (cipbi cwl lst)
  (cond [(null? cwl) lst]
        [(= 1 (string-length (car cwl))) (cipbi (cdr cwl) lst)]
        [else (let* ([word (car cwl)]
                     [listword (string->list word)]
                     [twolet (list->string (list (car listword) (cadr listword)))]
                     [dellet (list->string (cdr listword))])
                (cipbi (cons dellet (cdr cwl)) (addlet twolet lst)))]))

(define (striplist lst)
  (if (= 0 (cdar lst)) '() (cons (car lst) (striplist (cdr lst)))))

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

(define (addtolist x lst)
  (if (null? lst)
      (list (cons x 1))
      (if (equal? (caar lst) x) (cons (cons x (+ 1 (cdar lst))) (cdr lst))
          (cons (car lst) (addtolist x (cdr lst))))))
      




(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  (cond [(equal? 'predecessor mode) (une cipher-bigrams-list ciphunne1 cadr)]
        [(equal? 'successor mode) (une cipher-bigrams-list ciphunne1 car)]
        [else (une cipher-bigrams-list ciphunne2 x)])
  )

(define (ciphunne1 lst l c ad)
 (if (null? lst) c
     (if (eq? l (ad (string->list (caar lst))))
       (ciphunne1 (cdr lst) l (+ c 1) ad)
       (ciphunne1 (cdr lst) l c ad))))

(define (ciphunne2 lst l c ad)
 (if (null? lst) c
     (if (or (eq? l (cadr (string->list (caar lst)))) (eq? l (car (string->list (caar lst)))))
       (ciphunne2 (cdr lst) l (+ c 1) ad)
       (ciphunne2 (cdr lst) l c ad))))


(define (une lst func ad) (lc (cons x (func lst x 0 ad)) : x <- (string->list "abcefghijklmnopqrstuvwxyz")))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter, but counts each
;; occurrence distinctly. This comment contains 6 bigrams with "w", all with "i" or "h".
;; So "w" has:
;; when mode is 'both,        6 neighbours
;; when mode is 'predecessor, 6 neighbours
;; when mode is 'successor,   0 neighbours
(define (cipher-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
(cond [(equal? 'predecessor mode) (une cipher-bigrams-list fciphunne1 cadr)]
        [(equal? 'successor mode) (une cipher-bigrams-list fciphunne1 car)]
        [else (une cipher-bigrams-list fciphunne2 x)])
  )

(define freq '())

(define (fciphunne1 lst l c ad)
 (if (null? lst) c
     (if (eq? l (ad (string->list (caar lst))))
       (ciphunne1 (cdr lst) l (+ c (cdar lst)) ad)
       (ciphunne1 (cdr lst) l c ad))))

(define (fciphunne2 lst l c ad)
 (if (null? lst) c
     (if (or (eq? l (cadr (string->list (caar lst)))) (eq? l (car (string->list (caar lst)))))
       (ciphunne2 (cdr lst) l (+ c (cdar lst)) ad)
       (ciphunne2 (cdr lst) l c ad))))

;; Takes the cipher-word-list and produces a list of 3-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-trigrams cipher-word-list)
  '())

;; Takes the cipher-word-list and produces a list of 4-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-quadgrams cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of single letter words, sorted
;; in decreasing order of frequency. Each element must be a string!


(define (nword cwl n lst)
(if (null? cwl) lst
    (if (= (string-length (car cwl)) n)
        (nword (cdr cwl) n (addlet (car cwl) lst))
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
