#lang racket

;; You can require more modules of your choice.
(require racket/list
         (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt"))

(require "list-comprehension.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                     ;;
;; Strategies                                                                          ;;
;; ==========                                                                          ;;
;; For the purpose of this assignment, just the `etai` strategy is expected, since     ;;
;; we have dictionary-closure and secret-word-enumeration to leap-frog to the right    ;;
;; key. This technique would fail for harder keys which are arbitrary permutations of  ;;
;; the alphabet. We will be forced to explore many more strategies (along with         ;;
;; dictionary-closure of course).                                                      ;;
;;                                                                                     ;;
;; Strategies to guess substitutions for the key using statistical information about   ;;
;; - the English language from utils.rkt                                               ;;
;; - the cipher text      from statistics.rkt                                          ;;
;;                                                                                     ;;
;; Follow the function signature as indicated below. Deviations will make it           ;;
;; impossible for automatic grading of your submission.                                ;;
;; Moreover, we do not expect your strategies to require any more/different            ;;
;; arguments. Note that you recieve the key as argument, so you can at the very        ;;
;; least ensure that all the substitutions are monoalphabetic wrt this key.            ;;
;;                                                                                     ;;
;; Signature:                                                                          ;;
;; ```                                                                                 ;;
;; (define (my-fundoo-strategy key)                                                    ;;
;;   ;; Make use of `utils:ciphertext`, `utils:cipher-word-list`                       ;;
;;   ...)                                                                              ;;
;; ```                                                                                 ;;
;;                                                                                     ;;
;; Substitutions                                                                       ;;
;; -------------                                                                       ;;
;; In order to extend the key incrementally, we use `utils:add-substitution` to        ;;
;; extend a given key with a substitution.                                             ;;
;;                                                                                     ;;
;; A substitution is a list of pairs, each pair mapping a plaintext char to a          ;;
;; ciphertext char. For example, to extend the key with T -> a and O -> r              ;;
;; (simultaneously), we use the substitution:                                          ;;
;; ```                                                                                 ;;
;; (list (cons #\T #\a) (cons #\O #\r))                                                ;;
;; ```                                                                                 ;;
;; For a single substitution use a singleton list (containing just one pair).          ;;
;;                                                                                     ;;
;; **CAUTION**                                                                         ;;
;; -----------                                                                         ;;
;; 1. Note that add-substitution does not do sanity checks on the substitution and use ;;
;;    of `utils:is-monoalphabetic` is recommended to ensure that you don't             ;;
;;    inadvertently create invalid keys.                                               ;;
;; 2. You must provide a list called `compositions` in this module.                    ;;
;;                                                                                     ;;
;; See docs in "utils.rkt" for more information.                                       ;;
;;                                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You must add "public" functions of this module to this list.
(provide etai
         ;; Some more suggested strategies:
         
         ;; common-words-double
         ;; bigrams
         ;; common-initial-letters
         ;; common-final-letters
         ;; common-words-triple
         ;; trigrams
         ;; common-double-letters
         ;; common-words-quadruple
         ;; quadgrams
         
         ;; lists of strategies
         composition)

;; A strategy that uses some statistical information to generate potential
;; substitutions for E, T, A and I.
;; Refer the assignment manual for tips on developing this strategy. You can
;; interact with our etai with the executable we provide.

(define keys (build-list 26 (lambda (_) #\_)))

(define (init-key key)
  (list (cons #\E (list-ref key 4)) (cons #\T (list-ref key 19)) (cons #\A (list-ref key 0)) (cons #\I (list-ref key 8))))

(define (make-ai lst)
  (lc (append (list (cons #\A x)) (list (cons #\I y))) : x <- lst y <-(remove x lst)))
  

(define (nletters lst n)
  (if (null? lst) '()
  (if (= n 0) '()
      (cons (car lst) (nletters (cdr lst) (- n 1))))))


(define (leastt lst lst2)
  ( if (equal? (member (caar lst2) lst) #f) (leastt lst (cdr lst2))
       (member (caar lst2) lst)))


(define (removen lst lst2 n)
  (if (null? lst2)
      lst
      (if (= n 0) lst
      (removen (remove (car lst2) lst) (cdr lst2) (- n 1))))) 
      
(define (conv lst)
  (if (null? lst) '()
      (cons (string-ref (car lst) 0) (conv (cdr lst)))))



(define (etai key)
  (let*  ( [init-etai (init-key key)]
           [text utils:ciphertext]
           [wordlist utils:cipher-word-list]
           [sing  (conv (stats:cipher-common-words-single wordlist))]
           [fiveletters (nletters (stats:cipher-monograms text) 5)]
           [rem5let (removen fiveletters sing 2)]
           [probt (car (leastt rem5let (reverse (stats:cipher-unique-neighbourhood (stats:cipher-bigrams wordlist) 'both))))]
           [rest (cons probt  (remove probt rem5let))] )
    (cond [(> (length sing) 1)
            (lc (list (cons #\E w) (cons #\T x) (cons #\A y) (cons #\I z)) : y <- (what-to-sub (nletters sing 2) key 0)
                                                                             z <- (what-to-sub (remove y (nletters sing 2)) key 8)
                                                                             w <- (what-to-sub rem5let key 4)
                                                                             x <- (what-to-sub (remove w rest) key 19)
                                                                             )]
           [(= (length sing) 1)
            (lc (list (cons #\E w) (cons #\T x) (cons #\A y) (cons #\I z)) : y <- (what-to-sub (cons (car sing) (rem5let)) key 0)
                                                                             z <- (what-to-sub (if (= y (car sing)) (reverse rem5let)
                                                                                                       (car sing)) key 8)
                                                                             w <- (what-to-sub (remove y (remove z rem5let)) key 4)
                                                                             x <- (what-to-sub (remove y (remove z (remove x rest))) key 19)
                                                                             )]
           [else (lc (list (cons #\E w) (cons #\T x) (cons #\A y) (cons #\I z)) : w <- (what-to-sub rem5let key 4)
                                                                                  x <- (what-to-sub (remove w rest) key 19)
                                                                                 
                                                                                  y <- (what-to-sub (remove x (remove w (reverse rem5let))) key 0)
                                                                                  z <- (what-to-sub (remove x (remove y (remove w rem5let))) key 8))]
    
           
      )))

(define (what-to-sub lst key i)
  (if (equal? #\_ (list-ref key i)) lst (list (list-ref key i))))

;; A suggested composition of strategies that might work well. Has not been
;; exhaustively tested by us. Be original ;)
(define composition (list etai))
                  ;; common-words-double
                  ;; bigrams
                  ;; common-initial-letters
                  ;; common-final-letters
                  ;; common-words-triple
                  ;; trigrams
                  ;; common-double-letters))
                  ;; common-words-quadruple
                  ;; quadgrams))

