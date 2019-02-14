#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

(provide dictionary-closure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dictionary Closure                                                               ;;
;; ==================                                                               ;;
;;                                                                                  ;;
;; A choice of substitution can really trigger more substitutions by looking at the ;;
;; partially decrypted text - surely there will be some words which can be uniquely ;;
;; determined using the dictionary. It is prudent to concretize this "choice" as    ;;
;; this purely deterministic (involving absolutely no guess-work). In more          ;;
;; technical terms, this is called "maintaining arc-consistency" (look it up on     ;;
;; Wikipedia).                                                                      ;;
;;                                                                                  ;;
;; This function must utilise the dictionary and the cipher-word-list. Decrypt each ;;
;; word (`utils:decrypt`) and check if the dictionary has:                          ;;
;;                                                                                  ;;
;; 1. a unique completetion!                                                        ;;
;;    - Extend your key using the information with this match. Continue exploring   ;;
;;      the words under the extended key.                                           ;;
;; 2. many completions.                                                             ;;
;;    - Do nothing, just continue exploring the words under the same key. If none   ;;
;;      of the words fall into (1), return with the key that you have built so far. ;;
;; 3. no completions!                                                               ;;
;;    - Return `#f` (false), indicating that this partial key is wrong, and we must ;;
;;      revert to the original key.                                                 ;;
;;                                                                                  ;;
;; Returns either of:                                                               ;;
;; a. a (possibly) extended-key.                                                    ;;
;; b. `#f` if this key led to case (3) for some word.                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define keys (build-list 26 (lambda (_) #\_)))

;; Returns if there is unique substitution for cryptword to dicword

(define (compare-words cryptword dicword)
  (compare-word-list (string->list cryptword) (string->list dicword)))

(define (compare-word-list cryptword dicword)
  (if (not (= (length cryptword) (length dicword))) #f
      (cond [(null? cryptword) '()]
            [(eq? (car cryptword) (car dicword)) (compare-word-list (cdr cryptword) (cdr dicword))]
            [(char-lower-case? (car cryptword)) (let ([cmp (compare-word-list (cdr cryptword) (cdr dicword))]
                                                      [sub (cons (car dicword) (car cryptword))])
                                                  (if (boolean? cmp) cmp
                                                      (if (valid-substitution sub cmp)
                                                          (remove-duplicates (cons sub cmp))
                                                          #f)) )]
            [else #f])))

(define (valid-substitution sub sub-list)
  (if (null? sub-list) #t
   (cond [(equal? (cdr sub) (cdar sub-list))
          (if (equal? (car sub) (caar sub-list)) (valid-substitution sub (cdr sub-list)) #f)]
         [(equal? (car sub) (caar sub-list))
          (if (equal? (cdr sub) (cdar sub-list)) (valid-substitution sub (cdr sub-list)) #f)]

         [else (valid-substitution sub (cdr sub-list))])))


  


;; Checks if a list has a unique element

(define (unique-list lst)
  (cond [(null? lst) #t]
        [(= 1 (length lst)) #t]
        [else (unique-list-helper lst (car lst))]))

(define (unique-list-helper lst ele)
  (cond [(null? lst) #t]
        [(equal? ele (car lst)) (unique-list-helper (cdr lst) ele)]
        [else #f]))


(define (list-of-substitutions-h cipherword dict lst)
  (if (= (length lst) 2) lst
         (if (null? dict) lst
     (let ([comp (compare-words cipherword (car dict))])
       (if (boolean? comp) (list-of-substitutions-h cipherword (cdr dict) lst)
            (list-of-substitutions-h cipherword (cdr dict) (cons comp lst)))))))

(define (list-of-substitutions cipherword dict)

  (list-of-substitutions-h cipherword dict '()))


(define (possible-substitution cipherword dict)
  (let ([lst (list-of-substitutions cipherword dict)])
 (if (unique-list lst)
     (if (null? (list-of-substitutions cipherword dict)) 'nomatch
         (car (list-of-substitutions cipherword dict)))
     (cons #f lst))))


(define (partial-decrypt key cipherlst)
  (map (lambda (x) (utils:decrypt key x)) cipherlst))

(define (isupper word)
  (define (isupper-help wordlist)
    (if (null? wordlist) #t
        (if (char-lower-case? (car wordlist)) #f
            (isupper-help (cdr wordlist)))))
  (isupper-help (string->list word)))


(define (dictionary-attack cipherlist dict key ocipher)
  (if (null? cipherlist) key
      (if (isupper (utils:decrypt key (car cipherlist))) (begin (displayln (string-append (utils:decrypt key (car cipherlist)) " --> skipping this one"))
                                        (dictionary-attack (cdr cipherlist) dict key ocipher)) ;;All capitals
      (let ([poss (possible-substitution (utils:decrypt key (car cipherlist)) dict)])
        (if (equal? poss 'nomatch) #f
            (if (and (pair? poss) (equal? (car poss) #f))
                (begin
                  (displayln (let* ([lst (cdr poss)]
                                    [key1 (utils:add-substitution (car lst) key)]
                                    [word1 (utils:decrypt key1 (utils:decrypt key (car cipherlist)))]
                                    [key2 (utils:add-substitution (cadr lst) key)]
                                    [word2 (utils:decrypt key2 (utils:decrypt key (car cipherlist)))])
                               (string-append (utils:decrypt key (car cipherlist)) " --> multiple matches ("
                                              word1 " " word2 " ...)")))
                  (dictionary-attack (cdr cipherlist) dict key ocipher))
                (if (null? poss) (begin (displayln (string-append (utils:decrypt key (car cipherlist)) " --> skipping this one"))
                                        (dictionary-attack (cdr cipherlist) dict key ocipher))
                    (if (utils:is-monoalphabetic? poss key)
                        (begin (let* ([lst (list-of-substitutions (utils:decrypt key (car cipherlist)) dict)]
                                      [key1 (utils:add-substitution (car lst) key)]
                                      [word1 (utils:decrypt key1 (utils:decrypt key (car cipherlist)))])
                                 (displayln (string-append (utils:decrypt key (car cipherlist)) " --> unique match   " word1)))
                               
                               (let ([newkey (utils:add-substitution poss key)])
                                 (dictionary-attack ocipher dict newkey ocipher))
                               ) #f))))))))

        






(define (dictionary-closure key)
  (dictionary-attack  utils:cipher-word-list utils:dictionary key utils:cipher-word-list))

(dictionary-closure keys)