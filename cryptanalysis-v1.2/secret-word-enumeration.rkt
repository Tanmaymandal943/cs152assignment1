#lang racket

;; You can require more modules of your choice.
(require racket/string
         racket/list
         (prefix-in utils: "utils.rkt")
         "list-comprehension.rkt")

(provide secret-word-enumeration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                           ;;
;; Secret Word Enumeration                                                                   ;;
;; =======================                                                                   ;;
;;                                                                                           ;;
;; This step exploits the fact that all keys begin with a secret word that is a              ;;
;; proper SIX-LETTER word from the English dictionary.                                       ;;
;;                                                                                           ;;
;; Given a partial key, we can query the dictionary for a list of 6 letter words             ;;
;; that can potentially begin the key.                                                       ;;
;; We can then filter out potential candidates whose keys do not agree with our partial key. ;;
;;                                                                                           ;;
;; It is possible that given a (wrong) partial key, we discover that none of these           ;;
;; potential candidates can agree with our partial key. This really implies a                ;;
;; mistake in the choice of substitution, since dictionary-closure is completely             ;;
;; deterministic (modulo any bugs in your implementation :)                                  ;;
;;                                                                                           ;;
;; Hence, this function must return either of:                                               ;;
;; a. `#f` if there are no consistent candidates for this key.                               ;;
;; b. the original key if there are multiple consistent candidates.                          ;;
;; c. the complete key if there's only one consistent candidate!                             ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (comp-word-list keyword dictword)
  (if (null? keyword) '()
      (let ([nextr     (comp-word-list (cdr keyword) (cdr dictword))])
        (if (equal? #f nextr) #f
            (if (equal? (car keyword) #\_)
                (if (member (car dictword) nextr) #f
                    (cons (car dictword) nextr))
                (if (equal? (car keyword) (car dictword))
                    (cons (car keyword) (comp-word-list (cdr keyword) (cdr dictword)))
                    #f))))))

(define dictionary  (remove-duplicates (lc (string-downcase x) : x <- utils:dictionary)))

(define (comp-word keyword dicword)
  (let ([dictword (string->list dicword)]) 
  (if (= (length keyword) (length dictword))
      (comp-word-list keyword dictword)
      #f)))


(define (valid-possible-key part-key complete-key)
  (if (null? part-key) '()
      (let ([v (valid-possible-key (cdr part-key) (cdr complete-key))])
        (if (equal? v #f) #f
            (if (equal? (car part-key) (car complete-key))
                (cons (car part-key) v)
                (if (equal? (car part-key) #\_)
                    
                      (if (member (car complete-key) v) #f (cons (car complete-key) v))
                    #f))))))

(define (dict-attact keyword dict lst key)
  (if (null? dict) lst
      (if (= (length lst) 2) lst
          (let ([comp (comp-word keyword (car dict))])
            (if (equal? #f comp) (dict-attact keyword (cdr dict) lst key)
                (if (list? (valid-possible-key key (utils:encryption-key (car dict))))
                     (dict-attact keyword (cdr dict) (cons comp lst) key)
                     (dict-attact keyword (cdr dict) lst key)))))))

(define (nletters lst n)
  (if (null? lst) '()
  (if (= n 0) '()
      (cons (car lst) (nletters (cdr lst) (- n 1))))))

 

    
  
(define (secret-word-enumeration key-after-dictionary-closure) ;; Returns a key or false (#f)
  (let ([listkeys (dict-attact (nletters key-after-dictionary-closure 6) dictionary '() key-after-dictionary-closure)])
    (cond [(= 0 (length listkeys)) #f]
          [(= 1 (length listkeys)) (utils:encryption-key (list->string (car listkeys)))]
          [else key-after-dictionary-closure])))
