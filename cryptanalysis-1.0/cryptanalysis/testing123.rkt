#lang racket

(require "list-comprehension.rkt")

(define freq (lc (cons x 0) : x <- (string->list "abcdefghijklmnopqrstuvwxyz")))

(define (addlet l lst)  
  (if (null? lst) '()
      (if (equal? (caar lst) l)
          (cons (cons l (+ 1 (cdar lst))) (cdr lst))
          (cons (car lst) (addlet l (cdr lst))))))


(define (cipher-mono cipherlst lst)  ;; returns a list of (cons char freq) call with freq
  (if (null? cipherlst)
      lst
      (cipher-mono (cdr cipherlst)(addlet (car cipherlst) lst))))