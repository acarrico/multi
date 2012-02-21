#!/usr/bin/racket

;; AUTHOR: Anthony Carrico <acarrico@memebeam.org>

#lang racket

(require rackunit "multi.rkt")

;; A normal return:
(check-eq?
 (multi 'hello-world)
 'hello-world)

;; A normal return to multi:
(check-equal?
 (multi 'hello-world
        ((x) (cons 0 x))
        ((x) (cons 1 x)))
 (cons 0 'hello-world))

;; A multi return to multi:
(check-equal?
 (multi (multi 'hello-world 0)
        ((x) (cons 0 x))
        ((x) (cons 1 x)))
 (cons 0 'hello-world))

;; A multi return to multi second return point:
(check-equal?
 (multi (multi 'hello-world 1)
        ((x) (cons 0 x))
        ((x) (cons 1 x)))
 (cons 1 'hello-world))

;; A normal return through multi:
(check-eq?
 (multi 'hello-world
        0
        ((x) (cons 0 x))
        ((x) (cons 1 x)))
 'hello-world)

;; A multi return through multi:
(check-eq?
 (multi
  (multi 'hello-world 0)
  0
  ((x) (cons 0 x))
  ((x) (cons 1 x)))
 'hello-world)

;; Second return point to the first return point:
(check-equal?
 (multi (multi 'hello-world 1)
        ((x) (cons 0 x))
        0)
 'hello-world)

;; This failed in an early version, because the third multi was
;; evaluating inside the second multi's contiuation prompt:
(check-eq? (multi (multi 'ignore (_ (multi 'foo 1))) 0 0) 'foo)
;; This worked in that version:
(check-eq? (multi (multi 'foo 1) 0 0) 'foo)

;; Shivers/Fisher parsimonious filter example:
(define (filter f lis)
  (multi
   (let recur ((lis lis))
     (cond ((null? lis)
            (multi (values) 1))
           ((pair? lis)
            (let ((x (car lis))
                  (xs (cdr lis)))
              (if (f x)
                  (multi (recur xs)
                         ((ans) (cons x ans))
                         1)
                  (multi (recur xs)
                         0
                         (_ xs)))))))
   ;; First return point: output list is shorter than input list:
   0
   ;; Second return point: output list equals input list:
   (_ lis)))

(check-equal? '(2 4 6) (filter even? '(1 2 3 4 5 6)))

(let ((lis '(1 2 3 3 3 3)))
  ;; filter everything:
  (check-eq? '() (filter (lambda (x) #f) lis))
  ;; Share the whole list:
  (check-eq? lis (filter (lambda (x) #t) lis))
  ;; Filtering works:
  (check-equal? '(1 3 3 3 3) (filter odd? lis))
  ;; Share the longest tail:
  (check-eq? (cddr lis) (cdr (filter odd? lis)))
  )

;; Shivers/Fisher alternate three return point parsimonious filter challenge:
;;
;; I'm not sure how to interpret the phrase "output is a proper tail
;; of the input". I've implemented output is (cdr input) in that case.
;; I'm probably missing the point of this exercise, please enlighten
;; me.
(define (filter* f lis)
  (multi
   (let recur ((lis lis))
     (cond ((null? lis)
            (multi (values) 1))
           ((pair? lis)
            (let ((x (car lis))
                  (xs (cdr lis)))
              (if (f x)
                  (multi (recur xs)
                         (_ (multi (cons x (cdr xs)) 2))
                         1
                         ((ans) (multi (cons x ans) 2)))
                  (multi (recur xs)
                         (_ (multi (cdr xs) 2))
                         0
                         2))))))
   ;; output is a proper tail of the input:
   (_ (cdr lis))
   ;; output = input:
   (_ lis)
   ;; neither:
   0))

(check-equal? '(2 4 6) (filter* even? '(1 2 3 4 5 6)))

(let ((lis '(1 2 3 3 3 3)))
  ;; filter everything:
  (check-eq? '() (filter* (lambda (x) #f) lis))
  ;; Share the whole list:
  (check-eq? lis (filter* (lambda (x) #t) lis))
  ;; Filtering works:
  (check-equal? '(1 3 3 3 3) (filter* odd? lis))
  ;; Share the longest tail:
  (check-eq? (cddr lis) (cdr (filter* odd? lis)))
  )

;; ISSUE: should do tests with multiple values...
