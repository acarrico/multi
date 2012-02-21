#lang racket

;; AUTHOR: Anthony Carrico <acarrico@memebeam.org>

(provide multi)

(begin-for-syntax
 (define (syntax->source-string s)
   (format "~a line ~a column ~a" (syntax-source s) (syntax-line s) (syntax-column s))))

(define-syntax multi
  (lambda (<exp>)
    (syntax-case <exp>
      ()
      ;; No return points:
      ((_ <e>)
       ;; How to get rid of the old return points? Just ignore them for now:
       #'<e>)
      ;; SHOULD OPTIMIZE THE CASE WHERE THERE ARE NO LAMBDA RPS TO GET
      ;; RID OF PROMPT.
      ((_ <e> <r> ...)
       #`(call-with-immediate-continuation-mark
          'multi-return
          (lambda (old-return-points)
            (let* ((prompt-tag (make-continuation-prompt-tag 'return-point))
                   (return-points (vector (expand-return-point old-return-points prompt-tag <r>) ...)))
              (call-with-continuation-prompt
               (lambda ()
                 (call-with-values
                     (lambda () (with-continuation-mark 'multi-return return-points <e>))
                   (vector-ref return-points 0)))
               prompt-tag
               (lambda (thunk) (thunk))))))))))

(define-syntax expand-return-point
  (lambda (<exp>)
    (syntax-case <exp>
        ()
      ((_ <old-return-points> <prompt-tag> 0)
       #'(if (and <old-return-points> (< 0 (vector-length <old-return-points>)))
             (vector-ref <old-return-points> 0)
             ;; Without return points, zero is just a normal return, so drop through:
             (lambda args
               (abort-current-continuation <prompt-tag> (lambda () (apply values args))))))
      ((_ <old-return-points> <prompt-tag> <i>)
       (and (integer? (syntax->datum #'<i>)) (positive? (syntax->datum #'<i>)))
       #`(return-point-ref <old-return-points> <i> #,(syntax->source-string #'<i>)))
      ((_ <old-return-points> <prompt-tag> (<args> <body> ...))
       #'(lambda <args>
           (abort-current-continuation <prompt-tag> (lambda () <body> ...)))))))

(define (return-point-ref return-points index src)
  (if (and return-points (< index (vector-length return-points)))
      (vector-ref return-points index)
      (let ((rp-count (if return-points (vector-length return-points) 1)))
        (error
         (format "multi: no such return point index ~a in ~a, there ~a ~a return point~a."
                 index src (if (= rp-count 1) "is" "are") rp-count (if (= rp-count 1) "" "s"))
         return-points))))
