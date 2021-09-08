(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  'replace-this-line)

(define (zip pairs)
  'replace-this-line)


;;
;; Returns a list of two-element lists
(define (enumerate s)
  ;
  (define (helper s num)
    (if (equal? s nil)
      nil
      (cons (cons num (cons (car s) nil)) (helper (cdr s) (+ num 1)))
    )
  )
  (helper s 0)
  )
  ;

;;
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ;
  (if (equal? denoms nil)
      nil
      (cond
          ((equal? 0 total) '(nil))
          ((> (car denoms) total) (list-change total (cdr denoms)))
          (else (append
                  ((lambda (first_elem rest_lst) (map (lambda (lst) (cons first_elem lst)) rest_lst))
                      (car denoms)
                      (list-change (- total (car denoms)) denoms))
                  (list-change total (cdr denoms))
                )
          )
      )
  )
)
  ;

;;
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ;
         expr
         ;
         )
        ((quoted? expr)
         ;
         expr
         ;
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ;
           (cons form (cons params (let-to-lambda body)))
           ;
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ;
           (append (cons (cons 'lambda (cons (car ((lambda (x)(list (map car x)(map (lambda (x) (car (cdr x))) x))) values)) (let-to-lambda body))) nil)
                   (car (cdr ((lambda (x)(list (map car x)(map (lambda (x) (car (cdr x))) x))) (let-to-lambda values))))
            )
           ;
           ))
        (else
         ;
         (map let-to-lambda expr)
         ; 
         )))
