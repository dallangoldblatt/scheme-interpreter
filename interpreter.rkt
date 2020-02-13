#lang racket
(require "simpleParser.rkt")

(define interpret
  (lambda (file)
    (M-value-statement-list (parser file) (new-state))))

;;;; STATEMENT FORMATS
;; Atoms in UPPERCASE implement M-state and M-value
;; Symbols and atoms in lowercase are part of the language

;; Variable declaration
; (var VARIABLE)
; (var VARIABLE EXPRESSION)

;; Assignment
; (= VARIABLE EXPRESSION)

;; Return
; (return EXPRESSION)

;; If statement
; (if CONDITIONAL then STATEMENT)
; (if CONDITIONAL then STATEMENT else STATEMENT)

;; While statement
; (while CONDITIONAL STATEMENT)


;; STATEMENT LIST

; Calculate the value of a statement list
(define M-value-statement-list
  (lambda (statement-list state)
    (cond
      ((null? statement-list) '())
      ((eq? 'return (first-statement-type statement-list)) (M-value-statement (first-statement statement-list) state))
      (else (M-value-statement-list (remaining-statements statement-list) (M-state-statement (first-statement statement-list) state))))))
    
; Calculate the state resulting from a statement list
(define M-state-statement-list
  (lambda (statement-list state) state)) ; TODO

; Statement abstractions
(define first-statement-type caar)
(define first-statement car)
(define remaining-statements cdr)


;; STATEMENT
    
; Calculate the value of a statement
(define M-value-statement
  (lambda (statement state)
    (cond
      ((eq? 'return (statement-type statement)) (M-value-expression (return-expression statement) state)))))

; Calculate the state resulting from a statement
(define M-state-statement
  (lambda (statement state) state)) ; TODO

; Statement abstractions
(define statement-type car)
(define return-expression cadr)


;; CONDITIONAL
; test with: (M-value-conditional '(&& (!= 3 5) (< 3 4)) '())
(define M-value-conditional
  (lambda (conditional state)
    (cond
      ((null? conditional) (error "Null parameter passed to M-value-conditional"))
      ((eq? '&& (connective conditional)) (c-and (M-value-conditional (leftoperand conditional) state)
                                                           (M-value-conditional (rightoperand conditional) state)))
      ((eq? '|| (connective conditional)) (c-or (M-value-conditional (leftoperand conditional) state)
                                                          (M-value-conditional (rightoperand conditional) state)))
      ((eq? '! (connective conditional)) (c-not (M-value-conditional (leftoperand conditional) state)))
      (else (M-value-comparison conditional state)))))

; A comparison will not change the state (for now)
(define M-state-conditional
  (lambda (expression state) state))


;; COMPARISON

(define M-value-comparison
  (lambda (comparison state)
    (cond
      ((null? comparison) (error "Null parameter passed to M-value-comparison"))
      ((eq? 'true comparison) 'true)
      ((eq? 'false comparison) 'false)
      ((eq? '< (comparator comparison)) (c-< (M-value-expression (leftoperand comparison) state)
                                           (M-value-expression (rightoperand comparison) state)))
      ((eq? '> (comparator comparison)) (c-> (M-value-expression (leftoperand comparison) state)
                                           (M-value-expression (rightoperand comparison) state)))
      ((eq? '== (comparator comparison)) (c-== (M-value-expression (leftoperand comparison) state)
                                             (M-value-expression (rightoperand comparison) state)))
      ((eq? '<= (comparator comparison)) (c-<= (M-value-expression (leftoperand comparison) state)
                                             (M-value-expression (rightoperand comparison) state)))
      ((eq? '>= (comparator comparison)) (c->= (M-value-expression (leftoperand comparison) state)
                                             (M-value-expression (rightoperand comparison) state)))
      ((eq? '!= (comparator comparison)) (c-!= (M-value-expression (leftoperand comparison) state)
                                             (M-value-expression (rightoperand comparison) state)))
      (else (error 'badop "The comparator is unknown")))))

; A comparison will not change the state (for now)
(define M-state-comparison
  (lambda (comparison state) state))

;; EXPRESSION
    
; Calculate the value of a mathematical expression
(define M-value-expression
  (lambda (expression state)
    (cond
      ((null? expression) (error "Null parameter passed to M-value-expression"))
      ((number? expression) expression)
      ((atom? expression) (lookup expression state))
      ((eq? '+ (operator expression)) (+ (M-value-expression (leftoperand expression) state)
                                         (M-value-expression (rightoperand expression) state)))
      ((eq? '- (operator expression)) (- (M-value-expression (leftoperand expression) state)
                                         (M-value-expression (rightoperand expression) state)))
      ((eq? '* (operator expression)) (* (M-value-expression (leftoperand expression) state)
                                         (M-value-expression (rightoperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (M-value-expression (leftoperand expression) state)
                                                (M-value-expression (rightoperand expression) state)))
      ((eq? '% (operator expression)) (modulo (M-value-expression (leftoperand expression) state)
                                              (M-value-expression (rightoperand expression) state)))
      (else (error 'badop "The operator is unknown")))))

; A mathematical expression will not change the state (for now)
(define M-state-expression
  (lambda (expression state) state))

; Expression and Conditional abstractions
(define comparator car)
(define connective car)
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

; TODO make this call a utility function
(define c-and
  (lambda (loperand roperand)
    (if (and (eq? loperand 'true) (eq? roperand 'true))
        'true
        'false)))
(define c-or
  (lambda (loperand roperand)
    (if (or (eq? loperand 'true) (eq? roperand 'true))
        'true
        'false)))
(define c-not
  (lambda (operand)
    (if (eq? operand 'false)
        'true
        'false)))
(define c-<
  (lambda (loperand roperand)
    (if (< loperand roperand)
        'true
        'false)))
(define c->
  (lambda (loperand roperand)
    (if (> loperand roperand)
        'true
        'false)))
(define c-==
  (lambda (loperand roperand)
    (if (= loperand roperand)
        'true
        'false)))
(define c-<=
  (lambda (loperand roperand)
    (if (<= loperand roperand)
        'true
        'false)))
(define c->=
  (lambda (loperand roperand)
    (if (>= loperand roperand)
        'true
        'false)))
(define c-!=
  (lambda (loperand roperand)
    (if (not (= loperand roperand))
        'true
        'false)))

     
;; STATE
    
; State functions
(define new-state
  (lambda () '())) ; TODO is this hard-coded?

(define lookup
  (lambda (variable state)
    (car (car state))))  ; TODO

(define add
  (lambda (variable value state)
    (cons '(variable value) state))) ; TODO

(define remove
  (lambda (variable state)
    (cons '(variable value) state)))  ; TODO

; Utility functions
(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))