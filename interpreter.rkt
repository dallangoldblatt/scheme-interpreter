#lang racket
(require "simpleParser.rkt")

; Entrypoint into the interpreter
(define interpret
  (lambda (file)
    (S-lookup 'return (M-state-statement-list (parser file) (S-new)))))


;;;; STATEMENT FORMATS
;; -----------------------------------------------------------------------
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


;; -----------------------------------------------------------------------
;;;; The following prefixes have been used to distinguish abstaction functions
;; Condtional: C-
;; State:      S-


;;;; STATEMENT LIST
;; -----------------------------------------------------------------------

; Calculate the value of a statement list
; TODO A statement list has no value?
(define M-value-statement-list '
  (lambda (statement-list state)
    '()))
    
; Calculate the state resulting from a statement list
(define M-state-statement-list
  (lambda (statement-list state) state
    (cond
      ((null? statement-list) state)
      (else (M-state-statement-list (remaining-statements statement-list) (M-state-statement (first-statement statement-list) state))))))

; Statement abstractions
(define first-statement-type caar)
(define first-statement car)
(define remaining-statements cdr)


;;;; STATEMENT
;; -----------------------------------------------------------------------
; Calculate the value or resulting state of a particular statement

; The following statement types should have a return value:
;   declaration, assignment, return

; Calcualte the state resulting from a genereric statement
(define M-state-statement
  (lambda (statement state)
    (cond
      ((eq? 'return (statement-type statement)) (M-state-return statement state))
      ((eq? 'var (statement-type statement)) (M-state-declare statement state))
      ((eq? '= (statement-type statement)) (M-state-assign statement state))
      (else state))))

; Calculate the state resulting from a return statement
(define M-state-return
  (lambda (statement state)
    (S-assign 'return (M-value-expression (return-expression statement) state) state)))

; Calculate the state resulting from a declare statement
(define M-state-declare
  (lambda (statement state)
    (if (declare-has-assignment statement)
        (S-assign (declare-name statement) (M-value-expression (declare-expression statement) state) state)
        (S-assign (declare-name statement) 0 state)))) ; TODO initialize vars to 0?

; Calculate the state resulting from an assign statement
(define M-state-assign
  (lambda (statement state)
    (S-assign (assign-name statement) (M-value-expression (assign-expression statement) state) state)))

; Statement abstractions
(define statement-type car)
(define return-expression cadr)
(define declare-name cadr)
(define declare-expression caddr)
(define assign-name cadr)
(define assign-expression caddr)

(define declare-has-assignment
  (lambda (statement)
    (not (null? (cddr statement)))))


;;;; CONDITIONAL
;; -----------------------------------------------------------------------
;; Conditionals return 'true or 'false and connect one or more comparisons

; test with: (M-value-conditional '(&& (!= 3 5) (< 3 4)) '())
(define M-value-conditional
  (lambda (conditional state)
    (cond
      ((null? conditional) (error "Null parameter passed to M-value-conditional"))
      ((eq? '&& (connective conditional)) (C-and (M-value-conditional (leftoperand conditional) state)
                                                 (M-value-conditional (rightoperand conditional) state)))
      ((eq? '|| (connective conditional)) (C-or (M-value-conditional (leftoperand conditional) state)
                                                (M-value-conditional (rightoperand conditional) state)))
      ((eq? '! (connective conditional)) (C-not (M-value-conditional (leftoperand conditional) state)))
      (else (M-value-comparison conditional state)))))

; A comparison will not change the state (for now)
(define M-state-conditional
  (lambda (expression state) state))


;;;; COMPARISON
;; -----------------------------------------------------------------------
;; Comparisons return 'true or 'false and compare expressions

(define M-value-comparison
  (lambda (comparison state)
    (cond
      ((null? comparison) (error "Null parameter passed to M-value-comparison"))
      ((eq? 'true comparison) 'true)
      ((eq? 'false comparison) 'false)
      ((eq? '< (comparator comparison)) (C-< (M-value-expression (leftoperand comparison) state)
                                             (M-value-expression (rightoperand comparison) state)))
      ((eq? '> (comparator comparison)) (C-> (M-value-expression (leftoperand comparison) state)
                                             (M-value-expression (rightoperand comparison) state)))
      ((eq? '== (comparator comparison)) (C-== (M-value-expression (leftoperand comparison) state)
                                               (M-value-expression (rightoperand comparison) state)))
      ((eq? '<= (comparator comparison)) (C-<= (M-value-expression (leftoperand comparison) state)
                                               (M-value-expression (rightoperand comparison) state)))
      ((eq? '>= (comparator comparison)) (C->= (M-value-expression (leftoperand comparison) state)
                                               (M-value-expression (rightoperand comparison) state)))
      ((eq? '!= (comparator comparison)) (C-!= (M-value-expression (leftoperand comparison) state)
                                               (M-value-expression (rightoperand comparison) state)))
      (else (error 'badop "The comparator is unknown")))))

; A comparison will not change the state (for now)
(define M-state-comparison
  (lambda (comparison state) state))


;;;; EXPRESSION
;; -----------------------------------------------------------------------
;; Expressions are numerical calulations and boolean values
    
; Calculate the value of a mathematical expression
(define M-value-expression
  (lambda (expression state)
    (cond
      ((null? expression) (error "Null parameter passed to M-value-expression"))
      ((number? expression) expression)
      ((true-or-false? expression) expression)
      ((atom? expression) (S-lookup expression state))
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

; Conditional, comparison, and expression abstractions
(define comparator car)
(define connective car)
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

(define C-and
  (lambda (loperand roperand)
    (true? (and (eq? loperand 'true) (eq? roperand 'true)))))
(define C-or
  (lambda (loperand roperand)
    (true? (or (eq? loperand 'true) (eq? roperand 'true)))))
(define C-not
  (lambda (operand)
    (true? (eq? operand 'false))))
(define C-<
  (lambda (loperand roperand)
    (true? (< loperand roperand))))
(define C->
  (lambda (loperand roperand)
    (true? (> loperand roperand))))
(define C-==
  (lambda (loperand roperand)
    (true? (= loperand roperand))))
(define C-<=
  (lambda (loperand roperand)
    (true? (<= loperand roperand))))
(define C->=
  (lambda (loperand roperand)
    (true? (>= loperand roperand))))
(define C-!=
  (lambda (loperand roperand)
    (true? (not (= loperand roperand)))))


;;;; State functions
;; -----------------------------------------------------------------------

; Return a new state with an empty return variable
(define S-new
  (lambda () '((return 0)))) ; TODO is this hard-coded?

; Search for a variable by name in the state and returns its value
; Creates an error if the state does not contain the variable
(define S-lookup
  (lambda (variable state)
    (cond 
      ((null? state) (error "Variable not found in state"))
      ((eq? variable (first-var state)) (first-val state))
      (else (S-lookup variable (cdr state))))))
  
; Update the value of a variable in the state
; If it does not exist, add it to the state
(define S-assign
  (lambda (variable value state)
    (cond 
      ((null? state) (S-add variable value '()))
      ((eq? variable (first-var state)) (S-add variable value (S-remove variable state)))
      (else (cons (first-binding state) (S-assign variable value (cdr state)))))))

; Add a new variable and value to the state
(define S-add
  (lambda (variable value state)
    (cons (list variable value) state)))

; Remove a variable from the state
; If the variable is already not present, no error is created
(define S-remove
  (lambda (variable state)
    (cond 
      ((null? state) '())
      ((eq? variable (first-var state)) (cdr state))
      (else (cons (first-binding state) (S-remove variable (cdr state)))))))

(define first-var caar)
(define first-val cadar)
(define first-binding car)

  
;; Utility functions
;; -----------------------------------------------------------------------

; Determine if x is an atom
(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

; Determine if x is 'true or 'false
(define true-or-false?
  (lambda (x)
    (or (eq? x 'true) (eq? x 'false))))

; Convert a #t and #f to 'true and 'false
(define true?
  (lambda (condition)
    (if condition
        'true
        'false)))