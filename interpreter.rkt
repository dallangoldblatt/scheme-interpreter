#lang racket

(require "simpleParser.rkt")


;;;; Interpreter, Part 1
;; -----------------------------------------------------------------------
; Dallan Goldblatt
; Robbie Dozier
; Ethan Voss


; Entrypoint into the interpreter
(define interpret
  (lambda (file)
    (S-lookup 'return (M-state-statement-list (parser file) (S-new)))))


;;;: M-value and M-state definitions
;; -----------------------------------------------------------------------
; Since variables can take both integer and boolean valules, M-value functions can return both:
;     M-value: statement X state -> {integer, 'true, 'false, error}
;
; The M-state functions return the state resulting from a statement
;     M-state: statement X state -> {state, error}
;
; state is stored as a list of tupleas in the format (name value):
;     ((x 12) (y 4) ...);


;;;; STATEMENT FORMATS
;; -----------------------------------------------------------------------
;; Atoms in UPPERCASE can implement M-state and M-value
;; Symbols and atoms in lowercase are part of the language

;; Variable declaration
; (var VARIABLE)
; (var VARIABLE EXPRESSION)
; (var VARIABLE CONDITIONAL)

;; Assignment
; (= VARIABLE EXPRESSION)
; (= VARIABLE CONDTIONAL)

;; Return
; (return EXPRESSION)
; (return CONDITIONAL)

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
    
; Calculate the state resulting from a statement list
(define M-state-statement-list
  (lambda (statement-list state) state
    (cond
      ((null? statement-list) state)
      ((not (eq? (S-lookup 'return state) 'null)) state)
      (else (M-state-statement-list (remaining-statements statement-list)
                                    (M-state-statement (first-statement statement-list) state))))))

; Statement list abstractions
(define first-statement-type caar)
(define first-statement car)
(define remaining-statements cdr)


;;;; STATEMENT
;; -----------------------------------------------------------------------
; Calculate the value or resulting state of a particular statement

; Calculate the state resulting from a genereric statement
(define M-state-statement
  (lambda (statement state)
    (cond
      ((eq? 'return (statement-type statement)) (M-state-return statement state))
      ((eq? 'var (statement-type statement)) (M-state-declare statement state))
      ((eq? '= (statement-type statement)) (M-state-assign statement state))
      ((eq? 'if (statement-type statement)) (M-state-if statement state))
      ((eq? 'while (statement-type statement)) (M-state-while statement state))
      (else state))))

; Calculate the value resulting from a return conditional/expression and assign it to 'return in the state
(define M-state-return
  (lambda (statement state)
    (S-assign 'return (M-value-expression (return-expression statement) state) state)))

; Calculate the state resulting from a declare statement
; Declared but un-assigned variables have the value 'null
(define M-state-declare
  (lambda (statement state)
    (cond
      ((S-name? (declare-name statement) state)
       (error "Variable already declared:" (declare-name statement)))
      ((declare-has-assignment? statement)
       (S-add (declare-name statement) (M-value-expression (declare-expression statement) state) state))
      (else (S-add (declare-name statement) 'null state)))))

; Calculate the state resulting from an assign statement
(define M-state-assign
  (lambda (statement state)
    (S-assign (assign-name statement) (M-value-expression (assign-expression statement) state) state)))

; Calculate the state resulting from an if statement
(define M-state-if
  (lambda (statement state)
    (cond
      ((true? (M-value-expression (if-condtion statement) state))
       (M-state-statement (if-statement statement) state)) ; Calculate the state resulting from the if block
      ((if-has-else? statement)
       (M-state-statement (else-statement statement) state)) ; Calculate the state resulting from the else block
      (else state))))

; Calculate the state resulting from a while statement
(define M-state-while
  (lambda (statement state)
    (cond
      ((true? (M-value-expression (while-condtion statement) state))
       (M-state-while statement (M-state-statement (while-statement statement) state)))
      ((not (eq? (S-lookup 'return state) 'null)) state)
      (else state))))

; Statement abstractions
(define statement-type car)
(define return-expression cadr)
(define declare-name cadr)
(define declare-expression caddr)
(define assign-name cadr)
(define assign-expression caddr)
(define if-condtion cadr)
(define if-statement caddr)
(define else-statement cadddr)
(define while-condtion cadr)
(define while-statement caddr)

; Determine if an if statement includes the optional else
(define if-has-else?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; Determine if a declare statement includes the optional assignment
(define declare-has-assignment?
  (lambda (statement)
    (not (null? (cddr statement)))))


;;;; EXPRESSION
;; -----------------------------------------------------------------------
;; Expressions return one of {integer, 'true, 'false, error}

(define M-value-expression
  (lambda (expression state)
    (cond
      ((null? expression) (error "Null parameter passed to M-value-expression"))
      ((logical-calculation? expression) (M-value-conditional expression state))
      (else (M-value-term expression state)))))


;;;; CONDITIONAL
;; -----------------------------------------------------------------------
;; Conditionals return 'true or 'false and connect one or more comparisons

(define M-value-conditional
  (lambda (conditional state)
    (cond
      ((null? conditional) (error "Null parameter passed to M-value-conditional"))
      ((eq? '&& (connective conditional)) (C-and (M-value-expression (leftoperand conditional) state)
                                                 (M-value-expression (rightoperand conditional) state)))
      ((eq? '|| (connective conditional)) (C-or (M-value-expression (leftoperand conditional) state)
                                                (M-value-expression (rightoperand conditional) state)))
      ((eq? '! (connective conditional)) (C-not (M-value-expression (unaryoperand conditional) state)))
      (else (M-value-comparison conditional state)))))


;;;; COMPARISON
;; -----------------------------------------------------------------------
;; Comparisons return 'true or 'false and compare terms

(define M-value-comparison
  (lambda (comparison state)
    (cond
      ((null? comparison) (error "Null parameter passed to M-value-comparison"))
      ((eq? '< (comparator comparison)) (C-< (M-value-term (leftoperand comparison) state)
                                             (M-value-term (rightoperand comparison) state)))
      ((eq? '> (comparator comparison)) (C-> (M-value-term (leftoperand comparison) state)
                                             (M-value-term (rightoperand comparison) state)))
      ((eq? '== (comparator comparison)) (C-== (M-value-term (leftoperand comparison) state)
                                               (M-value-term (rightoperand comparison) state)))
      ((eq? '<= (comparator comparison)) (C-<= (M-value-term (leftoperand comparison) state)
                                               (M-value-term (rightoperand comparison) state)))
      ((eq? '>= (comparator comparison)) (C->= (M-value-term (leftoperand comparison) state)
                                               (M-value-term (rightoperand comparison) state)))
      ((eq? '!= (comparator comparison)) (C-!= (M-value-term (leftoperand comparison) state)
                                               (M-value-term (rightoperand comparison) state)))
      (else (error "The operator is unknown:" (comparator comparison))))))


;;;; Terms
;; -----------------------------------------------------------------------
;; Terms are integer calulations and boolean values
    
; Calculate the value of a mathematical expression
(define M-value-term
  (lambda (term state)
    (cond
      ((null? term) (error "Null parameter passed to M-value-term"))
      ((eq? 'true term) 'true)
      ((eq? 'false term) 'false)
      ((number? term) term)
      ((and (atom? term) (eq? 'null (S-lookup term state)))
       (error "Variable referenced before assignment:" term))
      ((atom? term) (S-lookup term state))
      ((negation? term) (* -1
                           (M-value-term (unaryoperand term) state)))
      ((eq? '= (operator term)) (M-state-assign term state))
      ((eq? '+ (operator term)) (+ (M-value-term (leftoperand term) state)
                                   (M-value-term (rightoperand term) state)))
      ((eq? '- (operator term)) (- (M-value-term (leftoperand term) state)
                                   (M-value-term (rightoperand term) state)))
      ((eq? '* (operator term)) (* (M-value-term (leftoperand term) state)
                                   (M-value-term (rightoperand term) state)))
      ((eq? '/ (operator term)) (quotient (M-value-term (leftoperand term) state)
                                          (M-value-term (rightoperand term) state)))
      ((eq? '% (operator term)) (modulo (M-value-term (leftoperand term) state)
                                        (M-value-term (rightoperand term) state)))
      (else (error "The operator is unknown:" (operator term))))))

; Conditional, comparison, and term abstractions
(define comparator car)
(define connective car)
(define operator car)
(define unaryoperand cadr)
(define leftoperand cadr)
(define rightoperand caddr)
(define missing-rightoperand?
  (lambda (term)
    (null? (cddr term))))

; Check if a - operator refers to subtraction or negation
(define negation?
  (lambda (term)
    (and (eq? '- (operator term))
         (missing-rightoperand? term))))

; Definitions of logial conenctives and comparisons in terms of 'true and 'false
(define C-and
  (lambda (loperand roperand)
    (C-true? (and (true? loperand) (true? roperand)))))
(define C-or
  (lambda (loperand roperand)
    (C-true? (or (true? loperand) (true? roperand)))))
(define C-not
  (lambda (operand)
    (C-true? (eq? operand 'false))))
(define C-<
  (lambda (loperand roperand)
    (C-true? (< loperand roperand))))
(define C->
  (lambda (loperand roperand)
    (C-true? (> loperand roperand))))
(define C-==
  (lambda (loperand roperand)
    (C-true? (= loperand roperand))))
(define C-<=
  (lambda (loperand roperand)
    (C-true? (<= loperand roperand))))
(define C->=
  (lambda (loperand roperand)
    (C-true? (>= loperand roperand))))
(define C-!=
  (lambda (loperand roperand)
    (C-true? (not (= loperand roperand)))))

; Check if an operator is a logical operator
(define logical-calculation?
  (lambda (expression)
    (if (pair? expression)
        (in? (operator expression)
             '(&& || ! < > == <= >= !=))
        #f)))
    

;;;; State functions
;; -----------------------------------------------------------------------

; Return a new state with a null return variable
(define S-new
  (lambda () (S-add' return 'null '())))

; Search for a variable by name in the state and return its value
; Creates an error if the state does not contain the variable or if the variable is unassigned
(define S-lookup
  (lambda (variable state)
    (cond 
      ((null? state) (error "Variable referenced before declaration:" variable))
      ((eq? variable (first-var state)) (first-val state))
      (else (S-lookup variable (remaining-bindings state))))))

; Search for a variable by name in the state and return if it exists
(define S-name?
  (lambda (variable state)
    (cond
      ((null? state) #f)
      ((eq? variable (first-var state)) #t)
      (else (S-name? variable (remaining-bindings state))))))
  
; Update the value of a variable in the state
; If it does not exist, return an error
(define S-assign
  (lambda (variable value state)
    (cond 
      ((null? state) (error "Variable assigned before declaration:" variable))
      ((eq? variable (first-var state)) (S-add variable value (S-remove variable state)))
      (else (cons (first-binding state) (S-assign variable value (remaining-bindings state)))))))

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
      ((eq? variable (first-var state)) (remaining-bindings state))
      (else (cons (first-binding state) (S-remove variable (remaining-bindings state)))))))

; State abstractions
(define first-var caar)
(define first-val cadar)
(define first-binding car)
(define remaining-bindings cdr)

  
;; Utility functions
;; -----------------------------------------------------------------------

; Determine if x is an atom
(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

; Convert a #t and #f to 'true and 'false
(define C-true?
  (lambda (condition)
    (if condition
        'true
        'false)))

; Convert a 'true or 'false to #t or #f, throws error if other value is encountered
(define true?
  (lambda (value)
    (cond
      [(eq? value 'true) #t]
      [(eq? value 'false) #f]
      [else (error "Cannot cast to boolean:" value)])))

; Return true if x is in lis
(define in?
  (lambda (x lis)
    (cond
      [(null? lis) #f]
      [(eq? (car lis) x) #t]
      [else (in? x (cdr lis))])))
