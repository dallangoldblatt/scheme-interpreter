#lang racket

(require "simpleParser.rkt")
(provide (all-defined-out))

;;;; Interpreter, Part 1
;; -----------------------------------------------------------------------
; Dallan Goldblatt
; Robbie Dozier
; Ethan Voss


; Entrypoint into the interpreter
(define interpret
  (lambda (file)
    (M-state-statement-list (parser file)
                            (S-new)
                            (lambda (return) (S-lookup 'return return))
                            (lambda (normal) (S-lookup 'return normal)))))


;;;: M-quantity and M-state definitions
;; -----------------------------------------------------------------------
; Since variables can take both integer and boolean valules, M-quantity functions can return both:
;     M-quantity: statement X state -> {integer, 'true, 'false, error}
;
; The M-state functions return the state resulting from a statement
;     M-state: statement X state -> {state, error}
;
; state is stored as a list of tuples in the format (name value):
;     ((x 12) (y 4) ...);


;;;; STATEMENT FORMATS
;; -----------------------------------------------------------------------
;; Atoms in UPPERCASE can implement M-state and/or M-quantity
;; Symbols and atoms in lowercase are part of the language

;; Variable declaration
; (var VARIABLE)
; (var VARIABLE EXPRESSION)

;; Assignment
; (= VARIABLE EXPRESSION)

;; Return
; (return EXPRESSION)

;; If statement
; (if EXPRESSION then STATEMENT)
; (if EXPRESSION then STATEMENT else STATEMENT)

;; While statement
; (while EXPRESSION STATEMENT)


;; -----------------------------------------------------------------------
;;;; The following prefixes have been used to distinguish abstaction functions
;; Condtional: C-
;; State:      S-


;;;; STATEMENT LIST
;; -----------------------------------------------------------------------
;; A statement list is a list of zero or more valid STATEMENTs
    
; Calculate the state resulting from a statement list
(define M-state-statement-list
  (lambda (statement-list state return normal)
    (cond
      ((null? statement-list) (normal state))
      (else (M-state-statement (first-statement statement-list)
                               state
                               return
                               (lambda (normal-state)
                                 (M-state-statement-list (remaining-statements statement-list)
                                                         normal-state
                                                         return
                                                         normal)))))))

; Statement list abstractions
(define first-statement-type caar)
(define first-statement car)
(define remaining-statements cdr)
(define return-assigned?
  (lambda (state)
    (not (eq? (S-lookup 'return state) 'null))))


;;;; STATEMENT
;; -----------------------------------------------------------------------
; Calculate the value or resulting state of a particular valid STATEMENT

; Calculate the state resulting from a genereric statement
(define M-state-statement
  (lambda (statement state return normal)
    (cond
      ((eq? 'return (statement-type statement)) (M-state-return statement state return))
      ((eq? 'var (statement-type statement)) (M-state-declare statement state normal))
      ((eq? '= (statement-type statement)) (M-state-assign statement state normal))
      ((eq? 'if (statement-type statement)) (M-state-if statement state return normal))
      ((eq? 'while (statement-type statement)) (M-state-while statement state return normal))
      (else (normal state)))))

; Calculate the state resulting from a return expression (assign its value to 'return in the state)
(define M-state-return
  (lambda (statement state return)
    (M-quantity-expression
     (return-expression statement)
     state
     (lambda (v)
       (return (S-assign 'return v state))))))

; Calculate the state resulting from a declare statement
; Declared but un-assigned variables have the value 'null
(define M-state-declare
  (lambda (statement state normal)
    (cond
      ((S-name? (declare-name statement) state)
       (error "Variable already declared:" (declare-name statement)))
      ((declare-has-assignment? statement)
       (M-quantity-expression (declare-expression statement)
                              state
                              (lambda (v)
                                (normal (S-add (declare-name statement) v state)))))
      (else (normal (S-add (declare-name statement) 'null state))))))

; Calculate the state resulting from an assign statement
(define M-state-assign
  (lambda (statement state normal)
    (M-quantity-expression (assign-expression statement)
                           state
                           (lambda (v)
                             (normal (S-assign (assign-name statement) v state))))))

; Calculate the state resulting from an if statement
(define M-state-if
  (lambda (statement state return normal)
    ; Calulate if condition
    (M-quantity-expression (if-condition statement)
                           state
                           (lambda (condtion)
                             (cond
                               ((true? condtion)
                                (M-state-statement (if-statement statement) state return normal))
                               ((if-has-else? statement)
                                (M-state-statement (else-statement statement) state return normal))
                               (else (normal state)))))))

; Calculate the state resulting from a while statement
(define M-state-while
  (lambda (statement state return normal)
    ; Calulate while condition
    (M-quantity-expression (while-condition statement)
                           state
                           (lambda (condition)
                             (if (true? condition)
                                 (M-state-statement(while-statement statement)
                                                   state
                                                   return
                                                   (lambda (normal-state)
                                                     (M-state-while statement
                                                                    normal-state
                                                                    return
                                                                    normal)))
                                 (normal state))))))

; Statement abstractions
(define statement-type car)
(define return-expression cadr)
(define declare-name cadr)
(define declare-expression caddr)
(define assign-name cadr)
(define assign-expression caddr)
(define if-condition cadr)
(define if-statement caddr)
(define else-statement cadddr)
(define while-condition cadr)
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
;; Expressions have the value of {integer, 'true, 'false, error} and do not change the state

(define M-quantity-expression
  (lambda (expression state value-cont)
    (cond
      ((null? expression) (error "Null parameter passed to M-quantity-expression"))
      ((logical-calculation? expression) (M-quantity-conditional expression state value-cont))
      (else (M-quantity-term expression state value-cont)))))


;;;; CONDITIONAL
;; -----------------------------------------------------------------------
;; Conditionals return 'true or 'false and connect one or more COMPARISONs

(define M-quantity-conditional
  (lambda (conditional state value-cont)
    (cond
      
      ((null? conditional) (error "Null parameter passed to M-quantity-conditional"))
      ((eq? '&& (connective conditional)) (M-quantity-expression (leftoperand conditional)
                                                                 state
                                                                 (lambda (v1)
                                                                   (M-quantity-expression (rightoperand conditional)
                                                                                          state
                                                                                          (lambda (v2)
                                                                                            (value-cont (C-and v1 v2)))))))
      ((eq? '|| (connective conditional)) (M-quantity-expression (leftoperand conditional)
                                                                 state
                                                                 (lambda (v1)
                                                                   (M-quantity-expression (rightoperand conditional)
                                                                                          state
                                                                                          (lambda (v2)
                                                                                            (value-cont (C-or v1 v2)))))))
      ((eq? '! (connective conditional)) (M-quantity-expression (leftoperand conditional)
                                                                state
                                                                (lambda (v)
                                                                  (value-cont (C-not v)))))
      (else (M-quantity-comparison conditional state value-cont)))))


;;;; COMPARISON
;; -----------------------------------------------------------------------
;; Comparisons return 'true or 'false and compare TERMs

(define M-quantity-comparison
  (lambda (comparison state value-cont)
    (if (null? comparison)
        (error "Null parameter passed to M-quantity-comparison")
        (M-quantity-term (leftoperand comparison)
                         state
                         (lambda (v1)
                           (M-quantity-term (rightoperand comparison)
                                            state
                                            (lambda (v2)
                                              (cond
                                                ((eq? '< (comparator comparison)) (value-cont (C-< v1 v2)))
                                                ((eq? '> (comparator comparison)) (value-cont (C-> v1 v2)))
                                                ((eq? '== (comparator comparison)) (value-cont (C-== v1 v2)))
                                                ((eq? '<= (comparator comparison)) (value-cont (C-<= v1 v2)))
                                                ((eq? '>= (comparator comparison)) (value-cont (C->= v1 v2)))
                                                ((eq? '!= (comparator comparison)) (value-cont (C-!= v1 v2)))
                                                (else (error "The operator is unknown:" (comparator comparison)))))))))))


;;;; Terms
;; -----------------------------------------------------------------------
;; Terms are integer calulations and boolean values
    
; Calculate the value of a mathematical expression
(define M-quantity-term
  (lambda (term state value-cont)
    (cond
      ((null? term) (error "Null parameter passed to M-quantity-term"))
      ((C-boolean? term) (value-cont term))
      ((number? term) (value-cont term))
      ((S-unassigned? term state)
       (error "Variable referenced before assignment:" term))
      ((atom? term) (value-cont (S-lookup term state)))
      ((negation? term) (M-quantity-term (unaryoperand term)
                                         state
                                         (lambda (v)
                                           (value-cont (* -1 v)))))
      (else (M-quantity-term (leftoperand term)
                             state
                             (lambda (v1)
                               (M-quantity-term (rightoperand term)
                                                state
                                                (lambda (v2)
                                                  (cond
                                                    ((eq? '+ (operator term)) (value-cont (+ v1 v2)))
                                                    ((eq? '- (operator term)) (value-cont (- v1 v2)))
                                                    ((eq? '* (operator term)) (value-cont (* v1 v2)))
                                                    ((eq? '/ (operator term)) (value-cont (quotient v1 v2)))
                                                    ((eq? '% (operator term)) (value-cont (remainder v1 v2)))
                                                    (else (error "The operator is unknown:" (operator term))))))))))))

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

; Check if a "-" operator refers to subtraction or negation
(define negation?
  (lambda (term)
    (and (eq? '- (operator term))
         (missing-rightoperand? term))))

; Definition of boolean? in terms of 'true and 'false
(define C-boolean?
  (lambda (term)
    (or (eq? 'true term) (eq? 'false term))))

; Definitions of logial connectives and comparisons in terms of 'true and 'false
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
; The state has the form: '(layer1 layer2)
; A layer has the form:   '((var1 var2...) (val1 val2...))
; Example with 2 layers:  '(((x y z) (1 2 3)) ((w) (2)))

; Return a new state with a null return variable
(define S-new
  (lambda () (S-add 'return 'null (S-push-layer empty-layer empty-state))))

; Search for a variable by name in the state and return its value
; Searches layers in sequence
; Creates an error if the state does not contain the variable
(define S-lookup
  (lambda (variable state)
    (cond 
      ((null? state) (error "Variable referenced before declaration:" variable))
      ((S-layer-null? (first-layer state)) (S-lookup variable (remaining-layers state)))
      ((eq? variable (first-var state)) (first-val state))
      (else (S-lookup variable (remaining-bindings state))))))

; Search for a variable by name in the state and return if it exists
(define S-name?
  (lambda (variable state)
    (cond
      ((null? state) #f)
      ((S-layer-null? (first-layer state)) (S-name? variable (remaining-layers state)))
      ((eq? variable (first-var state)) #t)
      (else (S-name? variable (remaining-bindings state))))))
  
; Update the value of a variable in the state
; The first occuance of the varible in the highest layer is changed
; If it does not exist, then return an error
(define S-assign
  (lambda (variable value state)
    (cond 
      ((null? state) (error "Variable assigned before declaration:" variable))
      ((in? variable (first-layer-variables state)) (S-add variable value (S-remove variable state)))
      (else (S-push-layer (first-layer state) (S-assign variable value (remaining-layers state)))))))

; Add a new variable and value to the first layer of the state
(define S-add
  (lambda (variable value state)
    (S-push-layer (list (cons variable (first-layer-variables state))
                        (cons value (first-layer-values state)))
                  (remaining-layers state))))

; Remove a the first occurance of a variable from the a layer of the state
; If the variable is already not present, no error is created
(define S-remove
  (lambda (variable state)
    (cond
      ((null? state) empty-state)
      ((S-layer-null? (first-layer state)) (S-push-layer empty-layer (S-remove variable (remaining-layers state))))
      ((eq? variable (first-var state)) (remaining-bindings state))
      (else (S-add (first-var state) (first-val state) (S-remove variable (remaining-bindings state)))))))

; Check if a variable has been declared but not assigned a value
(define S-unassigned?
  (lambda (term state)
    (and (S-name? term state)
         (eq? 'null (S-lookup term state)))))

; Check if the a layer is empty
(define S-layer-null?
  (lambda (layer)
    (null? (car layer))))

; Add a layer onto the state
(define S-push-layer
  (lambda (layer state)
    (cons layer state)))

; Remove the first layer from the state
(define S-pop-layer
  (lambda (state)
    (remaining-layers state)))

; State abstractions
(define first-var caaar)
(define first-val caadar)
(define first-layer car)
(define first-layer-variables caar)
(define first-layer-values cadar)
(define remaining-layers cdr)

(define remaining-bindings
  (lambda (state)
    (cons (map cdr (first-layer state))
          (remaining-layers state))))

(define empty-state '())
(define empty-layer '(() ()))

  
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
