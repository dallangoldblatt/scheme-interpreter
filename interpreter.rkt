#lang racket

(require "functionParser.rkt")
(provide (all-defined-out))

; An interpreter for the simple language using tail recursion for the M_state functions

; The functions that start interpret-...  all return the current environment.  These are the M_state functions.
; The functions that start eval-...  all return a value.  These are the M_value and M_boolean functions.

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.
(define interpret
  (lambda (file)
    (scheme->language
     (compile (parser file) (newenvironment)))))

; ------------------------------------------
; GLOBAL VARIABLE AND FUNCTION DEFINITION LAYER 
; ------------------------------------------

; formats: 
; function: (function name (params) ((body)
; closure:  (name (params) ((body)) (state-function)

; Compiles (adds to environment) global variables and function declarations.
; Once all statements have been handled, call main() 
(define compile
  (lambda (statement-list environment)
    (if (null? statement-list)
        (eval-expression '(funcall main)
                         environment
                         (lambda (v) v)
                         (lambda (v) (error "Uncaught exception thrown:")))
        (compile-statement (car statement-list) environment (lambda (env) (compile (cdr statement-list) env))))))

(define compile-statement
  (lambda (statement environment next)
    (cond
      ((eq? 'function (statement-type statement)) (next (insert (function-name statement) (create-function-closure statement environment) environment)))
      ((eq? 'var (statement-type statement))      (interpret-declare statement environment next (lambda (v) (error "Uncaught exception thrown:" v))))
      (else                                       (error "Unknown statement:" (statement-type statement))))))


;--------------------------------------
; FUNCTION DEFINITION AND EXECUTION HANDLING
;--------------------------------------

; Creates the closure for a newly defined function
(define create-function-closure
  (lambda (function environment)
    (list (function-name function)
          (function-params function)
          (function-body function)
          (create-environment-builder function environment))))

; Creates a the environment-building function for the closure
(define create-environment-builder
  (lambda (function function-def-environment)
    (lambda (environment)
      (add-functions-to-environment ; Adds the closures of any nested functions to the built environment
       (find-nested-function-definitions (function-body function) (push-frame environment) '())
       (push-frame (pop-n-frames (- (length environment) (length function-def-environment)) environment)))))) 

; Add a list of function closures to the top frame of the state
(define add-functions-to-environment
  (lambda (function-list environment)
    (if (null? function-list)
        environment
        (add-functions-to-environment
         (remaining-functions function-list)
         (insert (closure-name (first-function function-list)) (first-function function-list) environment)))))

; Read a function body and recursively create their closures
; Add these definions to the parent function's closure
(define find-nested-function-definitions
  (lambda (body local-environment function-list)
    (cond
      ((null? body)                                            function-list)
      ((eq? 'function (statement-type (first-statement body))) (find-nested-function-definitions
                                                                (remaining-statements body)
                                                                local-environment
                                                                (cons (create-function-closure (first-statement body) local-environment) function-list)))
      (else                                                    (find-nested-function-definitions (remaining-statements body) local-environment function-list)))))

; Helper function for popping n frames off of the given environment
(define pop-n-frames
  (lambda (n environment)
    (if (<= n 0)
        environment
        (pop-n-frames (- n 1) (cdr environment)))))

; Create the correct environment from the closure, bind the parameters, call the body
(define call-function
  (lambda (closure actual-params environment return throw next)
    (interpret-statement-list (closure-body closure)
                              (add-parameters
                               (closure-name closure)
                               actual-params
                               (closure-formal-params closure)
                               environment
                               ((closure-environment-builder closure) environment)
                               throw)
                              return
                              (lambda (env) (error "Break used outside of loop"))
                              (lambda (env) (error "Continue used outside of loop"))
                              throw
                              next)))

; Evaluate the actual parameters to a function call and add them to the environment
(define add-parameters
  (lambda (function-name actual-params formal-params call-environment function-environment throw)
    (cond
      ((and (null? actual-params) (null? formal-params)) function-environment)
      ((or (null? actual-params) (null? formal-params)) (error "The actual parameters do not match the formal parameters for:" function-name))
      (else (add-parameters function-name
                            (remaining-params actual-params)
                            (remaining-params formal-params)
                            call-environment
                            (insert (first-param formal-params) (eval-expression (first-param actual-params) call-environment (lambda (v) v) throw) function-environment)
                            throw)))))
                               
; Function closure abstractions
(define function-name cadr)
(define function-params caddr)
(define function-body cadddr)
(define closure-name car)
(define closure-formal-params cadr)
(define closure-body caddr)
(define closure-environment-builder cadddr)
(define first-statement car)
(define remaining-statements cdr)
(define first-function car)
(define remaining-functions cdr)
(define first-param car)
(define remaining-params cdr)

; ------------------------
; STATEMENT EXECUTION FUNCTIONS
; ------------------------

; interprets a list of statements. The state/environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw next)
    (if (null? statement-list)
        (next environment)
        (interpret-statement (car statement-list)
                             environment
                             return
                             break
                             continue
                             throw
                             (lambda (env) (interpret-statement-list (cdr statement-list) env return break continue throw next))))))

; interpret a statement in the environment with continuations for return, break, continue, throw, and "next statement"
(define interpret-statement
  (lambda (statement environment return break continue throw next)
    (cond
      ((eq? 'function (statement-type statement)) (next environment)) ; Function definitions are already handled in global compile
      ((eq? 'funcall (statement-type statement))  (interpret-function statement environment next throw))
      ((eq? 'return (statement-type statement))   (interpret-return statement environment return throw))
      ((eq? 'var (statement-type statement))      (interpret-declare statement environment next throw))
      ((eq? '= (statement-type statement))        (interpret-assign statement environment throw next))
      ((eq? 'if (statement-type statement))       (interpret-if statement environment return break continue throw next))
      ((eq? 'while (statement-type statement))    (interpret-while statement environment return throw next))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement))    (break environment))
      ((eq? 'begin (statement-type statement))    (interpret-block statement environment return break continue throw next))
      ((eq? 'throw (statement-type statement))    (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement))      (interpret-try statement environment return break continue throw next))
      (else                                       (error "Unknown statement:" (statement-type statement))))))

; Calls a function and ignores the return value (since this function is being called outside of an assignment)
(define interpret-function
  (lambda (statement environment next throw)
    (call-function (lookup (get-function-call-name statement) environment)
                   (get-function-actual-params statement)
                   environment
                   (lambda (return-val) (next environment))
                   throw
                   (lambda (env) (next environment))))) ; Ignore the environment returned by the function call
        
    
; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return throw)
    (eval-expression (get-expr statement) environment (lambda (v) (return v)) throw)))

; Adds a new variable binding to the environment. There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment next throw)
    (if (exists-declare-value? statement)
        (eval-expression (get-declare-value statement) environment (lambda (v) (next (insert (get-declare-var statement) v environment))) throw)
        (next (insert (get-declare-var statement) 'novalue environment)))))

; Updates the environment to add a new binding for a variable
(define interpret-assign
  (lambda (statement environment throw next)
    (eval-expression (get-assign-rhs statement) environment (lambda (v) (next (update (get-assign-lhs statement) v environment))) throw)))

; We need to check if there is an else condition. Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw next)
    (eval-expression (get-condition statement)
                     environment
                     (lambda (condition)
                       (cond
                         (condition (interpret-statement (get-then statement) environment return break continue throw next))
                         ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw next))
                         (else (next environment))))
                     throw)))

; Interprets a while loop. We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw next)
    (letrec ((loop (lambda (condition body environment)
                     (eval-expression condition
                                      environment
                                      (lambda (condition-v)
                                        (if condition-v
                                            (interpret-statement body
                                                                 environment
                                                                 return
                                                                 (lambda (env) (next env))
                                                                 (lambda (env) (loop condition body env))
                                                                 throw
                                                                 (lambda (env) (loop condition body env)))
                                            (next environment)))
                                      throw))))
    (loop (get-condition statement) (get-body statement) environment))))

; Interprets a block. The break, continue, and "next statement" continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw next)
    (interpret-statement-list (cdr statement)
                              (push-frame environment)
                              return
                              (lambda (env) (break (pop-frame env)))
                              (lambda (env) (continue (pop-frame env)))
                              throw ; Uses the current environent since changes inside the block are reflected in the boxes
                              (lambda (env) (next (pop-frame env))))))
                              

; We use a continuation to throw the proper value.  Because we are using boxes, the environment does not need to be thrown as well
(define interpret-throw
  (lambda (statement environment throw)
    (eval-expression (get-expr statement) environment (lambda (v) (throw v)) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw next finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (interpret-block finally-block env return break continue throw (lambda (env2) (throw ex))))) 
      ((not (eq? 'catch (statement-type catch-statement))) (error "Incorrect catch statement"))
      (else (lambda (ex)
              (interpret-statement-list 
               (get-body catch-statement) 
               (insert (catch-var catch-statement) ex (push-frame environment))
               return 
               (lambda (env2) (break (pop-frame env2))) 
               (lambda (env2) (continue (pop-frame env2))) 
               (lambda (v) (throw v)) 
               (lambda (env2) (interpret-block finally-block (pop-frame env2) return break continue throw next))))))))

; To interpret a try block, we must adjust the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw next)
    (let* ((finally-block (make-finally-block (get-finally statement)))
           (try-block (make-try-block (get-try statement)))
           (new-return (lambda (v) (interpret-block finally-block environment return break continue throw (lambda (env2) (return v)))))
           (new-break (lambda (env) (interpret-block finally-block env return break continue throw (lambda (env2) (break env2)))))
           (new-continue (lambda (env) (interpret-block finally-block env return break continue throw (lambda (env2) (continue env2)))))
           (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw next finally-block)))
      (interpret-block try-block environment new-return new-break new-continue new-throw (lambda (env) (interpret-block finally-block env return break continue throw next))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (error "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment value-cont throw)
    (cond
      ((number? expr) (value-cont expr))
      ((eq? expr 'true) (value-cont #t))
      ((eq? expr 'false) (value-cont #f))
      ((function-call? expr) (eval-function expr environment value-cont throw))
      ((not (list? expr)) (value-cont (lookup expr environment)))
      (else (eval-operator expr environment value-cont throw)))))

; Get the value returned by a function call
(define eval-function
  (lambda (statement environment value-cont throw)
    (call-function (lookup (get-function-call-name statement) environment)
                   (get-function-actual-params statement)
                   environment
                   (lambda (return-val) (value-cont return-val))
                   throw
                   (lambda (env) (value-cont 'novalue)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order.
(define eval-operator
  (lambda (expr environment value-cont throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment (lambda (v) (value-cont (not v))) throw)))
      ((negation? expr) (- (eval-expression (operand1 expr) environment (lambda (v) (value-cont (* -1 v))) throw)))
      (else (eval-expression (operand1 expr) environment (lambda (v1) (eval-binary-op2 expr v1 environment value-cont throw)) throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr v1 environment value-cont throw)
    (eval-expression (operand2 expr)
                     environment
                     (lambda (v2)
                       (cond
                         ((eq? '+ (operator expr))  (value-cont (+  v1 v2)))
                         ((eq? '- (operator expr))  (value-cont (- v1 v2)))
                         ((eq? '* (operator expr))  (value-cont (* v1 v2)))
                         ((eq? '/ (operator expr))  (value-cont (quotient v1 v2)))
                         ((eq? '% (operator expr))  (value-cont (remainder v1 v2)))
                         ((eq? '== (operator expr)) (value-cont (isequal v1 v2)))
                         ((eq? '!= (operator expr)) (value-cont (not (isequal v1 v2))))
                         ((eq? '< (operator expr))  (value-cont (< v1 v2)))
                         ((eq? '> (operator expr))  (value-cont (> v1 v2)))
                         ((eq? '<= (operator expr)) (value-cont (<= v1 v2)))
                         ((eq? '>= (operator expr)) (value-cont (>= v1 v2)))
                         ((eq? '|| (operator expr)) (value-cont (or v1 v2)))
                         ((eq? '&& (operator expr)) (value-cont (and v1 v2)))
                         (else (error "Unknown operator:" (operator expr)))))
                     throw)))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))

;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

(define function-call?
  (lambda (expr)
    (and (list? expr) (eq? (operator expr) 'funcall))))

(define missing-rightoperand?
  (lambda (term)
    (null? (cddr term))))

; Check if a "-" operator refers to subtraction or negation
(define negation?
  (lambda (expr)
    (and (eq? '- (operator expr)) (= 2 (length expr)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)
(define get-function-call-name cadr)
(define get-function-actual-params cddr)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (error "Variable referenced before assignment:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (error "Variable referenced before declaration:" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (error "Variable referenced before declaration:" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (unbox (car l)))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (error "Variable already declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (error "Variable assigned before declaration:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (box val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (begin (set-box! (car vallist) val) (car vallist)) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))


; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))
