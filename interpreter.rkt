#lang racket

(require "classParser.rkt")
(provide (all-defined-out))

; An interpreter for the simple language using tail recursion for the M_state functions

; Functions that start compile-...  all set up class/function closuere prior to program executuon.
; The functions that start interpret-...  all return the current environment.  These are the M_state functions.
; The functions that start eval-...  all return a value.  These are the M_value and M_boolean functions.

; The main entrypoint.  Calls parser to get the parse tree and interprets it with a new environment.
; The class is entered as a string and must be converted to an s-expression
(define interpret
  (lambda (file class)
    (scheme->language
     (compile (parser file) (newframe) (string->symbol class)))))

; ------------------------------------------
; CLASS DEFINITION LAYER 
; ------------------------------------------

; formats:
; class:       (class A () body)
;              (class B (extends A) body)
; c closure:   (super methods field-names field-defs main constructor)

; Compiles all class closures
; The list of class definitions is held in a single frame: '((names) (closures))
; Once all declarations have been handled, calls main of the selected class
(define compile
  (lambda (class-declarations class-list main-class)
    (if (null? class-declarations)
        ;(eval-expression `(funcall (dot (new ,main-class) main))       ; TODO uncomment this and remove below when dot is working
        ;                 (newenvironment)
        ;                 class-list
        ;                 (lambda (v) v)
        ;                 (lambda (v) (error "Uncaught exception thrown:")))
        (interpret-statement-list (closure-main (lookup-in-class-list main-class class-list))
                                  'novalue
                                  (newenvironment)
                                  class-list
                                  (lambda (v) v)
                                  (lambda (env) (error "Break used outside of loop"))
                                  (lambda (env) (error "Continue used outside of loop"))
                                  (lambda (v) (error "Uncaught exception thrown:"))
                                  (lambda (env) (error "No return value for main")))
        (compile-class (first-class class-declarations) class-list (lambda (cl) (compile (remaining-classes class-declarations) cl main-class))))))

; Add the closure of a class to the current class closure list
(define compile-class
  (lambda (declaration class-list next)
    (cond
      ((extends-class? declaration)              (next (add-to-frame (class-name declaration)
                                                                     (create-class-closure declaration
                                                                                           (super-class declaration)
                                                                                           (lookup-in-class-list (super-class declaration) class-list))
                                                                     class-list)))
      ((eq? 'class (statement-type declaration)) (next (add-to-frame (class-name declaration)
                                                                     (create-class-closure declaration
                                                                                           '_nosuper
                                                                                           '())
                                                                     class-list)))
      (else                                      (error "Expected class declaration:" declaration)))))

; TODO make this prettier? It does nearly the same thing in both cases
; Create the closure for a class from its definition
(define create-class-closure
  (lambda (declaration super-name super-closure)
    (if (null? super-closure)
        ; No super class, do not need to get super variables or methods 
        (compile-class-definition
         (class-body declaration)
         '()
         '() ; This function uses a custom return function with multiple values
         '() ; so empty lists have to be supplied for the recursion
         '()
         (lambda (var-list val-list method-list main)
           (list super-name
                 method-list
                 var-list
                 val-list
                 main
                 (create-default-constructor (class-name declaration) val-list))))
        ; Get the variables and methods from super closure
        (compile-class-definition
         (class-body declaration)
         (closure-field-names super-closure)
         (closure-field-defs super-closure)
         (closure-methods super-closure)
         '()
         (lambda (var-list val-list method-list main)
           (list super-name
                 method-list
                 var-list
                 val-list
                 main
                 (create-default-constructor (class-name declaration) val-list)))))))

; Compiles class methods and variables and returns them in lists
; The only static function that is handled is main()
(define compile-class-definition
  (lambda (statement-list var-list val-list method-list main return)
    (if (null? statement-list)
        (return var-list val-list method-list main)
        (compile-statement (first-statement statement-list)
                           var-list
                           val-list
                           method-list
                           main
                           (lambda (va vl ml ma) (compile-class-definition (remaining-statements statement-list) va vl ml ma return))))))

; Handle a single statement inside a class definition
(define compile-statement
  (lambda (statement var-list val-list method-list main return)
    (cond
      ; The only static method is the main method
      ((eq? 'static-function (statement-type statement)) (return var-list val-list method-list (function-body statement)))
      ; Create the function closure for a method, the environment should be top-level for the instance TODO maybe even empty?
      ((eq? 'function (statement-type statement)) (return var-list val-list (cons (create-function-closure statement (newenvironment)) method-list) main))
      ; Add the variable and value to the variable list, the value is not evaluated until instantiation
      ((eq? 'var (statement-type statement)) (get-var-and-value statement (lambda (var val) (return (cons var var-list) (cons val val-list) method-list main))))
      ; Overloaded constructors not supported
      ((eq? 'constructor (statement-type statement)) (error "User-defined constructors are not supported"))
      (else (error "Unexpected statement in class definition:" statement)))))

; Extracts the variable name and value from the declaration
(define get-var-and-value
  (lambda (statement return)
    (if (exists-declare-value? statement)
        (return (get-declare-var statement) (get-declare-value statement))
        (return (get-declare-var statement) 'novalue))))

; Finds the class closure in the class-list, this uses the same frame structure as the environment
(define lookup-in-class-list
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (error "Class could not be resovled to a type:" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Class abstractions
(define class-name cadr)
(define class-body cadddr)
(define extends-class?
  (lambda (statement)
    (not (null? (caddr statement)))))
(define super-class
  (lambda (statement)
    (cadr (caddr statement))))
(define first-def car)
(define remaining-defs cdr)
(define first-class car)
(define remaining-classes cdr)
(define closure-super car)
(define closure-methods cadr)
(define closure-field-names caddr)
(define closure-field-defs cadddr)
(define closure-main
  (lambda (closure)
    (if (null? (cadr (cdddr closure)))
        (error "Cannot call missing main method")
        (cadr (cdddr closure)))))
(define closure-constructor
  (lambda (closure)
    (caddr (cdddr closure))))
(define runtime-type car)
(define instance-attrs cadr)

;--------------------------------------
; INSTANCE DEFINITION AND EXECUTION HANDLING
;--------------------------------------
; formats:
; i closure:    (runtime-type (instance-values))

; Creates a default constructor that returns an instance of a class in a given environment
(define create-default-constructor
  (lambda (class field-defs)
    (lambda (environment class-list)
      (list class
            (reverse (eval-class-instance-vars field-defs class-list environment))))))

; Evaluates the instance variables for a new instantiation of a class
; Returns every value together in a list
(define eval-class-instance-vars
  (lambda (field-defs class-list environment)
    (if (null? field-defs)
        '()
        (cons (box (eval-expression (first-def field-defs)
                                    'novalue ; TODO the current value of this could be built as this iterates
                                    environment
                                    class-list
                                    (lambda (v) v)
                                    (lambda (v) (error "Exception in init:")))) ; TODO I guess a try could be passed to the instance creator
              (eval-class-instance-vars (remaining-defs field-defs) class-list environment)))))

; Returns the current value of an instance variable given the class list
(define get-instance-value
  (lambda (var instance class-list)
    (get-instance-value-from-list var
                                  (instance-vals instance)
                                  (closure-field-names (lookup-in-class-list (instance-type instance) class-list)))))

; Returns the current value of an instance variable given its class's var list
; Note that the val-list and var-list are paired in opposite order
(define get-instance-value-from-list
  (lambda (var val-list var-list)
    (get-value (indexof-instance var var-list) val-list)))

; Calculates the index of the instance variable in the (reversed) value list
(define indexof-instance
  (lambda (var var-list)
    (- (- (length var-list) (indexof var var-list)) 1)))

; Creates a new instance closure with the a updated variable value
(define set-instance-value
  (lambda (var val instance class-list)
    ; Check if the dot has been fully resolved
    (if (dot-resolved? var)
        ; Update the value in this instance
        (list (instance-type instance)
              (set-instance-value-at-index val
                                           (indexof-instance var
                                                             (closure-field-names (lookup-in-class-list (instance-type instance) class-list)))
                                           (instance-vals instance)))
        ; Recusively call on the nested instance
        (list (instance-type instance)
              (set-instance-value-at-index (set-instance-value (dot-var var)
                                                               val
                                                               (get-instance-value (dot-var) instance class-list)
                                                               class-list)
                                           (indexof-instance var
                                                             (closure-field-names (lookup-in-class-list (instance-type instance) class-list)))
                                           (instance-vals instance))))))

; Sets the the value of a box at a partiular index
(define set-instance-value-at-index
  (lambda (val i val-list)
    (cond
      ((zero? i) (cons (begin (set-box! (car val-list) val) (car val-list)) (cdr val-list)))
      (else (cons (car val-list) (set-instance-value-at-index val (- i 1) (cdr val-list)))))))

; Instance abstractions
(define instance-type car)
(define instance-vals cadr)
(define dot-instance cadr)
(define dot-var caddr)
(define dot-resolved?
  (lambda (v)
    (not (list? v))))
(define leftmost-dot
  (lambda (v)
    (if (dot-resolved? (dot-instance v))
        (dot-instance v)
        (leftmost-dot (dot-instance v)))))

;--------------------------------------
; FUNCTION DEFINITION AND EXECUTION HANDLING
;--------------------------------------
; formats:
; function:    (function name (params) ((body)
; f closure:   (name (params) ((body)) (state-function)

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
        (pop-n-frames (- n 1) (remaining-frames environment)))))

; Create the correct environment from the closure, bind the parameters, call the body
(define call-function
  (lambda (closure actual-params this environment class-list return throw next)
    (interpret-statement-list (closure-body closure)
                              this
                              (add-parameters
                               (closure-name closure)
                               (cons this actual-params)
                               (cons 'this (closure-formal-params closure))
                               environment
                               ((closure-environment-builder closure) environment)
                               this
                               class-list
                               throw)
                              class-list
                              return
                              (lambda (env) (error "Break used outside of loop"))
                              (lambda (env) (error "Continue used outside of loop"))
                              throw
                              next)))

; Evaluate the actual parameters to a function call and add them to the environment
(define add-parameters
  (lambda (function-name actual-params formal-params call-environment function-environment this class-list throw)
    (cond
      ((and (null? actual-params) (null? formal-params)) function-environment)
      ((or (null? actual-params) (null? formal-params)) (error "The actual parameters do not match the formal parameters for:" function-name))
      ((eq? (first-param formal-params) 'this) (add-parameters function-name
                                                               (remaining-params actual-params)
                                                               (remaining-params formal-params)
                                                               call-environment
                                                               (insert (first-param formal-params) (first-param actual-params) function-environment)
                                                               this
                                                               class-list
                                                               throw))
      (else (add-parameters function-name
                            (remaining-params actual-params)
                            (remaining-params formal-params)
                            call-environment
                            (insert (first-param formal-params) (eval-expression (first-param actual-params) this call-environment class-list (lambda (v) v) throw) function-environment)
                            this
                            class-list
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
(define remaining-frames cdr)

; ------------------------
; STATEMENT EXECUTION FUNCTIONS
; ------------------------

; interprets a list of statements. The state/environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list this environment class-list return break continue throw next)
    (if (null? statement-list)
        (next environment)
        (interpret-statement (car statement-list)
                             this
                             environment
                             class-list
                             return
                             break
                             continue
                             throw
                             (lambda (env) (interpret-statement-list (cdr statement-list) this env class-list return break continue throw next))))))

; interpret a statement in the environment with continuations for return, break, continue, throw, and "next statement"
(define interpret-statement
  (lambda (statement this environment class-list return break continue throw next)
    (cond
      ((eq? 'function (statement-type statement)) (next environment)) ; Function definitions are already handled in global compile
      ((eq? 'funcall (statement-type statement))  (interpret-function statement this environment class-list next throw))
      ((eq? 'return (statement-type statement))   (interpret-return statement this environment class-list return throw))
      ((eq? 'var (statement-type statement))      (interpret-declare statement this environment class-list next throw))
      ((eq? '= (statement-type statement))        (interpret-assign statement this environment class-list throw next))
      ((eq? 'if (statement-type statement))       (interpret-if statement this environment class-list return break continue throw next))
      ((eq? 'while (statement-type statement))    (interpret-while statement this environment class-list return throw next))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement))    (break environment))
      ((eq? 'begin (statement-type statement))    (interpret-block statement this environment class-list return break continue throw next))
      ((eq? 'throw (statement-type statement))    (interpret-throw statement this environment class-list throw))
      ((eq? 'try (statement-type statement))      (interpret-try statement this environment class-list return break continue throw next))
      (else                                       (error "Unknown statement:" (statement-type statement))))))

; Calls a function and ignores the return value (since this function is being called outside of an assignment)
(define interpret-function
  (lambda (statement this environment class-list next throw)
    (if (list? (get-function-call-name statement))
        (call-function (lookup-in-closure-list
                        (operand2 (get-function-call-name statement))
                        (closure-methods (lookup-in-class-list (get-runtime-type (operand1 (get-function-call-name statement)) this environment class-list) class-list)))
                       (get-function-actual-params statement)
                       (lookup (dot-instance (operand1 statement)) environment)
                       environment
                       class-list
                       (lambda (return-val) (next environment))
                       throw
                       (lambda (env) (next environment))) ; Ignore the environment returned by the function call
        (call-function (lookup (get-function-call-name statement) environment)
                       (get-function-actual-params statement)
                       this
                       environment
                       class-list
                       (lambda (return-val) (next environment))
                       throw
                       (lambda (env) (next environment)))))) ; Ignore the environment returned by the function call
        
    
; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement this environment class-list return throw)
    (eval-expression (get-expr statement) this environment class-list (lambda (v) (return v)) throw)))

; Adds a new variable binding to the environment. There may be an assignment with the variable
(define interpret-declare
  (lambda (statement this environment class-list next throw)
    (if (exists-declare-value? statement)
        (eval-expression (get-declare-value statement) this environment class-list (lambda (v) (next (insert (get-declare-var statement) v environment))) throw)
        (next (insert (get-declare-var statement) 'novalue environment)))))

; Updates the environment to add a new binding for a variable
(define interpret-assign
  (lambda (statement this environment class-list throw next)
    (eval-expression (get-assign-rhs statement)
                     this
                     environment
                     class-list
                     (lambda (v)
                       ; Check to see if the assignment is to an instance var
                       ; TODO check for 'this in and then these two
                       (if (list? (get-assign-lhs statement))
                           ; Update the instance box in the environment
                           (next (update (leftmost-dot (get-assign-lhs statement))
                                         (set-instance-value (dot-var (get-assign-lhs statement))
                                                             v
                                                             (lookup (leftmost-dot (get-assign-lhs statement)) environment)
                                                             class-list)
                                         environment))
                           (next (update (get-assign-lhs statement) v environment))))
                     throw)))

; We need to check if there is an else condition. Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement this environment class-list return break continue throw next)
    (eval-expression (get-condition statement)
                     this
                     environment
                     class-list
                     (lambda (condition)
                       (cond
                         (condition (interpret-statement (get-then statement) this environment class-list return break continue throw next))
                         ((exists-else? statement) (interpret-statement (get-else statement) this environment class-list return break continue throw next))
                         (else (next environment))))
                     throw)))

; Interprets a while loop. We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement this environment class-list return throw next)
    (letrec ((loop (lambda (condition body environment)
                     (eval-expression condition
                                      this
                                      environment
                                      class-list
                                      (lambda (condition-v)
                                        (if condition-v
                                            (interpret-statement body
                                                                 this
                                                                 environment
                                                                 class-list
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
  (lambda (statement this environment class-list return break continue throw next)
    (interpret-statement-list (cdr statement)
                              this
                              (push-frame environment)
                              class-list
                              return
                              (lambda (env) (break (pop-frame env)))
                              (lambda (env) (continue (pop-frame env)))
                              throw ; Uses the current environent since changes inside the block are reflected in the boxes
                              (lambda (env) (next (pop-frame env))))))
                              

; We use a continuation to throw the proper value.  Because we are using boxes, the environment does not need to be thrown as well
(define interpret-throw
  (lambda (statement this environment class-list throw)
    (eval-expression (get-expr statement) this environment class-list (lambda (v) (throw v)) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement this environment class-list return break continue throw next finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (interpret-block finally-block this env class-list return break continue throw (lambda (env2) (throw ex))))) 
      ((not (eq? 'catch (statement-type catch-statement))) (error "Incorrect catch statement"))
      (else (lambda (ex)
              (interpret-statement-list 
               (get-body catch-statement)
               this
               (insert (catch-var catch-statement) ex (push-frame environment))
               class-list
               return 
               (lambda (env2) (break (pop-frame env2))) 
               (lambda (env2) (continue (pop-frame env2))) 
               (lambda (v) (throw v)) 
               (lambda (env2) (interpret-block finally-block (pop-frame env2) return break continue throw next))))))))

; To interpret a try block, we must adjust the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement this environment class-list return break continue throw next)
    (let* ((finally-block (make-finally-block (get-finally statement)))
           (try-block (make-try-block (get-try statement)))
           (new-return (lambda (v) (interpret-block finally-block this environment class-list return break continue throw (lambda (env2) (return v)))))
           (new-break (lambda (env) (interpret-block finally-block this env class-list return break continue throw (lambda (env2) (break env2)))))
           (new-continue (lambda (env) (interpret-block finally-block this env class-list return break continue throw (lambda (env2) (continue env2)))))
           (new-throw (create-throw-catch-continuation (get-catch statement) this environment class-list return break continue throw next finally-block)))
      (interpret-block try-block this environment class-list new-return new-break new-continue new-throw (lambda (env) (interpret-block finally-block this env class-list return break continue throw next))))))

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
  (lambda (expr this environment class-list value-cont throw)
    (cond
      ((number? expr) (value-cont expr))
      ((eq? expr 'true) (value-cont #t))
      ((eq? expr 'false) (value-cont #f))
      ((not (list? expr)) (value-cont (lookup-ref expr this environment class-list)))
      ((eq? (operator expr) 'new) (value-cont (eval-constructor (operand1 expr) environment class-list))) 
      ((eq? (operator expr) 'dot) (value-cont (get-instance-value (operand2 expr) (lookup-ref (operand1 expr) this environment class-list) class-list)))
      ((function-call? expr) (eval-function expr this environment class-list value-cont throw))
      (else (eval-operator expr this environment class-list value-cont throw)))))

; Search for a variable in the environment before the instance vars
(define lookup-ref
  (lambda (var this environment class-list)
    ;TODO delete: and (not (eq? this 'novalue)) (exists-in-list? var (instance-vals this)))
    (cond
      ((eq? var 'this) this)
      ((exists? var environment) (lookup var environment))
      (else (get-instance-value var this class-list)))))

; Create and return new instance of a class
(define eval-constructor
  (lambda (class environment class-list)
    ((closure-constructor (lookup-in-class-list class class-list)) environment class-list)))

; TODO is probably not the car
(define get-runtime-type
  (lambda (variable this environment class-list)
    (car (lookup-ref variable this environment class-list))))

; Get the value returned by a function call
; TODO search in class methods before environment by handling dot
;      the environment only holds nested functions
(define eval-function
  (lambda (statement this environment class-list value-cont throw)
    (if (list? (get-function-call-name statement))
        (call-function (lookup-in-closure-list
                        (operand2 (get-function-call-name statement))
                        (closure-methods (lookup-in-class-list (get-runtime-type (operand1 (get-function-call-name statement)) this environment class-list) class-list)))
                       (get-function-actual-params statement)
                       (lookup (dot-instance (operand1 statement)) environment)
                       environment
                       class-list
                       (lambda (return-val) (value-cont return-val))
                       throw
                       (lambda (env) (value-cont 'novalue)))
        (call-function (lookup (get-function-call-name statement) environment)
                       (get-function-actual-params statement)
                       this
                       environment
                       class-list
                       (lambda (return-val) (value-cont return-val))
                       throw
                       (lambda (env) (value-cont 'novalue))))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order.
(define eval-operator
  (lambda (expr this environment class-list value-cont throw)
    (cond
      ((eq? '! (operator expr)) (eval-expression (operand1 expr) this environment class-list (lambda (v) (value-cont (not v))) throw))
      ((negation? expr) (eval-expression (operand1 expr) this environment class-list (lambda (v) (value-cont (* -1 v))) throw))
      (else (eval-expression (operand1 expr) this environment class-list (lambda (v1) (eval-binary-op2 expr v1 this environment class-list value-cont throw)) throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr v1 this environment class-list value-cont throw)
    (eval-expression (operand2 expr)
                     this
                     environment
                     class-list
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

; Looks up a method in a list of closures, returns it if one exists
(define lookup-in-closure-list
  (lambda (name list)
    (cond
      ((null? list) (error "Method not in closure list"))
      ((eq? name (caar list)) (car list))
      (else (lookup-in-closure-list name (cdr list))))))
  
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
      ((null? l) 0)
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
