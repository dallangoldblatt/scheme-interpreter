#lang racket

(require test-engine/racket-tests)
(require "../../interpreter.rkt")

(check-expect (interpret "test1.txt" "A") 15)
(check-expect (interpret "test2.txt" "A") 12)
(check-expect (interpret "test3.txt" "A") 125)
(check-expect (interpret "test4.txt" "A") 36)
(check-expect (interpret "test5.txt" "A") 54)
(check-expect (interpret "test6.txt" "A") 110)
(check-expect (interpret "test7.txt" "C") 26)
(check-expect (interpret "test8.txt" "Square") 117)
(check-expect (interpret "test9.txt" "Square") 32)
(check-expect (interpret "test10.txt" "List") 15)
(check-expect (interpret "test11.txt" "List") 123456)
(check-expect (interpret "test12.txt" "List") 5285)
(check-expect (interpret "test13.txt" "C") -716)

(test)