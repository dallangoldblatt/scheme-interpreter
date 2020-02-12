#lang racket
(require "simpleParser.rkt")

(define interpret
  (lambda (file)
    (parser file)))