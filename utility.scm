;; utility.scm
;; Utility functons used by other functions.

(import (r7rs)
        (shell)
        (srfi-13))

(define (string-insert s i t)
  (string-replace s t i i))

(: remove-trailing-newline (string -> string))
(define (remove-trailing-newline str)
  (list->string (reverse (cdr (reverse (string->list str))))))

(: get-x ((list number number) -> number))
(define (get-x coords)
  (car coords))

(: get-y ((list number number) -> number))
(define (get-y coords)
  (cdr coords))

(: safe-sub1 (number -> number))
(define (safe-sub1 n)
  (if (= n 0)
      0
      (sub1 n)))

(: safe-add1 (number number -> number))
(define (safe-add1 n max-val)
  (if (= n max-val)
      n
      (add1 n)))

(define (get-max-columns)
  (sub1 (string->number (remove-trailing-newline (capture (tput cols))))))

(define (get-max-rows)
  (sub1 (string->number (remove-trailing-newline (capture (tput lines))))))

;; modified https://stackoverflow.com/a/63548144
(define (read-n-lines file n)
  (let ((infile (open-input-file file)))
    (let loop ((lines '())
               (next-line (read-line infile))
               (lines-read 0))
      (if (or (eof-object? next-line)
              (= lines-read n))
          (begin (close-input-port infile)
                 (reverse lines))
          (loop (cons next-line lines)
                (read-line infile)
                (add1 lines-read))))))
