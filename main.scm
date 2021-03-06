(import (r7rs)
        (srfi-1)
        (srfi-13)
        (srfi-193)
        (ncurses)
        (shell)
        (chicken io)
        (chicken file posix)
        ;; while, should switch to simple let loop
        miscmacros)

(load "utility.scm")

;; Arguments

(define +args+ (cdr (command-line)))
(if (= 0 (length +args+))
    (begin (display "usage: te filename")
           (newline)
           (exit)))

;; Global file-buffer

(define +max-vals+ (cons (get-max-columns) (get-max-rows)))
(define file-buffer (read-n-lines (car +args+) (get-y +max-vals+)))
(define *buffer-changed* #f)


;; Drawing functions
;; TODO: switch so that the internal buffer is a list, and the draw functions
;; convert those lists to strings before printing.

(define (draw-empty-part line-number max-rows)
  (mvprintw line-number 0 "~")
  (if (= (add1 line-number) max-rows)
      #f
      (draw-empty-part (add1 line-number) max-rows)))

(define (draw-file-buffer row fb)
  (mvprintw row 0 (car fb))
  (if (= 0 (length (cdr fb)))
      #f
      (draw-file-buffer (add1 row) (cdr fb))))


;; Movement

(define (move-up buffer coords)
  (cons (get-x coords)
        (safe-sub1 (get-y coords))))

(define (move-down buffer coords +max-vals+)
  (cons (get-x coords)
        (safe-add1 (get-y coords) (sub1 (length file-buffer)))))

(define (move-left buffer coords)
  (cons (safe-sub1 (get-x coords))
        (get-y coords)))

(define (move-right buffer coords +max-vals+)
  (let ((line-length (length (string->list (list-ref buffer (get-y coords))))))
    (if (< (get-x coords) line-length)
        (cons (safe-add1 (get-x coords) (get-x +max-vals+))
              (get-y coords))
        (cons line-length (get-y coords)))))

(define (move-bol coords)
  (cons 0 (get-y coords)))

(define (move-eol buffer coords)
  (cons (length (string->list (list-ref buffer (get-y coords))))
        (get-y coords)))


;; Editing
;; seems way too complicated
;; may be fixed by making the buffer a vector?

(define (delete-char buffer coords)
  (let ((head (take (string->list (list-ref buffer (get-y coords))) (get-x coords)))
        (tail (drop (string->list (list-ref buffer (get-y coords))) (get-x coords))))
    (if (= 0 (length (string->list (list-ref buffer (get-y coords)))))
        (append (take buffer (get-y coords))
                (drop buffer (add1 (get-y coords))))
        (append (take buffer (get-y coords))
                (if (= 0 (length tail))
                    (list (list->string (reverse (cdr (reverse head)))))
                    (list (list->string (append (reverse (cdr (reverse head))) tail))))
                (drop buffer (add1 (get-y coords)))))))

(define (input-char buffer coords c)
  (append (take buffer (get-y coords))
          (list (string-insert (list-ref buffer (get-y coords)) (get-x coords) (string c)))
          (drop buffer (+ (get-y coords) 1))))


;; I/O
(define (save-file buffer filename)
  (let ((outf (file-open filename (+ open/write open/wronly))))
    (let loop ((lines-to-write buffer))
      (if (= 0 (length lines-to-write))
          (file-close outf)
          (begin (file-write outf (car lines-to-write))
                 (if (not (= 1 (length lines-to-write)))
                     (file-write outf "\n"))
                 (loop (cdr lines-to-write)))))))

;; Trigger this function on exiting the program
(on-exit endwin)

;; Entry point (main)

(define (main)
  (initscr)
  (cbreak)
  (noecho)
  (let ((should-exit #f)
        (user-coords (cons 0 0)) ; x y
        (last-key-escape? #f)
		(just-saved #f))
    (while (not should-exit)
      (clear)
      (curs_set 0) ;; hide cursor for drawing
      (draw-file-buffer 0 file-buffer)
      (draw-empty-part (length file-buffer) (get-y +max-vals+))
	  (when just-saved
		(mvprintw (get-y +max-vals+) 0 "Saved!")
		(set! just-saved #f))

      (move (get-y user-coords) (get-x user-coords))
      (curs_set 1) ;; show cursor again

      (refresh)

      (let ((c (getch)))
        (if last-key-escape?
            (begin
              (set! last-key-escape? #f)
              (cond ((eq? c #\j) (set! user-coords
                                   (move-left file-buffer user-coords)))
                    ((eq? c #\k) (set! user-coords
                                   (move-down file-buffer user-coords +max-vals+)))
                    ((eq? c #\l) (set! user-coords
                                   (move-up file-buffer user-coords)))
                    ((eq? c #\;) (set! user-coords
                                   (move-right file-buffer user-coords +max-vals+)))
                    ((eq? c #\a) (set! user-coords
                                   (move-bol user-coords)))
                    ((eq? c #\f) (set! user-coords
                                   (move-eol file-buffer user-coords)))
                    ((eq? c #\q) (set! should-exit #t))
                    ((eq? c #\d) (begin
                                   (set! user-coords
                                     (move-right file-buffer user-coords +max-vals+))
                                   (set! file-buffer (delete-char file-buffer user-coords))
                                   (set! user-coords (cons (safe-sub1 (get-x user-coords))
                                                           (get-y user-coords)))
                                   (when (= (get-y user-coords) (length file-buffer))
                                     (set! user-coords (cons (get-x user-coords)
                                                             (safe-sub1 (get-y user-coords)))))))
                    ((eq? c #\s) (begin (set! just-saved #t)
										(set! *buffer-changed* #f)
                                        (save-file file-buffer (car +args+))))))
            ;; else if
            (cond ((eq? c #\esc)
                   (set! last-key-escape? #t))
                  ;; not a shortcut, input literal character
                  (#t (begin (set! *buffer-changed* #t)
                             (set! file-buffer (input-char file-buffer user-coords c))
                             (set! user-coords (cons (safe-add1 (get-x user-coords)
                                                                (get-x +max-vals+))
                                                     (get-y user-coords))))))))
      

      ;; Exploit the fact that moving right correctly places you on the end of the line
      ;; for compensating moving up or down outside the range of a line.
      (when (> (get-x user-coords)
               (length (string->list (list-ref file-buffer (get-y user-coords)))))
        (set! user-coords (move-right file-buffer user-coords +max-vals+))))))

(main)
