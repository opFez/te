(import
  (srfi-1)
  (srfi-13)
  (srfi-193)
  (ncurses)
  (shell)
  (chicken io)
  ; while
  miscmacros)

;; Arguments

(define +args+ (cdr (command-line)))
(if (= 0 (length +args+))
	(begin (display "usage: te filename")
		   (newline)
		   (exit)))

;; should be moved
(: read-file (string -> (list string)))
(define (read-file f)
  (call-with-input-file f (lambda (port)
							(read-lines port))))


;; Global file-buffer

(define file-buffer (read-file (car +args+)))
(define buffer-changed? #f)


;; Drawing functions

(define (draw-empty-part line-number max-rows)
  (mvprintw line-number 0 "~")
  (if (= line-number max-rows)
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

(define (move-down buffer coords max-vals)
  (cons (get-x coords)
		(safe-add1 (get-y coords) (sub1 (length file-buffer)))))

(define (move-left buffer coords)
  (cons (safe-sub1 (get-x coords))
		(get-y coords)))

(define (move-right buffer coords max-vals)
  (let ((line-length (length (string->list (list-ref buffer (get-y coords))))))
	(if (< (get-x coords) line-length)
		(cons (safe-add1 (get-x coords) (get-x max-vals))
			  (get-y coords))
		(cons line-length (get-y coords)))))


;; Editing
;; seems way too complicated
;; may be fixed by making the buffer a vector?

(define (delete-char buffer coords)
  (let ((head (take (string->list (list-ref buffer (get-y coords))) (get-x coords)))
		(tail (drop (string->list (list-ref buffer (get-y coords))) (get-x coords))))
	(append (take buffer (get-y coords))
			(if (= 0 (length tail))
				(list (list->string (reverse (cdr (reverse head)))))
				(list (list->string (append (reverse (cdr (reverse head))) tail))))
			(drop buffer (+ (get-y coords) 1)))))

(define (input-char buffer coords c)
  (append (take buffer (get-y coords))
		  (list (string-insert (list-ref buffer (get-y coords)) (get-x coords) (string c)))
		  (drop buffer (+ (get-y coords) 1))))


;; Utility functions

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

(define (get-max-x)
  (sub1 (string->number (remove-trailing-newline (capture (tput cols))))))

(define (get-max-y)
  (sub1 (string->number (remove-trailing-newline (capture (tput lines))))))

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
		(max-vals (cons (get-max-x) (get-max-y))))
	(while (not should-exit)
	  (clear)
	  (curs_set 0) ;; hide cursor for drawing
	  (draw-file-buffer 0 file-buffer)
	  (draw-empty-part (length file-buffer) (get-y max-vals))

	  (move (get-y user-coords) (get-x user-coords))
	  (curs_set 1) ;; show cursor again

	  (refresh)

	  (let ((c (getch)))
		(if last-key-escape?
			(begin
			  (set! last-key-escape? #f)
			  (cond ((eq? c #\j) (set! user-coords (move-left  file-buffer user-coords)))
					((eq? c #\k) (set! user-coords (move-down  file-buffer user-coords max-vals)))
					((eq? c #\l) (set! user-coords (move-up    file-buffer user-coords)))
					((eq? c #\;) (set! user-coords (move-right file-buffer user-coords max-vals)))
					((eq? c #\q) (set! should-exit #t))
					((eq? c #\d) (begin
								   (set! user-coords (move-right file-buffer user-coords max-vals))
								   (set! file-buffer (delete-char file-buffer user-coords))
								   (set! user-coords (cons (safe-sub1 (get-x user-coords))
														   (get-y user-coords)))))))
			;; else if
			(cond ((eq? c #\esc)
				   (set! last-key-escape? #t))
				  ;; not a shortcut, input literal character
				  (#t (begin (set! file-buffer (input-char file-buffer user-coords c))
							 (set! user-coords (cons (safe-add1 (get-x user-coords)
																(get-x max-vals))
													 (get-y user-coords))))))))
	  

	  ;; Exploit the fact that moving right correctly places you on the end of the line
	  ;; for compensating moving up or down outside the range of a line.
	  (when (> (get-x user-coords)
			   (length (string->list (list-ref file-buffer (get-y user-coords)))))
		(set! user-coords (move-right file-buffer user-coords max-vals))))))

(main)
