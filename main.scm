(import
  (srfi-193)
  (srfi-1)
  (ncurses)
  (shell)
  (chicken io)
  ; while
  miscmacros)

;; Arguments

(define +args+ (cdr (command-line)))
(assert (not (= 0 (length +args+))))

(define (read-file f)
  (call-with-input-file f (lambda (port)
							(read-lines port))))

;; Global file-buffer

(define file-buffer (read-file (car +args+)))


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
  (let ((line-length (length (string->list (nth (get-y coords) file-buffer)))))
	(if (< (get-x coords) line-length)
		(cons (safe-add1 (get-x coords) (get-x max-vals))
			  (get-y coords))
		(cons line-length (get-y coords)))))



;; Utility functions

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

(: nth (number (list *) -> *))
(define (nth n l)
  (when (or (< n 0)
			(> n (length l)))
	(error "Invalid index."))
  (if (= 0 n)
	  (car l)
	  (nth (sub1 n) (cdr l))))

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
		(c #\a)
		(user-coords (cons 0 0)) ; x y
		(max-vals (cons (get-max-x) (get-max-y))))
	(while (not should-exit)
	  (clear)
	  (draw-file-buffer 0 file-buffer)
	  (draw-empty-part (length file-buffer) (get-y max-vals))

	  (move (get-y user-coords) (get-x user-coords))

	  (refresh)

	  (set! c (getch))
	  (cond ((eq? c #\j) (set! user-coords (move-left  file-buffer user-coords)))
			((eq? c #\k) (set! user-coords (move-down  file-buffer user-coords max-vals)))
			((eq? c #\l) (set! user-coords (move-up    file-buffer user-coords)))
			((eq? c #\;) (set! user-coords (move-right file-buffer user-coords max-vals)))
			((eq? c #\q) (set! should-exit #t)))
	  ;; Exploit the fact that moving right correctly places you on the end of the line
	  ;; for compensating moving up or down outside the range of a line.
	  (when (> (get-x user-coords)
			   (length (string->list (nth (get-y user-coords) file-buffer))))
		(set! user-coords (move-right file-buffer user-coords max-vals))))))

(main)
