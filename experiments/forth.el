;;;; Emacs Lisp version of inner interpreter.

;;; At this point, the dictionary is a vector.  Maybe consider using a
;;; buffer in the future?

(require 'cl)

(defvar *forth-ip*)
(defvar *forth-sp*)
(defvar *forth-rp*)

(defmacro* defcode (name &body body)
  `(defun ,name (w)
     ,@body))

(defcode forth-enter
  (push *forth-ip* *forth-rp*)
  (setq *forth-ip* (+ w 3)))

(defcode forth-exit
  (setq *forth-ip* (pop *forth-rp*)))

(defcode forth-lit
  (push (aref *forth-dictionary* *forth-ip*) *forth-sp*)
  (incf *forth-ip*))

(defcode forth-type
  (message (pop *forth-sp*)))

(defcode forth-bye
  (throw 'bye nil))

(defcode forth-car
  (push (car (pop *forth-sp*)) *forth-sp*))

(defcode forth-cdr
  (push (cdr (pop *forth-sp*)) *forth-sp*)))

(defcode forth-@
  (push (aref *forth-dictionary* (pop *forth-sp*))))

(defcode forth-!
  (setf (aref *forth-dictionary* (pop *forth-sp*))
	(pop *forth-sp*)))

(defcode forth-sp@
   (push *forth-sp* *forth-sp*))

(defcode forth-sp!
  (setq *forth-sp* (pop *forth-sp*)))

(defcode forth-rp@
   (push *forth-rp* *forth-sp*))

(defcode forth-rp!
  (setq *forth-rp* (pop *forth-sp*)))

(defcode forth-plus
  (push (+ (pop *forth-sp*) (pop *forth-sp*))
	*forth-sp*))

(defcode forth->r
  (push (pop *forth-rp*) *forth-sp*))

(defcode forth-r>
  (push (pop *forth-sp*) *forth-rp*))

(defvar *forth-dictionary*
  [nil

   "warm"	;1 name
   0		;2 link
   forth-enter	;3 code
   13		;4 lit
   "lbForth"	;5
   22           ;6 type
   9		;7 quit
   19		;8 bye

   "quit"	;9 name
   1		;10 link
   forth-enter	;11 code
   16		;12 exit

   "lit"	;13 name
   9		;14 link
   forth-lit	;15 code

   "exit"	;16 name
   13		;17 link
   forth-exit	;18 code

   "bye"	;19 name
   16		;20 link
   forth-bye	;21 code

   "type"	;22 name
   19		;23 link
   forth-type])	;24 code

(defun forth-next ()
  (prog1 (aref *forth-dictionary* *forth-ip*)
    (incf *forth-ip*)))

(defun forth-code (xt)
  (aref *forth-dictionary* (+ xt 2)))

(defun forth-execute (xt)
  (funcall (forth-code xt) xt))

(defun forth-cold ()
  (let ((*forth-sp* nil)
	(*forth-rp* nil)
	(*forth-ip* 4))
    (catch 'bye
      (while t
	(forth-execute (forth-next))))))
