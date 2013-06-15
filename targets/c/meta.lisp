;;;; -*- lisp -*- Copyright 2004, 2013 Lars Brinkhoff

;;; Meta compiler to C target.
;;
;; Usage: (compile-forth "nucleus.fth" <optionally more sources> "kernel.fth")
;; Output: kernel.c and kernel.h.

;;; Words (partially) supported by this meta compiler:
;;
;; ( \ [IF] [ELSE] [THEN] [DEFINED] [UNDEFINED]
;; : ; IMMEDIATE DOES> DEFER CODE END-CODE
;; VARIABLE CREATE ALLOT , ' CELLS >CODE @ INVERT RSHIFT = CHAR -
;; [CHAR] ['] [ ] LITERAL POSTPONE IS ." S"
;; IF ELSE THEN DO LEAVE LOOP +LOOP BEGIN AGAIN WHILE REPEAT UNTIL
;; CELL JMP_BUF NAME_LENGTH TO_NEXT TO_CODE TO_DOES TO_BODY
;; .CS

;;; Restrictions and special features:
;;
;; Many words are immediate and only work in compilation mode,
;; i.e. they always append code to the current definition.
;;
;; When >CODE is interpeted, the top of the control stack must be the
;; result of ' (tick).  Likewise, @ only works on the result of >CODE.
;;
;; CODE may be followed by a \ comment which specifies the generated C
;; function declaration.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Retreive some information passed from C.
(load "params.lisp")

(defvar *this-word*)
(defvar *previous-word*)
(defvar *vocabulary* nil)
(defvar *control-stack* nil)
(defvar *state* 'interpret-word)
(defvar *input*)
(defvar *output*)
(defvar *header*)
(defvar *ip* 0)
(defvar *code* (make-array '(0) :adjustable t :fill-pointer 0))

(defun trivial-quit ()
  #+sbcl
  (sb-ext:quit)
  #+clisp
  (ext:quit)
  #+ecl
  (si:quit)
  #-(or sbcl clisp ecl)
  (implementation-dependent-quit))

#-ecl
(declaim (ftype function interpret-file compile-word header-name mangle-char
		mangle-word output output-line output-name output-end-of-word
		quoted read-word whitespacep))

(defun compile-forth (&rest input-files
		      &aux
		      (output-file (output-name input-files))
		      (header-file (header-name input-files)))
  (with-open-file (*output* output-file :direction :output
			                :if-exists :supersede)
    (output-line "#include \"forth.h\"")
    (output "#include \"~A\"" (namestring header-file))
    (with-open-file (*header* header-file :direction :output
					  :if-exists :supersede)
      (format *header* "~&code_t enter_code, dodoes_code;~%")
      (let ((*previous-word* "0"))
	(dolist (file input-files)
	  (interpret-file file))
	(output-end-of-word))))
  (trivial-quit))

(defun interpret-file (file)
  (with-open-file (*input* file)
    (do ((word (read-word) (read-word)))
	((null word))
      (funcall *state* word))))

(defun emit (string)
  (unless (stringp string)
    (setq string (format nil "~A" string)))
  (vector-push-extend string *code*)
  (incf *ip*))

(defun trunc-word (word)
  (subseq word 0 (min (length word) 15)))

(defun tick (word)
  (format nil "&~A_word" (mangle-word word)))

(defun word-body (word &optional (n 0))
  (format nil "~A.param[~D]" (tick word) n))

(defun branch-target (dest)
  (word-body *this-word* dest))

(defun emit-word (word)
  (emit (tick word)))

(defun emit-literal (x)
  (emit-word "(literal)")
  (if (and (integerp x) (plusp x))
      (emit (format nil "~DU" x))
      (emit x)))

(defun emit-branch (word dest)
  (emit-word word)
  (if (eq dest :unresolved)
      (push *ip* *control-stack*)
      (setq dest (branch-target dest)))
  (emit dest))

(defun resolve-branch (&optional (orig (pop *control-stack*)))
  (setf (aref *code* orig) (branch-target *ip*)))

(defun output (format &rest args)
  (output-line (apply #'format nil format args)))

(defun output-line (line)
  (fresh-line *output*)
  (write-line line *output*))

(defun output-name (files)
  (merge-pathnames (make-pathname :type "c") (car (last files))))

(defun output-end-of-word ()
  (unless (string= *previous-word* "0")
    (output-line "} };")))

(defun header-name (files)
  (merge-pathnames (make-pathname :type "h") (car (last files))))

(defvar *peeked-word* nil)

(defun read-word (&optional (delimiter nil delimp))
  (or (shiftf *peeked-word* nil)
      (let ((word-end-p
	     (if delimp
		 (lambda (char) (eql char delimiter))
		 (progn
		   (peek-char t *input* nil)
		   #'whitespacep))))
	(do ((word nil)
	     (char (read-char *input* nil) (read-char *input* nil)))
	    ((or (null char)
		 (funcall word-end-p char))
	     word)
	  (setq word (concatenate 'string word (string char)))))))

(defun peek-word (&rest args)
  (setq *peeked-word* (apply #'read-word args)))

(defun whitespacep (char)
  (or (char= char #\Tab)
      (char= char #\Space)
      (char= char #\Newline)))

(let ((table (make-hash-table :test #'equalp)))
  (defun immediate-word (name)
    (gethash name table))
  (defun (setf immediate-word) (fn name)
    (setf (gethash name table) fn)))

(defmacro defimmediate (name lambda-list &body body)
  `(eval-when (:execute :compile-toplevel :load-toplevel)
     (setf (immediate-word ,(string name))
           (lambda ,lambda-list ,@body))))

(defun compile-word (word)
  (cond
    ((immediate-word word)
     (funcall (immediate-word word)))
    ((multiple-value-bind (i p) (parse-integer word :junk-allowed t)
       (when (and i (= p (length word)))
	 (emit-literal i))))
    (t
     (emit-word word))))

(defmacro definterpreted (name lambda-list &body body)
  `(eval-when (:execute :compile-toplevel :load-toplevel)
     (setf (get ',name 'interpreted) (lambda ,lambda-list ,@body))))

(defun interpret-word (word)
  (let* ((sym (find-symbol (string-upcase word)))
	 (fn (and sym (get sym 'interpreted))))
    (cond
      (fn			(funcall fn))
      ((immediate-word word)	(funcall (immediate-word word)))
      (t			(push word *control-stack*)))))

(defun declare-word (word)
  (push word *vocabulary*)
  (format *header* "~&struct word ~A_word;~%" (mangle-word word)))

(defun output-header (name code does &optional immediatep codep)
  (declare-word name)
  (let* ((mangled (mangle-word name))
	 (lastxt (tick name))
	 (name (trunc-word name))
	 (len (length name)))
    (when immediatep
      (setq len (- len)))
    (unless codep
      (output-end-of-word))
    (output "struct word ~A_word = { ~D, \"~A\", ~A, ~A, ~A, {"
	    mangled len (quoted name) *previous-word* code does)
    (setq *previous-word* lastxt)))

(defun mangle-word (name)
  (cond
    ((and (char= (char name 0) #\()
	  (char= (char name (1- (length name))) #\)))
     (concatenate 'string "do" (mangle-word (string-trim "()" name))))
    ((equal name "0branch")	"zbranch")
    ((equal name ">r")		"to_r")
    ((equal name "2>r")		"two_to_r")
    ((equal name "r>")		"r_from")
    ((equal name "2r>")		"two_r_from")
    ((equal name "0=")		"zero_equals")
    ((equal name "0<")		"zero_less")
    ((equal name "0>")		"zero_greater")
    (t				(apply #'concatenate 'string
				       (map 'list #'mangle-char name)))))

(defun mangle-char (char)
  (case char
    (#\!	"store")
    (#\"	"quote")
    (#\#	"number")
    (#\'	"tick")
    (#\(	"paren")
    (#\*	"star")
    (#\+	"plus")
    (#\,	"comma")
    (#\-	"minus")
    (#\.	"dot")
    (#\/	"slash")
    (#\0	"zero")
    (#\1	"one")
    (#\2	"two")
    (#\3	"three")
    (#\4	"four")
    (#\:	"colon")
    (#\;	"semicolon")
    (#\<	"lt")
    (#\=	"eq")
    (#\>	"gt")
    (#\?	"query")
    (#\@	"fetch")
    (#\[	"lbracket")
    (#\]	"rbracket")
    (#\\	"backslash")
    (t		(string char))))

(defun quoted (name)
  (concatenate 'string
    (loop for char across name
	  when (or (eql char #\") (eql char #\\))
	    collect #\\
	  collect char)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definterpreted |:| ()
  (setf (fill-pointer *code*) 0)
  (setq *ip* 0)
  (setq *this-word* (read-word))
  (setq *state* 'compile-word))
  
(defun immediatep ()
  (and (equal (peek-word) "immediate")
       (read-word)))

;;;definterpreted immediate

(defimmediate |;| ()
  (emit-word "exit")
  (output-header *this-word* "enter_code" "0" (immediatep))
  (do ((end (fill-pointer *code*))
       (i 0 (1+ i)))
      ((= i end))
    (output "  (cell)~A~:[~;,~] /* ~D */"
	    (aref *code* i) (/= (1+ i) end) i))
  (setq *state* 'interpret-word))

(definterpreted defer ()
  (output-header (read-word) "dodoes_code" (word-body "(defer)"))
  (output "  (cell)~A" (tick "abort")))

(defimmediate is ()
  (emit-literal (word-body (read-word)))
  (emit-word "!"))

(defimmediate does> ()
  (emit-word "(does>)"))

(definterpreted code ()
  (let* ((name (read-word))
	 (mangled (format nil "~A_code" (mangle-word name)))
	 (special-code-p nil))
    (output-end-of-word)
    (cond
      ((equal (read-word) "\\")
       (let ((ret-type (read-word)))
	 (setq special-code-p t
	       mangled (read-word))
	 (output "~A ~A ~A" ret-type mangled (read-line *input*))))
      (t
       (read-line *input*)
       (output "xt_t * REGPARM ~A (xt_t *IP, struct word *word)" mangled)))
    (output-line "{")
    (do ((line (read-line *input*) (read-line *input*)))
	((equalp (string-trim " " line) "end-code"))
      (output-line line))
    (unless special-code-p
      (output-line "    return IP;"))
    (output-line "}")
    (output-header name (format nil "(code_t *)~A" mangled) "0" nil t)))

;;;definterpreted end-code

(defun pop-integer ()
  (let ((x (pop *control-stack*)))
    (etypecase x
      (number	x)
      (string	(parse-integer x)))))

(definterpreted allot ()
  (loop repeat (ceiling (pop-integer) *cell-size*)
        do (output "  (cell)0,")))

(definterpreted |,| ()
  (output "  (cell)~A," (pop *control-stack*)))

(definterpreted |'| ()
  (push (tick (read-word)) *control-stack*))

(defun cells (n)
  (* *cell-size* n))

(definterpreted cells ()
  (push (cells (pop-integer)) *control-stack*))

(definterpreted create ()
  (output-header (read-word) "dodoes_code" (word-body "nop" 0)))

(defimmediate |(| ()
  (do ()
      ((eql (read-char *input*) #\)))))

(defimmediate \\ ()
  (do ()
      ((eql (read-char *input*) #\Newline))))

(defimmediate literal ()
  (emit-literal (pop *control-stack*)))

(defimmediate postpone ()
  (let ((word (read-word)))
    (if (immediate-word word)
	(emit-word word)
	(progn
	  (emit-literal (tick word))
	  (emit-word ",")))))

(defimmediate |."| ()
  (let ((string (read-word #\")))
    (emit-literal (concatenate 'string "\"" (quoted string) "\""))
    (emit-literal (length string))
    (emit-word "type")))

(defimmediate |s"| ()
  (let ((string (read-word #\")))
    (emit-literal (concatenate 'string "\"" (quoted string) "\""))
    (emit-literal (length string))))

(defun roll (n list)
  (let ((tail (nthcdr n list)))
    (append (list (first tail)) (ldiff list tail) (rest tail))))

(defun cs-roll (n)
  (setq *control-stack* (roll n *control-stack*)))

(defimmediate if ()
  (emit-branch "0branch" :unresolved))

(defimmediate else ()
  (emit-branch "branch" :unresolved)
  (cs-roll 1)
  (resolve-branch))

(defimmediate then ()
  (resolve-branch))

(defvar *leave*)

(defimmediate do ()
  (emit-word "2>r")
  (setq *leave* nil)
  (push *ip* *control-stack*))

(defimmediate leave ()
  (emit-word "branch")
  (setq *leave* *ip*)
  (emit :unresolved))

(defun emit-loop (word)
  (emit-word word)
  (emit-branch "0branch" (pop *control-stack*))
  (when *leave*
    (resolve-branch *leave*)
    (setq *leave* nil))
  (emit-word "unloop"))
  
(defimmediate loop ()
  (emit-literal "1")
  (emit-loop "(+loop)"))

(defimmediate +loop ()
  (emit-loop "(+loop)"))

(defimmediate begin ()
  (push *ip* *control-stack*))

(defimmediate again ()
  (emit-branch "branch" (pop *control-stack*)))

(defimmediate while ()
  (emit-branch "0branch" :unresolved)
  (cs-roll 1))

(defimmediate repeat ()
  (emit-branch "branch" (pop *control-stack*))
  (resolve-branch))

(defimmediate until ()
  (emit-branch "0branch" (pop *control-stack*)))

(definterpreted - ()
  (let ((n2 (pop *control-stack*))
	(n1 (pop *control-stack*)))
    (push (- n1 n2) *control-stack*)))

(definterpreted char ()
  (push (char-code (char (read-word) 0)) *control-stack*))

(defimmediate [char] ()
  (let ((char (char (read-word) 0)))
    (emit-literal (cond
		    ((char= char #\') "'\\''")
		    ((char= char #\\) "'\\\\'")
		    (t (format nil "'~A'" char))))))

(defimmediate |[']| ()
  (emit-literal (tick (read-word))))

(definterpreted variable ()
  (output-header (read-word) "dodoes_code" (word-body "nop" 0))
  (output-line "  0"))

(definterpreted cell ()
  (push *cell-size* *control-stack*))

(defimmediate cell ()
  (emit-literal *cell-size*))

(definterpreted jmp_buf ()
  (push *jmp_buf-size* *control-stack*))

(defimmediate name_length ()
  (emit-literal *name-size*))

(defimmediate to_next ()
  (emit-literal *next-offset*))

(defimmediate to_code ()
  (emit-literal *code-offset*))

(defimmediate to_does ()
  (emit-literal *does-offset*))

(defimmediate to_body ()
  (emit-literal *body-offset*))

(defimmediate |[| ()
  (setq *state* 'interpret-word))

(definterpreted |]| ()
  (setq *state* 'compile-word))

(defun ends-with-p (string1 string2)
  (let ((n (- (length string1) (length string2))))
    (and (>= n 0) (string= string1 string2 :start1 n))))

(definterpreted >code ()
  (let ((x (pop *control-stack*)))
    (check-type x string)
    (assert (ends-with-p x "_word"))
    (push (format nil "~A.code" x) *control-stack*)))

(definterpreted |@| ()
  (let ((x (pop *control-stack*)))
    (check-type x string)
    (assert (ends-with-p x "_word.code"))
    (let ((y (subseq x 1 (- (length x) 10))))
      (push (format nil "~A_code" y) *control-stack*))))

(definterpreted invert ()
  (let ((x (pop-integer)))
    (push (logand (lognot x) (1- (ash 1 (* 8 *cell-size*))))
	  *control-stack*)))

(definterpreted rshift ()
  (let ((n (pop-integer))
	(x (pop-integer)))
    (push (ash x (- n)) *control-stack*)))

(definterpreted = ()
  (let ((x (pop-integer))
	(y (pop-integer)))
    (push (if (= x y) -1 0) *control-stack*)))

(defun defined (word)
  (if (member word *vocabulary* :test #'string=) -1 0))

(defimmediate [defined] ()
  (push (defined (read-word)) *control-stack*))

(defimmediate [undefined] ()
  (push (lognot (defined (read-word))) *control-stack*))

(defun skip-until (&rest words)
  (do ((word (read-word) (read-word)))
      ((some (lambda (x) (string-equal x word)) words))
    (when (string-equal word "[if]")
      (skip-until "[then]" "[else]"))))
      
(defimmediate [if] ()
  (when (zerop (pop-integer))
    (skip-until "[then]" "[else]")))

(defimmediate [else] ()
  (skip-until "[then]"))

(defimmediate [then] ()
  nil)

;;; Print control stack.
(defimmediate .cs ()
  (format *trace-output* "<~D> " (length *control-stack*))
  (dolist (x (reverse *control-stack*))
    (format *trace-output* "~A " x)))
