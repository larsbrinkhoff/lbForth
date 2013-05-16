;;;; -*- lisp -*- Copyright 2004, 2013 Lars Brinkhoff

(load "params.lisp")

(defvar *this-word*)
(defvar *previous-word*)
(defvar *non-immediate-words* nil)
(defvar *control-stack* nil)
(defvar *state* 'interpret-word)
(defvar *input*)
(defvar *output*)
(defvar *header*)
(defvar *ip* 0)
(defvar *code* (make-array '(0) :adjustable t :fill-pointer 0))

(declaim (ftype function compile-word header-name mangle-char
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
	  (with-open-file (*input* file)
	    (do ((word (read-word) (read-word)))
		((null word))
	      (funcall *state* word))
	    #+(or)
	    (format t "~&Non-immediate words used:~%")
	    #+(or)
	    (dolist (cons (sort *non-immediate-words*
				(lambda (x y) (< (cdr x) (cdr y)))))
	      (format t "~&~30<~A~> ~D~%" (car cons) (cdr cons)))))
	(output-end-of-word))))
  #+sbcl
  (quit)
  #-sbcl
  (implementation-dependent-quit))

(defun emit (string)
  (unless (stringp string)
    (setq string (format nil "~A" string)))
  (let ((cons (assoc string *non-immediate-words* :test #'string=)))
    (if cons
	(incf (cdr cons))
	(push (cons string 1) *non-immediate-words*)))
  (vector-push-extend string *code*)
  (incf *ip*))

(defun trunc-word (word)
  (subseq word 0 (min (length word) 15)))

(defun emit-word (word)
  (emit (format nil "&~A_word" (mangle-word word))))

(defun emit-literal (x)
  (emit-word "(literal)")
  (emit x))

(defun emit-branch (word dest)
  (emit-word word)
  (if (eq dest :unresolved)
      (push *ip* *control-stack*)
      (setq dest (format nil "&~A_word.param[~D]"
			 (mangle-word *this-word*) dest)))
  (emit dest))

(defun resolve-branch (&optional (orig (pop *control-stack*)))
  (setf (aref *code* orig)
	(format nil "&~A_word.param[~D]" (mangle-word *this-word*) *ip*)))

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

(defun words-name (file)
  (merge-pathnames (make-pathname :name "words" :type "c") file))

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
  (format *header* "~&struct word ~A_word;~%" (mangle-word word)))

(defun output-header (name code does &optional immediatep codep)
  (declare-word name)
  (let* ((mangled (mangle-word name))
	 (name (trunc-word name))
	 (len (length name)))
    (when immediatep
      (setq len (- len)))
    (unless codep
      (output-end-of-word))
    (output "struct word ~A_word = { ~D, \"~A\", ~A, ~A, ~A, {"
	    mangled len (quoted name) *previous-word* code does)
    (setq *previous-word* (format nil "&~A_word" mangled))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definterpreted |:| ()
  (setf (fill-pointer *code*) 0)
  (setq *ip* 0)
  (setq *this-word* (read-word))
  (setq *state* 'compile-word))
  
(defimmediate |;| ()
  (emit-word "exit")
  (output-header *this-word* "enter_code" "0" (equal (peek-word) "immediate"))
  (do ((end (fill-pointer *code*))
       (i 0 (1+ i)))
      ((= i end))
    (output "  (cell)~A~:[~;,~] /* ~D */"
	    (aref *code* i) (/= (1+ i) end) i))
  (setq *state* 'interpret-word))

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

(defun pop-integer ()
  (let ((x (pop *control-stack*)))
    (etypecase x
      (number	x)
      (string	(parse-integer x)))))

(definterpreted allot ()
  (loop repeat (ceiling (pop-integer) *sizeof-cell*)
        do (output "  (cell)0,")))

(definterpreted |,| ()
  (output "  (cell)~A," (pop *control-stack*)))

(definterpreted |'| ()
  (push (format nil "&~A_word" (mangle-word (read-word))) *control-stack*))

(defun cells (n)
  (* *sizeof-cell* n))

(definterpreted cells ()
  (push (cells (pop-integer)) *control-stack*))

(definterpreted create ()
  (output-header (read-word) "dodoes_code" "&tickexit_word.param[0]"))

(defimmediate |(| ()
  (do ()
      ((eql (read-char *input*) #\)))))

(defimmediate \\ ()
  (do ()
      ((eql (read-char *input*) #\Newline))))

(defimmediate literal ()
  (emit-literal (pop *control-stack*)))

(defimmediate postpone ()
  (let* ((word (read-word))
	 (code (format nil "&~A_word" (mangle-word word))))
    (if (immediate-word word)
	(emit-word word)
	(progn
	  (emit-literal code)
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

(defimmediate [char] ()
  (let ((char (char (read-word) 0)))
    (emit-literal (cond
		    ((char= char #\') "'\\''")
		    ((char= char #\\) "'\\\\'")
		    (t (format nil "'~A'" char))))))

(defimmediate |[']| ()
  (emit-literal (format nil "&~A_word" (mangle-word (read-word)))))

(definterpreted variable ()
  (output-header (read-word) "dodoes_code" "&tickexit_word.param[0]")
  (output-line "  0"))

(defimmediate /cell ()
  (emit-literal *sizeof-cell*))

(definterpreted jmp_buf ()
  (push *sizeof-jmp_buf* *control-stack*))

(defimmediate name_length ()
  (emit-literal *NAME_LENGTH*))

(defimmediate to_next ()
  (emit-literal *TO_NEXT*))

(defimmediate to_code ()
  (emit-literal *TO_CODE*))

(defimmediate to_does ()
  (emit-literal *TO_DOES*))

(defimmediate to_body ()
  (emit-literal *TO_BODY*))

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
    (push (logand (lognot x) (1- (ash 1 (* 8 *sizeof-cell*))))
	  *control-stack*)))

(definterpreted rshift ()
  (let ((n (pop-integer))
	(x (pop-integer)))
    (push (ash x (- n)) *control-stack*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
