;;;; -*- lisp -*-

(defvar *this-word*)
(defvar *this-name*)
(defvar *previous-word*)
(defvar *non-immediate-words* nil)
(defvar *control-stack* nil)
(defvar *input*)
(defvar *output*)
(defvar *header*)
(defvar *words*)
(defvar *ip* 0)
(defvar *code* (make-array '(0) :adjustable t :fill-pointer 0))

(defun compile-forth (input-file
		      &optional
		      (output-file (output-name input-file))
		      (header-file (header-name input-file))
		      (words-file (words-name input-file)))
  (with-open-file (*input* input-file)

    (with-open-file (*output* output-file :direction :output)
      (output-line "#include \"forth.h\"")
      (output "#include \"~A\"" (namestring header-file))

      (with-open-file (*header* header-file :direction :output)
	(format *header* "~&void enter_code (struct word *);~%")
	(format *header* "~&void dovariable_code (struct word *);~%")

	(with-open-file (*words* words-file :direction :output)
	  (format *words* "~&#include \"forth.h\"~%")
	  (format *words* "~&#include \"~A\"~%" (namestring header-file))
	  (format *words* "~&xt_t words[1000] = {~%")

	  (let ((*previous-word* "0"))
	    (do ((word (read-word) (read-word)))
		((null word))
	      (compile-word word))

	    (format *words* "~&};~%")
	    (output "struct word *lastxt = ~A;" *previous-word*)
	    (format t "~&Non-immediate words used:~%")
	    (dolist (cons (sort *non-immediate-words*
				(lambda (x y) (< (cdr x) (cdr y)))))
	      (format t "~&~30<~A~> ~D~%" (car cons) (cdr cons)))))))))

(defun emit (string)
  (unless (stringp string)
    (setq string (format nil "~A" string)))
  (let ((cons (assoc string *non-immediate-words* :test #'string=)))
    (if cons
	(incf (cdr cons))
	(push (cons string 1) *non-immediate-words*)))
  (vector-push-extend string *code*)
  (incf *ip*))

(defun emit-word (word)
  (emit (format nil "&~A_word" (mangle-word word))))

(defun emit-literal (x)
  (emit-word "(literal)")
  (emit x))

(defun emit-branch (word dest)
  (emit-word word)
  (if (eq dest :unresolved)
      (push *ip* *control-stack*)
      (setq dest (format nil "&~A.param[~D]" *this-word* dest)))
  (emit dest))

(defun resolve-branch (dest &key (orig (pop *control-stack*)))
  (setf (aref *code* orig)
	(format nil "&~A.param[~D]" *this-word* dest)))

(defun output (format &rest args)
  (output-line (apply #'format nil format args)))

(defun output-line (line)
  (fresh-line *output*)
  (write-line line *output*))

(defun output-name (file)
  (merge-pathnames (make-pathname :type "c") file))

(defun header-name (file)
  (merge-pathnames (make-pathname :type "h") file))

(defun words-name (file)
  (merge-pathnames (make-pathname :name "words" :type "c") file))

(defvar *peeked-word* nil)

(defun read-word (&optional (delimiter nil delimp))
  (if *peeked-word*
      (prog1 *peeked-word*
	(setq *peeked-word* nil))
      (let ((word-end-p
	     (if delimp
		 (lambda (char) (eql char delimiter))
		 (progn
		   (peek-char t *input* nil)
		   (lambda (char) (whitespacep char))))))
	(do ((word nil)
	     (char (read-char *input* nil) (read-char *input* nil)))
	    ((or (null char)
		 (funcall word-end-p char))
	     word)
	  (setq word (concatenate 'string word (string char)))))))

(defun peek-word (&rest args)
  (let ((word (apply #'read-word args)))
    (setq *peeked-word* word)))

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

(defun declare-word (word)
  (format *header* "~&struct word ~A_word;~%" (mangle-word word))
  (format *words* "~&  &~A_word,~%" (mangle-word word)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defimmediate |:| ()
  (let* ((name (read-word))
	 (mangled (mangle-word name)))
    (setf (fill-pointer *code*) 0)
    (setq *ip* 0)
    (setq *this-name* name)
    (setq *this-word* (format nil "~A_word" mangled))
    (declare-word name)))
  
(defimmediate |;| ()
  (emit-word "exit")
  (let ((len (length *this-name*))
	(next-word (peek-word)))
    (when (equal next-word "immediate")
      (setq len (- len)))
    (output "struct word ~A = { ~D, \"~A\", ~A, enter_code, {"
	    *this-word* len (quoted *this-name*) *previous-word*))
  (setq *previous-word* (concatenate 'string "&" *this-word*))
  (do ((end (fill-pointer *code*))
       (i 0 (1+ i)))
      ((= i end))
    (output "  (cell)~A~:[~;,~] /* ~D */"
	    (aref *code* i) (/= (1+ i) end) i))
  (output-line "} };"))

(defimmediate does> ()
  (emit-word "exit"))

(defimmediate code ()
  (let* ((name (read-word))
	 (mangled (mangle-word name)))
    (read-line *input*)
    (output "void ~A_code (struct word *word)" mangled)
    (output-line "{")
    (do ((line (read-line *input*) (read-line *input*)))
	((equalp (string-trim " " line) "end-code"))
      (output-line line))
    (output-line "}")
    (declare-word name)
    (output "struct word ~A_word = { ~D, \"~A\", ~A, ~A_code, {} };"
	    mangled (length name) (quoted name) *previous-word* mangled)
    (setq *previous-word* (format nil "&~A_word" mangled))))

(defimmediate create ()
  (let* ((word (read-word))
	 (mangled (mangle-word word)))
    (declare-word word)
    (output "struct word ~A_word = { ~D, \"~A\", ~A, dovariable_code, {"
	    mangled (length word) (quoted word) *previous-word*)
    (do ((line (read-line *input*) (read-line *input*)))
	((equalp (string-trim " " line) ""))
      (with-input-from-string (*input* line)
	(let ((word1 (read-word))
	      (word2 (read-word))
	      (word3 (read-word)))
	  (unless (and (equal word1 "'") (equal word3 ","))
	    (error "can't handle CREATE data"))
	  (output "  (cell)&~A_word," (mangle-word word2)))))
    (output-line "} };")
    (setq *previous-word* (format nil "&~A_word" mangled))))

(defimmediate |(| ()
  (do ()
      ((eql (read-char *input*) #\)))))

(defimmediate \\ ()
  (do ()
      ((eql (read-char *input*) #\Newline))))

(defimmediate C ()
  (emit-literal (read-word)))

(defimmediate literal ()
  (error "can't handle literal"))

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

(defimmediate unresolved ()
  (emit-literal (format nil "&~A_word" (mangle-word (read-word))))
  (emit-word ",")
  (emit-word "here")
  (emit-literal "0")
  (emit-word ","))

(defimmediate if ()
  (emit-branch "0branch" :unresolved))

(defimmediate else ()
  (resolve-branch (+ *ip* 2))
  (emit-branch "branch" :unresolved))

(defimmediate then ()
  (resolve-branch *ip*))

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
    (resolve-branch *ip* :orig *leave*)
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
  (emit-branch "0branch" :unresolved))

(defimmediate repeat ()
  (resolve-branch (+ *ip* 2))
  (emit-branch "branch" (pop *control-stack*)))

(defimmediate until ()
  (emit-branch "0branch" (pop *control-stack*)))

(defimmediate |[| ()
  (error "can't handle literal"))

(defimmediate [char] ()
  (let ((char (char (read-word) 0)))
    (emit-literal (cond
		    ((char= char #\') "'\\''")
		    ((char= char #\\) "'\\\\'")
		    (t (format nil "'~A'" char))))))

(defimmediate |[']| ()
  (emit-literal (format nil "&~A_word" (mangle-word (read-word)))))

(defimmediate variable ()
  (let* ((word (read-word))
	 (mangled (mangle-word word)))
    (declare-word word)
    (output "struct word ~A_word = { ~D, \"~A\", ~A, dovariable_code, { 0 } };"
	    mangled (length word) (quoted word) *previous-word*)
    (setq *previous-word* (format nil "&~A_word" mangled))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-word (word)
  (cond
    ((immediate-word word)
     (funcall (immediate-word word)))
    ((multiple-value-bind (i p) (parse-integer word :junk-allowed t)
       (when (and i (= p (length word)))
	 (emit-literal i)
	 t)))
    (t
     (emit-word word))))

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
