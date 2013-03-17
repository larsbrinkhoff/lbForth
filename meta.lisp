;;;; -*- lisp -*- Copyright 2004, 2013 Lars Brinkhoff

(defvar *this-word*)
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

    (with-open-file (*output* output-file :direction :output
					  :if-exists :supersede)
      (output-line "#include \"forth.h\"")
      (output "#include \"~A\"" (namestring header-file))

      (with-open-file (*header* header-file :direction :output
					    :if-exists :supersede)
	(format *header* "~&xt_t * enter_code (xt_t *, struct word *) REGPARM;~%")
	(format *header* "~&xt_t * dodoes_code (xt_t *, struct word *) REGPARM;~%")

	(with-open-file (*words* words-file :direction :output
				            :if-exists :supersede)
	  (format *words* "~&#include \"forth.h\"~%")
	  (format *words* "~&#include \"~A\"~%" (namestring header-file))
	  (format *words* "~&xt_t words[1000] = {~%")

	  (let ((*previous-word* "0"))
	    (do ((word (read-word) (read-word)))
		((null word))
	      (compile-word word))

	    (format *words* "~&};~%")
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

(defun declare-word (word)
  (format *header* "~&struct word ~A_word;~%" (mangle-word word))
  (format *words* "~&  &~A_word,~%" (mangle-word word)))

(defun output-header (name code does &optional immediatep)
  (declare-word name)
  (let* ((mangled (mangle-word name))
	 (name (trunc-word name))
	 (len (length name)))
    (when immediatep
      (setq len (- len)))
    (output "struct word ~A_word = { ~D, \"~A\", ~A, ~A, ~A, {"
	    mangled len (quoted name) *previous-word* code does)
    (setq *previous-word* (format nil "&~A_word" mangled))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defimmediate |:| ()
  (setf (fill-pointer *code*) 0)
  (setq *ip* 0)
  (setq *this-word* (read-word)))
  
(defimmediate |;| ()
  (emit-word "exit")
  (output-header *this-word* "enter_code" "0" (equal (peek-word) "immediate"))
  (do ((end (fill-pointer *code*))
       (i 0 (1+ i)))
      ((= i end))
    (output "  (cell)~A~:[~;,~] /* ~D */"
	    (aref *code* i) (/= (1+ i) end) i))
  (output-line "} };"))

(defimmediate does> ()
  (emit-word "(does>)"))

(defimmediate code ()
  (let* ((name (read-word))
	 (mangled (mangle-word name)))
    (read-line *input*)
    (output "xt_t * REGPARM ~A_code (xt_t *IP, struct word *word)" mangled)
    (output-line "{")
    (do ((line (read-line *input*) (read-line *input*)))
	((equalp (string-trim " " line) "end-code"))
      (output-line line))
    (output-line "    return IP;")
    (output-line "}")
    (output-header name (format nil "~A_code" mangled) "0")
    (output-line "} };")))

(defimmediate create ()
  (output-header (read-word) "dodoes_code" "&tickexit_word.param[0]")
  (do ((line (read-line *input*) (read-line *input*)))
      ((equalp (string-trim " " line) ""))
    (with-input-from-string (*input* line)
      (let ((word1 (read-word))
	    (word2 (read-word))
	    (word3 (read-word)))
	(cond ((and (equal word1 "'") (equal word3 ","))
	       (output "  (cell)&~A_word," (mangle-word word2)))
	      ((and (equal word1 "C") (equal word3 ","))
	       (output "  (cell)~A," word2))
	      (t
	       (error "can't handle CREATE data"))))))
  (output-line "} };"))

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

(defun roll (n list)
  (let ((tail (nthcdr n list)))
    (append (list (first tail)) (ldiff list tail) (rest tail))))

(defun cs-roll (n)
  (setq *control-stack* (roll n *control-stack*)))

(defimmediate unresolved ()
  (emit-literal (format nil "&~A_word" (mangle-word (read-word))))
  (emit-word ",")
  (emit-word "here")
  (emit-literal "0")
  (emit-word ","))

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

(defimmediate |[| ()
  (error "can't handle ["))

(defimmediate [char] ()
  (let ((char (char (read-word) 0)))
    (emit-literal (cond
		    ((char= char #\') "'\\''")
		    ((char= char #\\) "'\\\\'")
		    (t (format nil "'~A'" char))))))

(defimmediate |[']| ()
  (emit-literal (format nil "&~A_word" (mangle-word (read-word)))))

(defimmediate variable ()
  (output-header (read-word) "dodoes_code" "&tickexit_word.param[0]")
  (output-line "  0")
  (output-line "} };"))

(defimmediate |rp!| ()
  (emit-literal "&RP")
  (emit-word "!"))

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
