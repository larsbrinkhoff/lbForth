;; dup ( x -- x x )
;; return ( x -- )
;; discard ( x -- )		"drop"
;; gotoifnil ( n [x1 x2] -- )	"0branch"
;; eq ( x x -- flag )		"="
;; call ( f n*x [n] -- y )
;; goto ( [x1 x2] -- )		"branch"
;; gotoifnonnill ( n [x1 x2] -- )
;; gotoifnilelsepop
;; gotoifnonnilelsepop
;; Rgoto, Rgotoifnil, Rgotoifnonnil, Rgotoifnilelsepop, Bgotoifnonnilelsepop
;; constant, constant2 ( -- x )	"(literal)"
;; catch ( x y -- z )
;; not ( x -- (not x) )
;; aref ( n a -- x )		"@"
;; aset ( n a x -- )		"!"
;; sub1 ( n -- n-1 )		"1-"
;; add1 ( n -- n-1 )		"1+"
;; eqlsign ( x1 x2 -- flag )e
;; gtr, lss, leq, geq ( x1 x2 -- flag )
;; diff ( n1 n2 -- n1-n2 )	"-"
;; negate ( n -- -n )
;; plus ( n1 n2 -- n1+n2 )	"+"
;; max, min
;; mult ( n1 n2 -- n1*n2 )	"*"
;; quo, rem
;; equal
;; stack_ref ( [n] -- x )	"~pick"
;; stack_set ( x [n] -- )
;; discardN ( n*x [n] -- )
;; cons ( x l1 -- l2 )
;; car ( l -- x )
;; cdr ( l1 -- l2 )
;; nth ( n l -- x )
;; setcar ( l x -- x )
;; setcdr ( l x -- x )

(defun compile-forth ()
  (byte-compile-lapcode
   '((byte-constant . 0)	;42 dup +
     (byte-dup)
     (byte-plus)
     (byte-return))))

;;; : ?dup   dup if dup then ;
(disassemble
 (let ((tag '(TAG nil)))
   (make-byte-code 0
     (byte-compile-lapcode ; ?dup
      `((byte-dup)			;dup
	(byte-constant . 0)
	(byte-eq)			
	(byte-goto-if-not-nil . ,tag)	;if
	(byte-dup)			;dup
	,tag				;then
	(byte-return)))
     [0] 100)))

;;; code rp@   ((byte-varref 0))
;;; code rp!   ((byte-varset 0))
;;; : >r      rp@ cons rp! ; inline
;;; : r>      rp@ dup cdr rp! car ; inline
;;; : r@      rp@ car ;
;;; : r!      rp@ setcar ;
;;; : rdrop   rp@ cdr rp! ;

;;; Forth TOS is on bytecode stack, rest is in list?
;;; : +   sp@ dup car swap plus swap cdr sp! ;


(defun make-forth-function ()
  ;; args may be an integer: mandatory + rest<<7 + nonrest<<8
  (make-byte-code '() ;args
		  (compile-forth)
		  [42]
		  100)) ;depth

(setq lexical-binding t)

(defun foo ()
  (1+ (bar 42)))
;;; 0	constant  bar
;;; 1	constant  42
;;; 2	call	  1
;;; 3	add1	  
;;; 4	return	  

(defun bar (x y)
  (+ x y))
;;;  args: 514 = 2<<8 + 2
;;; 0	stack-ref 1
;;; 1	stack-ref 1
;;; 2	plus	  
;;; 3	return	  
(defun bar (x y)
  (+ y x))
;;; 0	dup	  
;;; 1	stack-ref 2
;;; 2	plus	  
;;; 3	return	  
