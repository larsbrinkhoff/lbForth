code next
    MOVE (IP)+,W
    JMP (W)
end-code
code enter ( -- ) ( R: -- ret )
    MOVE IP,-(RP)
    ADD #4,W
    MOVE W,IP
end-code
code exit ( R: ret -- )
    MOVE (RP)+,IP
end-code
code dodoes ( -- addr ) ( R: -- ret )
    MOVE IP,-(RP)
    ADD #4,W
    MOVE W,-(SP)	MOVE T,-(SP) ; MOVE W,T
    MOVE -2(W),IP
end-code
code 0branch ( x -- )
    MOVE (SP)+,W	JZ T,zero
    JZ W,zero		ADD #2,IP
    ADD #2,IP		JMP nz
    JMP nz	      zero:
  zero:			MOVE (IP),IP
    MOVE (IP),IP      nz:
  nz:			MOVE (SP)+,T
end-code
code (literal) ( -- n )
    MOVE (IP)+,W	MOVE T,-(SP)
    MOVE W,-(SP)	MOVE (IP)+,T
end-code
code ! ( x addr -- )
    MOVE (SP)+,W	MOVE (SP)+,W
    MOVE (SP)+,T	MOVE W,(T)
    MOVE T,(W)		MOVE (SP)+,T
end-code
code @ ( addr -- x )
    MOVE (SP)+,W	MOVE (T),T
    MOVE (W),W
    MOVE W,-(SP)
end-code
code + ( x y -- x+y )
    MOVE (SP)+,W	MOVE (SP)+,W
    MOVE (SP)+,T	ADD W,T
    ADD T,W
    MOVE W,-(SP)
end-code
code >r  ( x -- ) ( R: -- x )
    MOVE (SP)+,W	MOVE T,-(RP)
    MOVE W,-(RP)	MOVE (SP)+,T
end-code
code r> ( -- x ) ( R: x -- )
    MOVE (RP)+,W	MOVE T,-(SP)
    MOVE W,-(SP)	MOVE (RP)+,T
end-code
code nand ( x y -- ~(x&y) )
    MOVE (SP)+,W	MOVE (SP)+,W
    MOVE (SP)+,T	NAND W,T
    NAND T,W
    MOVE W,-(SP)
end-code
code c! ( c addr -- )
    MOVE (SP)+,W	MOVE (SP)+,W
    MOVE (SP)+,T	MOVB W,(T)
    MOVB T,(W)		MOVE (SP)+,T
end-code
code c@ ( addr -- c )
    MOVE (SP)+,W	MOVB (T),T
    MOVB (W),W
    MOVE W,-(SP)
end-code



