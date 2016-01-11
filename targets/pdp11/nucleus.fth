code next
    MOVE (IP)+,W
    JMP (W)
end-code
code docol ( -- ) ( R: -- ret )
    MOVE IP,-(RP)
    ADD #2,W
    MOVE W,IP
end-code
code exit ( R: ret -- )
    MOVE (RP)+,IP
end-code
code dodoes ( -- addr ) ( R: -- ret )
    MOVE (RP),T
    MOVE IP,(RP)
    MOVE T,IP
    ADD #2,W
    MOVE W,-(SP)
end-code
code 0branch ( x -- )
    MOVE (IP)+,W	MOVE (IP)+,W
    TST (SP)+		TST T
    BNE nz		BNE nz
    MOVE W,IP		MOVE W,IP
nz:		      nz:
			MOVE (SP)+,T
end-code
code (literal) ( -- n )
    MOVE (IP)+,-(SP)	MOVE T,-(SP)
			MOVE (IP)+,T
end-code
code ! ( x addr -- )
    MOVE (SP)+,W	MOVE (SP)+,(T)
    MOVE (SP)+,(W)	MOVE (SP)+,T
end-code
code @ ( addr -- x )
    MOVE @(SP),(SP)	MOVE (T),T
end-code
code + ( x y -- x+y )
    MOVE (SP)+,W	ADD (SP)+,T
    ADD W,(SP)
end-code
code >r  ( x -- ) ( R: -- x )
    MOVE (SP)+,-(RP)	MOVE T,-(RP)
			MOVE (SP)+,T
end-code
code r> ( -- x ) ( R: x -- )
    MOVE (RP)+,-(SP)	MOVE T,-(SP)
			MOVE (RP)+,T
end-code
code nand ( x y -- ~(x&y) )
    MOVE (SP)+,W
    COM W
    BIC W,(SP)
end-code
code c! ( c addr -- )
    MOVE (SP)+,W	MOVB (SP),(T)
    MOVB (SP),(W)	ADD #2,SP
    ADD #2,SP		MOVE (SP)+,T
end-code
code c@ ( addr -- c )
    MOVB @(SP),(SP)	MOVB (T),T
end-code



