\ Microchip PIC.

\ PIC12 - 8-bit data, 12-bit instructions.
\ PIC16 - 8-bit data, 14-bit instructions.
\ PIC18 - 8-bit data, 16-bit instructions.
\ PIC24 - 16-bit data, 24-bit instructions.  Not included here.

hex

\ PIC12		\ PIC16		\ PIC18
\ 000 nop	\ 0000 nop	\ 0000 nop
\		\ 0001*reset	\ 0003 sleep
\ 002 option	\ 0008 return	\ 0004 clrwdt
\ 003 sleep	\ 0009 retfie	\ 0005 push
\		\ 000A*callw	\ 0006 pop
\		\ 000A*brw	\ 0007 daw
\		\ 0010*moviw	\ 0008*tblrd
\		\ 0018*movwi	\ 000C tblwr
\		\ 0020*movlb	\ 0010 retfie
\ 004 clrwdt	\ 0062 option	\ 0012 return
\ 004 tris	\ 0063 sleep	\ 0014*callw
\ 010 movlb			\ 00FF reset
\ 01E return
\ 01F retfie
\				\ 0100 movlb

\				\ 0200 mulwf
\ 020 movwf	\ 0080 movwf	\ 6E00 movwf
\ 040 clr	\ 0100 clr	\ 6A00 clrf
\ 080 subwf	\ 0200 subwf	\ 5C00 subwf
\ 0C0 decf	\ 0300 decf	\ 0400 decf
\ 100 irowf	\ 0400 irowf	\ 1000 iorwf
\ 140 andwf	\ 0500 andwf	\ 1400 andwf
\ 180 xorwf	\ 0600 xorwf	\ 1800 xorwf
\ 1C0 addwf	\ 0700 addwf	\ 2400 addwf
\ 200 movf	\ 0800 movf	\ 5000 movf
\ 240 comf	\ 0900 comf	\ 1C00 comf
\ 280 incf	\ 0A00 incf	\ 2800 incf
\ 2C0 decfsz	\ 0B00 decfsz	\ 2C00 decfsz
\ 300 rrf	\ 0C00 rrf	\ 3000 rrcf
\ 340 rlf	\ 0D00 rlf	\ 3400 rlcf
\ 380 swapf	\ 0E00 swapf	\ 3800 swapf
\ 3C0 incfsz	\ 0F00 incfsz	\ 3C00 incfsz
\				\ 2000 addwfc
\				\ 4000 rrncf
\				\ 4040 rlncf
\				\ 4080 infsnz
\				\ 40C0 dcfsnz
\				\ 5400 subfwb
\				\ 5800 subwfb

\				\ 6000 cpfslt
\				\ 6200 cpfseq
\				\ 6400 cpfsgt
\				\ 6600 tstfsz
\				\ 6800 setf
\				\ 6C00 negf

\				\ 7000 btg
\ 400 bcf	\ 1000 bcf	\ 9000 bcf
\ 500 bsf	\ 1400 bsf	\ 8000 bsf
\ 600 btfsc	\ 1800 btfsc	\ B000 btfsc
\ 700 btfss	\ 1C00 btfss	\ A000 btfss

\				\ C000 movff
\ 800 retlw
\ 900 call	\ 2000 call	\ D800 rcall
\ A00 goto	\ 2800 goto	\ D000 bra
\				\ EC00 call
\				\ EE00 lrsr
\				\ EF00 goto

\				\ E000 bz
\				\ E100 bnz
\				\ E200 bc
\				\ E300 bnc
\				\ E400 bov
\				\ E500 bnov
\				\ E600 bn
\				\ E700 bnn

\				\ E800 addfsr
\				\ E8C0 addulnk
\				\ E900 subfsr
\				\ EA00 pushl
\				\ EB00 movsf

\				\ 0D00 mullw
\ C00 movlw	\ 3000 movlw	\ 0E00 movlw
\		\ 3100*addfsr
\		\ 3180*movlp
\		\ 3200*bra
\		\ 3400 retlw	\ 0C00 retlw
\		\ 3500*lslf
\		\ 3600*lsrf
\		\ 3700*asrf
\ D00 irolw	\ 3800 irolw	\ 0900 iorlw
\ E00 andlw	\ 3900 andlw	\ 0B00 andlw
\ F00 xorlw	\ 3A00 xorlw	\ 0A00 xorlw
\		\ 3B00*subwfb
\		\ 3C00 sublw	\ 0800 sublw
\		\ 3D00*addwfc
\		\ 3E00 addlw	\ 0F00 addlw
\		\ 3F00*moviw
\		\ 3F80*movwi
