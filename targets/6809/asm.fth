\ Copyright 2016 Lars Brinkhoff

\ Assembler for 6809.

hex

( Indexed addressing postbyte. )

\ 00 5-bit offset
\ 80 auto increment, by 1
\ 81 auto increment, by 2
\ 82 auto decrement, by 1
\ 83 auto decrement, by 2
\ 84 no offset
\ 85 B offset
\ 86 A offset
\ 88 8-bit offset
\ 89 16-bit offset
\ 8B D offset
\ 8C PC + 8-bit offset
\ 8D PC + 16-bit offset
\ 8F extended indirect

\ Index register for above.
\ 00 X
\ 20 Y
\ 40 U
\ 60 S

( EXG and TFR postbyte. )

\ 00 D
\ 01 X
\ 02 Y
\ 03 U
\ 04 S
\ 05 PC
\ 06 A
\ 08 A
\ 09 B
\ 0A CC
\ 0B Direct Page

( PSH and PUL postbyte. )

\ 01 CC
\ 02 A
\ 04 B
\ 08 DP
\ 10 X
\ 20 Y
\ 40 S/U
\ 80 PC

( Opcodes. )

\ 00 neg
\ 03 com
\ 04 lsr
\ 06 ror
\ 07 asr
\ 08 asl
\ 08 lsl
\ 09 rol
\ 0A dec
\ 0C inc
\ 0D tst
\ 0E jmp
\ 0F clr

\ 12 nop
\ 13 sync
\ 16 lbra
\ 17 lbsr
\ 19 daa
\ 1A orcc
\ 1C andcc
\ 1D sex
\ 1E exg
\ 1F tfr

\ 20 bra
\ 21 brn
\ 22 bhi
\ 23 bls
\ 24 bcc
\ 25 bcs
\ 26 bne
\ 27 beq
\ 28 bvc
\ 29 bvs
\ 2A bpl
\ 2B bmi
\ 2C bge
\ 2D blt
\ 2E bgt
\ 2F ble

\ 30 lea
\ 34 psh
\ 35 pul
\ 39 rts
\ 3A abx
\ 3B rti
\ 3C cwai
\ 3D mul
\ 3F swi

\ 80 suba	\ C0 subb
\ 81 cmpa	\ C1 cmpb
\ 82 sbca	\ C2 sbcb
\ 83 subd	\ C3 addd
\ 84 anda	\ C4 andb
\ 85 bita	\ C5 bitb
\ 86 lda	\ C6 ldb
\ 87 sta	\ C7 stb
\ 88 eora	\ C8 eorb
\ 89 adca	\ C9 adcb
\ 8A ora	\ CA orb
\ 9B adda	\ CB addb
\ 8C cmpx	\ CC ldd
\ 8D bsr	\ CD std
\ 8E ldx	\ CE ldu
\ 8F stx	\ CF stu

\ 1020 lbra
\ 1021 lbrn
\ 1022 lbhi
\ 1023 lbls
\ 1024 lbcc
\ 1025 lbcs
\ 1026 lbne
\ 1027 lbeq
\ 1028 lbvc
\ 1029 lbvs
\ 102A lbpl
\ 102B lbmi
\ 102C lbge
\ 102D lblt
\ 102E lbgt
\ 102F lble

\ 103F swi2
\ 1083 cmpd
\ 108C cmpy
\ 108E ldy
\ 1093 cmpd
\ 109C cmpy
\ 109E ldy
\ 109F sty
\ 10CE lds
\ 10DF sts
\ 113F swi3
\ 1183 cmpu
\ 118C cmps
