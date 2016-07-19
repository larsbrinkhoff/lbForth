\ Copyright 2016 Lars Brinkhoff

\ Assembler for Thumb2.

hex

\ 0000 mov, \ reg
\ 0000 lsl, \ #
\ 0800 lsr, \ #
\ 1000 asr, \ #
\ 1800 add, \ reg + reg
\ 1A00 sub, \ reg + reg
\ 1C00 add, \ reg + #
\ 1E00 sub, \ reg + #
\ 2000 mov, \ #
\ 2800 cmp, \ #
\ 3000 add, \ #
\ 3800 sub, \ #
\ 4000 and, \ reg
\ 4040 eor, \ reg
\ 4080 lsl, \ reg
\ 40C0 lsr, \ reg
\ 4100 asr, \ reg
\ 4140 adc, \ reg
\ 4180 sbc, \ reg
\ 41C0 ror, \ reg
\ 4200 tst,
\ 4240 rsb, \ #
\ 4280 cmp, \ reg
\ 42C0 cmn, \ reg
\ 4300 orr, \ reg
\ 4340 mul,
\ 4380 bic, \ reg
\ 43C0 mvn, \ reg
\ 4400 add, \ reg
\ 4500 cmp, \ reg
\ 4600 mov, \ reg
\ 4700 bx,
\ 4780 blx,
\ 4800 ldr, \ pc
\ 5000 str, \ reg + reg
\ 5200 strh, \ reg + reg
\ 5400 strb, \ reg + reg
\ 5600 ldrsb, \ reg
\ 5800 ldr, \ reg
\ 5A00 ldrh, \ reg
\ 5C00 ldrb, \ reg
\ 5E00 ldrsh, \ reg
\ 6000 str, \ reg + #
\ 6800 ldr, \ reg + #
\ 7000 strb, \ reg + #
\ 7800 ldrb, \ reg + #
\ 8000 strh, \ reg + #
\ 8800 ldrh, \ reg + #
\ 9000 str, \ sp + #
\ 9800 ldr, \ sp + #
\ A000 adr,
\ A800 add, \ sp + reg
\ B000 add, \ sp + #
\ B080 sub, \ sp + #
\ B100 cbz,
\ B200 sxth,
\ B240 sxtb,
\ B280 uxth,
\ B2C0 uxtb,
\ B400 push,
\ B660 cps,
\ B900 cbnz,
\ BA00 rev,
\ BA40 rev16,
\ BAC0 revsh,
\ BC00 pop,
\ BE00 bkpt,
\ BF00 it,
\ BF00 nop,
\ BF10 yield,
\ BF20 wfe,
\ BF30 wfi,
\ BF40 sev,
\ C800 stm,
\ C800 ldm,
\ D000 beq,
\ D100 bne,
\ D200 bcs,
\ D300 bcc,
\ D400 bmi,
\ D500 bpl,
\ D600 bvs,
\ D700 bvc,
\ D800 bhi,
\ D900 bls,
\ DA00 bge,
\ DB00 blt,
\ DC00 bgt,
\ DD00 ble,
\ DE00 undefined,
\ DF00 svc,
\ E000 b,
\ E8000000 stm,
\ E8000000 ldm,
\ E8400000 \ load/store dual or exclusive, table branch
\ EA000000 \ data processing
\ EC000000 \ coprocessor
\ F0000000 and,
\ F0000F00 tst,
\ F0008000 b,
\ F000D000 bl,
\ F0400000 bic,
\ F0800000 orr,
\ F0800F00 mov,
\ F0C00000 orn,
\ F0C00F00 mvn,
\ F1000000 eor,
\ F1000F00 teq,
\ F2000000 add,
\ F2000F00 cmn,
\ F2800000 adc,
\ F2C00000 sbc,
\ F3400000 sub,
\ F3400F00 cmp,
\ F3800000 rsb,
\ F7008000 msr,
\ F7608000 mrs,
\ F8000000 ldr,
\ F8000000 \ store single data item
\ F8100000 ldrb,
\ F8300000 ldrh,
\ F8500000 ldr,
\ F8700000 \ undefined
\ FA000000 \ data processing
\ FB000000 mul,
\ FB000000 mla,
\ FB800000 mull,
\ FC000000 \ coprocessor


