\ Assembler for x86.

\ Adds to FORTH vocabulary: ASSEMBLER CODE ;CODE.
\ Creates ASSEMBLER vocabulary with: END-CODE and x86 opcodes.

s" search.fth" included

vocabulary assembler

also assembler definitions

' here  defer here  is here
' c,    defer c,    is c,

: h,   dup c,  8 rshift c, ;
: ,   dup h,  16 rshift h, ;

: (;code)      r> code! ;
: start-code   also assembler ;
: end-code     align previous ;

base @  hex

: nop,     90 c, ;
: repz,    f3 c, ;
: ret,     c3 c, ;

: next,    ret, ;
: call,    e8 c, , ;
: jump,    e9 c, , ;

base !  previous definitions  also assembler

: code    create  latestxt >body code!  start-code  ;
: ;code   postpone (;code) reveal postpone [ ?csp start-code ; immediate

previous

\ code foo
\    next,
\ end-code
\ 
\ : bar   create 42 , ;code  base @ hex
\    8b c, 52 c, 1c c, \ mov 28(%edx),%edx
\    next,
\ end-code  base !

variable imm
variable op-size
variable mod-r/m
: # ( x -- x )   1 imm ! ;
: !op8   0 op-size ! ;
: !op32   1 op-size ! ;
: !op16   66 c,  !op32 ;
: al ( -- x )   !op8 0 ;
: ax ( -- x )   !op16 0 ;
: eax ( -- x )   !op32 0 ;
: add ( src dst -- )  imm @ if 80 op-size @ + mod-r/m + c,
   else 00 op-size @ + c, then ;

code exit
   ip edx mov,
   edx ) eax mov,
   4 # edx add,
   edx ip mov,
   next,
end-code

code dodoes
   ip ecx mov,
   ebx push,
   -4 ecx )# ebx lea,
   ebx ip mov,
   1C edx )# ebx lea,
   ebx -4 ecx )# mov,
   ip ecx mov,
   -4 ecx )# ebx lea,
   ebx ip mov,
   eax -4 ecx )# mov,
   14 edx )# eax mov,
   ebx pop,
   next,
end-code

code 0branch
   ip edx mov,
   ebx push,
   eax ) ecx mov,
   4 # eax add,
   edx ) ebx mov,
   4 # edx add,
   edx ip mov,
   ebx ebx test,
   ecx eax cmove,
   ebx pop,
   next,
end-code

code (literal)
  d0:	8b 15 1c 00 00 00    	mov    0x1c,%edx
  d6:	8d 4a fc             	lea    -0x4(%edx),%ecx
  d9:	89 0d 1c 00 00 00    	mov    %ecx,0x1c
  df:	8b 08                	mov    (%eax),%ecx
  e1:	83 c0 04             	add    $0x4,%eax
  e4:	89 4a fc             	mov    %ecx,-0x4(%edx)
  e7:	c3                   	ret    
  e8:	90                   	nop
  e9:	8d b4 26 00 00 00 00 	lea    0x0(%esi,%eiz,1),%esi
end-code

code store
  f0:	8b 15 1c 00 00 00    	mov    0x1c,%edx
  f6:	53                   	push   %ebx
  f7:	8b 5a 04             	mov    0x4(%edx),%ebx
  fa:	8b 0a                	mov    (%edx),%ecx
  fc:	83 c2 08             	add    $0x8,%edx
  ff:	89 15 1c 00 00 00    	mov    %edx,0x1c
 105:	89 19                	mov    %ebx,(%ecx)
 107:	5b                   	pop    %ebx
 108:	c3                   	ret    
 109:	8d b4 26 00 00 00 00 	lea    0x0(%esi,%eiz,1),%esi
end-code

code fetch
 110:	8b 15 1c 00 00 00    	mov    0x1c,%edx
 116:	8b 0a                	mov    (%edx),%ecx
 118:	8b 09                	mov    (%ecx),%ecx
 11a:	89 0a                	mov    %ecx,(%edx)
 11c:	c3                   	ret    
 11d:	8d 76 00             	lea    0x0(%esi),%esi
end-code

code plus
 120:	83 ec 08             	sub    $0x8,%esp
 123:	8b 15 1c 00 00 00    	mov    0x1c,%edx
 129:	89 1c 24             	mov    %ebx,(%esp)
 12c:	89 74 24 04          	mov    %esi,0x4(%esp)
 130:	8b 5a 04             	mov    0x4(%edx),%ebx
 133:	8d 72 04             	lea    0x4(%edx),%esi
 136:	8b 0a                	mov    (%edx),%ecx
 138:	89 35 1c 00 00 00    	mov    %esi,0x1c
 13e:	8b 74 24 04          	mov    0x4(%esp),%esi
 142:	01 d9                	add    %ebx,%ecx
 144:	8b 1c 24             	mov    (%esp),%ebx
 147:	89 4a 04             	mov    %ecx,0x4(%edx)
 14a:	83 c4 08             	add    $0x8,%esp
 14d:	c3                   	ret    
 14e:	66 90                	xchg   %ax,%ax
end-code

code greaterr
 150:	8b 15 1c 00 00 00    	mov    0x1c,%edx
 156:	53                   	push   %ebx
 157:	8b 0a                	mov    (%edx),%ecx
 159:	83 c2 04             	add    $0x4,%edx
 15c:	89 15 1c 00 00 00    	mov    %edx,0x1c
 162:	8b 15 1c 00 00 00    	mov    0x1c,%edx
 168:	8d 5a fc             	lea    -0x4(%edx),%ebx
 16b:	89 1d 1c 00 00 00    	mov    %ebx,0x1c
 171:	89 4a fc             	mov    %ecx,-0x4(%edx)
 174:	5b                   	pop    %ebx
 175:	c3                   	ret    
 176:	8d 76 00             	lea    0x0(%esi),%esi
 179:	8d bc 27 00 00 00 00 	lea    0x0(%edi,%eiz,1),%edi
end-code

code rto
 180:	8b 15 1c 00 00 00    	mov    0x1c,%edx
 186:	53                   	push   %ebx
 187:	8b 0a                	mov    (%edx),%ecx
 189:	83 c2 04             	add    $0x4,%edx
 18c:	89 15 1c 00 00 00    	mov    %edx,0x1c
 192:	8b 15 1c 00 00 00    	mov    0x1c,%edx
 198:	8d 5a fc             	lea    -0x4(%edx),%ebx
 19b:	89 1d 1c 00 00 00    	mov    %ebx,0x1c
 1a1:	89 4a fc             	mov    %ecx,-0x4(%edx)
 1a4:	5b                   	pop    %ebx
 1a5:	c3                   	ret    
 1a6:	8d 76 00             	lea    0x0(%esi),%esi
 1a9:	8d bc 27 00 00 00 00 	lea    0x0(%edi,%eiz,1),%edi
end-code

code nand
 1b0:	83 ec 08             	sub    $0x8,%esp
 1b3:	8b 0d 1c 00 00 00    	mov    0x1c,%ecx
 1b9:	89 1c 24             	mov    %ebx,(%esp)
 1bc:	89 74 24 04          	mov    %esi,0x4(%esp)
 1c0:	8b 19                	mov    (%ecx),%ebx
 1c2:	8d 71 04             	lea    0x4(%ecx),%esi
 1c5:	8b 51 04             	mov    0x4(%ecx),%edx
 1c8:	89 35 1c 00 00 00    	mov    %esi,0x1c
 1ce:	8b 74 24 04          	mov    0x4(%esp),%esi
 1d2:	21 da                	and    %ebx,%edx
 1d4:	8b 1c 24             	mov    (%esp),%ebx
 1d7:	f7 d2                	not    %edx
 1d9:	89 51 04             	mov    %edx,0x4(%ecx)
 1dc:	83 c4 08             	add    $0x8,%esp
 1df:	c3                   	ret    
end-code

code cstore
 1e0:	8b 15 1c 00 00 00    	mov    0x1c,%edx
 1e6:	53                   	push   %ebx
 1e7:	8b 5a 04             	mov    0x4(%edx),%ebx
 1ea:	8b 0a                	mov    (%edx),%ecx
 1ec:	83 c2 08             	add    $0x8,%edx
 1ef:	89 15 1c 00 00 00    	mov    %edx,0x1c
 1f5:	88 19                	mov    %bl,(%ecx)
 1f7:	5b                   	pop    %ebx
 1f8:	c3                   	ret    
 1f9:	8d b4 26 00 00 00 00 	lea    0x0(%esi,%eiz,1),%esi
end-code

code cfetch
 200:	8b 15 1c 00 00 00    	mov    0x1c,%edx
 206:	8b 0a                	mov    (%edx),%ecx
 208:	0f b6 09             	movzbl (%ecx),%ecx
 20b:	89 0a                	mov    %ecx,(%edx)
 20d:	c3                   	ret    
 20e:	66 90                	xchg   %ax,%ax
end-code
