\ Definitions necessary for cross-compilation to the PDP-1 target.

\ vocabulary target
\ also target definitions

1 constant cell
: cells      ;
: aliged     ;
: align      ;
: header,    align here  name,  link, ;
\ :          header,  ['] enter jsp,  ] !csp ;
\ create     header,  ['] dovar jsp,  reveal ;
\ variable   create cell allot ;
\ code!      lastxt @ >code >h jsp, h> ;


((
IN HOST:
   : cell      4 ;
   : header,    align here  name,  link, ( code ) , 0 , ;
   : create     'dodoes header, reveal (does>) ;
   : variable   create cell allot ;

   5 "cell"    <link> <enter> 0 <(literal)> 4 <exit>
   7 "header,"  <link> <enter> 0 ... <exit>
   6 "create"   <link> <enter> 0 ... <exit>
   8 "variable" <link> <enter> 0 <create> <(literal)> 4 <allot> <exit>

IN HOST, TARGET COMPILER:
   1 constant cell
   : header,    align here  name,  link, ;
   : create     header,  ['] dovar jsp,  reveal ;
   : variable   create cell allot ;

   5 "cell"    <link> <dodoes> <?> 1
   7 "header,"  <link> <enter> 0 ... <exit>
   6 "create"   <link> <enter> 0 <header,> <(literal)> <dovar> <jsp,> <exit>
   8 "variable" <link> <enter> 0 <create> <literal 4> <allot> <exit>

AT TARGET:
   1 constant cell
   : variable   create 1 allot ;
   : create     header,  ['] dovar jsp,  reveal ;
   : variable   create cell allot ;

   5 "cell"    <link> <jsp docon> 1
   7 "header,"  <link> <jsp enter> ... <exit>
   6 "create"   <link> <jsp enter> ... <exit>
   8 "variable" <link> <jsp enter> <t-create> <(literal)> 1 <allot> <exit>
))
