@echo off

set cell_1="warm"
set cell_2=0
set cell_3=enter_code
set cell_4=9
set cell_5=12
set cell_6=19

set cell_7="banner"
set cell_8=1
set cell_9=banner_code

set cell_10="quit"
set cell_11=7
set cell_12=enter_code
set cell_13=16

set cell_14="exit"
set cell_15=10
set cell_16=exit_code

set cell_17="bye"
set cell_18=14
set cell_19=bye_code

:cold_code
set w=3
set sp=100
set rp=200

:enter_code
set /a rp=rp-1
set cell_%rp%=%ip%
set /a ip=%w%+1

:next
call set w=%%cell_%ip%%%
set /a ip=ip+1
call goto %%cell_%w%%%

:banner_code
echo "lbForth (batch file)"
goto next

:exit_code
call set ip=%%cell_%rp%%%
set /a rp=rp+1
goto next

:bye_code
echo BYE
exit /b
