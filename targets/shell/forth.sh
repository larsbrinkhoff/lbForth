#!/bin/sh
# Schell script version.

cell_1="warm"
cell_2=0
cell_3=enter_code
cell_4=9
cell_5=12
cell_6=19

cell_7="banner"
cell_8=1
cell_9=banner_code

cell_10="quit"
cell_11=7
cell_12=enter_code
cell_13=16

cell_14="exit"
cell_15=10
cell_16=exit_code

cell_17="bye"
cell_18=14
cell_19=bye_code

cold_code() {
    ip=4
    sp=100
    rp=200
    while :; do
	eval "w=\$cell_$ip"
	ip=`expr $ip + 1`
	eval "\$cell_$w"
    done
}

enter_code() {
    rp=`expr $rp - 1`
    eval "cell_$rp=$ip"
    ip=`expr $w + 1`
}

banner_code() {
    echo "lbForth (shell sript)"
}

exit_code() {
    eval "ip=\$cell_$rp"
    rp=`expr $rp + 1`
}

bye_code() {
    echo BYE
    exit 0
}

cold_code
