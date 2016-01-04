fail() {
    echo "File $1 wrong copyright:" `grep Copyright $1`
    exit 1
}

logopts="-1 --follow --diff-filter=M --format=format:%aD"

find . -name '*.fth' | while read i; do
   if grep 'Copyright .* Brinkhoff' "$i" >/dev/null; then
     d=`git log $logopts $i | cut -d' ' -f4`
     grep "$d" $i >/dev/null || fail "$i" "$d"
   fi
done

exit 0
