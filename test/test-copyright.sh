fail() {
    echo "File $1 wrong copyright:" `grep Copyright $1`
    exit 1
}

find . -name '*.fth' | while read i; do
   if grep 'Copyright .* Brinkhoff' "$i" >/dev/null; then
     d=`git log -1 --format=format:%aD $i | cut -d' ' -f4`
     grep "$d" $i >/dev/null || fail "$i" "$d"
   fi
done

exit 0
