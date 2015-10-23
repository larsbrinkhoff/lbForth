fail () {
  echo "Error: $1"
  exit 1
}

cd lib

for i in *.fth; do
  grep $i README >/dev/null || fail "lib/README is missing $i."
done

cat README | while read i; do
  set $i
  test -r "$1" || fail "lib/README contains $1, but here is no such file."
done

exit 0
