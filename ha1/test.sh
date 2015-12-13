ha=$(basename $(pwd))
if [ ! -f "$ha.ml" ]; then
  echo "No $ha.ml file!"
  exit 1
fi
mv -f $ha.ml $ha.org.ml
cat ../nostdlib.ml $ha.org.ml > $ha.ml
ocamlbuild -clean
ocamlbuild -use-ocamlfind -package kaputt ${ha}_test.native
mv -f $ha.org.ml $ha.ml
# enable stacktrace
export OCAMLRUNPARAM=b
./${ha}_test.native
