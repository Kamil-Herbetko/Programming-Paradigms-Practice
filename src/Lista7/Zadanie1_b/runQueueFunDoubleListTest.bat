ocamlc -c queueFunDoubleList.mli
ocamlc -c queueFunDoubleList.ml
ocamlc -c queueFunDoubleListTest.ml
ocamlc -o queueFunDoubleListTest queueFunDoubleList.cmo queueFunDoubleListTest.cmo
ocamlrun queueFunDoubleListTest