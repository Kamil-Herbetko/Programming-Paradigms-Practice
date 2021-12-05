ocamlc -c queueFunList.mli
ocamlc -c queueFunList.ml
ocamlc -c queueFunListTest.ml
ocamlc -o queueFunListTest queueFunList.cmo queueFunListTest.cmo
ocamlrun queueFunListTest