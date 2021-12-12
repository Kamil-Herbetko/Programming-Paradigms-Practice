(* Kamil Herbetko *)
(* Zadanie 2 *)

let menu (opt) =
  let numItems = Array.length opt-1
  in
    begin
      print_string "\n\n=================================================== \n";
      print_string opt.(0);print_newline();
      for i=1 to numItems do  print_int i; print_string (". "^opt.(i)); print_newline() done;
      print_string "\nSelect an option: ";
      flush stdout;
      let choice = ref (read_int())
      in
	while !choice < 1 || !choice > numItems do
	  print_string ("Choose number between 1 and " ^ string_of_int numItems ^ ": ");
	  choice := read_int();
	done;
	!choice
    end
;;

let q = ref (QueueMut.empty(10));;
let menuItems = Array.make 8 "";;
let quit = ref false;;
let choice = ref 9;;

menuItems.(0) <- "Queue Operations (implementation on cyclic array)";
menuItems.(1) <- "empty";
menuItems.(2) <- "first";
menuItems.(3) <- "enqueue";
menuItems.(4) <- "dequeue";
menuItems.(5) <- "isEmpty";
menuItems.(6) <- "isFull";
menuItems.(7) <- "end program";
while not !quit do
  begin
    choice := menu(menuItems);
    match !choice with
    1 ->
	  begin
	    print_string "New queue size = ";
	    q := QueueMut.empty(read_int());
	    print_string "\n Created new, empty queue";
	  end
      | 2 ->
	  begin
	    begin
	      try print_int (QueueMut.first(!q))  with
		  QueueMut.Empty m -> print_string ("Exception: "^m);
	    end;
	    print_newline();
	  end
	  | 3 ->
      begin
        print_string "Queue item = ";
        begin
          try QueueMut.enqueue (read_int(), !q)  with
          QueueMut.Full m -> print_string ("Exception: "^m);
        end;
        print_newline();
      end
      | 4 ->
	  begin
	    QueueMut.dequeue !q;
	    print_string "dequeue\n";
	  end
      | 5 ->
	    print_string ("Queue is "^(if QueueMut.isEmpty !q then "" else "not ")^"empty.\n");
      | 6 ->
        print_string ("Queue is "^(if QueueMut.isFull !q then "" else "not ")^"full.\n");
      | 7 ->
	    quit := true
      | _ ->
	    print_string "IMPOSSIBLE!!!\n"
  end
done
