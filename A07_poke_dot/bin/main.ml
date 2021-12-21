let clearScreen () = Printf.printf "\027[2J"
let setPosition x y  = Printf.printf "\027[%d;%dH" y  x 
let setBackColor n = Printf.printf "\027[4%dm" n
let setCharColor n = Printf.printf "\027[3%dm" n
let setColor n = setBackColor n ; setCharColor n 
;;


let row = ref 1

let printDotsLine lst =
  let f index value = 
    setPosition ((index + 1) * 2) !row ;
    if value == 1 then setColor 3
    else setColor 0 ;
    Printf.printf "%s" "AA" ;
    flush stdout ;
  in
  List.iteri f lst 
;;

let rec printDots lst = match lst with 
  | [] -> Printf.printf "%s" "END"
  | first :: rest -> printDotsLine first; row := !row + 1; printDots rest ;
;;

let draw () = 
  let dots = [
    [1;1;1;0;0;0;1;1;0;0;0;1;1;1];
    [1;1;0;1;0;0;1;1;0;0;1;0;1;1];
    [0;1;0;0;1;1;1;1;1;1;0;0;1;0];
    [0;0;1;0;0;0;0;0;0;0;0;1;0;0];
    [0;0;1;0;1;0;0;0;0;1;0;1;0;0];
    [0;0;1;0;0;0;1;1;0;0;0;1;0;0];
    [0;1;0;1;0;1;1;1;1;0;1;0;1;0];
    [0;1;0;0;0;0;0;0;0;0;0;0;1;0];
    [0;1;0;0;1;0;0;0;0;1;0;0;1;0];
    [0;1;0;1;1;0;0;0;0;1;1;0;1;0];
    [0;1;0;0;0;0;0;0;0;0;0;0;1;0];
    [0;0;1;0;0;1;1;1;1;0;0;1;0;0];
    [0;0;0;1;1;0;0;0;0;1;1;0;0;0];
  ] in
  printDots dots 
;;

let () =
 clearScreen () ;
 draw () ;
 setCharColor 9;
 print_endline "pikka"