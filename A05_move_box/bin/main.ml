let p = ref 1

let clearScreen () = Printf.printf "\027[2J"
let setPosition x y  = Printf.printf "\027[%d;%dH" y  x 
let setCharColor n = Printf.printf "\027[3%dm" n
let setBackColor n = Printf.printf "\027[4%dm" n

let printTime () = 
    clearScreen () ;
    setPosition !p (if !p >=5 then 5 else !p) ;
    setCharColor 2 ;
    setBackColor 2 ;
    Printf.printf "%s" ".."  ;
    setBackColor 9 ;
    setCharColor 9 ;
    flush stdout ;
;;

let rec tic () =
  if !p >= 0 then printTime () ;
  p := !p + 1 ;
  let%lwt () = Lwt_unix.sleep 0.1 in
  tic () ;
;;

Lwt_main.run (tic ())