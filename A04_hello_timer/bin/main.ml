let value = ref 10.0

let clearScreen () = Printf.printf "\027[2J"
let setPosition x y  = Printf.printf "\027[%d;%dH" y  x 

let printTime () = 
    clearScreen () ;
    setPosition 2 2 ;
    Printf.printf "Timer: %.1f" !value ;
    flush stdout ;
;;

let rec tic () =
  if !value >= 0.0 then printTime () ;
  value := !value -. 0.1 ;
  let%lwt () = Lwt_unix.sleep 0.1 in
  tic () ;
;;

Lwt_main.run (tic ())