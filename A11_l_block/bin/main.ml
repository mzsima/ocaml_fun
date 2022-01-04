open Stdio
;;

let clearScreen () = printf "\027[2J"
let setPosition x y  = printf "\027[%d;%dH" y  x 
let setBackColor n = printf "\027[4%dm" n
let setCharColor n = printf "\027[3%dm" n
let setColor n = setBackColor n ; setCharColor n 
let setAttribute n = printf "\027[%dm" n

let y0 = ref 1

let mapL = [|
  0;1;1;0;
  0;1;0;0;
  0;1;0;0;
  0;0;0;0;
|]

let blockL () = 
  for i = 0 to 15 do 
    if mapL.(i) = 1 then
      let x' = i mod 4 in
      let y' = i / 4 in
      setPosition (x' * 2 + 10) (!y0 + y') ; 
      printf "  "
  done 



let rec tic () = 
  setCharColor 2 ;
  setBackColor 7 ;
  setAttribute 7 ;
  blockL () ;
  setPosition 10 10 ;
  flush stdout ;

  let%lwt () = Lwt_unix.sleep 0.5 in
    setCharColor 2 ;
    setBackColor 7 ;
    setAttribute 0 ;
    blockL () ;
    flush stdout ;
    y0 := !y0 + 1 ; 
    tic () 


let _ = Lwt_unix.on_signal 2 (fun _ -> exit 0)
;;

Lwt_main.at_exit (fun () -> 
  setAttribute 0 ;
  setBackColor 0 ;
  setCharColor 9 ;
  Lwt.return(print_endline "exit")
)
;;

Lwt_main.run ( clearScreen () ; tic ())
