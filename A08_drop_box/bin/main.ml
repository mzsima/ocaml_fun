open Base 
open Stdio
;;

let clearScreen () = printf "\027[2J"
let setPosition x y  = printf "\027[%d;%dH" y  x 
let setBackColor n = printf "\027[4%dm" n
let setCharColor n = printf "\027[3%dm" n
let setColor n = setBackColor n ; setCharColor n 

let p = ref 0
let world = ref (Array.create ~len:100 0)

let randomCreate line = 
  let i = Random.int 10 in
  line.(i) <- (Random.int 8) ;
  line

let drop () =  
  let newLine = randomCreate (Array.create ~len:10 0) in
  world := Array.append newLine (Array.sub !world ~pos:0 ~len:90)

let printWorld () =
  drop () ;
  let f index value = match value with
    | 0 -> ()
    | _ -> setPosition ((index mod 10) * 2) (index / 10) ;
      setColor value ;
      printf "%s" ".." ;
      flush stdout ;
      setColor 0 
  in
  Array.iteri !world ~f:f 
;;

let rec tic () =
  if !p <= 50 then printWorld () ;
  p := !p + 1 ;
  clearScreen () ;
  let%lwt () = Lwt_unix.sleep 0.2 in
  tic () ;
;;
clearScreen () ;
Lwt_main.run ( tic ())
