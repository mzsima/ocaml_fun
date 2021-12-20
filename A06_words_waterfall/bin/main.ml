let p = ref 1
let words = ref []

let clearScreen () = Printf.printf "\027[2J"
let setPosition x y  = Printf.printf "\027[%d;%dH" y  x 
let setCharColor n = Printf.printf "\027[3%dm" n

let rec range a b = 
  if a > b then []
  else a :: range (a + 1) b
;;

let rec rondomWord a =
  let newWord = (Char.chr (Random.int 24 + (Char.code 'a'))) in
  if (List.length a) >= 50 then a
  else rondomWord (newWord :: a)
;;

let printTime () = 
  setCharColor 2 ;
    clearScreen () ;
    let newWord = String.of_seq (List.to_seq (rondomWord [])) in
    words := newWord :: !words ;
    let f index word  = 
      setPosition 0 index ;
      Printf.printf "%s" word  ;
    in  
    List.iteri f !words ;
    flush stdout ;
;;

let rec tic () =
  if !p <= 50 then printTime () ;
  p := !p + 1 ;
  let%lwt () = Lwt_unix.sleep 0.1 in
  tic () ;
;;

Lwt_main.run (tic ())