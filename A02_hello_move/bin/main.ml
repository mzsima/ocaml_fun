let minisleep (sec: float) =
    ignore (Unix.select [] [] [] sec)

let  clearScreen () = Printf.printf "\027[2J"

let  setPosition x y  = Printf.printf "\027[%d;%dH" y  x 

let () =
  for i = 1 to 10 do
    minisleep 0.5 ;
    if i mod 3 = 1 then clearScreen () ;
    setPosition i i;
    Printf.printf "Hello\n" ;
    flush stdout ;
  done
