open Base 
open Stdio
;;

let clearScreen () = printf "\027[2J"
let setPosition x y  = printf "\027[%d;%dH" y  x 
let setBackColor n = printf "\027[4%dm" n
let setCharColor n = printf "\027[3%dm" n
let setColor n = setBackColor n ; setCharColor n 

let p = ref 0
let world = ref (Array.append (Array.create ~len:90 0) (Array.create ~len:10 2))

let collision () =
  let f i value = match value with
  | 3 -> (i + 10) < 100 && !world.(i + 10) == 2
  | _ -> false
  in
  let check = Array.existsi !world ~f:f in
  if check then (
    let f i value = match value with
    | 3 ->  !world.(i) <- 2
    | _ -> ()
    in
    Array.iteri !world ~f:f
  );
  ()
;;

let move () =
  let f i value = 
  let i' = 99 - i in 
  let value' = !world.(i') in match value' with
  | 3 -> 
    !world.(i') <- 0 ; 
    if (i' + 10) < 90 then !world.(i' + 10) <- 3 ;
  | _ -> ()
  in
  Array.iteri !world ~f:f

let isCreate () = Array.for_alli !world ~f: (fun i v -> !world.(i) != 3)

let lineClear () = 
  let isClear = ref false in
  for i = 0 to 8 do 
    let lineArr = Array.sub !world ~pos: (i * 10) ~len: 10 in
    let lineCheck = Array.for_all lineArr ~f: (fun v -> v == 2)
    in match lineCheck with 
      | true -> (
          for j = 0 to 9 do !world.(i * 10 + j) <- 0 done ;
          for j = 0 to i * 10 do 
            if !world.(j) == 2 then !world.(j) <- 3 ;
          done 
        )
      | _ -> () ;
    isClear := !isClear || lineCheck 
  done ;
  !isClear
;;

let randomCreate () = 
  let i = Random.int 10 in
  let moveColor = 3 in
  !world.(i) <- moveColor ;
  !world.(i+10) <- moveColor 
;;

let printWorld () =
  collision () ;
  let isClear = lineClear () in
  if (not isClear) then (
    move () ;
    if (isCreate ()) then randomCreate () ;
  ) ; 
  let f index value = setPosition ((index mod 10 + 1) * 2) (index / 10) ;
      setColor value ;
      printf "%s" ".." ;
      flush stdout ;
      setColor 0
  in
  Array.iteri !world ~f:f 
;;

let rec tic () =
  if !p <= 250 then 
    printWorld () ;
    p := !p + 1 ;
    clearScreen () ;
    let%lwt () = Lwt_unix.sleep 0.2 in
    tic () ;
;;

let _ = Lwt_unix.on_signal 2 (fun _ -> exit 0)
;;

Lwt_main.at_exit (fun () -> 
  setBackColor 0 ;
  setCharColor 9 ;
  Lwt.return(print_endline "exit")
)
;;

Lwt_main.run ( tic ())