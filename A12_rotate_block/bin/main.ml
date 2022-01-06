open Base
open Stdio
open Sdlevent

let clearScreen () = printf "\027[2J"
let setPosition x y  = printf "\027[%d;%dH" y  x 
let setBackColor n = printf "\027[4%dm" n
let setCharColor n = printf "\027[3%dm" n
let setColor n = setBackColor n ; setCharColor n 
let setAttribute n = printf "\027[%dm" n

let y0 = ref 1
;;

type my_block = { data: int array; color: int; }
;;

let mapL = [|
  0;1;1;0;
  0;1;0;0;
  0;1;0;0;
  0;0;0;0;
|]

let mapT = [|
  0;0;0;0;
  1;1;1;0;
  0;1;0;0;
  0;0;0;0;
|]


let blockL = { data = mapL; color = 3; }
let blockT = { data = mapT; color = 4; }

let rotate t = 
  let tmp = Array.create ~len:16 0 in
  for i = 0 to 15 do 
    let x' = i mod 4 in
    let y' = i / 4 in
    tmp.(i) <- t.data.(x' * 4 + (3 - y')) ;
  done ;
  for i = 0 to 15 do t.data.(i) <- tmp.(i) done

let drawBlock () = 
  for i = 0 to 15 do 
    if blockL.data.(i) = 1 then
      let x' = i mod 4 in
      let y' = i / 4 in
      setCharColor blockL.color ;
      setPosition (x' * 2 + 30) (!y0 + y') ; 
      printf "  " ;
      flush stdout;
  done ;
  for i = 0 to 15 do 
    if blockT.data.(i) = 1 then
      let x' = i mod 4 in
      let y' = i / 4 in
      setCharColor blockT.color ;
      setPosition (x' * 2 + 10) (!y0 + y') ; 
      printf "  " ;
      flush stdout
  done



let proc_events = function
  | KeyDown e -> 
    rotate blockT;
    rotate blockL 
  | Quit e ->
      printf "Quit(timestamp:%ld)\n%!" e.quit_timestamp;
      Sdl.quit ();
      exit 0
  | _ -> ()

let draw () =
  setCharColor 2 ;
  setAttribute 7 ;
  drawBlock () ;
  setPosition 10 10 ;
  flush stdout

let clear () =
  setCharColor 2 ;
  setBackColor 7 ;
  setAttribute 0 ;
  drawBlock () ;
  flush stdout 

let () =
  Sdl.init [`VIDEO];
  at_exit Sdl.quit;

  let rec event_loop () =
    match Sdlevent.poll_event () with
    | Some ev -> proc_events ev; event_loop ()
    | None -> ()
  in
  let rec main_loop () =
    event_loop ();
    draw () ;
    Sdltimer.delay ~ms:20;
    clear () ; 
    main_loop ()
  in
  clearScreen () ;
  main_loop ()