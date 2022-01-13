open Unix
open Base
open Stdio
open Sdlevent

let clearScreen () = printf "\027[2J"
let setPosition x y  = printf "\027[%d;%dH" y  x 
let setBackColor n = printf "\027[4%dm" n
let setCharColor n = printf "\027[3%dm" n
let setColor n = setBackColor n ; setCharColor n 
let setAttribute n = printf "\027[%dm" n

let speed_up = ref false
let start = ref (Unix.gettimeofday())
let threshold = 0.5

type my_block = { data: int array; color: int; mutable y: int; mutable x: int}
;;

let mapR = [|
  0;0;0;0;
  0;1;1;0;
  0;1;1;0;
  0;0;0;0;
|]

let active_block = { data = mapR; color = 4; y = 1; x = 20}

let drawBlock block () = 
  for i = 0 to 15 do 
    if block.data.(i) = 1 then
      let x' = i mod 4 in
      let y' = i / 4 in
      setCharColor block.color ;
      setPosition (x' * 2 + block.x) (y' + block.y) ; 
      printf "  " ;
      flush stdout;
  done 

let key_down = function
| "Down" -> speed_up := true
| "Left" -> active_block.x <- active_block.x - 2
| "Right" -> active_block.x <- active_block.x + 2
| _ -> ()

let key_up = function 
| "Down" -> speed_up := false
| _ -> ()



let proc_events = function
  | KeyDown e -> key_down (Sdlkeycode.to_string e.keycode)
  | KeyUp e -> key_up (Sdlkeycode.to_string e.keycode)
  | Quit e ->
      printf "Quit(timestamp:%ld)\n%!" e.quit_timestamp;
      Sdl.quit ();
      exit 0
  | _ -> ()

let draw () =
  setCharColor 2 ;
  setAttribute 7 ;
  drawBlock active_block () ;
  setPosition 10 10 ;
  flush stdout

let clear () =
  setCharColor 2 ;
  setBackColor 7 ;
  setAttribute 0 ;
  drawBlock active_block () ;
  flush stdout 

let move () =
  let velocity = match !speed_up with
  | true -> 5
  | _ -> 1 
  in
  active_block.y <- active_block.y + velocity ;
  if active_block.y > 15 then active_block.y <- 0

let () =
  Sdl.init [`VIDEO];
  at_exit Sdl.quit;
  let rec event_loop () =
    match Sdlevent.poll_event () with
    | Some ev -> proc_events ev
    | None -> ()
  in
  let rec main_loop () =
    event_loop ();
    draw () ;
    Sdltimer.delay ~ms:100;
    clear () ; 
    if Float.(Unix.gettimeofday() -. !start > threshold) then (
      move () ;
      start := Unix.gettimeofday()
    ) ;
    main_loop ()
  in
  clearScreen () ;
  main_loop ()