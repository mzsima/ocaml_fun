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

type my_block = { data: int array; color: int; mutable position: int}
;;

let mapL = [|
  0;1;1;0;
  0;1;0;0;
  0;1;0;0;
  0;0;0;0;
|]

let active_block = { data = mapL; color = 3; position = 1; }

let drawBlock block () = 
  for i = 0 to 15 do 
    if block.data.(i) = 1 then
      let x' = i mod 4 in
      let y' = i / 4 in
      setCharColor block.color ;
      setPosition (x' * 2 + 30) (y' + block.position) ; 
      printf "  " ;
      flush stdout;
  done 

let key_down = function
| "Down" -> speed_up := true
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
  active_block.position <- active_block.position + velocity ;
  if active_block.position > 15 then active_block.position <- 0

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
    Sdltimer.delay ~ms:200;
    clear () ; 
    move () ;
    main_loop ()
  in
  clearScreen () ;
  main_loop ()