open Unix
open Base
open Stdio
open Sdlevent

let clearScreen () = printf "\027[2J"
let setPosition x y  = printf "\027[%d;%dH" y  (x + 2) 
let setBackColor n = printf "\027[4%dm" n
let setCharColor n = printf "\027[3%dm" n
let setColor n = setBackColor n ; setCharColor n 
let setAttribute n = printf "\027[%dm" n

let speed_up = ref false
let start = ref (Unix.gettimeofday())
let threshold = 0.3
let block_w = 12

type my_block = { mutable data: int array; mutable color: int; mutable y: int; mutable x: int}

let world =  
  let rec createWall n w = match n with 
    | 0 -> w
    | _ -> 
      w.(n * block_w) <- 1 ;
      w.(n * block_w + block_w - 1) <- 1 ;
      createWall (n - 1) w
  in
  createWall 19 (Array.append (Array.create ~len:(block_w * 19) 0) (Array.create ~len:block_w 1))

let mapR = [|
  0;0;0;0;
  0;1;1;0;
  0;1;1;0;
  0;0;0;0;
|]

let mapUp = [|
  0;0;0;0;
  0;1;0;0;
  1;1;1;0;
  0;0;0;0;
|]

let active_block = { data = mapR; color = 4; y = 1; x = 5}

let collision block =
  let xyMap i b = 
    let x' = i mod 4 in
    let y' = i / 4 in
    (b, x', y')
  in
  let checkCollision target = 
    let value, x', y' = target in 
    value = 1 
    && world.((y' + block.y) * block_w + x' + block.x) = 1 
  in
  let rec checkBlock lst = match lst with
    [] -> false 
    | first :: rest -> checkCollision first || checkBlock rest
  in
  checkBlock (Array.to_list (Array.mapi ~f:xyMap block.data))

let drawBlock block () = 
  for i = 0 to 15 do 
    if block.data.(i) = 1 then
      let x' = i mod 4 in
      let y' = i / 4 in
      setCharColor block.color ;
      setPosition (x' * 2 + block.x * 2) (y' + block.y) ; 
      printf "  " ;
      flush stdout;
  done 

let drawWorld () =
  let drawWall i v = 
    if v = 1 then
      let x' = i mod block_w in
      let y' = i / block_w in
      setCharColor 9 ;
      setPosition (x' * 2) y' ; 
      printf "  " ;
      flush stdout
  in
  Array.iteri ~f:drawWall world

let move () =
  let velocity = 1 
  in
  active_block.y <- active_block.y + velocity 

let putWorld block = 
  let projection i b =
    if b = 1 then 
      world.(((i / 4) + block.y - 1) * block_w + block.x + (i mod 4)) <- 1 ;
    ()
  in
  Array.iteri ~f:projection block.data ;
  active_block.y <- 1 ;
  active_block.data <- if active_block.color = 3 then mapR else mapUp ;
  active_block.color <- if active_block.color = 3 then 4 else 3 ;
  active_block.x <- 5 


let key_down = function
| "Down" -> speed_up := true
| "Left" -> 
  let moved = { data = active_block.data; color = 4; y = active_block.y; x = active_block.x - 1} in
  if not (collision moved) then active_block.x <- active_block.x - 1
| "Right" -> 
  let moved = { data = active_block.data; color = 4; y = active_block.y; x = active_block.x + 1} in
  if not (collision moved) then active_block.x <- active_block.x + 1
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
  drawWorld () ;
  setPosition 0 0 ;
  flush stdout

let clear () =
  setCharColor 2 ;
  setBackColor 7 ;
  setAttribute 0 ;
  drawBlock active_block () ;
  flush stdout 

let down () =
  let velocity = 1
  in 
  active_block.y <- active_block.y + velocity 
  
let () =
  Sdl.init [`VIDEO];
  at_exit Sdl.quit;
  let event_loop () =
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
      if collision active_block then putWorld active_block ;
      start := Unix.gettimeofday()
    ) ;
    main_loop ()
  in
  clearScreen () ;
  main_loop ()