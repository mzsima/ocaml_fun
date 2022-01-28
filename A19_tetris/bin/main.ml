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
    | -1 -> w
    | _ -> 
      w.(n * block_w) <- 1 ;
      w.(n * block_w + block_w - 1) <- 1 ;
      createWall (n - 1) w
  in
  createWall 20 (Array.append (Array.create ~len:(block_w * 20) 0) (Array.create ~len:block_w 1))

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

let mapL = [|
  0;1;0;0;
  0;1;0;0;
  0;1;1;0;
  0;0;0;0;
|]

let mapL' = [|
  0;0;1;0;
  0;0;1;0;
  0;1;1;0;
  0;0;0;0;
|]

let mapZ = [|
  0;0;0;0;
  1;1;0;0;
  0;1;1;0;
  0;0;0;0;
|]

let mapZ' = [|
  0;0;0;0;
  0;1;1;0;
  1;1;0;0;
  0;0;0;0;
|]


let mapBar = [|
  0;1;0;0;
  0;1;0;0;
  0;1;0;0;
  0;1;0;0;
|]

let blockMap = [|mapR; mapUp; mapL; mapL'; mapZ; mapZ'; mapBar|]

let active_block = { data = blockMap.(0); color = 4; y = 1; x = 5}
let newBlock () = 
  let index = Random.int 6 in
  let newb = { data = blockMap.(index); color = index + 2; y = 1; x = 5} in
  active_block.y <- 1;
  active_block.data <- newb.data;
  active_block.color <- index + 2;
  active_block.x <- 5

let range a b = 
  let rec aux a b = 
    if a > b then [] else a::aux (a+1) b
  in
    if a > b then List.rev (aux a b) else aux a b;;

let min a b =
  if a < b then a else b ;;

let collision block =
  let xyMap i b = 
    let x' = i mod 4 in
    let y' = i / 4 in
    (b, x', y')
  in
  let checkCollision target speedback = 
    let value, x', y' = target in 
    value = 1 
    && world.((min (y' + block.y - speedback) 20)* block_w + x' + block.x) = 1 
  in
  let rec checkBlock lst i = match lst with
    [] -> false 
    | first :: rest -> checkCollision first i || checkBlock rest i
  in
  let y0 = ref 0 in
  let res = ref false in
  let speed = if !speed_up then 4 else 0 in
  for i = 0 to speed do
    if checkBlock (Array.to_list (Array.mapi ~f:xyMap block.data)) i then 
      (y0 := i ; res := true)
  done ;
  block.y <- block.y - !y0 ;
  !res

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
    let x' = i mod block_w in
    let y' = i / block_w in
    setCharColor 9 ;
    if v = 1 then
      setAttribute 7 
    else 
      setAttribute 0 ;
    setPosition (x' * 2) y' ; 
    printf "  " 
  in
  Array.iteri ~f:drawWall world ;
  flush stdout

let rotate t = 
  let tmp = Array.create ~len:16 0 in
  for i = 0 to 15 do 
    let x' = i mod 4 in
    let y' = i / 4 in
    tmp.(i) <- t.data.(x' * 4 + (3 - y')) ;
  done ;
  for i = 0 to 15 do t.data.(i) <- tmp.(i) done

let putWorld block = 
  let projection i b =
    if b = 1 then 
      world.(((i / 4) + block.y - 1) * block_w + block.x + (i mod 4)) <- 1 ;
    ()
  in
  Array.iteri ~f:projection block.data ;
  newBlock ()


let key_down = function
| "Up" -> 
  let moved = { data = Array.copy active_block.data; color = 4; y = active_block.y; x = active_block.x} in
  rotate moved ;
  if not (collision moved) then rotate active_block
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
  drawWorld () ;
  drawBlock active_block () ;
  setPosition 100 0 ;
  flush stdout

let clear () =
  setCharColor 2 ;
  setBackColor 7 ;
  setAttribute 0 ;
  drawBlock active_block () ;
  flush stdout 

let delete () = 
  let rec fold_until f acc n = function
    | [] -> (acc, [])
    | h :: t as l -> if n = 0 then (acc, l) else fold_until f (f acc h) (n - 1) t
  in
  let slice list i k =
    let _, list = fold_until (fun _ _ -> []) [] i list in
    let taken, _ = fold_until (fun acc h -> h :: acc) [] (k - i + 1) list in
    List.rev taken
  in
  let deleteLine n = 
    for i = (n + 1) * block_w downto block_w do world.(i) <- world.(i - block_w) done
  in
  for y = 0 to 19 do 
    if List.for_all (slice (Array.to_list world) (y * block_w) ((y+1) * block_w - 1)) ~f: (fun i -> i > 0) then deleteLine y
  done

let down () =
  let velocity = match !speed_up with
  | true -> 5 
  | _ -> 1 
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
      down () ;
      if collision active_block then putWorld active_block ;
      delete () ;
      start := Unix.gettimeofday()
    ) ;
    main_loop ()
  in
  clearScreen () ;
  main_loop ()