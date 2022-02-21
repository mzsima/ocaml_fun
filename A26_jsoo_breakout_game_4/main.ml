open Js_of_ocaml
open Graphics_js

module Html = Dom_html
let timer = ref None
let pi = acos (-1.)
let ballRadius = 30.0
let dx = ref 10.0
let dy = ref (-. 5.0)
let x = ref 0.0 
let y = ref 0.0 

type canvasElement = Html.canvasElement Js.t  
type canvasCtx = Html.canvasRenderingContext2D Js.t

class paddle x =
  object
    val mutable x = x
    val mutable leftPress = false
    val mutable rightPress = false
    val speed = 10.
    val paddle_width = 200.

    method get_x = x
    method get_paddle_width = paddle_width

    method tick = match (leftPress, rightPress) with
    | (true, true) -> ()
    | (true, false) -> x <- x -. speed
    | (false, true) -> x <- x +. speed
    | _ -> () ;
    ()

    method draw (ctx : canvasCtx) = 
      ctx##beginPath ;
      ctx##rect x 760. paddle_width 20. ;
      ctx##.fillStyle := Js.string "#2222DD" ;
      ctx##fill ;
      ctx##closePath ;  
      ()
    method setup =
      Html.document##.onkeydown :=
        Html.handler (fun e -> match e##.keyCode with 
          | 37 -> (* left *) leftPress <- true; 
            Js._false
          | 39 -> (* right *) rightPress <- true ;
            Js._false
          | _ -> Js._true
        );      
      Html.document##.onkeyup :=
        Html.handler (fun e -> match e##.keyCode with 
          | 37 -> (* left *) leftPress <- false ; 
            Js._false
          | 39 -> (* right *) rightPress <- false ;
            Js._false
          | _ -> Js._true
        );
      ()
end

let setup canvas =
  x := (float canvas##.width) /. 2.0 ;
  y := (float canvas##.height) -. 30. ;
  ()


let drawBall c = 
  c##beginPath ;
  c##arc !x !y ballRadius 0. (pi *. 2.0) Js._true;
  c##.fillStyle := Js.string "#0095DD" ;
  c##fill ;
  c##closePath ;
  ()

let check canvas paddle =
  if (!x +. !dx) > (float canvas##.width) -. ballRadius || !x +. !dx < ballRadius then dx := (-. !dx) ;
  if !y +. !dy < ballRadius then dy := (-. !dy) 
  else if (!y +. !dy) > (float canvas##.height) -. ballRadius then (
    if !x > paddle#get_x && !x < (paddle#get_x +. paddle#get_paddle_width) then 
      dy := (-. !dy) 
    else (
      Html.window##alert (Js.string "GAME OVER") ;
      Html.window##.location##reload ;
      let clearTimer some_timer = match some_timer with 
        | None -> ()
        | Some t -> Html.window##clearInterval t 
      in
      clearTimer !timer
    )) ;
  ()

let mainloop canvas c nodes paddle = fun () ->
  c##clearRect 0. 0. (float canvas##.width) (float canvas##.height) ;
  List.iter (fun node -> node#tick) nodes ;
  List.iter (fun node -> node#draw c) nodes ;
  drawBall c ;
  check canvas paddle;
  x := !x +. !dx ;
  y := !y +. !dy ;
  ()

let init canvas =
  print_endline "initializing";
  let () = open_canvas canvas in
  setup canvas ;
  let c = canvas##getContext Html._2d_ in
  let paddle = new paddle 50.0 in
  paddle#setup ;
  let nodes = [paddle] in
  timer := Some (Dom_html.window##setInterval (Js.wrap_callback (mainloop canvas c nodes paddle)) 10.0) ;
  ()

let () = Js.export "init" init