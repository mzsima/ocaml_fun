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
let score = ref 0

type brick = {x: float; y: float; mutable status: int}
type canvasElement = Html.canvasElement Js.t  
type canvasCtx = Html.canvasRenderingContext2D Js.t

class virtual game_obj =
    object
      method virtual tick : unit
      method virtual collistionDetection : unit
      method virtual draw : canvasCtx -> unit
  end;;

class paddle x =
  object
    inherit game_obj
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
    method collistionDetection = 
      ()
end

class blocks =
  object
    inherit game_obj
    val brickRowCount = 2
    val brickColumnCount = 2
    val brickWidth = 240.
    val brickHeight = 40.
    val brickPadding = 12.
    val brickOffsetTop = 50.
    val brickOffsetLeft = 160.
    val bricks = Array.make_matrix 2 2 {x=0.; y=0.; status=1}
    
    method setup =
      for i = 0 to brickColumnCount - 1 do
        for j = 0 to brickRowCount - 1 do
          let x = (float i) *. (brickWidth +. brickPadding) +. brickOffsetLeft in
          let y = (float j) *. (brickHeight +. brickPadding) +. brickOffsetTop in
          bricks.(i).(j) <- {x; y; status=1}
        done
      done

    method tick = ()

    method draw (ctx : canvasCtx) = 
      let drawBlock i j = 
        let {x;y;_} = bricks.(i).(j) in
        ctx##beginPath ;
        ctx##rect x y brickWidth brickHeight ;
        ctx##.fillStyle := Js.string "#0095DD" ;
        ctx##fill ;
        ctx##closePath ;
        ()
      in
      for i = 0 to brickColumnCount - 1 do
        for j = 0 to brickRowCount - 1 do
          if bricks.(i).(j).status == 1 then 
            drawBlock i j 
        done
      done

    method collistionDetection = 
      for i = 0 to brickColumnCount - 1 do
        for j = 0 to brickRowCount - 1 do
          let b = bricks.(i).(j) in
          if (b.status == 1 && !x > b.x && !x < b.x +. brickWidth && !y > b.y && !y < b.y +. brickHeight) then (
            dy := (-. !dy) ;
            b.status <- 0 ;
            score := !score + 10 ;
            if !score == (brickColumnCount * brickRowCount * 10) then (
              Html.window##alert (Js.string "YOU WIN, CONGRATULATIONS!") ;
              Html.window##.location##reload ;
              let clearTimer some_timer = match some_timer with 
                | None -> ()
                | Some t -> Html.window##clearInterval t 
              in
              clearTimer !timer
            )
          )
        done
      done

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

let drawScore c =
  let (#%) = Printf.sprintf in
  c##.font := (Js.string "32px Arial");
  c##.fillStyle := (Js.string "#0095DD");
  c##fillText (Js.string ("Score: %d" #% !score) ) 8. 40.;
  ()

let mainloop canvas c nodes paddle = fun () ->
  c##clearRect 0. 0. (float canvas##.width) (float canvas##.height) ;
  List.iter (fun node -> node#tick; node#collistionDetection) nodes ;
  List.iter (fun node -> node#draw c) nodes ;
  drawBall c ;
  drawScore c ;
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
  let blocks = new blocks in
  paddle#setup ;
  blocks#setup ;
  let nodes = [(paddle :> game_obj); (blocks :> game_obj)] in
  timer := Some (Dom_html.window##setInterval (Js.wrap_callback (mainloop canvas c nodes paddle)) 10.0) ;
  ()

let () = Js.export "init" init