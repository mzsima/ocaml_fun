open Js_of_ocaml
open Graphics_js

module Html = Dom_html
let timer = ref None
let pi = acos (-1.)

type canvasElement = Html.canvasElement Js.t  
type canvasCtx = Html.canvasRenderingContext2D Js.t
type brick = {x: float; y: float; }
class blocks =
  object
    val brickRowCount = 20
    val brickColumnCount = 20
    val w = 60.
    val h = 60.
    val brickPadding = -3.
    val brickOffsetTop = 0.
    val brickOffsetLeft = 0.
    val bricks = Array.make_matrix 20 20 {x=0.; y=0.; }
    
    method setup =
      for i = 0 to brickColumnCount - 1 do
        for j = 0 to brickRowCount - 1 do
          let x = (float i) *. (w +. brickPadding) +. brickOffsetLeft in
          let y = (float j) *. (h +. brickPadding) +. brickOffsetTop in
          bricks.(i).(j) <- {x; y;}
        done
      done

    method draw (ctx : canvasCtx) = 
      let drawBlock i j = 
        let {x;y;} = bricks.(i).(j) in
        let rad = Random.float pi in
        ctx##save ;
        ctx##translate x y ;
        ctx##rotate rad ;
        ctx##beginPath ;
        ctx##rect (-0.5 *. w) (-0.5 *. h) w h ;
        ctx##.fillStyle := Js.string "#0095DD" ;
        ctx##fill ;
        ctx##closePath ;
        ctx##restore ;
        ()
      in
      for i = 0 to brickColumnCount - 1 do
        for j = 0 to brickRowCount - 1 do
            drawBlock i j 
        done
      done

end


let mainloop canvas c blocks = fun () ->
  c##clearRect 0. 0. (float canvas##.width) (float canvas##.height) ;
  blocks#draw c ;
  ()

let init canvas =
  print_endline "initializing";
  let () = open_canvas canvas in
  let c = canvas##getContext Html._2d_ in
  c##.globalCompositeOperation := Js.string "xor" ;
  let blocks = new blocks in
  blocks#setup ;
  timer := Some (Dom_html.window##setInterval (Js.wrap_callback (mainloop canvas c blocks)) 1000.0) ;
  ()

let () = Js.export "init" init