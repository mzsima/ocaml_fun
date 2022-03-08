open Js_of_ocaml
open Graphics_js

module Html = Dom_html
let pi = acos (-1.)

type canvasElement = Html.canvasElement Js.t  
type canvasCtx = Html.canvasRenderingContext2D Js.t
type brick = {x: float; y: float; }

class particle x y w color speed =
  object
    val x0 = x
    val y0 = y
    val mutable x = x
    val mutable y = y
    val mutable theta = (Random.float pi *. 2.)
    val t = (Random.float 250.)

    method rotate_draw (ctx : canvasCtx) = 
      theta <- theta +. speed ;
      let (x', y') = (x, y) in
      x <- x0 +. (cos theta) *. t ;
      y <- y0 +. (sin theta) *. t ;
      ctx##beginPath ;
      ctx##.lineWidth := w;
      ctx##.strokeStyle := color ;
      ctx##moveTo x' y' ;
      ctx##lineTo x y ;
      ctx##stroke ;
      ()
end

let particles: particle list ref = ref []


let request_animation_frame t =
  let _ = Dom_html.window##requestAnimationFrame (Js.wrap_callback (fun (_ : float) -> t ())) in
  () 

let rec mainloop canvas c = fun () ->
  (* c##clearRect 0. 0. (float canvas##.width) (float canvas##.height) ; *)
  c##.fillStyle := Js.string "rgba(0,0,0,0.03)" ;
  c##fillRect 0. 0. (float canvas##.width) (float canvas##.height) ;
  List.iter (fun n -> n#rotate_draw c) !particles ;
  request_animation_frame (mainloop canvas c) ;
  ()

let init canvas =
  print_endline "initializing";
  let w, h = (float canvas##.width), (float canvas##.height) in
  let generateColor () = 
    let hex = "0123456789ABCDEF" in
    let rec color cnt s = match cnt with
      | 0 -> s
      | _ -> color (cnt - 1) (s ^ (String.make 1 hex.[(Random.int 15)])) in
    color 6 "#"
  in
  for _ = 0 to 101 do
    let color = generateColor () in 
    particles := (new particle (w *. 0.5) (h *. 0.5) 8.  (Js.string color) 0.02)::!particles
  done ;
  let () = open_canvas canvas in
  let c = canvas##getContext Html._2d_ in
  c##.globalAlpha := 0.5 ;
  request_animation_frame (mainloop canvas c) ;
  ()

let () = Js.export "init" init