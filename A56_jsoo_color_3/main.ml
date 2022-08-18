open Js_of_ocaml
open Graphics_js

module Html = Dom_html
let pi = acos (-1.)

type point = {x : float; y : float}
type model = { w: float; h: float; count: int; } 
type canvasElement = Html.canvasElement Js.t  
type canvasCtx = Html.canvasRenderingContext2D Js.t

class colorbar model =
  object

    method draw (ctx : canvasCtx)  = 
      ctx##.lineWidth := 0.;
      ctx##translate 40. 120. ;
      for i = 0 to 359 do
        let fillColor = Color.of_hsl (float i) 1.0 0.5 in
        let fillColorOfRed = Color.of_rgb (Color.to_rgba fillColor).r 0 0 in
        let fillColorOfGreen = Color.of_rgb 0 (Color.to_rgba fillColor).g 0 in
        let fillColorOfBlue = Color.of_rgb 0 0 (Color.to_rgba fillColor).b in
        ctx##.fillStyle := Js.string (Color.to_css_rgba fillColor) ;
        ctx##fillRect ((float i) *. 2.) 50. 2. (model.h *. 0.1) ;
        ctx##.fillStyle := Js.string (Color.to_css_rgba fillColorOfRed) ;
        ctx##fillRect ((float i) *. 2.) 180. 2. (model.h *. 0.1) ;
        ctx##.fillStyle := Js.string (Color.to_css_rgba fillColorOfGreen) ;
        ctx##fillRect ((float i) *. 2.) 310. 2. (model.h *. 0.1) ;
        ctx##.fillStyle := Js.string (Color.to_css_rgba fillColorOfBlue) ;
        ctx##fillRect ((float i) *. 2.) 440. 2. (model.h *. 0.1) ;
      done
end

let init canvas =
  print_endline "initializing";
  let () = open_canvas canvas in
  let c = canvas##getContext Html._2d_ in
  let w, h = (float canvas##.width), (float canvas##.height) in
  let model = { w; h; count=160; } in
  let colorbar = new colorbar model in
  colorbar#draw c ;
  ()

let () = Js.export "init" init