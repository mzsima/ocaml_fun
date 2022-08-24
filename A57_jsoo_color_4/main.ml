open Js_of_ocaml
open Graphics_js

module Html = Dom_html
let pi = acos (-1.)

type point = {x : float; y : float}
type canvasElement = Html.canvasElement Js.t  
type canvasCtx = Html.canvasRenderingContext2D Js.t

class colorbar =
  object

    method draw (ctx : canvasCtx)  = 
      ctx##.lineWidth := 0.;
      ctx##translate 40. 120. ;
      for i = 0 to 359 do
        let fillColor = Color.of_hsl (float i) 1.0 0.5 in
        ctx##.fillStyle := Js.string "red" ;
        ctx##fillRect ((float i) *. 2.) (float (Color.to_rgba fillColor).r) 8. 8. ;

        ctx##.fillStyle := Js.string "green" ;
        ctx##fillRect ((float i) *. 2.) (float (Color.to_rgba fillColor).g) 8. 8. ;

        ctx##.fillStyle := Js.string "blue" ;
        ctx##fillRect ((float i) *. 2.) (float (Color.to_rgba fillColor).b) 8. 8. ;
      done
end

let init canvas =
  print_endline "initializing";
  let () = open_canvas canvas in
  let c = canvas##getContext Html._2d_ in
  let colorbar = new colorbar  in
  colorbar#draw c ;
  ()

let () = Js.export "init" init