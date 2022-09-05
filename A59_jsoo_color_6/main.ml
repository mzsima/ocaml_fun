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
      ctx##translate 40. 40. ;
      let r = 24.0 in
      for i = 0 to 19 do
        for j = 0 to 10 do
          let l = 0.3 +. (sin ((float j) *. pi /. 5.)) *. 0.2 in
          let fillColor = Color.of_hsl ((float i) *. 18.) 1.0 l in
          ctx##.fillStyle := Js.string (Color.to_css_hsla fillColor) ;
          let x, y = r *. (float i), 2. *. r *. ((float j) +. if i mod 2 == 0 then 0. else 0.5) in
          ctx##beginPath ;
          ctx##arc x y r 0. (pi *. 2.0) (Js.bool false) ;
          ctx##fill ;
        done
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