open Js_of_ocaml
open Graphics_js

module Html = Dom_html
let pi = acos (-1.)

type point = {x : float; y : float}
type model = { w: float; h: float; data: point list } 
type canvasElement = Html.canvasElement Js.t  
type canvasCtx = Html.canvasRenderingContext2D Js.t

let generateColor () = 
  Random.self_init () ;
  let hex = "6789ABCDEF" in
  let rec color cnt s = match cnt with
    | 0 -> s
    | _ -> color (cnt - 1) (s ^ (String.make 1 hex.[(Random.int 9)])) in
  color 6 "#"

let rec rangef a b =
  if a > b then []
  else a::rangef (a +. 1.) b

class pie_chart model =
  object

    method draw (ctx : canvasCtx)  = 
      let o = {x=model.w/.2.;y=model.h/.2.} in
      let r = 60.0 in
      let bgColor = Js.string "#2b3d5b" in

      ctx##beginPath ;
      ctx##.lineWidth := 2.;
      ctx##.fillStyle := bgColor ;
      ctx##.strokeStyle := Js.string "#101010" ;
      ctx##fillRect 0. 0. model.w model.h ;

      let stroke_line p0 p1 = 
        ctx##moveTo p0.x p0.y ;
        ctx##lineTo p1.x p1.y ;
        ctx##stroke 
      in

      let rec draw_child ctx node r cnt =
        if cnt < 1 then 
          ()
        else
          for i = 0 to 1 do
            let child = {x = r *. 2.62 *. (sin (float i *. 1.62)); y = r *. 2.62 *. (cos (float i *. 1.62))} in
            let o = { x = node.x +. child.x; y = node.y +. child.y} in
            let r = r /. 1.62 in
            ctx##.fillStyle := Js.string "#fefefe" ;
            ctx##.strokeStyle := Js.string "#fefefe" ;
            ctx##beginPath ;
            ctx##arc o.x o.y r 0. (pi *. 2.) (Js.bool false);
            ctx##fill ;
            stroke_line node o ;
            draw_child ctx o r (cnt-1)
          done
      in

      ctx##.fillStyle := Js.string "#fefefe" ;
      ctx##beginPath ;
      ctx##arc o.x o.y r 0. (pi *. 2.) (Js.bool false);
      ctx##fill ;
      draw_child ctx o r 2;
      ()
end


let init canvas =
  print_endline "initializing";
  let () = open_canvas canvas in
  let c = canvas##getContext Html._2d_ in
  let w, h = (float canvas##.width), (float canvas##.height) in
  let data = [{x=0.; y=0.}] in
  let model = { w; h; data } in
  let chart = new pie_chart model in
  chart#draw c ;
  ()

let () = Js.export "init" init