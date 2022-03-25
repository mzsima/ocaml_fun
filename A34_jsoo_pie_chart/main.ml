open Js_of_ocaml
open Graphics_js

module Html = Dom_html
let pi = acos (-1.)

type model = { w: float; h: float; data: float list } 

type canvasElement = Html.canvasElement Js.t  
type canvasCtx = Html.canvasRenderingContext2D Js.t
type point = {x : float; y : float}

let generateColor () = 
  Random.self_init () ;
  let hex = "0123456789ABCDEF" in
  let rec color cnt s = match cnt with
    | 0 -> s
    | _ -> color (cnt - 1) (s ^ (String.make 1 hex.[(Random.int 15)])) in
  color 6 "#"

class pie_chart model =
  object

    method draw (ctx : canvasCtx)  = 
      let o = {x=model.w/.2.;y=model.h/.2.} in
      let r = model.w /. 2. *. 0.46 in

      ctx##beginPath ;
      ctx##.lineWidth := 1.;
      ctx##.fillStyle := Js.string "#2b3d5b" ;
      ctx##fillRect 0. 0. model.w model.h ;

      let rec draw_pie prevAngle data total = match data with 
        | [] -> ()
        | head :: rest ->  
          let color = generateColor () in
          let angle = prevAngle +. (head /. total *. pi *. 2.) in
          ctx##.fillStyle := Js.string color;
          ctx##beginPath ;
          ctx##moveTo o.x o.y ;
          ctx##arc o.x o.y r prevAngle angle (Js.bool false) ;
          ctx##lineTo o.x o.y ;
          ctx##fill ;
          ctx##stroke ;
          draw_pie angle rest total ;
          ()
      in
      draw_pie 0. model.data (List.fold_left (+.) 0. model.data);
      ()
end

let init canvas =
  print_endline "initializing";
  let () = open_canvas canvas in
  let c = canvas##getContext Html._2d_ in
  let w, h = (float canvas##.width), (float canvas##.height) in
  let data = [ 16.; 68.; 20.; 30.; 54. ] in
  let model = { w; h; data } in
  let chart = new pie_chart model in
  chart#draw c ;
  ()

let () = Js.export "init" init