open Sdlevent

let position = ref (0, 0) 
;;


let display_of_key = function
| "Up" -> "上"
| "Right" -> "右"
| "Down" -> "下"
| "Left" -> "左"
| "A" -> "A"
| "B" -> "B"
| _ -> "-"
;;

let proc_events = function
  | KeyDown e -> 
    Printf.printf "%s" (display_of_key (Sdlkeycode.to_string e.keycode));
    flush stdout ;
  | Quit e ->
      Printf.printf "Quit(timestamp:%ld)\n%!" e.quit_timestamp;
      Sdl.quit ();
      exit 0
  | _ -> ()


let () =
  Sdl.init [`VIDEO];
  at_exit print_newline;

  let rec event_loop () =
    match Sdlevent.poll_event () with
    | Some ev -> proc_events ev; event_loop ()
    | None -> ()
  in
  let rec main_loop () =
    event_loop ();
    Sdltimer.delay ~ms:20;
    main_loop ()
  in
  main_loop ()