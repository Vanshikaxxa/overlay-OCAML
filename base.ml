(* open Graphics *)
open Tsdl

module KeyCodeSet = Set.Make(struct
  type t = Key.t
  let compare = compare
end)

module PlatformKey = Keysdl
module PlatformMouse = Mousesdl

(* FPS Calculation *)
let last_time = ref 0.0
let frame_count = ref 0
let fps_counter = ref 0

(* Stats toggle key *)
let stats_toggle_key = Key.F
let show_stats = ref false

(* Calculate FPS *)
let calculate_fps () =
  let now = Unix.gettimeofday () in
  let elapsed = now -. !last_time in
  frame_count := !frame_count + 1;
  if elapsed >= 1.0 then (
    fps_counter := !frame_count;
    frame_count := 0;
    last_time := now
  )

(* Font rendering *)
let draw_char x y c brightness create_pixel acc =
  match c with
  | 'F' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y); (x+3, y);
      (x, y+1);
      (x, y+2); (x+1, y+2); (x+2, y+2);
      (x, y+3);
      (x, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
       create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | 'P' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y);
      (x, y+1); (x+3, y+1);
      (x, y+2); (x+1, y+2); (x+2, y+2);
      (x, y+3);
      (x, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | 'S' ->
     let pixels = [
      (x+1, y); (x+2, y); (x+3, y);
      (x, y+1);
      (x+1, y+2); (x+2, y+2);
      (x+3, y+3);
      (x, y+4); (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | 'R' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y);
      (x, y+1); (x+3, y+1);
      (x, y+2); (x+1, y+2); (x+2, y+2);
      (x, y+3); (x+2, y+3);
      (x, y+4); (x+3, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | 'E' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y); (x+3, y);
      (x, y+1);
      (x, y+2); (x+1, y+2); (x+2, y+2);
      (x, y+3);
      (x, y+4); (x+1, y+4); (x+2, y+4); (x+3, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | 'D' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y);
      (x, y+1); (x+3, y+1);
      (x, y+2); (x+3, y+2);
      (x, y+3); (x+3, y+3);
      (x, y+4); (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | 'O' ->
     let pixels = [
      (x+1, y); (x+2, y);
      (x, y+1); (x+3, y+1);
      (x, y+2); (x+3, y+2);
      (x, y+3); (x+3, y+3);
      (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | 'T' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y); (x+3, y);
      (x+1, y+1); (x+2, y+1);
      (x+1, y+2); (x+2, y+2);
      (x+1, y+3); (x+2, y+3);
      (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | ':' ->
     let pixels = [
      (x+1, y+1); (x+2, y+1);
      (x+1, y+2); (x+2, y+2);
      (x+1, y+3); (x+2, y+3)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | 'x' ->
     let pixels = [
      (x, y); (x+3, y);
      (x+1, y+1); (x+2, y+1);
      (x+1, y+2); (x+2, y+2);
      (x+1, y+3); (x+2, y+3);
      (x, y+4); (x+3, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | ' ' -> acc 
  | '0' ->
     let pixels = [
      (x+1, y); (x+2, y);
      (x, y+1); (x+3, y+1);
      (x, y+2); (x+3, y+2);
      (x, y+3); (x+3, y+3);
      (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '1' ->
     let pixels = [
      (x+1, y);
      (x, y+1); (x+1, y+1);
      (x+1, y+2);
      (x+1, y+3);
      (x, y+4); (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '2' ->
     let pixels = [
      (x+1, y); (x+2, y);
      (x, y+1); (x+3, y+1);
      (x+2, y+2); (x+3, y+2);
      (x+1, y+3);
      (x, y+4); (x+1, y+4); (x+2, y+4); (x+3, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '3' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y);
      (x+3, y+1);
      (x+1, y+2); (x+2, y+2);
      (x+3, y+3);
      (x, y+4); (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '4' ->
     let pixels = [
      (x, y); (x+3, y);
      (x, y+1); (x+3, y+1);
      (x, y+2); (x+1, y+2); (x+2, y+2); (x+3, y+2);
      (x+3, y+3);
      (x+3, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '5' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y); (x+3, y);
      (x, y+1);
      (x, y+2); (x+1, y+2); (x+2, y+2);
      (x+3, y+3);
      (x, y+4); (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '6' ->
     let pixels = [
      (x+1, y); (x+2, y);
      (x, y+1);
      (x, y+2); (x+1, y+2); (x+2, y+2);
      (x, y+3); (x+3, y+3);
      (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '7' ->
     let pixels = [
      (x, y); (x+1, y); (x+2, y); (x+3, y);
      (x+3, y+1);
      (x+2, y+2);
      (x+1, y+3);
      (x+1, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '8' ->
     let pixels = [
      (x+1, y); (x+2, y);
      (x, y+1); (x+3, y+1);
      (x+1, y+2); (x+2, y+2);
      (x, y+3); (x+3, y+3);
      (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | '9' ->
     let pixels = [
      (x+1, y); (x+2, y);
      (x, y+1); (x+3, y+1);
      (x+1, y+2); (x+2, y+2); (x+3, y+2);
      (x+3, y+3);
      (x+1, y+4); (x+2, y+4)
    ] in
    List.fold_left (fun acc (px, py) ->
      create_pixel (px*2) (py*2) brightness acc
    ) acc pixels
  | _ -> acc 

let draw_string x y text brightness create_pixel acc =
  let chars = List.of_seq (String.to_seq text) in
  let rec aux x y chars acc =
    match chars with
    | [] -> acc
    | c :: cs -> 
      let new_acc = draw_char (x/2) (y/2) c brightness create_pixel acc in
      aux (x + 10) y cs new_acc 
  in
  aux x y chars acc

(* Render stats function *)
let render_stats create_pixel entity_count width height =
  calculate_fps ();

  let fps_text = Printf.sprintf "FPS: %d" !fps_counter in
  let entity_text = Printf.sprintf "DOTS: %d" entity_count in
  let res_text = Printf.sprintf "RES: %dx%d" width height in

  let base_x = 2 in
  let base_y = 2 in
  let brightness = 15 in

  let fps_primitives = draw_string (base_x*2) (base_y*2) fps_text brightness create_pixel [] in
  let entity_primitives = draw_string (base_x*2) ((base_y + 14)*2) entity_text brightness create_pixel fps_primitives in
  draw_string (base_x*2) ((base_y + 28)*2) res_text brightness create_pixel entity_primitives

type input_state = {
  keys: KeyCodeSet.t;
  mouse: Mouse.t;
}

type boot_func = Screen.t -> Framebuffer.t
type tick_func = int -> Screen.t -> Framebuffer.t -> input_state -> Framebuffer.t

type bitmap_t = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

type functional_tick_func = int -> Screen.t -> KeyCodeSet.t -> Primitives.t list

(* ----- *)

let (>>=) = Result.bind
let (>|=) v f = Result.map f v

let sdl_init (width : int) (height : int) (title : string) (make_fullscreen : bool) =
  Sdl.init Sdl.Init.(video + events) >>= fun () ->
  Sdl.create_window ~w:width ~h:height title Sdl.Window.(if make_fullscreen then fullscreen else windowed) >>= fun w ->
  Sdl.create_renderer ~flags:Sdl.Renderer.(accelerated + presentvsync) w >>= fun r ->
  Sdl.show_cursor (not make_fullscreen) >|= fun _ -> (w, r)

let framebuffer_to_bigarray (s : Screen.t) (buffer : Framebuffer.t) (bitmap : bitmap_t) =
  let palette = Screen.palette s in
  Array.iteri (fun y row ->
    Array.iteri (fun x pixel ->
      bitmap.{x + (y * (Array.length row))} <- Palette.index_to_rgb palette pixel
    ) row
  ) (Framebuffer.to_array buffer)

let render_texture (r : Sdl.renderer) (texture : Sdl.texture) (s : Screen.t) (bitmap : bitmap_t) =
  let width, height = Screen.dimensions s in
  let scale = Screen.scale s in
  Sdl.render_clear r >>= fun () ->
  Sdl.update_texture texture None bitmap width >>= fun () ->
  let ow, oh = Result.get_ok (Sdl.get_renderer_output_size r) in
  let dst = Sdl.Rect.create ~x:((ow - (width * scale)) / 2) ~y:((oh - (height * scale)) / 2) ~w:(width * scale) ~h:(height * scale) in
  Sdl.render_copy ~dst:dst r texture >|= fun () ->
  Sdl.render_present r


(* Create a pixel function for overlay *)
let create_overlay_pixel x y brightness acc =
  (* Try using a tuple directly *)
  (Primitives.Pixel({ x = x; y = y }, brightness)) :: acc
(* ----- *)

let run (title : string) (boot : boot_func option) (tick : tick_func) (s : Screen.t) =
  let make_full = Array.to_list Sys.argv |> List.exists (fun a -> (String.compare a "-f") == 0) in

  let s = match make_full with
  | false -> s
  | true ->
      let w, h = Screen.dimensions s and p = Screen.palette s in
      (match Screen.font s with
       | None -> Screen.create w h 1 p
       | Some f -> Screen.create_with_font w h 1 f p)
  in

  let width, height = Screen.dimensions s and scale = Screen.scale s in

  match sdl_init (width * scale) (height * scale) title make_full with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok (w, r) ->
    match Sdl.create_texture r Sdl.Pixel.format_rgb888 ~w:width ~h:height Sdl.Texture.access_streaming with
    | Error (`Msg e) -> Sdl.log "texture error: %s" e; exit 1
    | Ok texture ->
      (* This is a conversion layer, but allocaing bigarrays frequently is frowned upon
        so we allocate it once here and re-use it. *)
      let bitmap = (Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (width * height)) in

      let initial_buffer = match boot with
      | None -> Framebuffer.init (width, height) (fun _x _y -> 0)
      | Some bfunc -> bfunc s
      in

      let e = Sdl.Event.create () in
      let input = { keys = KeyCodeSet.empty; mouse = Mouse.create scale } in

      let rec loop (t : int) (prev_buffer : Framebuffer.t) (input : input_state) last_t = (
        let now = Sdl.get_ticks () in
        let diff = Int32.(sub (of_int (1000 / 60)) (sub now last_t)) in
        if Int32.(compare diff zero) > 0 then Sdl.delay diff;

        (* Calculate FPS for stats *)
        calculate_fps ();

        (* Check if F key was pressed to toggle stats *)
        if KeyCodeSet.mem stats_toggle_key input.keys && 
           not (KeyCodeSet.mem stats_toggle_key 
             (match Sdl.poll_event (Some e) with
              | true -> (
                match Sdl.Event.(enum (get e typ)) with
                | `Key_down -> KeyCodeSet.empty 
                | _ -> input.keys)
              | false -> input.keys)) then
          show_stats := not !show_stats;

        let updated_buffer = tick t s prev_buffer input in
        let input = { input with mouse = Mouse.clear_events input.mouse } in

        (* Apply overlay if enabled *)
        let final_buffer = 
          if !show_stats then
            let entity_count = 0 in (* You may need to adjust this based on your application *)
            let stats_primitives = render_stats create_overlay_pixel entity_count width height in
            Framebuffer.render updated_buffer stats_primitives;
            Framebuffer.set_dirty updated_buffer;
            updated_buffer
          else
            updated_buffer
        in

        if (final_buffer != prev_buffer) || (Framebuffer.is_dirty final_buffer) || (Screen.is_dirty s) then (
          framebuffer_to_bigarray s final_buffer bitmap;
          (match render_texture r texture s bitmap with
           | Error (`Msg e) -> Sdl.log "Boot error: %s" e
           | Ok () -> ());
          Framebuffer.clear_dirty final_buffer;
          Screen.clear_dirty s
        );

        match render_texture r texture s bitmap with
        | Error (`Msg e) -> Sdl.log "Boot error: %s" e
        | Ok () -> (
          let exit, updated_input =
          match Sdl.poll_event (Some e) with
          | true -> (
            match Sdl.Event.(enum (get e typ)) with
            | `Quit -> (true, input)
            | `Key_down -> 
                let key = PlatformKey.of_backend_keycode (Sdl.Event.(get e keyboard_keycode)) in
                (false, { input with keys = KeyCodeSet.add key input.keys })
            | `Key_up -> 
              let key = PlatformKey.of_backend_keycode (Sdl.Event.(get e keyboard_keycode)) in
              (false, { input with keys = KeyCodeSet.remove key input.keys })
            | `Mouse_button_down | `Mouse_button_up | `Mouse_motion | `Mouse_wheel ->
                let mouse = PlatformMouse.handle_event e input.mouse in
                (false, { input with mouse })
            | _ -> (false, input)
          )
          | false -> (false, input) in
          match exit with
          | true -> ()
          | false -> loop (t + 1) final_buffer updated_input now
        )
      ) in loop 0 initial_buffer input Int32.zero;

      Sdl.destroy_texture texture;
      Sdl.destroy_renderer r;
      Sdl.destroy_window w;
      Sdl.quit ()

let run_functional (title : string) (tick_f : functional_tick_func) (s : Screen.t) =
  let wrap_tick (t : int) (screen : Screen.t) (prev_framebuffer : Framebuffer.t) (input : input_state) : Framebuffer.t =
    let primitives : Primitives.t list = tick_f t screen input.keys in
    
    (* Check if F key was pressed to toggle stats *)
    if KeyCodeSet.mem stats_toggle_key input.keys && 
       not (KeyCodeSet.mem stats_toggle_key input.keys) then
      show_stats := not !show_stats;
    
    let all_primitives = 
      if !show_stats then
        let width, height = Screen.dimensions screen in
        let entity_count = List.length primitives in
        let stats_primitives = render_stats create_overlay_pixel entity_count width height in
        stats_primitives @ primitives
      else
        primitives
    in
    
    if all_primitives = [] then
      prev_framebuffer
    else
      let width, height = Screen.dimensions screen in
      let new_framebuffer = Framebuffer.init (width, height) (fun _x _y -> 0) in
      Framebuffer.render new_framebuffer all_primitives;
      new_framebuffer
  in
  run title None wrap_tick s
