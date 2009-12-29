open Graphics
open Printf


let width = 500
let height = 500

let zoom = ref 4.0
let ax = ref 0.0
let ay = ref 0.0
let quality = ref 60


let iteration_count x0 y0 =

    let rec iter' x y n =
        if n = !quality then 255 else
        if x *. x +. y *. y > 4.0 
        then n * 256 / !quality
        else iter' (x *. x -. y *. y +. x0) (2.0 *. x *. y +. y0) (succ n)
    in
    iter' 0.0 0.0 0

let redraw () =

    (* Draw "busy" marker *)
    set_color (rgb 0x99 0x33 0x33);
    fill_rect 5 5 10 10;
    synchronize ();


    (* Draw mandelbrot *)
    let zoom_c = !zoom /. float width in
    let offs_x = !ax -. !zoom /. 2.0 in
    let offs_y = !ay -. !zoom /. 2.0 in
    for j = 0 to width - 1 do
        for k = 0 to height - 1 do
            let ca = zoom_c *. float j +. offs_x in
            let cb = zoom_c *. float k +. offs_y in
            let c = iteration_count ca cb in
            set_color (rgb c c c);
            plot j k;
        done;
    done;

    let text = sprintf "(%.3f,%.3f) zoom=%.3f quality=%d. hjkl, +/-, z/x" !ax !ay !zoom !quality in
    set_color (rgb 0x33 0x33 0x33);
    moveto 6 (height - 20);
    draw_string text;
    moveto 5 (width - 21);
    draw_string text;
    moveto 6 (width - 21);
    draw_string text;
    set_color (rgb 0xdd 0xdd 0xdd);
    moveto 5 (width - 20);
    draw_string text;
    synchronize ()


let main () =

    let window_size = sprintf " %dx%d" width height in
    open_graph window_size;

    auto_synchronize false;

    let rec event_loop () = (
        redraw ();
        let bounce = !zoom /. 10. in
        match read_key () with
        | '=' | '+' | ']' -> zoom := !zoom /. 1.2;
        | '-' | '['       -> zoom := !zoom *. 1.2
        | 'h'          -> ax := !ax -. bounce
        | 'l'          -> ax := !ax +. bounce
        | 'j'          -> ay := !ay +. bounce
        | 'k'          -> ay := !ay -. bounce
        | 'z'          -> quality := max 1 (!quality - 5)
        | 'x'          -> quality := !quality + 5
        | 'q' | '\027' -> exit 0
        | c            -> printf "[%d]%!" (Char.code c)
        );
        event_loop ()
    in

    event_loop ()

    

let _ = main ()
