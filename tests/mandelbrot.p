
let points := 0.0;

let y_resolution := 120;
let x_resolution := 100;

let max_iterations := 50;
let escape_radius := 2.0;


let left := -1.5;
let right := 1.0;
let top := 1.0;
let bottom := -1.0;
let lower_boundary := bottom;

let height := top - bottom;
let width := right - left;

let x_increment := width / x_resolution;
let y_increment :=  height / y_resolution;

output x_increment;
output y_increment;

output "Begin looping";

for (left < right) {
    let line := "";    
    output "new line";
    for (bottom < top) {
        points := points + 1;
        output points;

        let in_mandelbrot := false;
        let iterations := 0;

        let zn_x := 0.0;
        let zn_iy := 0.0;
        let tmp_zx := 0.0;

        let not_escaped := true;
        for (not_escaped and (iterations < max_iterations)) {
                tmp_zx := left + (zn_x * zn_x - zn_iy * zn_iy);
                zn_iy := bottom + 2.0 * zn_x * zn_iy;
                zn_x := tmp_zx;
                iterations := iterations + 1;
                output "iterations";
                output iterations;            
                if ((zn_x*zn_x + zn_iy*zn_iy) < 2.0 *escape_radius) {
                    not_escaped := true;
                } else {
                    not_escaped := false;
                };                        
                output "not_escaped";
                output not_escaped;                        
        };
        if (iterations = max_iterations and not_escaped) {
            in_mandelbrot := true;
        } else {
            in_mandelbrot := false;
        };

        if (in_mandelbrot) {
            line := line + "*";            
        } else {
            line := line + ".";            
        };
        bottom := bottom + y_increment
    };
    output line;
    bottom := lower_boundary;
    left := left + x_increment
};
output points;
output "Complete!"
	
    
    

