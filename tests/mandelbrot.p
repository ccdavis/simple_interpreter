let y_resolution := 1600;
let x_resolution := 1600;

let max_iterations := 50;
let escape_radius := 2.0;


let left := -1.5;
let right := 1.0;
let top := 1.0;
let bottom := -1.0;

let height := bottom - top;
let width := right - left;

let x_increment := width / x_resolution;
let y_increment :=  height / y_resolution;

output "Begin looping";

for (left < right) {
	output "In first loop.";
    let line := "";
output "Declared line.";

    for (top < bottom) {
	output "in second loop.";

        let in_mandelbrot := false;
        let iterations := 0;

        let zn_x := 0.0;
        let zn_iy := 0.0;
        let tmp_zx := 0.0;

        let not_escaped := true;
        if ((zn_x*zn_x + zn_iy*zn_iy) < 2.0 *escape_radius) {
            not_escaped := false
        };
        for (iterations< max_iterations and not_escaped) {
            if (not_escaped) {
                tmp_zx := left + (zn_x * zn_x - zn_iy * zn_iy);
                zn_iy := top + 2.0 * zn_x * zn_iy;
                zn_x := tmp_zx;
                iterations := iterations + 1;
            };

	        if ((zn_x*zn_x + zn_iy*zn_iy) < 2.0 *escape_radius) {
                not_escaped := false
            }                        
        };
        if (not_escaped) {
            line := line + "*";            
        } else {
            line := line + ".";
        };
        top := top + y_increment
    };
    output line;
};
output "Complete!"
	
    
    

