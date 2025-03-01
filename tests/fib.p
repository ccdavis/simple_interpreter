let fib_of := 52;

let iter:=0;

let tmp_prev:=0;
let tmp_cur:=1;
let tmp_next:=0;

for (iter<=fib_of) {
	tmp_next := tmp_prev + tmp_cur;
	tmp_prev := tmp_cur;
	tmp_cur := tmp_next;
	iter := iter + 1;
};

output tmp_prev
