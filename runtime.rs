#![no_main]


#[no_mangle]
pub extern fn rt_print(x: i64) {
    println!("{}", x);
}
