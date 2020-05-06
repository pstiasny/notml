#![no_main]


#[no_mangle]
pub extern fn rt_print(x: i64) -> i64 {
    println!("{}", x);
    0
}
