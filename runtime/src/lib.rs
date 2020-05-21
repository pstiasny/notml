#[no_mangle]
pub extern fn rt_print(x: i64) -> i64 {
    println!("{}", x);
    0
}


#[no_mangle]
pub extern fn rt_pchar(x: i64) -> i64 {
    print!("{}", char::from(x as u8));
    0
}
