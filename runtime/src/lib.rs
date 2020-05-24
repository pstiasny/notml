pub struct Cons {
    head: i64,
    tail: *const Object,
}

pub enum Object {
    Cons(Cons),
    Nil
}

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


#[no_mangle]
pub extern fn rt_nil() -> *const Object {
    let ll = Box::new(Object::Nil);
    Box::into_raw(ll)
}


#[no_mangle]
pub unsafe extern fn rt_is_nil(dv: *const Object) -> bool {
    match *dv {
        Object::Nil => true,
        _ => false,
    }
}


#[no_mangle]
pub extern fn rt_cons(value: i64, tail: *const Object) -> *const Object {
    let ll = Box::new(Object::Cons(Cons {
        head: value,
        tail: tail,
    }));
    Box::into_raw(ll)
}


#[no_mangle]
pub unsafe extern fn rt_head(dv: *const Object) -> i64 {
    match *dv {
        Object::Cons(Cons { head, .. }) => head,
        _ => panic!("expected Cons"),
    }
}


#[no_mangle]
pub unsafe extern fn rt_tail(dv: *const Object) -> *const Object {
    match *dv {
        Object::Cons(Cons { tail, .. }) => tail,
        _ => panic!("expected Cons"),
    }
}
