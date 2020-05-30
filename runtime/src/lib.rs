pub struct Cons {
    head: *const Object,
    tail: *const Object,
}

pub enum Object {
    Cons(Cons),
    Nil,
    Integer(i64),
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
pub unsafe extern fn rt_is_nil(objref: *const Object) -> bool {
    match *objref {
        Object::Nil => true,
        _ => false,
    }
}


#[no_mangle]
pub extern fn rt_cons(head: *const Object, tail: *const Object) -> *const Object {
    let ll = Box::new(Object::Cons(Cons { head, tail }));
    Box::into_raw(ll)
}


#[no_mangle]
pub unsafe extern fn rt_head(objref: *const Object) -> *const Object {
    match *objref {
        Object::Cons(Cons { head, .. }) => head,
        _ => panic!("expected Cons"),
    }
}


#[no_mangle]
pub unsafe extern fn rt_tail(objref: *const Object) -> *const Object {
    match *objref {
        Object::Cons(Cons { tail, .. }) => tail,
        _ => panic!("expected Cons"),
    }
}


#[no_mangle]
pub extern fn rt_box_int(value: i64) -> *const Object {
    let ll = Box::new(Object::Integer(value));
    Box::into_raw(ll)
}


#[no_mangle]
pub unsafe extern fn rt_unbox_int(objref: *const Object) -> i64 {
    match *objref {
        Object::Integer(x) => x,
        _ => panic!("expected Integer"),
    }
}
