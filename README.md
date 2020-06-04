# NotML

Compiler and runtime for the toy NotML language.  Built with Rust and LLVM.


# Example programs

## Factorial

The following code prints the factorial of 5.
Tail calls are optimized into efficient loops.

    factrec n acc = if n > 1
      then factrec (n - 1) (n * acc)
      else acc;

    fact n = factrec n 1;
    main = print (fact 5);


## List processing

Computes sum of list items.  Composite types are currently dynamic.

    icons x tail: (Int * Object) -> Object = cons (box_int x) tail;
    ihead xs: (Object) -> Int = unbox_int (head xs);

    make_list: Object = icons 1000 (icons 200 (icons 30 (icons 4 nil)));

    sum xs acc: (Object * Int) -> Int =
      if is_nil xs
        then acc
        else sum (tail xs) (acc + ihead xs);

    main = print (sum make_list 0);


## Imperative blocks

The following code prints numbers from 10 down to 0.

    loop n = if n >= 0
      then
        do
          print n;
          loop (n - 1)
        end
      else
        0;

    main = loop 10;


# Some things currently missing

* Garbage collection
* Closures
* Better type system, ADTs, type inference
