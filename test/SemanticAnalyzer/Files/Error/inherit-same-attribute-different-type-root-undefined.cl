class Bar inherits Foo {
  x : Baz;
  bar(): Int {
     x
  };
};

class Foo {
  foo(): Int {
    1
  };
  x : Undefined;
};


class Main inherits Bar {
   main (): Int {
     x
   };
};
