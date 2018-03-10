class Bar inherits Foo {
  x : X;
  bar(): Int {
     "String" + 1
  };
};

class Foo {
  x : String;
};

class Baz inherits Foo {
  x : X;
  bar(): Int {
    not 1
  };
};

class Main inherits Bar {
   main (): Int {
     x
   };
};
