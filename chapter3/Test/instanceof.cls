% correct result: 0

class A extends object

class B extends A

class C extends B

let obj = new B()
in if instanceof obj object
   then if instanceof obj A
        then if instanceof obj B
             then if instanceof obj C
                  then 1
                  else 0
             else 2
        else 3
    else 4

