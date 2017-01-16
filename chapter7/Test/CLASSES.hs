module Test.CLASSES where

import           Test.Base
import qualified Test.IMPLICIT_REFS as IMPLICIT_REFS

testList = IMPLICIT_REFS.testList ++ [
    NumTest "create-empty-class"
            "class c1 extends object 3"
            3,
    NumTest "create-class-with-method"
            "class c1 extends object \
            \   field y \
            \   method gety() y \
            \33"
            33,
    NumTest "create-object"
            "class c1 extends object \
            \    method initialize() 0 \
            \let o1 = new c1() in 11"
            11,
    NumTest "send-msg-1"
            "class c1 extends object \
            \    field s \
            \    method initialize() set s = 44 \
            \    method gets() s \
            \    method sets(v) set s = v \
            \let o1 = new c1() in send o1 gets()"
            44,
    ListNumTest "send-msg-2"
            "class c1 extends object \
            \    field s \
            \    method initialize() set s = 44 \
            \    method gets() s \
            \    method sets(v) set s = v \
            \let o1 = new c1() \
            \in let t1 = 0 \
            \in let t2 = 0 \
            \in begin set t1 = send o1 gets(); \
            \         send o1 sets(33); \
            \         set t2 = send o1 gets(); \
            \         list(t1, t2) \
            \   end"
           [44,33],
    ListNumTest "test-self-1"
            "class c extends object \
            \    field s \
            \    method initialize(v) set s = v \
            \    method sets(v) set s = v \
            \    method gets() s \
            \    method testit() send self sets(13) \
            \let o = new c (11) \
            \    t1 = 0 \
            \    t2 = 0 \
            \in begin set t1 = send o gets(); \
            \         send o testit(); \
            \         set t2 = send o gets(); \
            \         list(t1,t2) \
            \   end"
            [11,13],
    -- next one is queue with shared counter object (passed at initialization)
    ListNumTest "counter-1"
            "class counter extends object \
            \    field count \
            \    method initialize() set count = 0 \
            \    method countup() set count = -(count, -1) \
            \    method getcount() count \
            \let o1 = new counter () \
            \    t1 = 0 \
            \    t2 = 0 \
            \in begin set t1 = send o1 getcount(); \
            \         send o1 countup(); \
            \         set t2 = send o1 getcount(); \
            \         list(t1,t2) \
            \   end"
            [0,1],
    -- Chris's first example
    NumTest "chris-1"
            "class aclass extends object \
            \    field i \
            \    method initialize(x) set i = x \
            \    method m(y) -(i,-(0,y)) \
            \let o1 = new aclass(3) \
            \in send o1 m(2)"
            5,
    NumTest "odd-even-via-self"
            "class oddeven extends object \
            \    method initialize() 1 \
            \    method even(n) if zero?(n) then 1 else send self odd  (-(n,1)) \
            \    method odd (n) if zero?(n) then 0 else send self even (-(n,1)) \
            \let o1 = new oddeven() in send o1 odd(13)"
            1,
    NumTest "inherit-1"
            "class c1 extends object \
            \    field ivar1 \
            \    method initialize() set ivar1 = 1 \
            \class c2 extends c1 \
            \    field ivar2 \
            \    method initialize() begin \
            \        super initialize(); \
            \        set ivar2 = 1 \
            \    end \
            \    method setiv1(n) set ivar1 = n \
            \    method getiv1() ivar1 \
            \let o = new c2 () \
            \    t1 = 0 \
            \in begin \
            \       send o setiv1(33); \
            \       send o getiv1() \
            \   end"
            33,
    ListNumTest "inherit-3"
            "class c1 extends object \
            \    method initialize() 1 \
            \    method m1() 1 \
            \class c2 extends c1 \
            \    method m1() super m1() \
            \    method m2() 2 \
            \class c3 extends c2 \
            \    method m1() 3 \
            \    method m2() super m2() \
            \    method m3() super m1() \
            \let o = new c3 () \
            \in list( send o m1(), \
            \         send o m2(), \
            \         send o m3())"
            [3,2,1],
    ListNumTest "chris-2"
            "class c1 extends object \
            \    method initialize() 1 \
            \    method ma() 1 \
            \    method mb() send self ma() \
            \class c2 extends c1 \
            \    method ma() 2 \
            \let x = new c2 () \
            \in list(send x ma(),send x mb())"
            [2,2],
    ListNumTest "for-book-2"
            "class c1 extends object \
            \    method initialize() 1 \
            \    method m1() 1 \
            \    method m2()100 \
            \    method m3()send self m2() \
            \class c2 extends c1 \
            \    method m2()2 \
            \let o1 = new c1() \
            \    o2 = new c2() \
            \in list(send o1 m1(), \
            \        send o1 m2(), \
            \        send o1 m3(), \
            \        send o2 m1(), \
            \        send o2 m2(), \
            \        send o2 m3() \
            \    )"
            {-
            \in list(send o1 m1(), \
            \        send o1 m2(), \
            \        send o1 m3(), \
            \        send o2 m1(), \
            \        send o2 m2(), \
            \        send o2 m3() \
            -}
            [1,100,100,1,2,2],
    NumTest "sum-leaves"
            "class tree extends object \
            \    method initialize()1 \
            \class interior_node extends tree \
            \    field left \
            \    field right \
            \    method initialize(l,r) begin \
            \        set left = l; set right = r \
            \    end \
            \    method sum() -(send left sum(), -(0, send right sum())) \
            \class leaf_node extends tree \
            \    field value \
            \    method initialize(v)set value = v \
            \    method sum()value \
            \let o1 = new interior_node ( \
            \             new interior_node ( \
            \                 new leaf_node(3), \
            \                 new leaf_node(4)), \
            \                 new leaf_node(5)) \
            \in send o1 sum()"
            12,
    NumTest "static-super"
            "class c1 extends object \
            \    method initialize () 1 \
            \    method m2() send self m3() \
            \    method m3() 13 \
            \class c2 extends c1 \
            \    method m2() 22 \
            \    method m3() 23 \
            \    method m1() super m2() \
            \class c3 extends c2 \
            \    method m2() 32 \
            \    method m3() 33 \
            \let o3 = new c3() \
            \in send o3 m1()"
            33,
    NumTest "example-for-impl"
            "class c1 extends object \
            \    field x \
            \    field y \
            \    method initialize () begin \
            \        set x = 11; \
            \        set y = 12 \
            \    end \
            \    method m1 () -(x,y) \
            \    method m2 () send self m3() \
            \class c2 extends c1 \
            \    field y \
            \    method initialize () begin \
            \        super initialize(); \
            \        set y = 22 \
            \    end \
            \    method m1 (u,v) -(-(x,u), -(y,v)) \
            \    method m3 () 23 \
            \class c3 extends c2 \
            \    field x \
            \    field z \
            \    method initialize () begin \
            \        super initialize(); \
            \        set x = 31; \
            \        set z = 32 \
            \    end \
            \    method m3 () -(x,-(y,z)) \
            \let o3 = new c3() \
            \in send o3 m1(7,8)"
            (-10),
    NumTest "instanceof-example"
            "class A extends object \
            \class B extends A \
            \class C extends B \
            \let obj = new B() \
            \in if instanceof obj object \
            \   then if instanceof obj A \
            \        then if instanceof obj B \
            \             then if instanceof obj C \
            \                  then 1 \
            \                  else 0 \
            \             else 2 \
            \        else 3 \
            \    else 4"
            0
    ]

