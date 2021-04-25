(* Executes preliminary analysis and VSA on the input cfg *)
let main cfg =
    SizeOf <- SizeOfAnalysis cfg
    availableVars <- availableVarsAnalysis cfg
    WorkList (new ValueSet(Top)) TransferFunctions cfg
;;

// Simplify Item[] notation when you need to access to an abstract state from the correspond node
// Dict.[Node(ID,Type(param),[])]  ==  Dict-->ID
// Not required, but used in some of these tests
let (-->) (dict : Dictionary<INode,AbstractState>) (n : int) =
    let rec f (ks : INode list) = 
        match ks with 
        |[] -> failwith("INode not found")
        |x::xs -> if x.ID()=n then dict.[x].ToString() else f xs
    in f ( dict.Keys |> Seq.map id |> Seq.toList)
;;

(******************************************************)
(***************** Some test cases ********************)
(******************************************************)

(*    var x,y  
 *    x = 3    
 *    y = 10   
 *    x = y+5  
 *    Return;  
 *)

// Test case 0: Uncomment to run
(*
let cfg0 = 
    new CFG(
        new Node(0, GlobalDec(["x";"y"]), [
            new Node(1, SimpleAss("x",3), [
                new Node(2, SimpleAss("y",10), [ 
                    new Node(3, SumConst("x","y",5), [
                        new Node(4, Return, [])
                    ])
                ])
            ])
        ])
    )
;;

let abs = main cfg0
abs-->0 // []
abs-->1 // [x -> {⊤,⊥} ; y -> {⊤,⊥}]
abs-->2 // [x -> {3,⊥} ; y -> {⊤,⊥}]
abs-->3 // [x -> {3,⊥} ; y -> {10,⊥}]
abs-->4 // [x -> {15,⊥}; y -> {10,⊥}]
*)

(*************************************************************)

(*    var x,y       
 *    x = 3         
 *    y = 10        
 *    if () then    
 *        y = 11    
 *        x = 101   
 *    else          
 *        x = 100   
 *    var z         
 *    x = z+1       
 *    Return;   
 *)

// Test case 1: Uncomment from here
(*
let cfg1 = 
    new CFG(
        new Node(0, GlobalDec(["x";"y"]),[ 
           new Node(1, SimpleAss("x",3), [
               new Node(2, SimpleAss("y",10),[
                    new Node(3, SimpleAss("y",11), [
                        new Node(4, SimpleAss("x",101), [
                            new Node(5, GlobalDec(["z"]), [
                                new Node(6, SumConst("x","z",1),[
                                    new Node(7, Return, [])
                                ])
                            ])
                        ])
                    ])
                    new Node(8, SimpleAss("x",100), [
                        new Node(5, GlobalDec(["z"]), [
                            new Node(6, SumConst("x","z",1),[
                                new Node(7, Return, [])
                            ])
                        ])
                    ]);
               ] )
           ]);
        ])
    )
;;

let asb = main cfg1
asb-->0 // []
asb-->1 // [x -> {⊤,⊥} ; y -> {⊤,⊥}]
asb-->2 // [x -> {3,⊥} ; y -> {⊤,⊥}]
asb-->3 // [x -> {3,⊥} ; y -> {10,⊥}]
asb-->4 // [x -> {3,⊥} ; y -> {11,⊥}]
asb-->8 // [x -> {3,⊥} ; y -> {10,⊥}]
asb-->5 // [x -> { {100; 101 } ,⊥} ; y -> { { 10; 11 } ,⊥}]
asb-->6 // [x -> { {100; 101 } ,⊥} ; y -> { { 10; 11 } ,⊥} ; z -> {⊤,⊥}]
asb-->7 // [x -> {⊤,1} ; y -> { { 10 ; 11 } ,⊥} ; z -> {⊤,⊥}]
*)

(*************************************************************)

(*    var x,y       
 *    x = 3         
 *    y = 10        
 *    if (x<=5) then
 *        y = 11    
 *        x = 101   
 *    else          
 *        x = 100   
 *    var z         
 *    x = z+1       
 *    Return;       
 *)

// Test case 2: Uncomment from here
(*
let cfg2 = 
    new CFG(
        new Node(0, GlobalDec(["x";"y"]),[ 
           new Node(1, SimpleAss("x",3), [
               new Node(2, SimpleAss("y",10),[
                    new Node(3, LeqConst("x",5), [

                        // then
                        new Node(4, SimpleAss("y",11), [
                            new Node(5, SimpleAss("x",101), [
                                new Node(6, GlobalDec(["z"]), [
                                    new Node(7, SumConst("x","x",2),[
                                        new Node(11, Return, [])
                                    ])
                                ])
                            ])
                        ])

                        // else
                        new Node(8, SimpleAss("x",100), [
                            new Node(9, GlobalDec(["z"]), [
                                new Node(10, SumConst("x","z",1),[
                                    new Node(11, Return, [])
                                ])
                            ])
                        ]);

                    ])
               ] )
           ]);
        ])
    )
;; 

let asb = main cfg2
asb-->0  // []
asb-->1  // [x -> {⊤,⊥} ; y -> {⊤,⊥}]
asb-->2  // [x -> {3,⊥} ; y -> {⊤,⊥}]
asb-->3  // [x -> {3,⊥} ; y -> {10,⊥}]
asb-->4  // [x -> {3,⊥} ; y -> {10,⊥}]
asb-->5  // [x -> {3,⊥} ; y -> {11,⊥}]
asb-->6  // [x -> {101,⊥} ; y -> {11,⊥}]
asb-->7  // [x -> {101,⊥} ; y -> {11,⊥} ; z -> {⊥,⊤}]
asb-->8  // [x -> {3,⊥} ; y -> {10,⊥}]
asb-->9  // [x -> {100,⊥} ; y -> {10,⊥}]
asb-->10 // [x -> {100,⊥} ; y -> {10,⊥} ; z -> {⊥,⊤}]
asb-->11 // [x -> {[1,2]; ⊤} ; y -> {(10,11),⊥} ; z -> {⊥,⊤} ]
*)

(*************************************************************)

// While test
(* var x
 * x = 0
 * while (x <= 5)
 *     x = x + 1
 * Return;
 *)

// Defining first-of-while node without succs and then assign them to the node, in order to compile correctly
// Test case 3: Uncomment from here
(*

let guard = Node(2, LeqConst("x",5), [])
let body = Node(3, SumConst("x","x",1), [guard])
let afterwhile = Node(4, Return, [])
guard.ChangeSucc([body;afterwhile])

let cfg3 = 
    new CFG(
        new Node(0,GlobalDec(["x"]),[
            new Node(1, SimpleAss("x",0), [
                guard
            ])
        ])
    )
;;
let asb = main cfg3;; 

asb-->0 // []
asb-->1 // x -> {⊤,⊥}
asb-->2 // x -> {[0,6],[1,inf]}
asb-->3 // x -> {[0,5],[1,inf]}
asb-->4 // x -> {[0,5],[1,inf]}
*)

(*************************************************************)

// nested-while test
(*
 * let x,y
 * x=0
 * y=0
 * while x<5
 *   while y<5
 *     y++
 *   x++
 * Return
 *)

// Test-case 4: uncomment from here
(*
let guard1 = new Node(3,LeqConst("x",5),[]);;
let guard2 = new Node(4,LeqConst("y",5),[]);;
let body2 = new Node(5,SumConst("y","y",1),[guard2]);;
let body1 = new Node(6, SumConst("x","x",1),[guard1]);;
let afterwhile = new Node(7,Return,[]);;
guard1.ChangeSucc([guard2;afterwhile]);;
guard2.ChangeSucc([body2;body1]);;
let cfg4 = 
    new CFG(
        new Node(0,GlobalDec(["x";"y"]), [
            new Node(1,SimpleAss("x",0), [
                new Node(2,SimpleAss("y",0), [
                    guard1
                ])
            ])
        ])
    )
;;

let abs = main cfg4;;
abs-->0 // []
abs-->1 // [x -> {⊤,⊥}, y -> {⊤,⊥}]
abs-->2 // [x -> {[0,0],⊥}, y -> {⊤,⊥}]
abs-->3 // [x -> {[0,6],⊥}, y -> {[0,5],[1,inf]}]
abs-->4 // [x -> {[0,5],⊥}, y -> {[0,6],[1,inf]}]
abs-->5 // [x -> {[0,5],⊥}, y -> {[0,5],[1,inf]}]
abs-->6 // [x -> {[0,5],⊥}, y -> {[0,5],[1,inf]}]
abs-->7 // [x -> {[0,5],⊥}, y -> {[0,5],[1,inf]}]
*)
(*
let cfg5 = 
    new CFG(
        new Node(0,GlobalDec(["x";"y"]), [
            new Node(1,SimpleAss("x",2), [
                new Node(2,SimpleAss("y",10), [
                    new Node(3, TimesAloc("x","x","y"), [
                        new Node(4, Return, [])
                    ])
                ])
            ])
        ])
    )
;;
main cfg5
*)

(*************************************************************)

// Factorial 
(* let x, fact
 * x = 3  // input x
 * fact = 1
 * while x >= 1
 *    fact = fact * x 
 *    x = x - 1
 * Return;
 *)

// Uncomment from here
(*
let n = 3;;
let Guard = new Node(3, GeqConst("x",1), []);;
let Body =new Node(4, TimesAloc("fact","fact","x"), [
              new Node(5, SumConst("x","x",-1), [Guard] )
          ])
;;
let Exit = new Node(6, Return, []);;
Guard.ChangeSucc([Body;Exit])

let cfg6 = 
    new CFG(
        new Node(0,GlobalDec(["x";"fact"]), [
            new Node(1,SimpleAss("x",n), [
                new Node(2,SimpleAss("fact",1), [
                    Guard
                ])
            ])
        ])
    )
;;

let abs = main cfg6;;
abs-->0 // []
abs-->1 // [x -> {⊤,⊥} ; fact -> {⊤,⊥} ]
abs-->2 // [x -> {[3,3],⊥} ; fact -> {⊤,⊥}]
abs-->3 // [x -> {[0,3],⊤} ; fact -> {⊤, [1, inf]}]
abs-->4 // [x -> {[1,3], [-inf, -1]} ; fact -> {⊤, [1, inf]}]
abs-->5 // [x -> {[1,3], [-inf, -1]} ; fact -> {⊤, [1, inf]}]
abs-->6 // [x -> {[1,3], [-inf, -1]} ; fact -> {⊤, [1, inf]}]
*)

(*************************************************************)

// Fibonacci
(* let f0, f1, f2, i, n
 * n = 5   // input n
 * f0 = 0; f1 = 0; f2 = 1
 * i = 0
 * while i < n
 *    f0 = f1
 *    f1 = f2
 *    f2 = f1 + f0
 *    i = i + 1
 * Return
 *)

// Uncomment from here
(*
let n = 5;;
let Guard = new Node(7, GeqAloc("m","i"), []);;
let Body = 
    new Node(8, SumConst("f0","f1",0), [
        new Node(9, SumConst("f1","f2",0), [
            new Node(10, SumAloc("f2","f1","f0"),[
                new Node(11, SumConst("i","i",1), [Guard])
            ])
        ])
    ])
;;
let Exit = new Node(12, Return, []);;
Guard.ChangeSucc([Body;Exit])

let cfg7 = 
    new CFG(
        new Node(0,GlobalDec(["n";"f0";"f1";"f2";"i";"m"]), [
            new Node(1, SimpleAss("n", n), [
                new Node(2,SimpleAss("f0",0), [
                    new Node(3,SimpleAss("f1",0), [
                        new Node(4, SimpleAss("f2",1), [
                            new Node(5, SimpleAss("i",0), [
                                new Node(6, SumConst("m","n",-1), [
                                    Guard
                                ])
                            ])
                        ])
                    ])
                ])
            ])
        ])
    )
;;

let abs = main cfg7;;
*)

(*************************************************************)

//Array test
(* let A[sizeof(int)*5] = (0,...,0)
 * let i
 * i = 0
 * while i<=4
 *    A[i] = i
 *    i = i+1
 * Return
 *)

// Uncomment from here
(*
let n = 5;;
let Guard = new Node(3, LeqConst("i",n-1),[]);;
let Body = new Node(4,ArrayAss("A","i","i"),[
               new Node(5, SumConst("i","i",1), [Guard])
           ])
;;
let Exit = new Node(6, Return, []);;
Guard.ChangeSucc([Body;Exit]);;

let cfg8 =
    new CFG(
        new Node(0,GlobalDec(["i"]), [
            new Node(1,Array("A",n*4,0), [
                new Node(2,SimpleAss("i",0), [
                    Guard
                ])
            ])
        ])
    )
;;

let abs = main cfg8
*)

(*************************************************************)

// Sieve of Eratosthenes (without some optimizations)

(* let i,j
 * let A[n+1] = (1,...,1)
 * A[0] = 0
 * A[1] = 0
 * i = 2
 * while i <= (n+1)
 *    if A[i] >= 1
 *       j = i*2
 *       while j <= n
 *          A[j] = 0
 *          j = j+i
 *)

// Uncomment from here
(*
let n = 7;;
let Guard0 = new Node(5, LeqConst("i",n+1), []);;
let Guard1 = new Node(6, ArrayGeqConst("A","i",1),[]);;
let Body1 = new Node(7, TimesConst("j","i",2), []);;
let Guard2 = new Node(8,LeqConst("j",n),[]);;
let Body2 = new Node(9, SimpleAss("zero",0), [
                new Node(10, ArrayAss("A","j","zero"),[
                    new Node(11, SumAloc("j","j","i"), [Guard2])
                ])
            ]);;
let Exit0 = new Node(12,Return,[]);;

Guard2.ChangeSucc([Body2;Guard1])
Body1.ChangeSucc([Guard2])
Guard1.ChangeSucc([Body1;Guard0]);;
Guard0.ChangeSucc([Guard1;Exit0]);;

let cfg9 =
    new CFG(
        new Node(0,GlobalDec(["i";"j";"zero"]), [
            new Node(1,Array("A",(n+1)*4,1), [
                new Node(2,SimpleAss("A+0",0), [
                    new Node(3, SimpleAss("A+4",0), [
                        new Node(4, SimpleAss("i",2),[ 
                            Guard0
                        ])
                    ])
                ])
            ])
        ])
    )
;;

let abs = main cfg9;;
*)