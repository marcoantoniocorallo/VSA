(* Executes preliminary analysis and VSA on the input cfg *)
let main cfg =
    SizeOf <- SizeOfAnalysis cfg
    availableVars <- availableVarsAnalysis cfg
    WideningThreshold <- max (ThresholdCalc cfg) WideningDefaultValue
    WorkList (new ValueSet(Top)) TransferFunctions cfg
;;

// Simplify Item[] notation when you need to access to an abstract state from the correspond node
// Dict.[Node(ID,Type(param),[])]  ==  Dict-->ID
// Not required, but used in some of these tests
let (-->) (dict : Dictionary<INode<Exp>,AbstractState>) (n : int) =
    let rec f (ks : INode<Exp> list) = 
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
asb-->5 // [x -> { {100; 101 } ,⊥} ; y -> { { 10; 11 } ,⊥}]
asb-->6 // [x -> { {100; 101 } ,⊥} ; y -> { { 10; 11 } ,⊥} ; z -> {⊤,⊥}]
asb-->7 // [x -> {⊤,⊥} ; y -> { { 10 ; 11 } ,⊥} ; z -> {⊤,⊥}]
asb-->8 // [x -> {3,⊥} ; y -> {10,⊥}]
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
                    new Node(3, If, [

                        // then
                        new Node(4, LeqConst("x",5), [
                            new Node(5, SimpleAss("y",11), [
                                new Node(6, SimpleAss("x",101), [
                                    new Node(7, GlobalDec(["z"]), [
                                        new Node(8, SumConst("x","x",2),[
                                            new Node(13, Return, [])
                                        ])
                                    ])
                                ])
                            ])
                        ]);

                        // else
                        new Node(9, GeqConst("x",6), [
                            new Node(10, SimpleAss("x",100), [
                                new Node(11, GlobalDec(["z"]), [
                                    new Node(12, SumConst("x","z",1),[
                                        new Node(13, Return, [])
                                    ])
                                ])
                            ]);
                       ])
                    ])
                ])
            ]);
        ])
    )
;; 
let abs = main cfg2;;
abs-->0  // []
abs-->1  // [x -> {⊤,⊥} ; y -> {⊤,⊥}]
abs-->2  // [x -> {3,⊥} ; y -> {⊤,⊥}]
abs-->3  // [x -> {3,⊥} ; y -> {10,⊥}]
abs-->4  // [x -> {3,⊥} ; y -> {10,⊥}]
abs-->5  // [x -> {3,⊥} ; y -> {10,⊥}]
abs-->6  // [x -> {3,⊥} ; y -> {11,⊥}]
abs-->7  // [x -> {101,⊥} ; y -> {11,⊥}]
abs-->8  // [x -> {101,⊥} ; y -> {11,⊥} ; z -> {⊥,⊤}]
abs-->9  // [x -> {3,⊥} ; y -> {10,⊥}]
abs-->10 // [x -> {⊥,⊥} ; y -> {10,⊥}]
abs-->11 // [x -> {100,⊥} ; y -> {10,⊥}]
abs-->12 // [x -> {100,⊥} ; y -> {10,⊥} ; z -> {⊤,⊥}]
abs-->13 // [x -> {⊤,⊥} ; y -> {(10,11),⊥} ; z -> {⊤,⊥} ]
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
let WhileNode = Node(2, While, []);;
let guard = Node(3, LeqConst("x",5), []);;
let body = Node(4, SumConst("x","x",1), [WhileNode]);;
guard.ChangeSucc([body]);;
let notguard = Node(5, GeqConst("x",6), []);;
let exit = Node(6, Return, []);;
notguard.ChangeSucc([exit]);;
WhileNode.ChangeSucc([guard;notguard])

let cfg3 = 
    new CFG(
        new Node(0,GlobalDec(["x"]),[
            new Node(1, SimpleAss("x",0), [
                WhileNode
            ])
        ])
    )
;;
let abs = main cfg3;; 

abs-->0 // []
abs-->1 // x -> {⊥, ⊤}
abs-->2 // x -> {⊥,[0,6]}
abs-->3 // x -> {⊥,[0,6]}
abs-->4 // x -> {⊥,[0,5]}
abs-->5 // x -> {⊥,[0,6]}
abs-->6 // x -> {⊥,[6,6]}
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
let whileNode1 = new Node(3, While, []);;
let guard1 = new Node(4,LeqConst("x",5),[]);;

let whileNode2 = new Node(5,While, []);;
let guard2 = new Node(6,LeqConst("y",5),[
    new Node(7,SumConst("y","y",1), [whileNode2])
]);;
let notguard2=new Node(8,GeqConst("y",6),[
    new Node(9,SumConst("x","x",1), [whileNode1])
]);;

whileNode2.ChangeSucc([guard2;notguard2]);;   
guard1.ChangeSucc([whileNode2]);;

let notguard1 = new Node(10,GeqConst("x",6),[
    new Node(11,Return,[])
]);;
whileNode1.ChangeSucc([guard1;notguard1]);;

let cfg4 = 
    new CFG(
        new Node(0,GlobalDec(["x";"y"]), [
            new Node(1,SimpleAss("x",0), [
                new Node(2,SimpleAss("y",0), [
                    whileNode1
                ])
            ])
        ])
    )
;;

let abs = main cfg4;;
abs-->0 // []
abs-->1 // [x -> {⊤,⊥}, y -> {⊤,⊥}]
abs-->2 // [x -> {[0,0],⊥}, y -> {⊤,⊥}]
abs-->3 // [x -> {[0,6],⊥}, y -> {[0,6],⊥}]
abs-->4 // [x -> {[0,6],⊥}, y -> {[0,6],⊥}]
abs-->5 // [x -> {[0,5],⊥}, y -> {[0,6],⊥}]
abs-->6 // [x -> {[0,5],⊥}, y -> {[0,6],⊥}]
abs-->7 // [x -> {[0,5],⊥}, y -> {[0,5],⊥}]
abs-->8 // [x -> {[0,5],⊥}, y -> {[0,6],⊥}]
abs-->9 // [x -> {[0,5],⊥}, y -> {[6,6],⊥}]
abs-->10// [x -> {[0,6],⊥}, y -> {[0,6],⊥}]
abs-->11// [x -> {[6,6],⊥}, y -> {[0,6],⊥}]
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
let whileNode = new Node(3, While, []);;

let Guard = new Node(4, GeqConst("x",1), []);;
let Body = new Node(5, TimesAloc("fact","fact","x"), [
          new Node(6, SumConst("x","x",-1), [whileNode] )
])
Guard.ChangeSucc([Body]);;
let notGuard = new Node(7, LeqConst("x",0), [
          new Node(8, Return, [])
]);;

whileNode.ChangeSucc([Guard;notGuard]);;

let cfg5 = 
    new CFG(
        new Node(0,GlobalDec(["x";"fact"]), [
            new Node(1,SimpleAss("x",n), [
                new Node(2,SimpleAss("fact",1), [
                    whileNode
                ])
            ])
        ])
    )
;;

let abs = main cfg5;;
abs-->0 // []
abs-->1 // [x -> {⊤,⊥} ; fact -> {⊤,⊥} ]
abs-->2 // [x -> {[3,3],⊥} ; fact -> {⊤,⊥}]
abs-->3 // [x -> {[0,3],⊥} ; fact -> {⊥, [1, inf]}]
abs-->4 // [x -> {[0,3],⊥} ; fact -> {⊥, [1, inf]}]
abs-->5 // [x -> {[1,3],⊥} ; fact -> {⊥, [1, inf]}]
abs-->6 // [x -> {[1,3],⊥} ; fact -> {⊥, [1, inf]}]
abs-->7 // [x -> {[0,3],⊥} ; fact -> {⊥, [1, inf]}]
abs-->8 // [x -> {[0,0],⊥} ; fact -> {⊥, [1, inf]}]
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
let whileNode = new Node(7, While, []);;
let Guard = new Node(8, GeqAloc("m","i"), []);;
let Body = 
    new Node(9, SumConst("f0","f1",0), [
        new Node(10, SumConst("f1","f2",0), [
            new Node(11, SumAloc("f2","f1","f0"),[
                new Node(12, SumConst("i","i",1), [whileNode])
            ])
        ])
    ])
;;
Guard.ChangeSucc([Body]);;
let notGuard = new Node(13, GeqAloc("i","m"), [
    new Node(14, Return, [])
]);;
whileNode.ChangeSucc([Guard;notGuard]);;

let cfg6 = 
    new CFG(
        new Node(0,GlobalDec(["n";"f0";"f1";"f2";"i";"m"]), [
            new Node(1, SimpleAss("n", n), [
                new Node(2,SimpleAss("f0",0), [
                    new Node(3,SimpleAss("f1",0), [
                        new Node(4, SimpleAss("f2",1), [
                            new Node(5, SimpleAss("i",0), [
                                new Node(6, SumConst("m","n",-1), [
                                    whileNode
                                ])
                            ])
                        ])
                    ])
                ])
            ])
        ])
    )
;;

let abs = main cfg6;;
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
let whileNode = new Node(3, While, []);;

let Guard = new Node(4, LeqConst("i",n-1),[
    new Node(5,ArrayAss("A","i","i"),[
            new Node(6, SumConst("i","i",1), [whileNode])
        ])
    ])
;;
let notGuard = new Node(7, GeqConst("i",n),[
   new Node(8, Return, [])
]);;
whileNode.ChangeSucc([Guard;notGuard]);;

let cfg7 =
    new CFG(
        new Node(0,GlobalDec(["i"]), [
            new Node(1,Array("A",n*4,0), [
                new Node(2,SimpleAss("i",0), [
                    whileNode
                ])
            ])
        ])
    )
;;

let abs = main cfg7
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
 * Return
 *)

// Uncomment from here
(*
let n = 7;;
let while0 = new Node(5, While, []);;
let while1 = new Node(13, While, []);;
let guard1 = 
    new Node(14,LeqConst("j",n),[
        new Node(15, SimpleAss("zero",0), [
            new Node(16, ArrayAss("A","j","zero"), [
                new Node(17, SumAloc("j","j","i"), [while1])
            ])
        ])
    ])
;;
let e2 = new Node(42,SumConst("i","i",1), [while0]);;
let notguard1 = new Node(18,GeqConst("j",n), [e2] );;
while1.ChangeSucc([guard1;notguard1]);;
let guard0 = 
    new Node(6, LeqConst("i",n), [
        new Node(7, If, [
            // then
            new Node(8, ArrayGeqConst("A","i",1), [
                new Node(9,TimesConst("j","i",2), [
                    while1
                ])
            ])
            // else
            new Node(10, ArrayLeqConst("A","i",1), [e2])
        ])
    ]);;
let notguard0 = 
    new Node(11, GeqConst("i",n+1), [
        new Node(12, Return, [])
    ])
;;
while0.ChangeSucc([guard0;notguard0]);;

let cfg8 =
    new CFG(
        new Node(0,GlobalDec(["i";"j";"zero"]), [
            new Node(1,Array("A",(n+1)*4,1), [
                new Node(2,SimpleAss("A+0",0), [
                    new Node(3, SimpleAss("A+4",0), [
                        new Node(4, SimpleAss("i",2),[ 
                            while0
                        ])
                    ])
                ])
            ])
        ])
    )
;;

let abs = main cfg8 |> ignore;;
*)

(*************************************************************)

// Off-By-One Test
(* var x
 * x = 0
 * while (x <= size)
 *     // other body instructions
 *     x = x + 1
 * Return;
 *)

// Test case 9: Uncomment from here
(*
let size = 9;

let WhileNode = Node(2, While, []);;
let guard = Node(3, LeqConst("x",size), []);;
let body = Node(4, SumConst("x","x",1), [WhileNode]);;
guard.ChangeSucc([body]);;
let notguard = Node(5, GeqConst("x",6), []);;
let exit = Node(6, Return, []);;
notguard.ChangeSucc([exit]);;
WhileNode.ChangeSucc([guard;notguard])

let cfg9 = 
    new CFG(
        new Node(0,GlobalDec(["x"]),[
            new Node(1, SimpleAss("x",0), [
                WhileNode
            ])
        ])
    )
;;
let abs = main cfg9;; 

abs-->0 // []
abs-->1 // x -> {⊥, ⊤}
abs-->2 // x -> {⊥,[0,6]}
abs-->3 // x -> {⊥,[0,6]}
abs-->4 // x -> {⊥,[0,5]}
abs-->5 // x -> {⊥,[0,6]}
abs-->6 // x -> {⊥,[6,6]}
*)

(*************************************************************)

// Buffer-overflow test
(* let size = input
 * let A[sizeof(int)*size] = (0,...,0)
 * let i
 * i = 0
 * while i<=size
 *    A[i] = i
 *    i = i+1
 * Return
 *)

// Uncomment from here
(*
let size = 5;;
let whileNode = new Node(3, While, []);;

let Guard = new Node(4, LeqConst("i",size),[
    new Node(5,ArrayAss("A","i","i"),[
            new Node(6, SumConst("i","i",1), [whileNode])
        ])
    ])
;;
let notGuard = new Node(7, GeqConst("i",size),[
   new Node(8, Return, [])
]);;
whileNode.ChangeSucc([Guard;notGuard]);;

let cfg10 =
    new CFG(
        new Node(0,GlobalDec(["i"]), [
            new Node(1,Array("A",size*4,0), [
                new Node(2,SimpleAss("i",0), [
                    whileNode
                ])
            ])
        ])
    )
;;

let abs = main cfg10
*) 