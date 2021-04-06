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
        |x::xs -> if x.ID()=n then dict.[x] else f xs
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
                    new Node(3, Ass1("x","y",5), [
                        new Node(4, Return, [])
                    ])
                ])
            ])
        ])
    )
;;

let abs = main cfg0
abs.[Node(0, GlobalDec(["x";"y"]), [])] // []
abs.[Node(1, SimpleAss("x",3),[])]      // [x -> {⊤,⊥} ; y -> {⊤,⊥}]
abs.[Node(2, SimpleAss("y",10),[])]     // [x -> {3,⊥} ; y -> {⊤,⊥}]
abs.[Node(3, Ass1("x","y",5), [])]      // [x -> {3,⊥} ; y -> {10,⊥}]
abs.[Node(4, Return, [])]               // [x -> {15,5}; y -> {10,⊥}]
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
                                new Node(6, Ass1("x","z",1),[
                                    new Node(7, Return, [])
                                ])
                            ])
                        ])
                    ])
                    new Node(8, SimpleAss("x",100), [
                        new Node(5, GlobalDec(["z"]), [
                            new Node(6, Ass1("x","z",1),[
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
asb.[Node(0, GlobalDec(["x";"y"]), [])] // []
asb.[Node(1, SimpleAss("x",3),[])]      // [x -> {⊤,⊥} ; y -> {⊤,⊥}]
asb.[Node(2, SimpleAss("y",10),[])]     // [x -> {3,⊥} ; y -> {⊤,⊥}]
asb.[Node(3, SimpleAss("y",11), [])]    // [x -> {3,⊥} ; y -> {10,⊥}]
asb.[Node(4, SimpleAss("x",101), [])]   // [x -> {3,⊥} ; y -> {11,⊥}]
asb.[Node(8, SimpleAss("x",100), [])]   // [x -> {3,⊥} ; y -> {10,⊥}]
asb.[Node(5, GlobalDec(["z"]), [])]     // [x -> { {-inf; 101 } ,⊥} ; y -> { { -inf; 11 } ,⊥}]
asb.[Node(6, Ass1("x","z",1), [])]      // [x -> { {-inf; 101 } ,⊥} ; y -> { { -inf; 11 } ,⊥} ; z -> {⊤,⊥}]
asb.[Node(7, Return, [])]               // [x -> {⊤,1} ; y -> { { -inf ; 11 } ,⊥} ; z -> {⊤,⊥}]
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
                                    new Node(7, Ass1("x","x",2),[
                                        new Node(11, Return, [])
                                    ])
                                ])
                            ])
                        ])

                        // else
                        new Node(8, SimpleAss("x",100), [
                            new Node(9, GlobalDec(["z"]), [
                                new Node(10, Ass1("x","z",1),[
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
asb-->11 // [x -> {[-inf,2]; ⊤} ; y -> {(-inf,11),⊥} ; z -> {⊥,⊤} ]
*)

(*************************************************************)

// Prova while

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
let body = Node(3, Ass1("x","x",1), [guard])
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
asb-->2 // x -> {[0,inf],⊥}
asb-->3 // x -> {[0,inf],⊥}
asb-->4 // x -> {[0,inf],⊥}
*)