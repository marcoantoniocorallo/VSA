(* Executes preliminary analysis and VSA on the input cfg *)
let main cfg =
    availableVars <- availableVarsAnalysis cfg
    WorkList (new ValueSet(Top)) TransferFunctions cfg

(******************************************************)
(***************** Some test cases ********************)
(******************************************************)

(*    var x,y   // []
 *    x = 3     // [x -> {⊤,⊥} ; y -> {⊤,⊥}]
 *    y = 10    // [x -> {3,⊥} ; y -> {⊤,⊥}]
 *    x = y+5   // [x -> {3,⊥} ; y -> {10,⊥}]
 *    Return;   // [x -> {15,5}; y -> {10,⊥}]
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
abs.[Node(0, GlobalDec(["x";"y"]), [])]
abs.[Node(1, SimpleAss("x",3),[])]
abs.[Node(2, SimpleAss("y",10),[])]
abs.[Node(3, Ass1("x","y",5), [])]
abs.[Node(4, Return, [])]
*)

(*************************************************************)

(*    var x,y       // []
 *    x = 3         // [x -> {⊤,⊥} ; y -> {⊤,⊥}]
 *    y = 10        // [x -> {3,⊥} ; y -> {⊤,⊥}]
 *    if () then    // TODO
 *        y = 11    // [x -> {3,⊥} ; y -> {10,⊥}]
 *        x = 101   // [x -> {3,⊥} ; y -> {11,⊥}]
 *    else          // TODO
 *        x = 100   // [x -> {3,⊥} ; y -> {10,⊥}]
 *    var z         // [x -> { {100; 101 } ,⊥} ; y -> { { 10; 11 } ,⊥}]
 *    x = z+1       // [x -> { {100; 101 } ,⊥} ; y -> { { 10; 11 } ,⊥} ; z -> {⊤,⊥}]
 *    Return;       // [x -> {⊤,1} ; y -> { { 10; 11 } ,⊥} ; z -> {⊤,⊥}]
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
asb.[Node(0, GlobalDec(["x";"y"]), [])]
asb.[Node(1, SimpleAss("x",3),[])]
asb.[Node(2, SimpleAss("y",10),[])]
asb.[Node(3, SimpleAss("y",11), [])]
asb.[Node(4, SimpleAss("x",101), [])]
asb.[Node(5, GlobalDec(["z"]), [])]
asb.[Node(6, Ass1("x","z",1), [])]
asb.[Node(8, SimpleAss("x",100), [])]
asb.[Node(7, Return, [])]
*)

(*************************************************************)