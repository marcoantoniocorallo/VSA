# VSA
Value-Set Analysis is a binary data-flow analysis for tracking the values that each variable may assume for each program point.
You can find more information about VSA [here](https://link.springer.com/chapter/10.1007/978-3-540-24723-4_2) and [here](https://link.springer.com/chapter/10.1007/978-3-540-69149-5_22).

For this project a static analyzer was developed for a lightweight version of the VSA, able to analyze each integer-type data object of a single-procedure program.

#### In order to analyze your programs, you must compile classes and modules following the order of dependencies:
1. Expression.fs
2. MemoryRegion.fs
3. Values.fs
4. INode.fsi
5. Node.fs
6. ICFG.fsi
7. CFG.fs
8. PreliminaryAnalysis.fs
9. ILattice.fsi
10. VSLattice.fs 
11. TransferFunctions.fs
12. Work-List.fs
13. Main.fs

After that, you can define your Control Flow Graph *cfg* using the language wrote in *Expression.fs* and the classes *ICFG* and *INode*, and run

    main cfg

Each Abstract State corresponds to the node preceding the current node. Then the information about a node *i* is reported in the abstract state relative to the node *i+1*.
