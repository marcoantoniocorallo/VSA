# VSA

#### In order to run ValueSet Analysis in your own test cases, you have to compile classes and modules in the following order:
1. Expression.fs
2. MemoryRegion.fs
3. RIC.fs
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

After that, you can define your Control Flow Graph cfg and run

    main cfg
