module MonteCarloPi

open System
open System.Threading
open System.Diagnostics
open MathNet.Numerics.Random
open System.Linq
open System.Collections.Generic
open QuantLib
open RDotNet
open RProvider
open RProvider.``base``
open RProvider.``stats``

type RNGMethod =
| BuiltIn = 0
| MathDotNet = 1
| AlgLib = 2
| QuantLib = 3
| RProvider = 4


type ParalMeth =
| None = 0
| ArrayParallel = 1
| PLINQ = 2
| Seq = 3

let dicAL = Dictionary<int, alglib.hqrnd.hqrndstate>()
let dicMDN = Dictionary<int, System.Random>()
let dicSR = Dictionary<int, System.Random>()
let dicQL = Dictionary<int, UniformRandomGenerator>()

let getRnd meth =
    match meth with
    | RNGMethod.AlgLib ->
        let id = Thread.CurrentThread.ManagedThreadId
        let state = 
            if dicAL.ContainsKey(id) then dicAL.[id]
            else
                let mutable st' =  (alglib.hqrnd.hqrndstate())
                alglib.hqrnd.hqrndrandomize(st')
                dicAL.[id] <- st'
                dicAL.[id]
        alglib.hqrnd.hqrnduniformr(state) // uses algo from P. L'Ecuyer, ``Efficient and Portable Combined Random Number Generators'', http://www.iro.umontreal.ca/~lecuyer/myftp/papers/cacm88.pdf
    
    
    | RNGMethod.MathDotNet ->
        let id = Thread.CurrentThread.ManagedThreadId
        let rngMDN = 
            if dicMDN.ContainsKey(id) then dicMDN.[id]
            else
                dicMDN.[id] <- new MersenneTwister(true)
                dicMDN.[id]
        rngMDN.NextDouble()
    | RNGMethod.QuantLib ->
        let id = Thread.CurrentThread.ManagedThreadId
        let rngQL = 
            if dicQL.ContainsKey(id) then dicQL.[id]
            else
                dicQL.[id] <- new UniformRandomGenerator()
                dicQL.[id]
        rngQL.nextValue()
    | _ -> 
        
        let id = Thread.CurrentThread.ManagedThreadId
        let rngBI = 
            if dicSR.ContainsKey(id) then dicSR.[id]
            else
                dicSR.[id] <- System.Random()
                dicSR.[id]
        rngBI.NextDouble()

let piEstimate meth parMeth c : float =

    match parMeth with
    
    | ParalMeth.ArrayParallel -> 
        let arr = (Array.init c id)
        let insideCount = ref 0
        arr |> Array.Parallel.map
            (fun i ->
                let x = getRnd(meth)
                let y = getRnd(meth)
                let len = Math.Sqrt(x * x + y * y)

                if len < 1.0 then 
                    Interlocked.Increment(&insideCount.contents) |> ignore
            ) 
        |> ignore
        4.0 * float(insideCount.Value) / float(arr.Length)

    | ParalMeth.PLINQ ->
        // PLINQ throws for BuiltIn, MDN and QL. Interesting that AlgLib doesn't thow under any conditions and the fastest
        let arr = (Array.init c id)
        let insideCount = arr.AsParallel().WithDegreeOfParallelism(4).Sum(fun x -> 
                let x = getRnd meth
                let y = getRnd meth
                let len = Math.Sqrt(x * x + y * y)
                if len < 1.0 then 1 else 0
            )
        4.0 * float(insideCount) / float(arr.Length)
    | ParalMeth.Seq ->
        match meth with
        | RNGMethod.QuantLib ->
            let insideCount = ref 0
            let rng = new UniformRandomGenerator()
            let seqGen = new UniformRandomSequenceGenerator(uint32(c), rng)
            let xs = seqGen.nextSequence().value()
            let ys = seqGen.nextSequence().value() 
            let res = 
                (xs, ys) ||> Seq.map2
                    (fun x y ->
                        let len = Math.Sqrt(x * x + y * y)
                        if len < 1.0 then 
                            Interlocked.Increment(&insideCount.contents) |> ignore
                    ) 
                |> Seq.toArray |> ignore
            4.0 * float(insideCount.Value) / float(c)
        | RNGMethod.RProvider ->
            
            let eval (text:string) =
                R.eval(R.parse(namedParams ["text", text ]))

            let xs = R.runif(c) 
            let ys = R.runif(c)     
            let xs' = R.assign("xs", xs) 
            let ys' = R.assign("ys", ys) 
            let expr = eval("sum((sqrt(xs*xs + ys*ys) < 1))")
            let insideCount = expr.AsNumeric().[0]
            4.0 * float(insideCount) / float(c)

        | _ -> raise (InvalidProgramException())
    | _ -> 
        let arr = (Array.init c id)
        let insideCount = ref 0
        arr |> Array.map
            (fun i ->
                let x = getRnd meth
                let y = getRnd meth
                let len = Math.Sqrt(x * x + y * y)

                if len < 1.0 then 
                    Interlocked.Increment(&insideCount.contents) |> ignore
            ) |> ignore
        4.0 * float(insideCount.Value) / float(arr.Length)

let mutable watch = Stopwatch.StartNew();
do 
    watch.Stop()


let run meth parMeth c =
    watch <- Stopwatch.StartNew();
    let piEst =
        try
             Some(piEstimate meth parMeth c)
        with
        | _ -> 
            None
    watch.Stop()
    let runtime = watch.ElapsedMilliseconds
    Console.WriteLine("RNG: {0}; Parallel: {1}", meth.ToString(), parMeth.ToString())
    if piEst.IsSome then
        Console.WriteLine("value: {0}; error: {1}%; msec: {2}",
            piEst.ToString(), (100.0 * Math.Round((piEst.Value/Math.PI - 1.0), 5)).ToString(),
            runtime.ToString()
            )
    else
        Console.WriteLine("exception...")


[<EntryPoint>]
let main argv = 

    while true do
        let c = Int32.Parse(Console.ReadLine())
        
        // Non-parallel is >2x slower but has no threading issues. Could make parallel at higher level if there are several independent simulations - will have simpler and more reliable code (no weird dict for memoization)

        // NB1: should compare Seq method with single-threaded! QL and R do their job in one thread
        // NB2: RProvider needs a warmup, first run is way too slow

        Console.WriteLine("")
        Console.WriteLine("Count: {0}", c.ToString())

        // Single thread
        Console.WriteLine("Single thread")
        run RNGMethod.BuiltIn ParalMeth.None c
        dicSR.Clear()
        run RNGMethod.AlgLib ParalMeth.None c
        dicAL.Clear()
        run RNGMethod.MathDotNet ParalMeth.None c
        dicMDN.Clear()
        run RNGMethod.QuantLib ParalMeth.None c
        dicQL.Clear()
        Console.WriteLine("")
        Console.WriteLine("Single thread, generate seqs")
        run RNGMethod.RProvider ParalMeth.Seq c
        run RNGMethod.QuantLib ParalMeth.Seq c
        Console.WriteLine("")

        // Parallel
        Console.WriteLine("Parallel")
        run RNGMethod.BuiltIn ParalMeth.ArrayParallel c
        dicSR.Clear()
        run RNGMethod.AlgLib ParalMeth.ArrayParallel c
        dicAL.Clear()
        run RNGMethod.MathDotNet ParalMeth.ArrayParallel c
        dicMDN.Clear()
        run RNGMethod.QuantLib ParalMeth.ArrayParallel c
        dicQL.Clear()



// exclude PLINQ because it throws (need more accurate threading with RNG or smth else?) 
//        run RNGMethod.BuiltIn ParalMeth.PLINQ c
//        dicSR.Clear()
//        run RNGMethod.AlgLib ParalMeth.PLINQ c
//        dicAL.Clear()
//        run RNGMethod.MathDotNet ParalMeth.PLINQ c
//        dicMDN.Clear()
//        run RNGMethod.QuantLib ParalMeth.PLINQ c
//        dicQL.Clear()

        


    printfn "%A" argv
    0 // return an integer exit code
