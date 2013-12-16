module MonteCarloPi

open System
open System.Threading
open System.Diagnostics
open MathNet.Numerics.Random
open System.Linq
open System.Collections.Generic


type RNGMethod =
| BuiltIn = 0
| MathDotNet = 1
| AlgLib = 2


type ParalMeth =
| None = 0
| ArrayParallel = 1
| PLINQ = 2


let dicAL = Dictionary<int, alglib.hqrnd.hqrndstate>()
let dicMDN = Dictionary<int, System.Random>()
let dicSR = Dictionary<int, System.Random>()

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
    | _ -> 
        
        let id = Thread.CurrentThread.ManagedThreadId
        let rngBI = 
            if dicSR.ContainsKey(id) then dicSR.[id]
            else
                dicSR.[id] <- System.Random()
                dicSR.[id]
        rngBI.NextDouble()

let piEstimate meth parMeth arr : float =
    match parMeth with
    
    | ParalMeth.ArrayParallel -> 
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
        let arr = arr
        let insideCount = arr.AsParallel().WithDegreeOfParallelism(4).Sum(fun x -> 
                let x = getRnd meth
                let y = getRnd meth
                let len = Math.Sqrt(x * x + y * y)
                if len < 1.0 then 1 else 0
            )
        4.0 * float(insideCount) / float(arr.Length)

    | _ -> 
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


let run meth parMeth arr =
    watch <- Stopwatch.StartNew();
    let piEst =  (piEstimate meth parMeth arr)
    watch.Stop()
    let runtime = watch.ElapsedMilliseconds
    Console.WriteLine("RNG: {0}; Parallel: {1}", meth.ToString(), parMeth.ToString())
    Console.WriteLine("value: {0}; error: {1}%; msec: {2}",
        piEst.ToString(), (100.0 * Math.Round((piEst/Math.PI - 1.0), 5)).ToString(),
        runtime.ToString()
        )

[<EntryPoint>]
let main argv = 

    while true do
        let c = Int32.Parse(Console.ReadLine())
        let arr = (Array.init c id)

        run RNGMethod.BuiltIn ParalMeth.None arr
        run RNGMethod.BuiltIn ParalMeth.ArrayParallel arr
        run RNGMethod.BuiltIn ParalMeth.PLINQ arr

        dicAL.Clear()
        dicMDN.Clear()
        dicSR.Clear()

        run RNGMethod.AlgLib ParalMeth.None arr
        run RNGMethod.AlgLib ParalMeth.ArrayParallel arr
        run RNGMethod.AlgLib ParalMeth.PLINQ arr

        dicAL.Clear()
        dicMDN.Clear()
        dicSR.Clear()

        run RNGMethod.MathDotNet ParalMeth.None arr
        run RNGMethod.MathDotNet ParalMeth.ArrayParallel arr
        run RNGMethod.MathDotNet ParalMeth.PLINQ arr

        dicAL.Clear()
        dicMDN.Clear()
        dicSR.Clear()

    printfn "%A" argv
    0 // return an integer exit code
