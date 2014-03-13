module MonteCarloPi

open System
open System.Threading
open System.Diagnostics
open MathNet.Numerics.Random
open System.Linq
open System.Collections.Generic

type RNGMethod =
| Naive = 0 // keep using dics
| ThreadLocal = 1 // 

type ParalMeth =
| Simple = 0
| AV = 1


let dicSR = Dictionary<int, System.Random>()
let threadLocalRNG = new ThreadLocal<System.Random>( fun _ -> new System.Random(Environment.TickCount))

let getRnd meth =
    match meth with
    | Naive -> 
        
        let id = Thread.CurrentThread.ManagedThreadId
        let rngBI = 
            if dicSR.ContainsKey(id) then dicSR.[id]
            else
                dicSR.[id] <- System.Random()
                dicSR.[id]
        rngBI.NextDouble()

    | ThreadLocal ->  threadLocalRNG.Value.NextDouble()

let piEstimate meth parMeth c : float =

    match parMeth with
    
    | ParalMeth.Simple -> 
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

    | _ -> // ParalMeth.AV 
        let arr = (Array.init (c/2) id)
        let insideCount = ref 0
        arr |> Array.Parallel.map
            (fun i ->
                let x = getRnd(meth)
                let y = getRnd(meth)
                let x' = 1.0 - x
                let y' = 1.0 - y
                let len = Math.Sqrt(x * x + y * y)
                let len' = Math.Sqrt(x' * x' + y' * y')
                if len < 1.0 then 
                    Interlocked.Increment(&insideCount.contents) |> ignore
                if len' < 1.0 then
                    Interlocked.Increment(&insideCount.contents) |> ignore
            ) 
        |> ignore
        4.0 * float(insideCount.Value) / float(arr.Length * 2)
    

let mutable watch = Stopwatch.StartNew();
do 
    watch.Stop()


let run meth parMeth c =
    let rounds = 100
    let mutable api = 0.0
    let mutable aerr = 0.0
    let mutable aruntime = 0L
    for i in 1..rounds do
        let piEst, err, runtime = 
            watch <- Stopwatch.StartNew();
            let piEst' = piEstimate meth parMeth c
            watch.Stop()
            let err' = Math.Abs(piEst'/Math.PI - 1.0)
            let runtime' = watch.ElapsedMilliseconds
            piEst', err', runtime'
        api <- api + piEst
        aerr <- aerr + err
        aruntime <- aruntime + runtime
    api <- api / float(rounds)
    aerr <- aerr / float(rounds)
    aruntime <- aruntime / (int64 rounds)

    Console.WriteLine("value: {0}; error: {1}%; msec: {2}",
        api.ToString(), (100.0 * Math.Round(aerr, 5)).ToString(),
        aruntime.ToString()
        )


[<EntryPoint>]
let main argv = 

    while true do
        Console.WriteLine("Enter number of iterations")
        let c = Int32.Parse(Console.ReadLine())
        
        Console.WriteLine("")
        Console.WriteLine("Count: {0}", c.ToString())

        Console.WriteLine("Naive simple")
        run RNGMethod.Naive ParalMeth.Simple c
        dicSR.Clear()
        Console.WriteLine("Naive AV")
        run RNGMethod.Naive ParalMeth.AV c
        dicSR.Clear()

        Console.WriteLine("ThreadLocal simple")
        run RNGMethod.ThreadLocal ParalMeth.Simple c
        Console.WriteLine("ThreadLocal AV")
        run RNGMethod.Naive ParalMeth.AV c


    printfn "%A" argv
    0 // return an integer exit code
