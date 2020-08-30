namespace EightBitBear

//module app =
//   open System.Collections.Generic
//   open System
   
//   type ShortName = string
//   type LongName = string
//   //type NameType = ShortName of string | LongName of string

//   type nameType = 
//      | Short of ShortName
//      | Long of LongName
//      | Invalid of unit

//   type CommandLineParameter = {
//      shortName : string;
//      longName : string;
//      help : string;
//      takesData : bool}

//   let paras = [|{shortName = "a"; longName = "alpha"; help = "cool text"; takesData = false}; {shortName = "b"; longName = "beta"; help = "cool text"; takesData = false}; {shortName = "c"; longName = "charlie"; help = "cool text"; takesData = true}|]

//   module ParamLengths =
//      let (|Short|Long|None|) (input:string) = 
//         if input.StartsWith("--") then Long 
//         else if input.StartsWith("-") then Short
//         else None
   
   
//   let FindName (arg : string) =
//      match arg with
//      | ParamLengths.Short -> Short(arg.Substring(1))
//      | ParamLengths.Long -> Long(arg.Substring(2))
//      | ParamLengths.None -> Invalid()


//   let ValidPara result = 
//      match result with
//      | Short s  -> Array.pick (fun p -> if p.shortName = s then Some(p) else None) paras
//      | Long l -> Array.pick (fun p -> if p.longName = l then Some(p) else None) paras

//   //let coolList =

//   //   Set.empty.Add(
//   //   .
//   let Iterate (arg:array<string>) =
//      let dict = new Dictionary<string, string>()
//      for i in 0 .. arg.Length - 1 do
//         let value =
//            Array.get arg i
//         let para =
//            FindName value |> ValidPara
//         if para.takesData then
//            dict.Add(para.longName, Array.get arg (i+1))
//         else
//            dict.Add(para.longName, null)
//      dict

//   //let proc1 arg (rules:string) =
//   //   FindName arg

//   //dict.Add("beta", FindName "beta"); 
//   let printResult result =
//      match result with
//      | Short s  -> printfn "Short: %s" s
//      | Long l -> printfn "Long: %s" l

//   FindName "-b" |> printResult
//   FindName "--beta" |> printResult
//   FindName "-a" |> printResult
//   FindName "--alpha" |> printResult
//   try
//      FindName "InvalidInput" |> printResult
//   with
//      | InvalidOperationException as ex -> printfn "Failed to parse parameter: %s" ex.Message

//   let testData = [|"-b"; "--alpha"|]
//   let testData2 = [|"--beta"; "-a";|]
//   let testData3 = [|"--beta"; "-c"; "butts"; "-a";|]
//   let testResult = Iterate testData
//   let testResult2 = Iterate testData2
//   let testResult3 = Iterate testData3

//   //let testSomething = testData3 li

//   for k in testResult.Keys do
//      printfn "%s" k
//   for k in testResult2.Keys do
//      printfn "%s" k
//   for k in testResult3.Keys do
//      printfn "%s" k
//   //let Process (args:array<string>) (rules:array<string>) =
//   //   for i 

module public CommandLineParser =
   open System.Collections.Generic
   open System

   type ShortName = string
   type LongName = string

   type nameType = 
      | Short of ShortName
      | Long of LongName
      | Invalid of unit

   type CommandLineParameter = {
      shortName : string;
      longName : string;
      help : string;
      takesData : bool}

   // Test input
   let private paras = [|{shortName = "a"; longName = "alpha"; help = "cool text"; takesData = false}; {shortName = "b"; longName = "beta"; help = "cool text"; takesData = false}; {shortName = "c"; longName = "charlie"; help = "cool text"; takesData = true}|]

   module ParamLengths =
      let (|Short|Long|None|) (input:string) = 
         if input.StartsWith("--") then Long 
         else if input.StartsWith("-") then Short
         else None

   let private FindName (arg : string) =
      match arg with
      | ParamLengths.Short -> Short(arg.Substring(1))
      | ParamLengths.Long -> Long(arg.Substring(2))
      | ParamLengths.None -> Invalid()

   let private ValidPara (parameters:array<CommandLineParameter>) result  = 
      match result with
      | Short s  -> Array.pick (fun p -> if p.shortName = s then Some(Some(p)) else None) parameters
      | Long l -> Array.pick (fun p -> if p.longName = l then Some(Some(p)) else None) parameters
      | Invalid -> None

   
   //let rec coolLoop (args:array<string>) (it:int) (error:string) (dict:Map<string,string option>) =
   //   if args.Length = it then
   //      (dict, error)
   //   else
   //      let arg = Array.get args it 
   //      let result (para:CommandLineParameter option) =
   //         match para with
   //         | Some x when x.takesData -> coolLoop args (it+2) error (dict.Add(x.longName, Some(Array.get args (it+1))))
   //         | Some x when not x.takesData -> coolLoop args (it+1) error (dict.Add(x.longName, None))
   //         | None -> (coolLoop args (it+1) (sprintf "%s\n%s didn't start with - or --" error arg) dict)
   //      arg |> FindName |> ValidPara paras |> result


   //let testData = [|"-b"; "--alpha"|]
   //let testData2 = [|"--beta"; "-a";|]
   //let testData3 = [|"--beta"; "-c"; "butts"; "-a";|]

   //let testResult = coolLoop testData 0 "" Map.empty 
   //let testResult2 = coolLoop testData2 0 "" Map.empty 
   //let testResult3 = coolLoop testData3 0 "" Map.empty

   //let printMapOption key value =
   //   match value with
   //   | Some -> printfn "Key: %s Value: %s" key value.Value
   //   | None -> printfn "Key: %s Value: None" key

   //let tupletStrip (a, b) =
   //   a

   //printfn "start:"
   //testResult |> tupletStrip |> Map.iter printMapOption
   //printfn "start:"
   //testResult2 |> tupletStrip |> Map.iter printMapOption
   //printfn "start:"
   //testResult3 |> tupletStrip |> Map.iter printMapOption

   let public ParseParameters (args:array<string>) (parameters:array<CommandLineParameter>) =
      let rec coolLoop (args:array<string>) (it:int) (error:string) (dict:Map<string,string option>) =
         if args.Length = it then
            (dict, error)
         else
            let arg = Array.get args it 
            let result (para:CommandLineParameter option) =
               match para with
               | Some x when x.takesData -> coolLoop args (it+2) error (dict.Add(x.longName, Some(Array.get args (it+1))))
               | Some x when not x.takesData -> coolLoop args (it+1) error (dict.Add(x.longName, None))
               | None -> (coolLoop args (it+1) (sprintf "%s\n%s didn't start with - or --" error arg) dict)
            arg |> FindName |> ValidPara parameters |> result
      coolLoop args 0 "" Map.empty

   let public ParseParametersCSharp (args:array<string>) (parameters:array<CommandLineParameter>) (error:string byref) =
      let (dic, errorrt)  = ParseParameters args parameters

      let stripOption (key:string, value:string option)  = 
         match value with
         | Some x -> (key, x)
         | None -> (key, null)

      let toDictionary (map : Map<_, _>) : Dictionary<_, _> = Dictionary(map)
      error <- errorrt
      Map.map (fun key (value) -> stripOption) dic |> toDictionary
