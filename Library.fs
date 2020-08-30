namespace EightBitBear

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

   let public ParseParametersCSharp (args:array<string>) (parameters:array<CommandLineParameter>) (error:string byref) : Dictionary<string,string> =
      let (dic, errorrt)  = ParseParameters args parameters

      let inline stripOption (key:string) (value:string option) : string  = 
         match value with
         | Some x -> x
         | None -> null

      let inline toDictionary (map : Map<string, string>) : Dictionary<string, string> = Dictionary(map)
      error <- errorrt
      Map.map (fun key (value) -> stripOption key value) dic |> toDictionary 

   let public PrintHelp (parameters:array<CommandLineParameter>) (textWriter:System.IO.TextWriter) =
      parameters |> Array.iter (fun (para) -> sprintf "%-4s %-20s %-50s" para.shortName para.longName para.help |> textWriter.WriteLine)
