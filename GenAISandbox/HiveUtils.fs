module HiveUtils
open System
open System.IO
open System.Data.Odbc
open System.Diagnostics
open Microsoft.Data.Analysis
open FsExcel

let DB_i360 = "DSN=i360;QueryTimeoutOverride=120; Schema=aep"
let DB_razr = "DSN=razr;QueryTimeoutOverride=360; Catalog=prdrzrlakehouse; Schema=ownedmedia"

let printColumns (dt:DataFrame) = 
    dt.Columns 
    |> Seq.iter (fun c -> printfn $"{c.Name}: {c.DataType}")

let showTable (dt:DataFrame) =    
    let cols = [for c in dt.Columns -> Cell [String  $"{c.Name}:{c.DataType.Name}"]]
    let rows = 
        seq{
            for r in dt.Rows do 
                yield! r.GetValues() |> Seq.map(fun x-> Cell [String (string x.Value)])
                yield Cell [Next NewRow]
        }
    let n = Path.GetTempFileName() + ".xlsx"
    let cells = cols @ [Cell [Next NewRow]] @ (Seq.toList rows)
    Render.AsFile n cells
    Process.Start(ProcessStartInfo(n,UseShellExecute=true)) |> ignore

let runQuery connectionString sql = 
    use connection = new OdbcConnection(connectionString)
    connection.Open()
    let command = connection.CreateCommand()
    command.CommandText <- sql
    use reader = command.ExecuteReader()
    DataFrame.LoadFrom(reader).Result

let runQueryLimit connectionString (sql:string) = 
    if sql.Contains("limit") then runQuery connectionString sql
    else runQuery connectionString (sql + " limit 1000")

let tableDesc connectionString tableName =
    let dt = runQuery connectionString $"DESCRIBE FORMATTED {tableName}"
    [for r in dt.Rows do 
        let cs = r.GetValues()
        (cs |> Seq.item 0 |> fun x -> string x.Value),
        (cs |> Seq.item 2 |> fun x -> string x.Value)
    ]

let tableNames connectionString = 
    let dt3 = runQuery connectionString "SHOW TABLES"
    [for r in dt3.Rows -> r.GetValues() |> Seq.item 1 |> fun x -> string x.Value]




