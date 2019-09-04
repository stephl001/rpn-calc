type UnaryHandler = float -> float
type BinaryHandler = float -> float -> float
type MultiHandler = float list -> float

type Operation =
    | UnaryOp of UnaryHandler
    | BinaryOp of BinaryHandler
    | MultiOp of MultiHandler

type CalcEntry =
    | Number of float
    | Oper of Operation

type RpnStack = 
    | Empty
    | Single of float
    | Multiple of float list

let stackToValue = function
| Empty -> Some 0.
| Single n -> Some n
| _ -> None

let pushNumber nb = function
| Empty -> Single nb
| Single s -> Multiple [nb; s]
| Multiple xs -> Multiple <| nb::xs

let pushOperator op stack =
    match (op,stack) with
    | (UnaryOp uop, Single nb) -> Single <| uop nb
    | (UnaryOp uop, Multiple (nb::xs)) -> Multiple <| uop nb :: xs
    | (BinaryOp bop, Multiple [n1;n2]) -> Single <| bop n1 n2
    | (BinaryOp bop, Multiple (n1::n2::xs)) -> Multiple <| bop n1 n2 :: xs
    | (MultiOp mop, Multiple xs) -> Single <| mop xs
    | _ -> stack
        
let pushEntry stack = function
| Number n -> pushNumber n stack
| Oper op -> pushOperator op stack

let (|Float|_|) str =
   match System.Double.TryParse(str) with
   | (true,int) -> Some(int)
   | _ -> None

let fact n =
    [2 .. n] |> List.fold (*) 1

let maxList =
    List.reduce max

let minList =
    List.reduce min

let parseOperator = function
| "+" -> BinaryOp (+)
| "-" -> BinaryOp (-)
| "*" -> BinaryOp (*)
| "/" -> BinaryOp (/)
| "max" -> MultiOp maxList
| "min" -> MultiOp minList
| "avg" -> MultiOp List.average
| "~" -> UnaryOp (~-)
| _ -> failwith "Invalid operator"

let parseOperatorEntry =
    parseOperator >> Oper

let parseElement = function
| Float i -> Number i
| op -> parseOperatorEntry op 

let parseExpression (str:string) =
    str.Split ' ' |> Array.map parseElement |> List.ofArray

let rec evaluateExpression = 
    List.fold pushEntry Empty

let printFloat = printfn "%f"

let evaluate =
    parseExpression 
    >> evaluateExpression 
    >> stackToValue 
    >> Option.iter printFloat