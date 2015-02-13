module Chess

open System

type Piece =
    | King
    | Queen
    | Bishop
    | Knight
    | Rook
    | Pawn

type Square =
    | White of Piece
    | Black of Piece
    | Empty with
    override x.ToString() =
        match x with
        | White King -> 9812
        | White Queen -> 9813
        | White Bishop -> 9815
        | White Knight -> 9816
        | White Rook -> 9814
        | White Pawn -> 9817
        | Black King -> 9818
        | Black Queen -> 9819
        | Black Bishop -> 9821
        | Black Knight -> 9822
        | Black Rook -> 9820
        | Black Pawn -> 9823
        | Empty -> 8195
        |> Char.ConvertFromUtf32
    member x.Mirrored =
        match x with
        | White p -> Black p
        | Black p -> White p
        | Empty -> Empty

let toAlgebraic =
    let letters = "ABCDEFGH".ToCharArray()
    fun (rank, line) -> sprintf "%c%d" letters.[line] (rank+1)

type Game =
    {
        Squares: Square array array
        IsWhiteTurn: bool
    } with
    static member Start =
            {
                IsWhiteTurn = true
                Squares =
                    [|
                        [| Rook ; Knight; Bishop; Queen; King ; Bishop; Knight; Rook  |] |> Array.map White
                        [| Pawn ; Pawn  ; Pawn  ; Pawn ; Pawn ; Pawn  ; Pawn  ; Pawn  |] |> Array.map White
                        [| Empty; Empty ; Empty ; Empty; Empty; Empty ; Empty ; Empty |]
                        [| Empty; Empty ; Empty ; Empty; Empty; Empty ; Empty ; Empty |]
                        [| Empty; Empty ; Empty ; Empty; Empty; Empty ; Empty ; Empty |]
                        [| Empty; Empty ; Empty ; Empty; Empty; Empty ; Empty ; Empty |]
                        [| Pawn ; Pawn  ; Pawn  ; Pawn ; Pawn ; Pawn  ; Pawn  ; Pawn  |] |> Array.map Black
                        [| Rook ; Knight; Bishop; Queen; King ; Bishop; Knight; Rook  |] |> Array.map Black
                    |]
            }
    member private this.TryGet(rank, file) =
        if rank >= 0 && rank <=7 && file >= 0 && file <= 7 then
            Some(this.Squares.[rank].[file])
        else
            None
    member private this.GetAvailableMoves(rank, file) =
        if not this.IsWhiteTurn then
            failwith "This method is only intended to be called for white player"
        else
            seq {
                // Now I 'only' need to implement every chess move
                match this.Squares.[rank].[file] with
                | White Pawn ->
                    if this.TryGet(rank+1,file) = Some Empty then
                        yield 1, 0
                        if rank = 1 && this.TryGet(rank+2,file) = Some Empty then
                            yield 2, 0
                    match this.TryGet(rank+1,file-1) with
                    | Some(Black _) -> yield 1, -1
                    | _ -> ()
                    match this.TryGet(rank+1,file+1) with
                    | Some(Black _) -> yield 1, 1
                    | _ -> ()

                | _ -> ()
            }
    member this.Mirrored =    
            {
                IsWhiteTurn = not this.IsWhiteTurn
                Squares = this.Squares |> Array.rev |> Array.map (Array.map (fun p -> p.Mirrored))
            }
    member this.NextMoves =
        seq {
            let game = if this.IsWhiteTurn then this else this.Mirrored
            for rank in 0 .. 7 do
            for file in 0 .. 7 do
                for rankChange, fileChange in game.GetAvailableMoves(rank, file) do
                    let startRank, startFile = (if this.IsWhiteTurn then rank else 7 - rank), file
                    let targetRank, targetFile = (if this.IsWhiteTurn then startRank + rankChange else startRank - rankChange), file + fileChange
                    let movedPiece = this.Squares.[startRank].[startFile]
                    let newSquares = this.Squares |> Array.map (Array.map id)
                    newSquares.[targetRank].[targetFile] <- movedPiece
                    newSquares.[startRank].[startFile] <- Empty
                    let newGame =
                        { this with
                            IsWhiteTurn = not this.IsWhiteTurn
                            Squares = newSquares
                        }
                    yield sprintf "%O%s%s" movedPiece (toAlgebraic (startRank, startFile)) (toAlgebraic (targetRank, targetFile)), newGame
        }
    override this.ToString() = 
        let sb = new System.Text.StringBuilder()
        for rank in 7 .. -1 .. 0 do
            sb.Append(".") |> ignore
            for file in 0 .. 7 do
                sb.Append(this.Squares.[rank].[file].ToString() + ".") |> ignore
            sb.AppendLine("") |> ignore
        sb.ToString()