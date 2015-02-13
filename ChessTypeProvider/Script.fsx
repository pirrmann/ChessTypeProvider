#load "Chess.fs"
open Chess

open System

(Game.Start.NextMoves |> Seq.toArray).[3] |> snd |> (fun game -> game.NextMoves)

