namespace ChessTypeProvider

open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes

open Chess

[<TypeProvider()>]
type ChessTypeProvider() as this =
  inherit TypeProviderForNamespaces()

  // Get the assembly and namespace used to house the provided types
  let asm = System.Reflection.Assembly.GetExecutingAssembly()
  let ns = "ChessGame"

  let rec getType (move:string, game:Game) =

    let t = ProvidedTypeDefinition(move, Some(typeof<obj>))
    t.IsErased <- true

    for move in game.NextMoves do
        t.AddMemberDelayed (fun() -> getType move)

    t.HideObjectMethods <- true

    let gameString = game.ToString()

    let docLines = seq {
        yield "<summary>"
        for line in gameString.Split('\n') do
            yield sprintf "<para>%s</para>" line
        yield "</summary>"
    }

    t.AddXmlDoc(System.String.Join("\n", docLines))
    
    let ctor = ProvidedConstructor([])
    ctor.InvokeCode <- fun args ->
      <@@
        gameString
      @@>
    t.AddMember ctor

    t

  let rootType = ProvidedTypeDefinition(asm, ns, "Game", Some (typeof<obj>), HideObjectMethods = true)
  do rootType.AddMember (getType ("Start", Game.Start))
  do this.AddNamespace(ns, [rootType])
  
[<assembly:TypeProviderAssembly>] 
do()
