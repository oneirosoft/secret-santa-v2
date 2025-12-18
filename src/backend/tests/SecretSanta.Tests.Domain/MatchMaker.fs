module MatchMakerTests

open Expecto
open SecretSanta
open SecretSanta.MatchMaker

let private player nickname tags = Player.create (nickname, tags)

let private tag color = color

let private pairSet pairs = pairs |> Set.ofList

[<Tests>]
let matchMakerTests =
    testList
        "MatchMaker"
        [ testCase "0 players returns empty pairs"
          <| fun () ->
              let pairs = pair []
              Expect.isEmpty pairs "No players should produce no pairs"

          testCase "1 player returns empty pairs"
          <| fun () ->
              let pairs = pair [ player "red" [ tag "red" ] ]
              Expect.isEmpty pairs "Single player cannot be paired"

          testCase "Majority shared tag returns empty pairs"
          <| fun () ->
              let players =
                  [ player "red-1" [ tag "red" ]
                    player "red-2" [ tag "red" ]
                    player "red-3" [ tag "red" ]
                    player "blue-1" [ tag "blue" ] ]

              let pairs = pair players
              Expect.isEmpty pairs "Too many same-tag players should fail pairing"

          testCase "Even count players are all paired"
          <| fun () ->
              let players =
                  [ player "red-1" [ tag "red" ]
                    player "green-1" [ tag "green" ]
                    player "blue-1" [ tag "blue" ]
                    player "gold-1" [ tag "gold" ] ]

              let pairs = pair players

              Expect.equal pairs.Count players.Length "Each player should appear in exactly one pair"

              pairs
              |> Set.iter (fun (giver, receiver) -> Expect.notEqual giver receiver "Giver should not equal receiver")

          testCase "Odd count players still pair all participants"
          <| fun () ->
              let players =
                  [ player "red-1" [ tag "red" ]
                    player "green-1" [ tag "green" ]
                    player "blue-1" [ tag "blue" ]
                    player "gold-1" [ tag "gold" ]
                    player "silver-1" [ tag "silver" ] ]

              let pairs = pair players

              Expect.equal pairs.Count players.Length "Each player should still appear exactly once"

              pairs
              |> Set.iter (fun (giver, receiver) -> Expect.notEqual giver receiver "Giver should not equal receiver")

          testCase "Players are not paired with matching tag"
          <| fun () ->
              let players =
                  [ player "red-1" [ tag "red" ]
                    player "red-2" [ tag "red" ]
                    player "green-1" [ tag "green" ]
                    player "blue-1" [ tag "blue" ] ]

              let pairs = pair players

              pairs
              |> Set.iter (fun (giver, receiver) ->
                  let giverTags =
                      players |> List.find (fun p -> p.nickname = giver) |> (fun p -> p.tags)

                  let receiverTags =
                      players |> List.find (fun p -> p.nickname = receiver) |> (fun p -> p.tags)

                  Expect.isEmpty (Set.intersect giverTags receiverTags) "Giver and receiver should not share tags")

          testCase "Players are not paired with themselves"
          <| fun () ->
              let players =
                  [ player "red-1" [ tag "red" ]
                    player "green-1" [ tag "green" ]
                    player "blue-1" [ tag "blue" ] ]

              let pairs = pair players

              Expect.all pairs (fun (giver, receiver) -> giver <> receiver) "No self-pairings" ]
