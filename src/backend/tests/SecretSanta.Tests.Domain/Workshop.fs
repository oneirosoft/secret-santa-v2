module WorkshopTests

open Expecto
open SecretSanta

let private makePlayer nickname = Player.create (nickname, [])
let private addWishlistItem title player = player |> Player.addItem (WishlistItem.create (title, "https://example.com/" + title) |> Option.get)

[<Tests>]
let workshopTests =
    testList
        "Workshop"
        [ testCase "create initializes an empty workshop"
          <| fun () ->
              let id = Pneumonic.create 2uy
              let workshop = Workshop.create (id, "winter party", 25.0f)

              Expect.equal workshop.name "winter party" "Name should be set"
              Expect.equal workshop.dollarLimit 25.0f "Spending limit should be set"
              Expect.isEmpty workshop.players "Workshop should start with no players"
              Expect.isEmpty workshop.pairs "Workshop should start with no pairs"

          testCase "createWithGeneratedId produces a valid pneumonic id"
          <| fun () ->
              let workshop = Workshop.createWithGeneratedId ("gift exchange", 50.0f)
              let idString = workshop.id |> string

              Expect.isTrue (Pneumonic.isValid idString) "Generated id should be a valid pneumonic"

          testCase "Adding players appends and preserves order"
          <| fun () ->
              let playerOne = makePlayer "blitzen"
              let playerTwo = makePlayer "comet"

              let workshop =
                  Workshop.createWithGeneratedId ("gift exchange", 50.0f)
                  |> Workshop.addPlayer playerOne
                  |> Workshop.addPlayer playerTwo

              Expect.equal workshop.players [ playerOne; playerTwo ] "Players should keep insertion order"

          testCase "Adding a player with an existing nickname does not change the roster"
          <| fun () ->
              let existingPlayer = makePlayer "frosty-elf"

              let initialWorkshop =
                  Workshop.createWithGeneratedId ("gift exchange", 50.0f)
                  |> Workshop.addPlayer existingPlayer

              let updatedWorkshop =
                  initialWorkshop
                  |> Workshop.addPlayer (makePlayer existingPlayer.nickname)

              Expect.equal
                  updatedWorkshop.players
                  initialWorkshop.players
                  "Duplicate nicknames should be ignored"

          testCase "Removing a player drops the first matching entry only"
          <| fun () ->
              let player = makePlayer "elf"
              let other = makePlayer "reindeer"

              let workshop =
                  Workshop.createWithGeneratedId ("gift exchange", 50.0f)
                  |> Workshop.addPlayer player
                  |> Workshop.addPlayer other
                  |> Workshop.addPlayer player

              let updated = Workshop.removePlayer player workshop
              Expect.equal updated.players [ other ]
                  "Should remove the matching player and keep order"

          testCase "Removing a missing player leaves the roster unchanged"
          <| fun () ->
              let player = makePlayer "elf"
              let workshop =
                  Workshop.createWithGeneratedId ("gift exchange", 50.0f)
                  |> Workshop.addPlayer player

              let updated = Workshop.removePlayer (makePlayer "krampus") workshop
              Expect.equal updated.players workshop.players "Roster should be untouched"

          testCase "Updating a player's wishlist appends items"
          <| fun () ->
              let basePlayer =
                  makePlayer "elf" |> addWishlistItem "cookies"

              let workshop =
                  Workshop.createWithGeneratedId ("gift exchange", 50.0f)
                  |> Workshop.addPlayer basePlayer

              let updated =
                  Workshop.updatePlayerWishlist
                      basePlayer.nickname
                      [ WishlistItem.create ("milk", "https://example.com/milk") |> Option.get
                        WishlistItem.create ("lights", "https://example.com/lights") |> Option.get ]
                      workshop

              let updatedPlayer = updated.players |> List.find (fun p -> p.nickname = basePlayer.nickname)

              Expect.equal
                  updatedPlayer.wishlist
                  [ WishlistItem.create ("cookies", "https://example.com/cookies") |> Option.get
                    WishlistItem.create ("milk", "https://example.com/milk") |> Option.get
                    WishlistItem.create ("lights", "https://example.com/lights") |> Option.get ]
                  "Wishlist items should append in order"

          testCase "Updating wishlist for a missing player leaves workshop unchanged"
          <| fun () ->
              let workshop = Workshop.createWithGeneratedId ("gift exchange", 50.0f)
              let updated =
                  Workshop.updatePlayerWishlist
                      "nonexistent"
                      [ WishlistItem.create ("coal", "https://example.com/coal") |> Option.get ]
                      workshop

              Expect.equal updated.players workshop.players "No matching nickname should leave players unchanged" ]
