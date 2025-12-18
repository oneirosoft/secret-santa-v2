module PlayerTests

open Expecto
open SecretSanta



let defaultItem =
    WishlistItem.create ("first item", "https://example.com") |> Option.get


let secondItem =
    WishlistItem.create ("second item", "https://example.com/second") |> Option.get

let thirdItem =
    WishlistItem.create ("third item", "https://example.com/third") |> Option.get

let firstTag = "friendly elf"
let secondTag = "mysterious helper"

let player = Player.create ("secret santa", []) |> Player.addItem defaultItem

let playerWithTags = player |> Player.addTag firstTag |> Player.addTag secondTag

[<Tests>]
let playerTests =
    testList
        "Player"
        [ testTheory "Successfully adds a wishilit item" [ player ]
          <| fun player -> Expect.contains player.wishlist defaultItem "Wishlist should contain new item"

          testTheory "Successfully removes a wishlist item" [ player ]
          <| fun player ->
              let updatedPlauer = Player.removeItem defaultItem player
              Expect.isEmpty updatedPlauer.wishlist "Wishlist item should have been removed"

          testTheory "Successfully removes fist occurrance of a wishlist item" [ player ]
          <| fun player ->
              let updatedPlauer =
                  player |> Player.addItem defaultItem |> Player.removeItem defaultItem

              Expect.contains updatedPlauer.wishlist defaultItem "Wishlist item should have been removed"

          testCase "addItems appends the provided items in order"
          <| fun () ->
              let updatedPlayer =
                  Player.create ("list", [])
                  |> Player.addItem defaultItem
                  |> Player.addItems [ secondItem; thirdItem ]

              Expect.equal
                  updatedPlayer.wishlist
                  [ defaultItem; secondItem; thirdItem ]
                  "addItems should append items in their supplied order"

          testCase "removeItems removes each matched entry sequentially"
          <| fun () ->
              let populatedPlayer =
                  Player.create ("sequence", [])
                  |> Player.addItem defaultItem
                  |> Player.addItem secondItem
                  |> Player.addItem thirdItem
                  |> Player.addItem secondItem

              let updatedPlayer = Player.removeItems [ secondItem; thirdItem ] populatedPlayer
              Expect.equal updatedPlayer.wishlist [ defaultItem; secondItem ]
                  "removeItems should remove each requested item in sequence"

          testCase "Adding multiple wishlist items maintains the addition order"
          <| fun () ->
              let updatedPlayer =
                  Player.create ("another", [])
                  |> Player.addItem defaultItem
                  |> Player.addItem secondItem

              Expect.equal updatedPlayer.wishlist [ defaultItem; secondItem ] "Items should be appended"

          testCase "Removing a non-existent wishlist item leaves the list unchanged"
          <| fun () ->
              let playerWithSingleItem = Player.create ("solo", []) |> Player.addItem defaultItem
              let updatedPlayer = Player.removeItem secondItem playerWithSingleItem
              Expect.equal updatedPlayer.wishlist playerWithSingleItem.wishlist "Wishlist should remain untouched"

          testCase "Creating a player deduplicates initial tags"
          <| fun () ->
              let duplicatedTags = Player.create ("tagged", [ firstTag; firstTag; secondTag ])

              Expect.equal duplicatedTags.tags (Set.ofList [ firstTag; secondTag ]) "Initial tags should be unique"

          testCase "Successfully adds a tag"
          <| fun () ->
              let updatedPlayer = Player.addTag firstTag player
              Expect.contains updatedPlayer.tags firstTag "Player should gain the new tag"

          testCase "Adding the same tag twice keeps tags unique"
          <| fun () ->
              let updatedPlayer = player |> Player.addTag firstTag |> Player.addTag firstTag

              Expect.equal (Set.count updatedPlayer.tags) 1 "Tags should stay unique when added multiple times"

          testCase "Adding a tag that already exists keeps the set unchanged"
          <| fun () ->
              let updatedPlayer = playerWithTags |> Player.addTag firstTag
              Expect.equal updatedPlayer.tags playerWithTags.tags "Adding an existing tag should not mutate the set"

          testCase "Removing a tag keeps the other tags intact"
          <| fun () ->
              let updatedPlayer = Player.removeTag firstTag playerWithTags
              Expect.isFalse (updatedPlayer.tags |> Set.contains firstTag) "Tagged entry should be removed"
              Expect.contains updatedPlayer.tags secondTag "Other tags should remain untouched"

          testCase "Removing a missing tag does nothing"
          <| fun () ->
              let updatedPlayer = Player.removeTag firstTag player
              Expect.isEmpty updatedPlayer.tags "Removing a tag that was not there should stay empty" ]
