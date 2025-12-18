module PneumonicTests

open Expecto
open SecretSanta
open System.Text.RegularExpressions

let validPneumonics =
    [ "jingle"
      "jingle-all"
      "jingle-all-the-way"
      "snowman"
      "holly-jolly"
      "frostbite-winter" ]

let invalidePneumonics =
    [ "jingle-"; "-jingle"; "-jingle1"; "jingle-all-the-1"; ""; "jingle--bells" ]

let mixedCasePneumonics =
    [ "Jingle", "jingle"
      "Jingle-All", "jingle-all"
      "SnowMan", "snowman"
      "FrostBite-Winter", "frostbite-winter"
      "SANTA-KRINGLE", "santa-kringle" ]

[<Tests>]
let pneumonicTests =
    testList
        "Pneumonic"
        [ testTheory "Valid Pneumonics" validPneumonics
          <| fun p -> (Pneumonic.isValid p, $"{p} should be a valid pneumonic") ||> Expect.isTrue

          testTheory "Invalid Pnemonics" invalidePneumonics
          <| fun p -> (Pneumonic.isValid p, $"{p} should not be a valid pneumonic") ||> Expect.isFalse

          testCase "0uy creates a single word pneumonic"
          <| fun () ->
              let pneumonic = 0uy |> Pneumonic.create |> string
              let isValid = Regex.IsMatch(pneumonic, "^[a-z]+$")
              (isValid, $"{pneumonic} should be valid") ||> Expect.isTrue

          testCase "2uy creates a pneumonic with two words"
          <| fun () ->
              let pneumonic = 2uy |> Pneumonic.create |> string
              let words = pneumonic.Split '-'
              Expect.hasLength words 2 $"{pneumonic} should be two words"

          testTheory "Pneumonic.from returns Some for valid strings" validPneumonics
          <| fun p -> Expect.isSome (Pneumonic.from p) $"{p} should be accepted"

          testTheory "Pneumonic.from normalizes casing" mixedCasePneumonics
          <| fun (input, expected) ->
              Expect.equal
                  (Pneumonic.from input |> Option.map string)
                  (Some expected)
                  $"{input} should normalize to {expected}"

          testTheory "Pneumonic.from returns None for invalid strings" invalidePneumonics
          <| fun p -> Expect.isNone (Pneumonic.from p) $"{p} should be rejected" ]
