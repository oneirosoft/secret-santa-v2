namespace SecretSanta

type Pneumonic =
    internal
    | Pneumonic of string

    override this.ToString() =
        match this with
        | Pneumonic value -> value

module Pneumonic =

    open System.Text.RegularExpressions

    let private winterWords: string[] =
        [| "winter"
           "snow"
           "snowfall"
           "snowflake"
           "snowstorm"
           "snowman"
           "snowbound"
           "blizzard"
           "frost"
           "frostbite"
           "frosty"
           "icy"
           "ice"
           "icicle"
           "sleet"
           "hail"
           "chill"
           "cold"
           "freezing"
           "glacial"
           "arctic"
           "polar"
           "solstice"
           "evergreen"
           "pine"
           "fir"
           "spruce"
           "holly"
           "mistletoe"
           "wreath"
           "garland"
           "ornament"
           "tinsel"
           "stocking"
           "chimney"
           "hearth"
           "fireplace"
           "sleigh"
           "sled"
           "sledding"
           "reindeer"
           "elf"
           "elves"
           "santa"
           "kringle"
           "northpole"
           "workshop"
           "present"
           "gift"
           "wrapping"
           "festive"
           "holiday"
           "carol"
           "caroling"
           "bells"
           "jingle"
           "yuletide"
           "noel"
           "nativity"
           "manger"
           "candle"
           "lantern"
           "skating"
           "snowball"
           "wintertime"
           "cozy"
           "cocoa"
           "gingerbread"
           "peppermint"
           "candycane"
           "chestnuts"
           "nutcracker"
           "pinecone"
           "skiing"
           "snowboard"
           "parka"
           "scarf"
           "mittens"
           "gloves"
           "boots"
           "firewood"
           "eggnog"
           "cranberry"
           "lights"
           "twinkle"
           "frosted"
           "wonderland"
           "midwinter"
           "sugarplum"
           "yulelog"
           "treetop"
           "seasonal"
           "cookies"
           "cracker"
           "stockings"
           "holidays"
           "chimneys"
           "sleds" |]

    let rec create =
        function
        | 0uy -> create 1uy
        | n -> winterWords |> Seq.randomChoices (int n) |> String.concat "-" |> Pneumonic

    let isValid (value: string) =
        (value, @"^([a-z]-?)+[a-z]$") |> Regex.IsMatch

    let from (value: string) =
        let value = value.ToLowerInvariant()
        if value |> isValid then Some <| Pneumonic value else None
