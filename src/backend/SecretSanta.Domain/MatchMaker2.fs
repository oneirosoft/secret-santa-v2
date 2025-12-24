module SecretSanta.MatchMaker2

open System

type Player = { name: string; tag: string }

let private interleaveNames =
    let rec interleave xs ys =
        match xs, ys with
        | x :: xs, y :: ys -> x :: y :: interleave xs ys
        | xs, [] -> xs
        | [], ys -> ys

    let rec groupLoop acc =
        function
        | group1 :: group2 :: remainingGroups ->
            let pairs = interleave group1 group2

            groupLoop (acc @ pairs) remainingGroups
        | [ lastGroup ] ->
            match lastGroup with
            | head :: remainingPlayers -> (acc |> List.insertAt (acc.Length - 1) head) @ remainingPlayers
            | _ -> acc
        | _ -> acc

    groupLoop []

let private populateEmptyTags =
    List.map (fun p ->
        match p.tag with
        | "" ->
            { p with
                tag = Guid.NewGuid() |> string }
        | _ -> p)

let private groupByTag = List.groupBy _.tag

let private shuffleGroups = List.randomShuffle

let private shufflePlayesInGroups =
    List.map (fun (key, players) -> key, List.randomShuffle players)

let private ungroupPlayers = List.map snd

let private extractNames = List.map _.name |> List.map

let private pairGiversWithReceivers lineup =
    let head, last = List.head lineup, List.last lineup
    List.pairwise lineup @ [ (last, head) ]

let private haveTagsConflict players =
    let countTags =
        List.map _.tag
        >> List.fold
            (fun tagCounts tag ->
                let currentCount = tagCounts |> Map.tryFind tag |> Option.defaultValue 0
                tagCounts |> Map.add tag (currentCount + 1))
            Map.empty
        >> Map.values

    players |> countTags |> Seq.exists ((<) players.Length)

let pair players =
    if players |> haveTagsConflict then
        Set.empty
    else
        players
        |> populateEmptyTags
        |> groupByTag
        |> shuffleGroups
        |> shufflePlayesInGroups
        |> ungroupPlayers
        |> extractNames
        |> interleaveNames
        |> pairGiversWithReceivers
        |> Set.ofList
