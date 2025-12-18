module SecretSanta.MatchMaker

let pairUp (players: Player list) : PlayerPairs =

    let pair (players: Player list) =
        let playerTagCount =
            players
            |> Seq.fold
                (fun acc player ->
                    player.tags
                    |> Seq.fold
                        (fun acc tag ->
                            let currentCount = acc |> Map.tryFind tag |> Option.defaultValue 0
                            acc |> Map.add tag (currentCount + 1))
                        acc)
                Map.empty

        let tooManyWithSameTag =
            playerTagCount
            |> Map.values
            |> Seq.exists (fun tagCount ->
                let playersWithoutTags = players.Length - tagCount
                playersWithoutTags < tagCount)

        let rec pair (pairs: PlayerPairs) (alreadyPicked: string Set) (i: int) (players: Player list) =
            if alreadyPicked |> Set.count = players.Length then
                pairs
            else
                let usedReceivers = pairs |> Set.map snd
                let giver = players |> List.item i

                let eligible =
                    players
                    |> List.filter (fun receiver -> receiver.nickname <> giver.nickname)
                    |> List.filter (fun receiver -> Set.intersect giver.tags receiver.tags |> Set.isEmpty)
                    |> List.filter (fun receiver -> usedReceivers |> Set.contains receiver.nickname |> not)
                    |> List.filter (fun receiver -> alreadyPicked |> Set.contains receiver.nickname |> not)

                if eligible |> List.isEmpty then
                    pairs
                else
                    let victim = eligible |> Seq.randomChoice
                    let updatedPairs = Set.add (giver.nickname, victim.nickname) pairs
                    pair updatedPairs (Set.add victim.nickname alreadyPicked) (i + 1) players

        let rec keepPairing players =
            let pairs = pair Set.empty Set.empty 0 (players |> List.randomShuffle)

            if pairs.Count = players.Length then
                pairs
            else
                keepPairing players

        if tooManyWithSameTag then
            Set.empty
        else
            keepPairing players

    match players with
    | []
    | [ _ ] -> Set.empty
    | players -> pair players
