module SecretSanta.MatchMaker

let private filterForEligibleVictims giver alreadyChosen =
    let receiverIsNotTheGiver reciever = reciever.nickname <> giver.nickname

    let thereAreNoTagConflicts receiver =
        Set.intersect receiver.tags giver.tags |> Set.isEmpty

    let receiverHasNotAlreadyBeenChosen receiver =
        Set.contains receiver.nickname alreadyChosen |> not

    List.where receiverIsNotTheGiver
    >> List.where thereAreNoTagConflicts
    >> List.where receiverHasNotAlreadyBeenChosen

let private countUniqueTags =
    let collectPlayTags = List.map _.tags >> List.collect Set.toList

    let createTallyMap =
        List.fold
            (fun tagsMap tag ->
                let currentCount = tagsMap |> Map.tryFind tag |> Option.defaultValue 0
                Map.add tag (currentCount + 1) tagsMap)
            Map.empty

    let gatTallyCounts = Map.values

    collectPlayTags >> createTallyMap >> gatTallyCounts

let private produceUniquePairs =

    let rec produceUniquePairsLoop currentPairs i (playerPool: Player list) =
        let updatePairs chosenVictims =
            let giver = playerPool |> List.item i
            let eligibleVictims = filterForEligibleVictims giver chosenVictims playerPool
            let thereAreNoEligibleVictims = eligibleVictims |> List.isEmpty

            if thereAreNoEligibleVictims then
                currentPairs
            else
                let victim = List.randomChoice eligibleVictims
                let updatedPairs = currentPairs |> Set.add (giver.nickname, victim.nickname)
                produceUniquePairsLoop updatedPairs (i + 1) playerPool

        let chosenVictims = Set.map snd currentPairs
        let allPlayesHaveBeenChosen = chosenVictims.Count = playerPool.Length

        if allPlayesHaveBeenChosen then
            currentPairs
        else
            updatePairs chosenVictims

    produceUniquePairsLoop Set.empty 0

let rec private startPairing players =
    let pairs = players |> List.randomShuffle |> produceUniquePairs
    let pairAndPlayerCountsAlign = players.Length = pairs.Count

    if pairAndPlayerCountsAlign then
        pairs
    else
        startPairing players

let pair (players: Player list) : PlayerPairs =

    let hasTagMajorityConflict =
        players
        |> countUniqueTags
        |> Seq.exists (fun tagCount ->
            let playersWithoutTags = players.Length - tagCount
            playersWithoutTags < tagCount)

    if hasTagMajorityConflict then
        Set.empty
    else
        match players with
        | []
        | [ _ ] -> Set.empty
        | players -> startPairing players
