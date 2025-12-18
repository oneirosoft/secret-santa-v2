namespace SecretSanta

type PlayerPairs = (string * string) Set

type Workshop =
    internal
        { id: Pneumonic
          name: string
          players: Player list
          dollarLimit: float32
          pairs: PlayerPairs }

module Workshop =

    let create (id: Pneumonic, name: string, dollarLimit: float32) =
        { id = id
          name = name
          dollarLimit = dollarLimit
          players = []
          pairs = Set.empty }

    let createWithGeneratedId (name: string, dollarLimit: float32) =
        let pneumonic = Pneumonic.create 7uy
        create (pneumonic, name, dollarLimit)

    let addPlayer player workshop =
        let playerExists =
            workshop.players |> List.exists (fun p -> p.nickname = player.nickname)

        if playerExists then
            workshop
        else
            { workshop with
                players = workshop.players @ [ player ] }

    let removePlayer player workshop =
        let rec loop acc =
            function
            | [] -> List.rev acc
            | x :: xs when x = player -> List.rev acc @ xs
            | x :: xs -> loop (x :: acc) xs

        { workshop with
            players = loop [] workshop.players }

    let updatePlayerWishlist nickname wishlistItems workshop =
        let updateIfMatch player =
            if player.nickname = nickname then
                Player.addItems wishlistItems player
            else
                player

        { workshop with
            players = workshop.players |> List.map updateIfMatch }
