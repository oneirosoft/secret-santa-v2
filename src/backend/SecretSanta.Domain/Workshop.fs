namespace SecretSanta

type Workshop =
    internal
        { id: Pneumonic
          name: string
          players: Player list
          dollarLimit: float32
          pairs: (string * string) list }

module Workshop =

    let create (id: Pneumonic, name: string, dollarLimit: float32) =
        { id = id
          name = name
          dollarLimit = dollarLimit
          players = []
          pairs = [] }

    let createWithGeneratedId (name: string, dollarLimit: float32) =
        let pneumonic = Pneumonic.create 7uy
        create (pneumonic, name, dollarLimit)

    let addPlayer player workshop =
        { workshop with
            players = player :: workshop.players |> List.rev }

    let removePlayer player workshop =
        let rec loop acc =
            function
            | [] -> List.rev acc
            | x :: xs when x = player -> List.rev acc @ xs
            | x :: xs -> loop (x :: acc) xs

        { workshop with
            players = loop [] workshop.players }
