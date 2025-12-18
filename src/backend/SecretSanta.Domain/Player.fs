namespace SecretSanta

type Player =
    internal
        { nickname: string
          tags: string Set
          wishlist: WishlistItem list }

module Player =
    let create (nickname: string, tags: string list) =
        { nickname = nickname
          tags = tags |> Set.ofList
          wishlist = [] }

    let addItem item player =
        { player with
            wishlist = item :: player.wishlist |> List.rev }

    let addItems items player =
        { player with
            wishlist = player.wishlist @ items }

    let removeItem item player =
        let rec loop acc =
            function
            | [] -> List.rev acc
            | x :: xs when x = item -> List.rev acc @ xs
            | x :: xs -> loop (x :: acc) xs

        { player with
            wishlist = loop [] player.wishlist }

    let removeItems items player =
        Seq.fold (fun player item -> removeItem item player) player items

    let addTag tag player =
        { player with
            tags = player.tags |> Set.add tag }

    let removeTag tag player =
        { player with
            tags = player.tags |> Set.remove tag }
