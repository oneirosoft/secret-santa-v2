namespace SecretSanta

type private Uri = System.Uri
type private UriKind = System.UriKind

type WishlistItem = { title: string; url: Uri }

module WishlistItem =
    let create (title, value) =
        let isHttpOrHttps (uri: Uri) =
            Set.ofList [ Uri.UriSchemeHttp; Uri.UriSchemeHttps ] |> Set.contains uri.Scheme

        match Uri.TryCreate(value, UriKind.Absolute) with
        | true, uri when isHttpOrHttps uri -> { title = title; url = uri } |> Some
        | _ -> None
