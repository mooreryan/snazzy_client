open! Core

let get uri =
  match Curly.get @@ Uri.to_string uri with
  | Ok resp ->
      if resp.code = 200 then Or_error.return resp.body
      else
        Or_error.error_string [%string "Non-OK response code: %{resp.code#Int}"]
  | Error err ->
      Or_error.error_string
      @@ Format.asprintf "Curly GET error: %a" Curly.Error.pp err

let get_soup uri =
  let%map.Or_error body = get uri in
  body |> Markup.string |> Markup.parse_xml |> Markup.signals
  |> Soup.from_signals
