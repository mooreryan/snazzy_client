open! Core
open! Async

let get_redirect_uri response =
  Cohttp.(Response.headers response |> Header.get_location)

(* [max_redirs] is set to 50 by default to match curl *)
let get ?headers ?(timeout = 10.0) ?(max_redirects = 50) uri =
  let rec loop ~uri ~max_redirects =
    let%bind.Deferred.Or_error ((response, body) as res) =
      Deferred.Or_error.try_with (fun () ->
          Cohttp_async.Client.get ?headers uri ~interrupt:(after (sec timeout)))
    in
    let code = Cohttp.Code.code_of_status @@ Cohttp.Response.status response in
    if Cohttp.Code.is_redirection code then
      (* Drain response body so we don't leak connections. *)
      let%bind _ = Cohttp_async.Body.drain body in
      match get_redirect_uri response with
      | None ->
          Deferred.Or_error.errorf "No redirect URI for '%s'"
            (Uri.to_string uri)
      | Some new_uri ->
          if max_redirects > 0 then
            loop ~max_redirects:(max_redirects - 1) ~uri:new_uri
          else
            Deferred.Or_error.errorf "Too many redirects for URI '%s'"
              (Uri.to_string uri)
    else Deferred.Or_error.return res
  in
  loop ~uri ~max_redirects

let soup (response, body) =
  match Cohttp.Response.status response with
  | `OK ->
      let%bind body_text = Cohttp_async.Body.to_string body in
      let stream = Markup.string body_text in
      stream |> Markup.parse_html |> Markup.signals |> Soup.from_signals
      |> Deferred.Or_error.return
  | status ->
      let%bind _ = Cohttp_async.Body.drain body in
      Deferred.Or_error.errorf "Non-Ok response '%s'"
        (Cohttp.Code.string_of_status status)

let get_soup ?headers ?(timeout = 10.0) ~max_redirects uri =
  let%bind.Deferred.Or_error res = get ?headers ~timeout ~max_redirects uri in
  soup res

let download_to_file body ~file_name =
  let pipe = Cohttp_async.Body.to_pipe body in
  Writer.with_file file_name ~perm:0o664 ~f:(fun writer ->
      Pipe.iter pipe ~f:(fun content -> return @@ Writer.write writer content))
