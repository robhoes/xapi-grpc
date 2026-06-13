module ServiceMap = Map.Make (String)

let debug (fmt : ('a, unit, string, 'b) format4) =
  Printf.ksprintf
    (fun s -> Printf.printf "%d: %s\n%!" Thread.(self () |> id) s)
    fmt

type service = H2.Reqd.t -> unit
type t = service ServiceMap.t

let v () = ServiceMap.empty
let add_service ~name ~service t = ServiceMap.add name service t

let handle_request t reqd =
  let request = H2.Reqd.request reqd in
  let respond_with code =
    H2.Reqd.respond_with_string reqd (H2.Response.create code) ""
  in
  let route () =
    let parts = String.split_on_char '/' request.target in
    if List.length parts > 1 then
      (* allow for arbitrary prefixes *)
      let service_name = List.nth parts (List.length parts - 2) in
      let service = ServiceMap.find_opt service_name t in
      match service with
      | Some service -> service reqd
      | None -> respond_with `Not_found
    else respond_with `Not_found
  in
  match request.meth with
  | `POST -> (
      match H2.Headers.get request.headers "content-type" with
      | Some s ->
          if
            String.starts_with s ~prefix:"application/grpc"
          then
            match H2.Headers.get request.headers "grpc-encoding" with
            | None | Some "identity" -> (
                match H2.Headers.get request.headers "grpc-accept-encoding" with
                | None -> route ()
                | Some encodings ->
                    let encodings = String.split_on_char ',' encodings in
                    if List.mem "identity" encodings then route ()
                    else respond_with `Not_acceptable)
            | Some _ ->
                (* TODO: not sure if there is a specific way to handle this in grpc *)
                respond_with `Bad_request
          else respond_with `Unsupported_media_type
      | None -> respond_with `Unsupported_media_type)
  | _ -> respond_with `Not_found

module Rpc = struct
  type unary = string -> Grpc.Status.t * string option
  type client_streaming = string Seq.t -> Grpc.Status.t * string option
  type server_streaming = string -> (string -> unit) -> Grpc.Status.t

  type bidirectional_streaming =
    string Seq.t -> (string -> unit) -> Grpc.Status.t

  type t =
    | Unary of unary
    | Client_streaming of client_streaming
    | Server_streaming of server_streaming
    | Bidirectional_streaming of bidirectional_streaming

  let bidirectional_streaming ~f reqd =
    debug "Rpc.bidirectional_streaming" ;
    let body = H2.Reqd.request_body reqd in
    let request_reader, request_writer = Seq.create_reader_writer () in
    let response_reader, response_writer = Seq.create_reader_writer () in
    Connection.grpc_recv_streaming body request_writer;
    let status_ch = Event.new_channel () in
    let th = Thread.create
      (fun () ->
        debug "Enter thread" ;
        let respond = Seq.write response_writer in
        debug "Seq.write done. Calling f" ;
        let status = f request_reader respond in
        Seq.close_writer response_writer;
        Event.(send status_ch status |> sync)) ()
    in
    (try Connection.grpc_send_streaming reqd response_reader status_ch
    with exn ->
      (* https://github.com/anmonteiro/ocaml-h2/issues/175 *)
      debug "%s" (Printexc.to_string exn)) ;
    Thread.join th

  let client_streaming ~f reqd =
    bidirectional_streaming reqd ~f:(fun requests respond ->
        let status, response = f requests in
        (match response with None -> () | Some response -> respond response);
        status)

  let server_streaming ~f reqd =
    bidirectional_streaming reqd ~f:(fun requests respond ->
        match Seq.read_and_exhaust requests with
        | None -> Grpc.Status.(v OK)
        | Some request -> f request respond)

  let unary ~f reqd =
    debug "unary" ;
    bidirectional_streaming reqd ~f:(fun requests respond ->
        debug "unary: call Seq.read_and_exhaust" ;
        match Seq.read_and_exhaust requests with
        | None -> Grpc.Status.(v OK)
        | Some request ->
            debug "unary: call f" ;
            let status, response = f request in
            (match response with
            | None -> ()
            | Some response -> respond response);
            status)
end

module Service = struct
  module RpcMap = Map.Make (String)

  type t = Rpc.t RpcMap.t

  let v () = RpcMap.empty
  let add_rpc ~name ~rpc t = RpcMap.add name rpc t

  let handle_request (t : t) reqd =
    let request = H2.Reqd.request reqd in
    let respond_with code =
      H2.Reqd.respond_with_string reqd (H2.Response.create code) ""
    in
    let parts = String.split_on_char '/' request.target in
    if List.length parts > 1 then
      let rpc_name = List.nth parts (List.length parts - 1) in
      let rpc = RpcMap.find_opt rpc_name t in
      match rpc with
      | Some rpc -> (
          match rpc with
          | Unary f -> Rpc.unary ~f reqd
          | Client_streaming f -> Rpc.client_streaming ~f reqd
          | Server_streaming f -> Rpc.server_streaming ~f reqd
          | Bidirectional_streaming f -> Rpc.bidirectional_streaming ~f reqd)
      | None -> respond_with `Not_found
    else respond_with `Not_found
end
