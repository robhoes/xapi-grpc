open Grpc_eio
open Xapi_grpc.Xapi
open Ocaml_protoc_plugin

module Network = struct
  let create (buffer : string) =
    let decode, encode = Service.make_service_functions Network_class.create in
    (* Decode the request. *)
    let network_record =
      Reader.create buffer |> decode |> function
      | Ok v -> v
      | Error e ->
          failwith
            (Printf.sprintf "Could not decode request: %s" (Result.show_error e))
    in
    Eio.traceln "Network.create: received {uuid=%s}" network_record.Network.uuid;
    let r = "OpaqueRef:xyz-xyz" in
    Eio.traceln "Network.create: created network %s" r;
    (Grpc.Status.(v OK), Some (r |> encode |> Writer.contents))
end

let xapi_network_service () =
  Server.Service.(
    v () |> add_rpc ~name:"Create" ~rpc:(Unary Network.create) |> handle_request)

let server () =
  Server.(
    v ()
    |> add_service ~name:"xapi.Network_class" ~service:(xapi_network_service ()))

(* $MDX part-end *)
let connection_handler server ~sw =
  let error_handler client_address ?request:_ _error start_response =
    Eio.traceln "Error in request from:%a" Eio.Net.Sockaddr.pp client_address;
    let response_body = start_response H2.Headers.empty in
    H2.Body.Writer.write_string response_body
      "There was an error handling your request.\n";
    H2.Body.Writer.close response_body
  in
  let request_handler _client_address request_descriptor =
    Eio.Fiber.fork ~sw (fun () ->
        Grpc_eio.Server.handle_request server request_descriptor)
  in
  fun socket addr ->
    H2_eio.Server.create_connection_handler ?config:None ~request_handler
      ~error_handler addr socket ~sw

(* $MDX part-begin=server-main *)
let serve server env =
  let port = 8080 in
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  Eio.Switch.run @@ fun sw ->
  let handler = connection_handler ~sw (server ()) in
  let server_socket =
    Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:10 addr
  in
  let rec listen () =
    Eio.Net.accept_fork ~sw server_socket
      ~on_error:(fun exn -> Eio.traceln "%s" (Printexc.to_string exn))
      handler;
    listen ()
  in
  Eio.traceln "Listening on port %i for grpc requests\n" port;
  listen ()

let () = Eio_main.run (serve server)
