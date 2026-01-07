open Grpc_unix
open Xapi_grpc.Xapi
open Ocaml_protoc_plugin

let debug (fmt : ('a, unit, string, 'b) format4) =
  Printf.ksprintf
    (fun s -> Printf.printf "%d: %s\n%!" Thread.(self () |> id) s)
    fmt

module Network = struct
  let create (buffer : string) =
    debug "network.create" ;
    let decode, encode = Service.make_service_functions Network_class.create in
    (* Decode the request. *)
    let network_record =
      Reader.create buffer |> decode |> function
      | Ok v -> v
      | Error e ->
          failwith
            (Printf.sprintf "Could not decode request: %s" (Result.show_error e))
    in
    debug "network.create: received {uuid=%s}" network_record.Network.uuid;
    let r = "OpaqueRef:xyz-xyz" in
    debug "network.create: created network %s" r;
    (Grpc.Status.(v OK), Some (r |> encode |> Writer.contents))
end

let xapi_network_service () =
  Server.Service.(
    v () |> add_rpc ~name:"create" ~rpc:(Unary Network.create) |> handle_request)

let server () =
  Server.(
    v ()
    |> add_service ~name:"network" ~service:(xapi_network_service ()))

let connection_handler grpc_server =
  let error_handler _client_socket ?request:_ _error start_response =
    debug "Error in request from" ;
    let response_body = start_response H2.Headers.empty in
    H2.Body.Writer.write_string response_body
      "There was an error handling your request.";
    H2.Body.Writer.close response_body
  in
  let request_handler _client_address _proxy {Gluten.reqd; _ } =
    let { H2.Request.meth; target; _ } = H2.Reqd.request reqd in
    debug "You made a %s request to the following resource: %s" (H2.Method.to_string meth) target ;
    let _ = Thread.create (fun () -> Grpc_unix.Server.handle_request grpc_server reqd) () in
    ()
  in
  fun addr socket ->
    H2_unix.Server.create_connection_handler ~request_handler
      ~error_handler addr socket

let main port =
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
  let domain = Unix.domain_of_sockaddr sockaddr in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
  let grpc_server = server () in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sockaddr;
  Unix.listen sock 5;
  while true do
    let s, caller = Unix.accept ~cloexec:true sock in
    debug "Accepted connection";
    connection_handler grpc_server caller s;
    debug "Dispatched connection handler"
  done

let () =
  let port = ref 8080 in
  Arg.parse
    [ ("-p", Arg.Set_int port, " Listening port number (8080 by default)") ]
    ignore "gRPC server";
  main !port
