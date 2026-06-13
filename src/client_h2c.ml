open Grpc_eio
open Xapi_grpc.Xenapi
open Ocaml_protoc_plugin

let debug (fmt : ('a, unit, string, 'b) format4) =
  Printf.ksprintf
    (fun s -> Printf.printf "%s\n%!" s)
    fmt

let client ~sw host port network =
  let inet, port =
    Eio_unix.run_in_systhread (fun () ->
        Unix.getaddrinfo host port [ Unix.(AI_FAMILY PF_INET) ])
    |> List.filter_map (fun (addr : Unix.addr_info) ->
           match addr.ai_addr with
           | Unix.ADDR_UNIX _ -> None
           | ADDR_INET (addr, port) -> Some (addr, port))
    |> List.hd
  in
  let addr = `Tcp (Eio_unix.Net.Ipaddr.of_unix inet, port) in
  let socket = Eio.Net.connect ~sw network addr in
  Httpun_eio.Client.create_connection ~sw
    ~config:{Httpun.Config.default with read_buffer_size=0x4000}
    socket

let login connection uname pwd version originator =
  debug "login" ;
  let request = Session.Login_with_password.Request.make ~uname ~pwd ~version ~originator () in
  let encode, decode = Service.make_client_functions Session.login_with_password in
  Client.call ~service:"grpc/session" ~rpc:"login_with_password"
    ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
    ~handler:
      (Client.Rpc.unary
         (encode request |> Writer.contents)
         ~f:(fun response ->
           match response with
           | Some response -> (
               Reader.create response |> decode |> function
               | Ok result -> result
               | Error e ->
                   failwith
                     (Printf.sprintf "Could not decode request: %s"
                        (Result.show_error e)))
           | None -> "no response"))
    ()

let host_get_all connection session_id =
  debug "host.get_all" ;
  let request = Host.Get_all.Request.make ~session_id () in
  let encode, decode = Service.make_client_functions Host.get_all in
  Client.call ~service:"grpc/host" ~rpc:"get_all"
    ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
    ~handler:
      (Client.Rpc.unary
         (encode request |> Writer.contents)
         ~f:(fun response ->
           match response with
           | Some response -> (
               Reader.create response |> decode |> function
               | Ok result -> result
               | Error e ->
                   failwith
                     (Printf.sprintf "Could not decode request: %s"
                        (Result.show_error e)))
           | None -> []))
    ()

let event_stream connection session_id classes =
  debug "event.stream" ;
  let request = Event.Stream.Request.make ~session_id ~classes () in
  let encode, decode = Service.make_client_functions Event.stream in
  let stream =
    Client.call ~service:"grpc/event" ~rpc:"stream"
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.server_streaming
           (encode request |> Writer.contents)
           ~f:(fun responses ->
               let stream =
                 Seq.map (fun response ->
                   Reader.create response |> decode |> function
                   | Ok result ->
                       result
                   | Error e ->
                       failwith
                         (Printf.sprintf "Could not decode request: %s"
                            (Result.show_error e)))
                   responses
               in
               stream))
      ()
  in
  match stream with
  | Ok (results, _ok) ->
      Seq.iter (fun event ->
        let open Event_record in
        debug "event: id=%s, ty=%s, op=%s, ref=%s"
          event.id event.ty event.op event.reference ;
        match event.snapshot with
        | `Vm vm ->
            debug "       %s \"%s\" %s" vm.Vm_record.uuid vm.Vm_record.name_label vm.Vm_record.power_state
        | `Host host ->
            debug "       %s \"%s\"" host.Host_record.uuid host.Host_record.name_label
        | _ -> ()
      ) results
  | Error _ ->
      debug "an error occurred"

let call_network_create connection network_record =
  debug "call_network_create" ;
  let encode, decode = Service.make_client_functions Network_class.create in
  let response =
    Client.call ~service:"grpc/network" ~rpc:"create"
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.unary
           (encode network_record |> Writer.contents)
           ~f:(fun response ->
             match response with
             | Some response -> (
                 Reader.create response |> decode |> function
                 | Ok network_ref -> network_ref
                 | Error e ->
                     failwith
                       (Printf.sprintf "Could not decode request: %s"
                          (Result.show_error e)))
             | None -> "no response"))
      ()
  in
  match response with
  | Ok (res, _ok) -> Printf.printf "RESPONSE = {%s}\n" res
  | Error _ -> Printf.printf "an error occurred"

let h2_settings = H2.Config.(to_settings default) |> H2.Settings.to_base64


let h2_response_handler ~on_eof response response_body =
  let open H2 in
  debug "Got HTTP/2 response" ;
  match response with
  | { Response.status = `OK; _ } ->
    debug "Status 200 OK" ;
    let rec on_read bs ~off ~len =
      debug "on_read: %d" (len - off);
      debug "on_read: %s" (Bigstringaf.substring ~off ~len bs);
      H2.Body.Reader.schedule_read response_body ~on_read ~on_eof
    in
    H2.Body.Reader.schedule_read response_body ~on_read ~on_eof
  | response ->
    Format.fprintf Format.err_formatter "%a\n\n%!" Response.pp_hum response ;
    on_eof ()

let h2_conn_error_handler err =
  let err =
    match err with
    | `Malformed_response err -> Format.sprintf "Malformed response: %s" err
    | `Invalid_response_body_length _ -> "Invalid body length"
    | `Exn exn -> Format.sprintf "Exn raised: %s" (Printexc.to_string exn)
    | `Protocol_error (c, s) -> Printf.sprintf "Protocol error: %s %s" (H2.Error_code.to_string c) s
  in
  debug "Error handling HTTP/2 connection: %s" err

let h2_error_handler err =
  let err =
    match err with
    | `Malformed_response err -> Format.sprintf "Malformed response: %s" err
    | `Invalid_response_body_length _ -> "Invalid body length"
    | `Exn exn -> Format.sprintf "Exn raised: %s" (Printexc.to_string exn)
    | `Protocol_error (c, s) -> Printf.sprintf "Protocol error: %s %s" (H2.Error_code.to_string c) s
  in
  debug "Error handling HTTP/2 response: %s"  err

let upgrade_hander ~on_eof upgraded_connection runtime http_request =
  let { Httpun.Request.headers; meth; target; _ } = http_request in
  let connection = H2.Client_connection.create_h2c ~headers ~target ~meth
    ~error_handler:h2_conn_error_handler
    (h2_response_handler ~on_eof, h2_error_handler) in
  let result = Stdlib.Result.map
    (fun connection ->
       (* Perform the runtime upgrade -- stop speaking HTTP/1.1, start
        * speaking HTTP/2 by feeding Gluten the `H2.Client_connection`
        * protocol. *)
       Gluten_eio.Client.upgrade
         runtime
         (Gluten.make (module H2.Client_connection) connection);
       { H2_eio.Client.connection; runtime })
    connection
  in
  (match result with
  | Ok connection ->
    debug "Connection state changed (HTTP/2 confirmed)";
    upgraded_connection := Some connection
  | _e ->
    debug "Failed to upgrade connection to HTTP/2";
    ()
  )


let handler ~on_eof upgraded_connection runtime http_request response _response_body =
  let open Httpun in
  debug "Got HTTP/1.1 response" ;
  match response with
  | { Response.status = `OK; _ } ->
    debug "Status 200 OK" ;
  | { Response.status = `Switching_protocols; _ } ->
    debug "Status 101 Switching Protocols" ;
    upgrade_hander ~on_eof upgraded_connection runtime http_request
  | response ->
    Format.fprintf Format.err_formatter "%a\n\n%!" Response.pp_hum response ;
    on_eof ()

let error_handler error =
  let error =
    match error with
    | `Malformed_response err -> Format.sprintf "Malformed response: %s" err
    | `Invalid_response_body_length _ -> "Invalid body length"
    | `Exn exn -> Format.sprintf "Exn raised: %s" (Printexc.to_string exn)
  in
  Printf.printf "Error handling response: %s\n\n%!" error

let login_call = "<?xml version='1.0'?><methodCall><methodName>session.login_with_password</methodName><params><param><value><string>root</string></value></param><param><value><string>rQcIp9BDl4iy</string></value></param></params></methodCall>"

let (let*) = Stdlib.Result.bind

let main env =
  let port = "80" in
  let host = "genus-34-25d.xenrt.citrite.net" in
  let network = Eio.Stdenv.net env in
  let () = Random.self_init () in

  let run sw =
    let connection = client ~sw host port network in
    
    let {Httpun_eio.Client.runtime; _} = connection in
    let upgraded_connection = ref None in

    let body = login_call in
    let headers =
        [ "Connection", "Upgrade, HTTP2-Settings"
        ; "Upgrade", "h2c"
        ; ( "HTTP2-Settings", Stdlib.Result.get_ok h2_settings)
        ; "host", host
        ; "content-length", Int.to_string (String.length body)
        ]
    in
    debug "doing HTTP/1.1 request" ;
    let request = Httpun.(Request.create ~headers:(Headers.of_list headers) `POST "/") in

    let exit_cond = Eio.Condition.create () in
    let response_handler =
      handler ~on_eof:(fun () ->
        Stdlib.Format.eprintf "eof@.";
        Eio.Condition.broadcast exit_cond)
        upgraded_connection runtime request
    in
    let request_body =
      Httpun_eio.Client.request
        (* ~flush_headers_immediately:true *)
        ~error_handler
        ~response_handler
        connection
        request
    in
    debug "sending body......" ;
    Httpun.Body.Writer.write_string request_body body ;
    Httpun.Body.Writer.close request_body ;
    Eio.Condition.await_no_mutex exit_cond;

    match !upgraded_connection with
    | Some connection ->
        debug "making gRPC request on upgraded connection" ;
        let request = Network_class.Create.Request.make ~uuid:"abc-abc" () in
        let _result = call_network_create connection request in
        let _result =
          let* session_id, _ = login connection "root" "9SmhjBv4XVg0" "0" "grpc-client" in
          let* host_refs, _ = host_get_all connection session_id in
          debug "hosts: %s" (String.concat ", " host_refs) ;
          let _result = event_stream connection session_id ["VM"; "host"] in
          Ok ()
        in
        Eio.Promise.await (H2_eio.Client.shutdown connection)
    | None ->
        Eio.Promise.await (Httpun_eio.Client.shutdown connection)
  in
  Eio.Switch.run run

let () = debug "start" ; Eio_main.run main
