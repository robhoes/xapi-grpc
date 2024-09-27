open Grpc_eio
open Xapi_grpc.Xapi
open Ocaml_protoc_plugin

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
  H2_eio.Client.create_connection ~sw ~error_handler:ignore socket

let call_network_create connection network_record =
  let encode, decode = Service.make_client_functions Network_class.create in
  let response =
    Client.call ~service:"xapi.Network_class" ~rpc:"Create"
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
  | Ok (res, _ok) -> Printf.printf "RESPONSE = {%s}" res
  | Error _ -> Printf.printf "an error occurred"

let main env =
  let port = "8080" in
  let host = "localhost" in
  let network = Eio.Stdenv.net env in
  let () = Random.self_init () in

  let run sw =
    let connection = client ~sw host port network in

    let request = Network_class.Create.Request.make ~uuid:"abc-abc" () in
    let result = call_network_create connection request in

    Eio.Promise.await (H2_eio.Client.shutdown connection);
    result
  in

  Eio.Switch.run run

let () = Eio_main.run main

(* $MDX part-end *)
