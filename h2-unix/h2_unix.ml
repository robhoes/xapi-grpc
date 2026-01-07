module Server = struct
  let create_connection_handler ?(config = H2.Config.default) ~request_handler
      ~error_handler client_addr socket =
    let create_connection =
      H2.Server_connection.create ~config
        ~error_handler:(error_handler client_addr)
    in
    Gluten_unix.Server.create_upgradable_connection_handler
      ~read_buffer_size:config.read_buffer_size
      ~protocol:(module H2.Server_connection)
      ~create_protocol:create_connection ~request_handler client_addr socket
end

module Client = struct
  type t =
    { connection : H2.Client_connection.t
    ; runtime : Gluten_unix.Client.t
    }

  let create_connection ?(config = H2.Config.default) ~error_handler socket =
    let connection = H2.Client_connection.create ~config ~error_handler () in
    let runtime =
      Gluten_unix.Client.create
        ~read_buffer_size:config.read_buffer_size
        ~protocol:(module H2.Client_connection)
        connection
        socket
    in
    { runtime; connection }

  let request t = H2.Client_connection.request t.connection
  let shutdown t = Gluten_unix.Client.shutdown t.runtime
  let is_closed t = Gluten_unix.Client.is_closed t.runtime
  let upgrade t protocol = Gluten_unix.Client.upgrade t.runtime protocol
end
