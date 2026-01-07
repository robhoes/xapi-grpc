let debug (fmt : ('a, unit, string, 'b) format4) =
  Printf.ksprintf
    (fun s -> Printf.printf "%d: %s\n%!" Thread.(self () |> id) s)
    fmt

include Stdlib.Seq

type 'a reader = 'a t
type 'a writer = { mutable ch : 'a node Event.channel }

let write writer item =
  debug "Seq.write" ;
  let ch = Event.new_channel () in
  let next = Cons (item, fun () -> Event.(receive ch |> sync)) in
  debug "write: sending event" ;
  Event.(send writer.ch next |> sync);
  writer.ch <- ch

let close_writer writer = debug "Seq.close_writer"; Event.(send writer.ch Nil |> sync)
let read reader = reader ()

let rec exhaust_reader reader =
  match reader () with Nil -> () | Cons (_, reader) -> exhaust_reader reader

let read_and_exhaust reader =
  debug "Seq.read_and_exhaust" ;
  match reader () with
  | Nil -> None
  | Cons (item, reader) ->
      exhaust_reader reader;
      Some item

let create_reader_writer () =
  let ch = Event.new_channel () in
  ((fun () -> 
    debug "Seq reader: waiting for event" ;
    Event.(receive ch |> sync)), { ch })