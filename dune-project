(lang dune 3.7)

(package (name xapi-grpc)
(depends
  (eio_main
   (>= 0.12))
  grpc-eio
  (h2
   (>= 0.9.0))
  h2-eio
  (ocaml-protoc-plugin
   (>= 4.5))
  ppx_deriving
  ppx_deriving_yojson
  (ppx_jane
   (>= v0.16.0))
))