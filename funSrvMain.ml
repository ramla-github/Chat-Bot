(*open modules to be used*)
open Lwt.Infix
(* define functions to help create the port inroder to establish a connection*)
let port = if Array.length Sys.argv > 1 then int_of_string (Sys.argv.(1)) else 16384
let s = Lwt_io.establish_server_with_client_address (Unix.ADDR_INET(Unix.inet_addr_any, port)) FunSrv.chat_server
let _ = Lwt_main.run (fst (Lwt.wait ()))
(*May not execute but it's used to get to top*)
let _ = s >>= Lwt_io.shutdown_server 
