
(*opens modules to be used *)
open Lwt.Infix
open Unix


(* a list associating user nicknames to the output channels that write to their connections *)
(* Once we fix the other functions this should change to []*)
let sessions = ref []
exception Quit


(*send_all sends a message to the user*)
let rec send_all sender msg = 
(*define time and use to create time variables to use to create a time stamp*)
(*iterate through sessions and only send to the message to those who aren't the sender, include a time stamp*)
Lwt_list.iter_p (fun (x,y) -> if x<>sender then Lwt_io.fprintl y (sender^": "^msg) else Lwt.return() ) !sessions 
;;

(*removes the session from the list using the given nick name*)
let remove_session nn =
(*session is removed from overall list*)
  sessions := List.remove_assoc nn !sessions;
  (*a message telling all the users that the sender has left the chat*)
  send_all nn "<left chat>" >>= fun () ->
  Lwt.return ()

(*handles error, typically the Quit exception which prompts the session to be closed than removed from the list of sessions.*)
let handle_error e nn inp outp = 
(*session is removed*)
if e <> Quit then remove_session nn
(*session is first closed than removed afterwards*)
else Lwt_io.close outp >>= fun() -> Lwt_io.close inp>>=fun()-> remove_session nn


(*changes the nick name to a new one which is given by the user*)
let change_nn nn outp new_nn = 
(*session is removed from overall list of sessions*)
sessions := List.remove_assoc !nn !sessions; 
(*the new nick name is added the list of sessions*)
sessions:=!sessions@[(new_nn,outp)];
let old_name = !nn in
nn:=new_nn;
(*a message telling all the users in the session that the sender changed their name*)
send_all old_name ("<changed nick to "^new_nn^">") >>= fun() -> 
Lwt.return ()
;;

(*user is prompted to enter their nickname and added to the list of sessions*)
let handle_login nr (inp,outp) = 
(*user is prompted to enter their nick name*)
    Lwt_io.fprintl outp "Enter initial nick:" >>= fun () -> 
    (*takes in user input and sets it as their nick name*)
    (Lwt_io.read_line inp) >>= fun s -> 
    nr:=s;
    (*their nickname is added to the sessions list of users*)
    sessions:=!sessions@[(!nr,outp)];
    (*it's announced to other users that the user joined the session*)
    send_all !nr "<joined>" >>= fun () -> Lwt.return ()
  
(*takes in /n, /q and /l from the user and matches it up with functions to be performed*)
let handle_input nr outp l=
    match (Str.string_before l 2) with
    (*changes the nick name of the user*)
	"/n"-> change_nn nr outp (Str.string_after l 3);
	(*causes the session to quit*)
	|"/q" -> raise Quit;
	(*lists all the users within the session*)
	|"/l" -> Lwt_list.iter_p (fun (x,y) -> Lwt_io.printl x ) !sessions 
	(*anything besides the above specified, results in whatever the user writes being sent as a message to all users in session*)
        | _ -> send_all !nr l
;;

(*chat_server handles streamlining the functions above*)
let chat_server _ (inp,outp) =
(*nick is first set as an empty string ref than changed to the users input given handle_login function*)
  let nick = ref "" in
  let _ =  handle_login nick (inp,outp) in
  (*the main_loop handles placing the users input into handle_input any errors results in handle_error being run*)
  let rec main_loop () =
	  Lwt_io.read_line inp >>= handle_input nick outp >>= main_loop in
  Lwt.catch main_loop (fun e -> handle_error e !nick inp outp)
