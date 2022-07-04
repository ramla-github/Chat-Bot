(*opens modules to be used *)
open Lwt.Infix


(* a list associating user nicknames to the output channels that write to their connections *)
(* Once we fix the other functions this should change to []*)
let sessions = ref []
exception Quit


(*send_all sends a message to the user*)
let rec send_all sender msg = 
(*define time and use to create time variables to use to create a time stamp*)
let t = Unix.localtime (Unix.time ()) in
let am_pm = if t.tm_hour < 12 then "am" else "pm" in
let tw_hr = if t.tm_hour <= 12 then t.tm_hour else t.tm_hour - 12 in
let tw_min = if t.tm_min < 10 then "0"^(string_of_int t.tm_min) else (string_of_int t.tm_min) in
let my_tm = (string_of_int tw_hr)^":"^tw_min^am_pm in
(*iterate through sessions and only send to the message to those who aren't the sender, include a time stamp*)
Lwt_list.iter_p (fun (x,y) -> if x<>sender then Lwt_io.fprintl y (sender^": "^msg^" ["^my_tm^"]") else Lwt.return() ) !sessions 
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

(*draw_emoji handles words and turns them into emoticons to users, happy, sad, angry, dance, hug, sleepy, excited, excited and cheer.*)
let draw_emoji sender em outp =
(*matches user input with the emoticons below and outputs it to all the users within the session*)
match em with 
"happy" -> send_all sender "ヽ(´▽`)/"
|"sad" -> send_all sender "(ಥ⌣ಥ)"
|"angry" -> send_all sender "┻━┻ ︵ヽ(`Д´)ﾉ︵ ┻━┻"
|"dance" -> send_all sender "ヾ(-_- )ゞ"
|"hug" -> send_all sender "(⊃｡•́‿•̀｡)⊃"
|"sleepy" -> send_all sender "눈_눈"
|"excited" -> send_all sender "☜(⌒▽⌒)☞"
|"cheers" -> send_all sender "（ ^_^）o自自o（^_^ ）"
(*If the input entered by a user doesn't match up with an emoticon than it won't be valid and all users in the session are told such*)
|_ -> send_all sender "Invalid emoji used" 
;;

(*handle_input below to detect /q, /n,/e and /l commands *)
(*takes in /n, /q, /e and /l from the user and matches it up with functions to be performed*)
let handle_input nr outp l=
    match (Str.string_before l 2) with
    (*changes the nick name of the user*)
	"/n"-> change_nn nr outp (Str.string_after l 3);
	(*causes the session to quit*)
	|"/q" -> raise Quit;
	(*lists all the users within the session*)
	|"/l" -> Lwt_list.iter_p (fun (x,y) -> Lwt_io.printl x ) !sessions 
	(*outputs an emoticon to all the users in the session if the emoticon is valid*)
	|"/e" -> draw_emoji !nr (Str.string_after l 3) outp;
	(*anything besides the above specified, results in whatever the user writes being sent as a message to all users in session*)
        | _ -> send_all !nr l
;;

(*This function handles creating the entire mechanism of when to use the functions to create the chat_server*)
let chat_server _ (inp,outp) =
(*We'll make nick an empty string reference than fill it in with user input later on after calling handle_login*)
  let nick = ref "" in
  let _ =  handle_login nick (inp,outp) in
  let rec main_loop () =
  (*We'll take in user data and input it into handle_input to perform operations*)
	  Lwt_io.read_line inp >>= handle_input nick outp >>= main_loop in
	  (*If any issues occur Lwt.catch will deal with them by using handle_error*)
  Lwt.catch main_loop (fun e -> handle_error e !nick inp outp)
