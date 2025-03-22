open Yojson.Basic.Util

(*--------------Shared Utils------------------*)
let to_float_option json =
  try
    Some (to_float json)
  with Type_error _ ->
    None


(*--------------ActivityRecord-----------------*)
type activity_record = {
  start_timestamp: float; 
  mutable end_timestamp: float option;
}

let activity_record_to_json a = 
  `Assoc [
    ("start_timestamp", `Float a.start_timestamp);
    ("end_timestamp", match a.end_timestamp with | Some f -> `Float f | None -> `Null);
  ]

let activity_record_from_json json =
  {
    start_timestamp = json |> member "start_timestamp" |> to_float;
    end_timestamp = json |> member "end_timestamp" |> to_float_option;
  }

(*--------------Activity-----------------*)
type activity = {
  mutable is_active: bool;
  name: string;
  records: activity_record list ref
}

let activity_to_json a = 
  `Assoc [
    ("name", `String a.name);
    ("is_active", `Bool a.is_active);
    ("records", `List (List.map activity_record_to_json !(a.records)));
  ]

let activity_from_json json =
  {
    name = json |> member "name" |> to_string;
    is_active = json |> member "is_active" |> to_bool;
    records = ref (json |> member "records" |> to_list |> List.map activity_record_from_json);
  }


(*----------------State------------------*)
type state = {
  activities: activity list ref;
}

let state_to_json s =
  `Assoc [
    ("activities", `List (List.map activity_to_json !(s.activities)));
  ]

let state_from_json json =
  {
    activities= ref (json |> member "activities" |> to_list |> List.map activity_from_json);
  }


(*----------------Files------------------*)
let read_json_file filename =
  let ic = open_in filename in
  let json = Yojson.Basic.from_channel ic in
  close_in ic;
  json

let write_json_file filename json =
  let oc = open_out filename in
  Printf.fprintf oc "%s\n" (Yojson.Basic.pretty_to_string json);
  close_out oc


(*----------------Utils---------------------*)
let get_active_activity (activities: activity list) : activity option =
  try 
    Some (List.find (fun a -> a.is_active = true) activities)
  with Not_found -> 
    None

let get_activity_by_name activities name : activity option = 
  try 
    Some (List.find (fun a -> a.name = name) activities)
  with Not_found -> 
    None

let get_saved_state = 
  try
    let state = state_from_json (read_json_file "data.json") in
    state
  with Sys_error _ -> 
    let state = { activities = ref [] } in
    state


(*----------------Commands------------------*)
let start_command idx state = 
  let name = Sys.argv.(idx) in
  let activities = !(state.activities) in
  let active_activity = get_active_activity activities in 
    match active_activity with
    | None -> 
        begin
          let record = { start_timestamp = Unix.time (); end_timestamp = None } in
          let activity = get_activity_by_name activities name in
          match activity with
          | None -> 
              state.activities := { name; is_active = true; records = ref [record] } :: !(state.activities);
              let json_to_write = state_to_json state in
              write_json_file "data.json" json_to_write;
              Printf.printf "Activity %s started.\n" name
          | Some a -> 
              begin
                a.records := { start_timestamp = Unix.time (); end_timestamp = None } :: !(a.records);
                a.is_active <- true;
                let json_to_write = state_to_json state in
                write_json_file "data.json" json_to_write;
                Printf.printf "Activity %s started.\n" name
              end
        end
    | Some _ -> Printf.printf "Activity %s already in progress.\n" name

let stop_command _ state = 
  let active_activity = get_active_activity !(state.activities) in
  match active_activity with
  | None -> 
      Printf.printf "No active activity.\n"
  | Some activity -> 
      activity.is_active <- false;
      let record = List.nth_opt !(activity.records) 0 in
      match record with
      | None -> raise (Sys_error "No records found")
      | Some r ->
          r.end_timestamp <- Some (Unix.time ());
          let json_to_write = state_to_json state in
          write_json_file "data.json" json_to_write;
          Printf.printf "Activity %s stopped.\n" activity.name

let session_command _ state =
  match !(state.activities) with
  | [] -> Printf.printf "No activities found.\n"
  | first_activity :: _ -> 
      match !(first_activity.records) with
      | [] -> Printf.printf "Activity %s has no records.\n" first_activity.name
      | first_record :: _ ->
          let time_diff = Unix.time () -. first_record.start_timestamp in
          let tm = Unix.localtime first_record.start_timestamp in
          let time_str = Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d"
            (tm.Unix.tm_year + 1900)
            (tm.Unix.tm_mon + 1)
            tm.Unix.tm_mday
            tm.Unix.tm_hour
            tm.Unix.tm_min
            tm.Unix.tm_sec in
          let hours = int_of_float (time_diff /. 3600.0) in
          let minutes = int_of_float ((time_diff -. (float_of_int hours *. 3600.0)) /. 60.0) in
          Printf.printf "%s started at %s, session lasts for %dh %dm\n" first_activity.name time_str hours minutes

          
let stats_command idx state =
  let activity = get_activity_by_name !(state.activities) Sys.argv.(idx) in
  match activity with
  | None -> Printf.printf "No activities found.\n"
  | Some a -> 
      begin
        List.iter (fun record -> 
          if Option.is_some record.end_timestamp then
          begin
            let tm = Unix.localtime record.start_timestamp in
            let time_str = Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d"
              (tm.Unix.tm_year + 1900)
              (tm.Unix.tm_mon + 1)
              tm.Unix.tm_mday
              tm.Unix.tm_hour
              tm.Unix.tm_min
              tm.Unix.tm_sec in
            let time_diff = Option.value record.end_timestamp ~default:0.0 -. record.start_timestamp in
            let hours = int_of_float (time_diff /. 3600.0) in
            let minutes = int_of_float ((time_diff -. (float_of_int hours *. 3600.0)) /. 60.0) in
            Printf.printf "Started: %s, total time: %dh %dm\n" time_str hours minutes
          end
        ) !(a.records)
      end
      
let unknown_command _ _ = Printf.printf "Unknown command\n"

let get_command v = match v with
  | "start" -> start_command 
  | "stop" -> stop_command 
  | "session" -> session_command
  | "stats" -> stats_command
  | _ -> unknown_command 


(*----------------MAIN------------------*)
let () = 
  let state = get_saved_state in
  let curr_command_idx = 1 in
  let command = get_command Sys.argv.(curr_command_idx) in
  let curr_command_idx = curr_command_idx + 1 in
  command curr_command_idx state

