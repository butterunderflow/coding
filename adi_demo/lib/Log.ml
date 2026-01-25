type log_level =
  | DEBUG
  | NO

let level = ref DEBUG

let debugf (fmt : ('a, out_channel, unit) format) =
  match !level with
  | DEBUG -> Printf.printf fmt
  | NO -> ()
