type t = {name:string;
  players:int;
  (** Temporary, will change *)
  rules: string list
}

let rep_ok t = 
  failwith "Unimplemented"
  
let rules t =
  failwith "Unimplemented"

let save_name t =
  failwith "Unimplemented"

let players t =
  t.players

let name t =
  t.name

let bet t chips =
  failwith "Unimplemented"


