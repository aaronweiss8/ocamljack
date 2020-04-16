type t = {name:string;
          chips: Chip.t list;
          bot:bool;}

let name t = t.name
let chips = t.chips