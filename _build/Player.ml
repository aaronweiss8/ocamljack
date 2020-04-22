type t = {name:string;
          chips:Chip.t;
          bot:bool}

let name t = t.name
let chips t = 
  t.chips
let score t = t.chips |> Chip.get_value

let update_chips t chip =
  {name=t.name;chips=chip;bot=t.bot}