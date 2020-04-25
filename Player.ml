type t = {name:string;
          chips:Chip.t;
          hand:Cards.deck list;
          bet:Chip.t;
          bot:bool}

let name t = t.name

let get_hand t = t.hand

let chips t = t.chips

let chips_value t = t.chips |> Chip.get_value

let bet t = t.bet

let bet_value t = t.bet |> Chip.get_value

let add_chips t chip = 
  {name=t.name;
  chips=(Chip.add t.chips chip);
  hand=t.hand;
  bet=t.bet;
  bot=t.bot
  }

let bet_chips t bet = 
  {name = t.name;
  chips = (Chip.bet t.chips bet);
  hand = t.hand;
  bet = (Chip.add t.bet bet);
  bot = t.bot}

let collect_bet t = 
  {name = t.name;
  chips = (Chip.add t.chips t.bet);
  hand = t.hand;
  bet = Chip.empty;
  bot = t.bot
  }

let new_player name chips hand bet bot = 
{name=name;
chips=chips;
hand=hand;
bet=bet;
bot=bot;
}

let update_hand t new_hand = 
  {name = t.name;
  chips = t.chips;
  hand = new_hand;
  bet = t.bet;
  bot = t.bot;
  }
