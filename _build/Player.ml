open Chip
open Cards

type t = {name:string;
          chips:Chip.t;
          hand:Cards.deck list;
          bet:Chip.t list;
          bot:bool}

let name t = t.name

let get_hand t = t.hand

let chips t = t.chips

let chips_value t = Chip.get_value t.chips

let bet t = t.bet

let bet_value t idx = List.nth_opt t.bet idx |> function
  |Some b -> Chip.get_value b
  |None -> failwith "Bet doesn't exist"


let add_chips chip t = 
  {name=t.name;
   chips=(Chip.add t.chips chip);
   hand=t.hand;
   bet=t.bet;
   bot=t.bot
  }

let bet_chips bet idx t = 
  let rec new_bet_lst lst idx accum =
    match lst with
    |h::r -> if idx = 0 then 
        let new_b = Chip.add h bet in
        (List.rev accum)@[new_b]@r
      else new_bet_lst r (idx - 1) (h::accum)
    |[] -> failwith "Bet does not exist" in

  {name = t.name;
   chips = (Chip.bet t.chips bet);
   hand = t.hand;
   bet = (new_bet_lst t.bet idx []);
   bot = t.bot}

let collect_bets t = 
  {name = t.name;
   chips = (List.fold_left (Chip.add) t.chips t.bet);
   hand = t.hand;
   bet = [Chip.empty];
   bot = t.bot
  }

let lose_bet idx t = 
  let rec lb_aux bet_lst idx accum =
    match bet_lst with
    |h::r -> if idx = 0 then
        (List.rev accum)@[Chip.empty]@r else
        lb_aux r (idx - 1) (h::accum)
    |[] -> failwith "Bet doesn't exist" in
  {name = t.name;
   chips = t.chips;
   hand = t.hand;
   bet = (lb_aux t.bet idx []);
   bot = t.bot}

let new_player name chips hand bet bot = 
  {name=name;
   chips=chips;
   hand=hand;
   bet=bet;
   bot=bot;
  }

let update_hand new_hand t = 
  {name = t.name;
   chips = t.chips;
   hand = new_hand;
   bet = t.bet;
   bot = t.bot;
  }

let add_to_hand c ind t =
  let rec choose_hand d c ind accum =
    match d with
    | [] -> failwith "hand does not exist"
    | h::r -> if ind = 0 then (List.rev accum)@[(Cards.add_to_deck c h)]@r 
      else (choose_hand r c (ind-1) (h::accum)) in
  {name = t.name;
   chips = t.chips;
   hand = choose_hand t.hand c ind [];
   bet = t.bet;
   bot = t.bot;
  }

let add_hand t = 
  {name = t.name;
   chips = t.chips;
   hand = (t.hand@[Cards.empty]);
   bet = t.bet;
   bot = t.bot;
  }

let add_bet t = 
  {name = t.name;
   chips = t.chips;
   hand = t.hand;
   bet = (t.bet@[Chip.empty]);
   bot = t.bot;
  }

let simplify_chips steps t = 
  {name = t.name;
   chips = (Chip.simplify t.chips steps);
   hand = t.hand;
   bet = t.bet;
   bot = t.bot;
  }

let break_chips steps t = 
  {name = t.name;
   chips = (Chip.break t.chips steps);
   hand = t.hand;
   bet = t.bet;
   bot = t.bot;
  }

let make_player_with_hand tups =
  {name="dealer";chips=Chip.empty;hand=[List.map (fun (c,s,r) -> Cards.make_card s c r) tups];bet=[Chip.empty];bot=true}