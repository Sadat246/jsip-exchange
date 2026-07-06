(** Student-written trading bots. This is the library's explicit main module,
    so each concrete bot must be re-exported here to be visible to callers. *)
open! Core

module Book_filler_sadat = Book_filler_sadat
module Bot_random = Bot_random
module Cancel_storm = Cancel_storm
module Robyn_market_maker_bot = Robyn_market_maker_bot
module Market_maker_bot_hansel = Market_maker_bot_hansel
module Robyn_noise_trader = Robyn_noise_trader
module Market_maker_bot_lijia = Market_maker_bot_lijia
module Momentum_trader_hansel = Momentum_trader_hansel
module Noise_trader_hansel = Noise_trader_hansel
module Slow_consumer = Slow_consumer
module Spammer = Spammer
