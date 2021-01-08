# Synthetic -1 Puckline Calculator | Andrew Mack | @gingfacekillah
# Weights a moneyline and -1.5 puckline (or runline) bet to create a push if team wins by exactly one goal.

synthpuckline <- function(ml_odds, pl_odds, amount_risked){
  ml <- amount_risked*(1/ml_odds)
  pl <- amount_risked-ml
  to_winml <- (ml_odds-1)*ml
  to_winpl <- (pl_odds-1)*pl
  weight_ml <- ml/amount_risked
  weight_pl <- pl/amount_risked
  synth_price <-((ml_odds*weight_ml)+(pl_odds*weight_pl))
    
    
  return(list("Moneyline"=round(ml,2),"Puckline"=round(pl,2), "Synthetic Price" = round(synth_price,3)))
}

synthpuckline(1.45, 2.45, 1000)
