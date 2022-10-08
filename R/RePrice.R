#' Calculate loading factor
price_ImpliedLoading <- function(ContractLoss, Premium, OtherDeduction = 0, prob = c(.99, .995, .996)) {
  # LR Method
  gMean(ContractLoss) / gMean(Premium)
  # SD Method
  # - E(Prem) = E(Loss) + E(Expense) + k * SD(Loss)
  tmpMean_UWProfit <- gMean(Premium) - gMean(ContractLoss)

  # VaR
  # - k = Mean(Profit) / Tail(Profit)
  tmpVaR_UWProfit <- vapply(prob, function(p) gVaR(ContractLoss - Premium, prob))
  tmpMean_UWProfit / tmpVaR_UWProfit

  # TVaR
  tmpTVaR_UWProfit <- vapply(prob, function(p) gTVaR(ContractLoss - Premium, prob))
  tmpMean_UWProfit / tmpTVaR_UWProfit

  # Output
  res <- list(
    "LossRatio" = data.table::data.table(prob = NA_real_, factor = gMean(ContractLoss) / gMean(Premium)),
    "SD" = data.table::data.table(prob = NA_real_, factor = tmpMean_UWProfit / gSD(ContractLoss)),
    "VaR" = data.table::data.table(prob = prob, factor = tmpMean_UWProfit / tmpVaR_UWProfit),
    "TVaR" = data.table::data.table(prob = prob, factor = tmpMean_UWProfit / tmpTVaR_UWProfit)
  ) |>
    data.table::rbindlist(idcol = "Method")

  res
}
