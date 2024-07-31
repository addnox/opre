prem <- Triangle(TRI$Premium)
loss <- Triangle(TRI$Incurred)

test_that("get latest values", {
  latest_prem <- c(628641372, 832441998, 849436802, 979691295, 1062547835, 1197566179, 1219597349, 1307589407, 1453962817, 1570134318, 1697016690, 1787184293, 1761095967)
  expect_equal(unname(tri_latest(prem)), latest_prem)
})

test_that("ChainLadder is correct", {
  ult_prem <- c(628641372, 832441998, 849436802, 979691295, 1062547835, 1197608673, 1219640624, 1307635804, 1454014408, 1570274224, 1698162713, 1790172260, 2083883590)
  res <- calc(prem, "ChainLadder", floor = 1)

  expect_equal(res$ultimate, ult_prem)
})

test_that("CapCod is correct", {
  ult_loss <- c(233176037, 897741057, 790329145, 651399524, 1209254454, 840591785, 952076853, 2131489637, 1277731718, 1259144480, 1305056498, 1503967871, 1994213824)
  ult_prem <- calc(prem, "ChainLadder", floor = 1)$ultimate
  ELR <- CapeCod(loss, premium = ult_prem, floor = 1)
  res <- calc(loss, "BF", expected_loss = ELR * ult_prem, floor = 1)
  expect_equal(res$ultimate, ult_loss)
})
