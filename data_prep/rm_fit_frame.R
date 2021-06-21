fit <- readRDS("../vultRmap_data_aux/ssf_fit_10pp.rds")

fit$frame <- NULL

saveRDS(fit, "../vultRmap_data_aux/ssf_fit_10pp.rds")
