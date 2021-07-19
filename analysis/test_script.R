.col_sel = col_sel;
.coefs = hgt_coef; .age = "ad";
.datadir = "../vultRmap_data_aux/";
.simsdir = "../vultRmap_data_aux/col_sims/";
.outdir = "../vultRmap_data_aux/col_hgt_risk/"


col_cc = unlist(.col_sel[,c("lon", "lat")]);
max_range = max(.sims$dist_col)/1000 + 100;
col_all = .col_all;
sfs = .sfs; scale = "hgt"

library(ggplot2)

counts %>%
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = count))

sims %>%
  ggplot() +
  geom_point(aes(x = lon, y = lat), size = 0.1) +
  geom_density2d(aes(x = lon, y = lat))

out %>%
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = hgt_risk))


col_to_pred = col_to_pred;
age = "ad";
totalsteps = 120000;
ncores = 40;
set_seed = 6548;
dist_lim = 1200;
sample_coefs = 40;
out_dir = "/home/crvfra001/vults/output/vultRmap/sims";
data_dir = "../vultRmap_data_aux"


.nsteps = nsteps[[1]]; .ssf_coef = ssf_coef[[1]];
.age = age; .hab = hab;
.mov_ker = vultRmap::mov_kernel;
.col_sel = unlist(col_sel[, c("lon","lat")]);
.maxdist = (dist_lim - dist_lim * 10/100)


ya <- list.files("../vultRmap_data_aux/col_hgt_risk", pattern = "juv")
ya <- gsub("_juv_hgt_risk.rds", "", ya)
ya <- ya[!ya %in% c("da_234", "da_233")]
col_to_pred <- col_to_pred %>%
  filter(!id %in% ya)
col_to_pred <- col_to_pred


"cvcol395", "cvcol379", "cvcol433", "cvcol414", "cvcol378", "cvcol376"

col_to_pred <- col_to_pred %>%
  dplyr::filter(id %in% c("cvcol395", "cvcol379", "cvcol433", "cvcol376"))

map_type = "hazard"
countsdir = "../vultRmap_data_aux/col_gam/"
outdir = "analysis/output/risk_maps/379_395_"

library(ggplot2)
ud %>%
  dplyr::filter(gamfit > 0.0001) %>%
  ggplot() +
  geom_tile(aes(x = lon , y = lat, fill = gamfit)) +
  coord_equal()
