library(tidyverse)
library(raster)
library(sf)
library(vultRmap)

rm(list = ls())


# Define regions --------------------------------------------------

SA <- readRDS("../cape_vulture_cr/data/working/gadm36_ZAF_1_sp.rds")
LST <- readRDS("../cape_vulture_cr/data/working/gadm36_LSO_1_sp.rds")
SWZ <- readRDS("../cape_vulture_cr/data/working/gadm36_SWZ_1_sp.rds")
BWA <- readRDS("../cape_vulture_cr/data/working/gadm36_BWA_1_sp.rds")
NAM <- readRDS("../cape_vulture_cr/data/working/gadm36_NAM_1_sp.rds")
ZWE <- readRDS("../cape_vulture_cr/data/working/gadm36_ZWE_1_sp.rds")
MOZ <- readRDS("../cape_vulture_cr/data/working/gadm36_MOZ_1_sp.rds")

# For Lesotho and Swaziland we will predict for the whole countries rather than
# provinces
LST$NAME_1 <- LST$NAME_0
SWZ$NAME_1 <- SWZ$NAME_0
BWA$NAME_1 <- BWA$NAME_0
NAM$NAME_1 <- NAM$NAME_0
ZWE$NAME_1 <- ZWE$NAME_0
MOZ$NAME_1 <- MOZ$NAME_0

# South Africa gets plotted with Lesotho and Swaziland
SA_total <- rbind(SA,LST,SWZ) %>%
      st_as_sf() %>%
      dplyr::select(NAME_1)

SA_total <- SA_total %>%
      group_by(NAME_1) %>%
      summarise(m = unique(NAME_1)) %>%
      st_cast()

range_total <- rbind(SA,LST,SWZ, BWA, NAM, ZWE, MOZ) %>%
      st_as_sf() %>%
      dplyr::select(NAME_1)

# Load data ---------------------------------------------------------------

# Load risk
risk_hgt <- raster("../vultRmap_data_aux/risk_maps/risk_hgt_total.tif")
risk <- raster("../vultRmap_data_aux/risk_maps/risk_total.tif")

# Load REDZ
redz1 <- st_read("../cape_vulture_cr/data/working/Phase1_REDZs/REDZs.shp")
redz2 <- st_read("../cape_vulture_cr/data/working/Phase2_REDZs/PHASE 2_REDZs.shp")

# Get both shapes in the same coordinate system and combine
redz1 <- st_transform(redz1, st_crs(redz2))

redz <- rbind(dplyr::select(redz1, Name),
              dplyr::select(redz2, Name))

plot(redz)


# Analyze risk per province -----------------------------------------------

# Extract provincial risk
prov_risk <- calcRegionRisk(range_total, ref_area = NULL, risk_map = risk)

sum(prov_risk$risk_total)
sum(values(risk), na.rm = T)
sum(values(risk_hgt), na.rm = T)

sum(values(risk), na.rm = T)/1.5

write.csv(st_drop_geometry(range_total), file = "../cape_vulture_cr/text/paper/figures/risk_range.csv")


# Analyze risk per REDZ -------------------------------------------------------

# Extract risk values for each REDZ using total range as reference
redz_national <- calcRegionRisk(region = redz, ref_area = range_total, risk_map = risk)

redz_national

# Order redz by increasing avg risk
lvs <- redz_national %>%
   arrange(desc(risk_avg)) %>%
   pull(Name) %>%
   unique()

redz_national <- redz_national %>%
   mutate(Name = factor(Name, levels = lvs))

write.csv(st_drop_geometry(redz_national), file = "../cape_vulture_cr/text/paper/figures/redz_national.csv")

# Analyse risk taking province as a reference
redz <- redz %>%
   mutate(province = case_when(Name == "Stormberg Wind" ~ "Eastern Cape",
                               Name == "Overberg Wind" ~ "Western Cape",
                               Name == "Emalahleni" ~ "Mpumalanga",
                               Name == "Beaufort West" ~ "Western Cape",
                               Name == "Klerksdorp" ~ "North West",
                               Name == "Cookhouse Wind" ~ "Eastern Cape",
                               Name == "Vryburg Solar" ~ "North West",
                               Name == "Kimberley Solar" ~ "Free State",
                               Name == "Komsberg Wind" ~ "Western Cape",
                               Name == "Springbok Wind" ~ "Northern Cape",
                               Name == "Upington Solar" ~ "Northern Cape"))

redz_prov <- vector("list", nrow(redz))

for(i in seq_along(redz_prov)){
   prov <- filter(SA_total, NAME_1 == redz$province[i])
   redz_prov[[i]] <- calcRegionRisk(region = redz[i,], ref_area = prov, risk_map = risk)
}

redz_prov <- do.call("rbind", redz_prov)

redz_prov <- redz_prov %>%
   st_drop_geometry() %>%
   rename(prop_prov = risk_prop) %>%
   left_join(redz_national %>%
                st_drop_geometry() %>%
                dplyr::select(Name, risk_prop),
             by = "Name") %>%
   rename(prop_nat = risk_prop)

redz_prov %>%
   arrange(desc(risk_avg)) %>%
   write.csv(file = "../cape_vulture_cr/text/paper/figures/redz_risk.csv")


# Plot REDZ risk ----------------------------------------------------------

# Relate risk to REDZs
redz_risk <- exactextractr::exact_extract(risk, redz)
redz_risk <- map2_dfr(redz_national$Name, redz_risk, ~tibble(name = .x, risk = .y$value))

# Remove 0.1% of activity to reduce mapping
cutoff <- 0.99
risk[risk < calcUDquantile(raster::values(risk), cutoff)] <- NA

# Reclassify risk raster
nlevels <- 10
udlevels <- seq(0, 1, length.out = nlevels + 1)

# Reclassify
cut_points <- calcUDquantile(raster::values(risk), udlevels)
rcl <- cbind(c(cut_points[-1], 0), cut_points, udlevels)
risk_UD <- reclassify(risk, rcl)

# Clip to South Africa for plotting
risk_df <- as.data.frame(crop(risk_UD, SA_total), xy = TRUE)

redz_bars <- redz_national %>%
   ggplot() +
   geom_pointrange(aes(x = risk_avg,
                       xmin = pmax(0, risk_avg - risk_sd),
                       xmax = risk_avg + risk_sd,
                       y = Name, colour = Name), fatten = 0.5, size = 0.5) +
   scale_y_discrete(limits=rev) +
   scale_colour_viridis_d(option = "D", direction = 1) +
   ylab("") + xlab("risk") +
   theme_bw() +
   theme(axis.text = element_text(size = 10),
         axis.text.y = element_blank(),
         axis.title = element_text(size = 10),
         legend.position = "none")

redz_violplot <- redz_risk %>%
      ggplot() +
      geom_violin(aes(x = log(risk), y = name, fill = name)) +
      scale_y_discrete(limits=rev) +
      scale_fill_viridis_d(option = "D", direction = 1) +
      ylab("") +
      theme_bw() +
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 10),
            legend.position = "none")

redz_riskmap <- ggplot() +
   geom_raster(data = risk_df, aes(x = x, y = y, fill = risk_total),
               alpha = 1, show.legend = FALSE) +
   geom_sf(data = SA_total, fill = NA, linetype = 2, size = 0.3) +
   geom_sf(data = redz_national, aes(colour = Name), fill = NA) +
   # scale_fill_gradient(low = "white", high = "#CC089B") +
   scale_fill_viridis_c(option = "inferno", direction = 1, na.value = NA) +
   scale_colour_viridis_d(option = "D", direction = 1, name = "") +
   scale_x_continuous(expand = c(0, 0)) +
   scale_y_continuous(expand = c(0, 0)) +
   theme_classic() +
   theme(axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         axis.line = element_blank(),
         legend.text = element_text(size = 10),
         panel.border = element_rect(fill = NA))

redzplot <- gridExtra::arrangeGrob(redz_violplot, redz_bars, redz_riskmap,
                                   nrow = 5, ncol = 3,
                                   layout_matrix = cbind(c(1,1,3,3,3),
                                                         c(1,1,3,3,3),
                                                         c(1,1,3,3,3),
                                                         c(2,2,3,3,3),
                                                         c(2,2,3,3,3)))

ggsave(filename = "../cape_vulture_cr/text/paper/figures/redz_risk.png", redzplot)
