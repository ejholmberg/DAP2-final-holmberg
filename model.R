library(stargazer)

directory <- "~/Mirror/2024-2025/DAP 2/final"
data_full <- readRDS(paste0(directory, "/data_full.RData"))

model <- data_full |> 
  mutate(ps1 = if_else(year>=2020, 1, 0),
         pop = total_pop/100000) |> 
  rename(nw = nonwhite_percent)


stargazer(lm(total_msp_per_100k ~ nw + pop + ps1, data = model), type = "html",
          font.size="tiny",
          single.row = T)
 

  