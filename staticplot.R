library(tidyverse)
library(scales)

# Load data saved from data.R, set filepath as needed in `directory`
directory <- "~/Mirror/2024-2025/DAP 2/final"
data_full <- readRDS(paste0(directory, "/data_full.RData"))

# Plot 1: scatterplot with MSP enrollment figures and policy change date

plot1_data <- data_full |> 
  group_by(date) |> 
  summarise(msp_per_100k = (sum(total_msp, na.rm = TRUE)/sum(beneficiaries, na.rm = TRUE))*100000,
            qmb_per_100k = (sum(qmb_only+qmb_plus_full_medicaid_benefits, na.rm = TRUE)/sum(beneficiaries, na.rm = TRUE))*100000) |> 
  mutate(policy_period = if_else(date >= as_date("2020-01-01"), "Post", "Pre"))

p <- ggplot(plot1_data, aes(x = date, y = msp_per_100k, color = policy_period)) +
  geom_point(color = "black") +
  geom_line() +
  geom_vline(xintercept = as_date("2020-01-01"), linetype = "dotted", color = "red") + 
  geom_label(aes(x = as_date("2018-03-01"), y = 29500, label = "Eligibility change"), size = 3.5, label.size = 0.5, color = "black", fill = "white") +  
  geom_segment(aes(x = as_date("2019-04-01"), y = 29500, xend = as_date("2019-11-01"), yend = 29500), 
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  scale_y_continuous(limits = c(20000, 30000), labels = scales::comma) +
  scale_color_brewer(palette = "Pastel2") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
    ) +
  labs(
    x = "Year",
    y = "Enrollment rate\n(per 100,000 Medicare beneficiaries)",
    title = "Massachusetts Medicare Savings Programs\nQuarterly Enrollment Rates, 2015-2023",
    caption = "Data sources: 5-year ACS (2015-2022) and 1-year ACS (2023), US Census Bureau;\nCMS Quarterly Enrollment Snapshot (06/2015-09/2023)"
  )

ggsave(p,
       filename="plot1_enrollment_plot.png",
         dpi = 320,
         width = 6,
         height = 4)

# Plot 2: map showing counties' growth post-2020 policy change

plot2_pre <- data_full |> 
  filter(date == "2019-12-30")

plot2_post <- data_full |> 
  filter(date == "2023-09-30")

plot2_data <- plot2_pre |> 
  left_join(plot2_post, join_by(FIPS_STCO), suffix = c("_pre", "_post")) |> 
  mutate(change = total_msp_per_100k_post - total_msp_per_100k_pre) |> 
  select(county_of_beneficiary_pre, geometry_pre, change)

p <- ggplot(plot2_data) +
  geom_sf(aes(fill = change, geometry = geometry_pre)) +
  scale_fill_distiller(palette = "RdBu", direction = 1, labels = scales::comma) +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  labs(title = "Change in Medicare Savings Programs\nEnrollment Rates, 12/2019 - 9/2023\n(per 100,000 Medicare beneficiaries)",
       fill = "Change in rate\n(per 100,000\nbeneficiaries)",
       caption = "Data sources: 5-year ACS (2015-2022) and 1-year ACS (2023), US Census Bureau;\nCMS Quarterly Enrollment Snapshot (06/2015-09/2023)")

ggsave(p,
       filename="plot2_enrollment_map.png",
       dpi = 320,
       width = 6,
       height = 4)
