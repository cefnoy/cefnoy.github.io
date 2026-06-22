#Goal: We want to find the distribution of alumni giving for the past 15 years by the date of their graduation, as well as the distribution of how much each gift was for


install.packages("devtools")
library(devtools)
install.packages("dplyr")
install.packages("ggplot2")
install.packages("shiny")
install_github("StatsWithR/statsr")
install.packages("xlsx")
library(dplyr)
library(ggplot2)



# Keep rows that are NOT "In Honor of" or "In Memory of" - IMO and IHO are not actual donors
FYdataforR <- FYdataforR[!FYdataforR$`Donor Type` %in% c("In Honor of", "In Memory of"), ]



# Identifying Unique Alumni

suid_law_years <- FYdataforR %>%
  filter(!is.na(`Social Class Year Law`), 
         `Social Class Year Law` != 0,
         !is.na(`Donor SUID`)) %>%
  select(`Donor SUID`, `Social Class Year Law`) %>%
  distinct()

# Count by year
year_counts <- suid_law_years %>%
  count(`Social Class Year Law`, name = "count_suids")

# Get quartile values
quartiles <- quantile(suid_law_years$`Social Class Year Law`, 
                      probs = c(0.25, 0.5, 0.75), 
                      na.rm = TRUE)

# Create plot with quartile lines, want to see total counts per class year
ggplot(year_counts, aes(x = `Social Class Year Law`, y = count_suids)) +
  geom_col(fill = "steelblue") +
  geom_vline(xintercept = quartiles, color = "red", linetype = "dashed", linewidth = 1) +
  geom_text(aes(label = count_suids), vjust = -0.5, size = 3) +
  annotate("text", x = quartiles, y = Inf, 
           label = c("Q1", "Median", "Q3"), 
           vjust = 2, color = "red", fontface = "bold") +
  labs(
    title = "Donors by Social Class Year",
    subtitle = "Median SCY is 1992",
    x = "Social Class Year Law",
    y = "Count of Unique SUIDs"
  ) +
  theme_minimal()


#Next, we want to see the time since graduating for each gift

#ID if a gift is from a Law School Alum

FYdataforR <- FYdataforR %>%
  mutate(
    Alumni_Gifts = ifelse(!is.na(`Social Class Year Law`) & 
`Social Class Year Law` != 0 & 
`Social Class Year Law` != "", 
"Alumni Gift", 
"Non-Alumni Gift"),
    years_since_graduation = ifelse(Alumni_Gifts == "Alumni Gift",
`Gift Date Fiscal Year` - `Social Class Year Law`, NA))

#Filter for alumni only gifts and find the median

alumni_years <- FYdataforR %>%
  filter(Alumni_Gifts == "Alumni Gift",
         !is.na(years_since_graduation),
         years_since_graduation >= 0) %>%
  pull(years_since_graduation)

median_value <- median(alumni_years, na.rm = TRUE)

#Graphing the years since graduation

# Create histogram
h <- hist(alumni_years,
  breaks = 50,
  main = "Distribution of Alumni Gifts by Years Since Graduation",
  xlab = "Years Since Law School Graduation",
  ylab = "Count",
  col = "steelblue",
  ylim = c(0, max(hist(alumni_years, breaks = 50, plot = FALSE)$counts) * 1.15))

# labels
text(x = h$mids,
     y = h$counts,
     labels = ifelse(h$counts > 0, h$counts, ""),
     pos = 3,
     cex = 0.7)
abline(v = median_value, col = "red", lty = 2, lwd = 2)
text(x = median_value, 
     y = max(h$counts) * 0.95,
     label = paste0("Median: ", round(median_value, 1), " years"),
     col = "red",
     pos = 4,
     font = 2)


#Identifying the most consistent donor Households by their donation streaks aka loyalty scores, regardless of multiple gifts in a year

# Longest Giving Streak per HH, show the top 20

loyalty_score <- FYdataforR %>%
  filter(!is.na(`Primary Contact SUID`), !is.na(`Gift Date Fiscal Year`)) %>%
  select(`Primary Contact SUID`, `Gift Date Fiscal Year`) %>%
  distinct() %>%
  arrange(`Primary Contact SUID`, `Gift Date Fiscal Year`) %>%
  group_by(`Primary Contact SUID`) %>%
  mutate(
    year_gap = `Gift Date Fiscal Year` - lag(`Gift Date Fiscal Year`, default = `Gift Date Fiscal Year`[1] - 2),
    streak_group = cumsum(year_gap != 1)
  ) %>%
  group_by(`Primary Contact SUID`, streak_group) %>%
  summarize(
    consecutive_years = n(),
    streak_start = min(`Gift Date Fiscal Year`),
    streak_end = max(`Gift Date Fiscal Year`),
    .groups = 'drop'
  ) %>%
  group_by(`Primary Contact SUID`) %>%
  slice_max(consecutive_years, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(consecutive_years)) %>%
  head(20) %>%
  select(`Primary Contact SUID`, consecutive_years, streak_start, streak_end)

loyalty_score


#Display the count of Gift Totals, using a logarithmic scale for visibility, break them into different giving levels

#Create the 3 different donor levels
gift_totals <- FYdataforR %>%
  filter(!is.na(`Receipt Number`), 
         !is.na(`Linked Donor Amount`),
         `Linked Donor Amount` > 0) %>%
  group_by(`Receipt Number`) %>%
  summarize(gift_total = sum(`Linked Donor Amount`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    donor_category = case_when(
      gift_total < 1000 ~ "Donors (<$1,000)",
      gift_total >= 1000 & gift_total < 10000 ~ "Leadership Donors ($1K-$10K)",
      gift_total >= 10000 ~ "Dean's Circle Donors ($10K+)",
      TRUE ~ NA_character_
    ),
    donor_category = factor(donor_category, 
                            levels = c("Donors (<$1,000)", 
                                       "Leadership Donors ($1K-$10K)", 
                                       "Dean's Circle Donors ($10K+)"))
  )

#Plot details, going for legibility
ggplot(gift_totals, aes(x = gift_total)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_log10(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::comma) +
  geom_vline(xintercept = 1000, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 10000, color = "darkred", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 500, y = Inf, label = "Donors", 
           vjust = 2.5, hjust = 1, color = "red", size = 4) +
  annotate("text", x = 3000, y = Inf, label = "Leadership", 
           vjust = 0.75, hjust = 0.5, color = "red", size = 4) +
  annotate("text", x = 30000, y = Inf, label = "Dean's Circle", 
           vjust = 1.5, hjust = 0.1, color = "darkred", size = 4) +
  labs(
    title = "Distribution of Gift Amounts",
    x = "Gift Amount",
    y = "Count of Gifts"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold")
  )

