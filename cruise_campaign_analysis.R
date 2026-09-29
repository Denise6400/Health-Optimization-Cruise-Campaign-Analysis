# Choose the Mailchimp CSV file
campaigns <- read.csv(
  file.choose(),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Check that it imported correctly
View(campaigns)
names(campaigns)
nrow(campaigns)

# Convert send dates into dates R can compare
campaigns$send_date <- as.Date(campaigns[["Date Sent"]])

# Remove commas and convert counts into numbers
campaigns$deliveries <- as.numeric(
  gsub(",", "", campaigns[["Email deliveries"]])
)

campaigns$clickers <- as.numeric(
  gsub(",", "", campaigns[["Email clicked"]])
)

# Keep campaigns that actually delivered emails
campaigns_clean <- subset(campaigns, deliveries > 0)

# Label campaigns relative to the announcement
campaigns_clean$period <- ifelse(
  campaigns_clean$send_date < as.Date("2026-09-03"),
  "Before announcement",
  "After announcement"
)

# Check the cleaned data
View(campaigns_clean)
table(campaigns_clean$period)

# Add up clickers and deliveries for each period
campaign_summary <- aggregate(
  cbind(clickers, deliveries) ~ period,
  data = campaigns_clean,
  FUN = sum
)

# Calculate the weighted click rate as a percentage
campaign_summary$click_rate_pct <- round(
  campaign_summary$clickers / campaign_summary$deliveries * 100,
  3
)

# Display the results
campaign_summary

# Calculate each period's exact click rate
rates <- with(
  campaign_summary,
  setNames(clickers / deliveries, period)
)

# Calculate the relative percentage increase
click_rate_increase <- (
  rates["After announcement"] / rates["Before announcement"] - 1
) * 100

# Display the result
round(click_rate_increase, 1)

install.packages("ggplot2")

library(ggplot2)

campaign_summary$period <- factor(
  
install.packages("ggplot2", type = "binary")

# Put the periods in chronological order
campaign_summary$period <- factor(
  campaign_summary$period,
  levels = c("Before announcement", "After announcement")
)

# Build the chart
click_chart <- ggplot(
  campaign_summary,
  aes(x = period, y = click_rate_pct, fill = period)
) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(
    aes(label = sprintf("%.3f%%", click_rate_pct)),
    vjust = -0.6,
    size = 5
  ) +
  scale_fill_manual(values = c("#64748B", "#0F766E")) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(
    title = "Cruise Campaign Click Engagement",
    subtitle = "Before and after the September 3, 2026 Dr. Greger announcement",
    x = NULL,
    y = "Weighted email click rate",
    caption = "Includes resends. Date based comparison; does not establish causation."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

# Display the chart
print(click_chart)

library(readxl)

# Select your cruise booking Excel file
bookings <- read_excel(file.choose())

# Check the imported data
View(bookings)
names(bookings)
nrow(bookings)

# Convert booking dates
bookings$booking_date <- as.Date(bookings[["Booking Date"]])

# Count passengers in equal comparison periods
before_count <- sum(
  bookings$booking_date >= as.Date("2026-08-07") &
    bookings$booking_date < as.Date("2026-09-03"),
  na.rm = TRUE
)

after_count <- sum(
  bookings$booking_date >= as.Date("2026-09-03") &
    bookings$booking_date < as.Date("2026-09-30"),
  na.rm = TRUE
)

# Display the comparison
booking_summary <- data.frame(
  period = c("Before announcement", "After announcement"),
  passengers_booked = c(before_count, after_count)
)

booking_summary

booking_summary$period <- factor(
  booking_summary$period,
  levels = c("Before announcement", "After announcement")
)

booking_chart <- ggplot(
  booking_summary,
  aes(x = period, y = passengers_booked, fill = period)
) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = passengers_booked), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("#64748B", "#0F766E")) +
  scale_y_continuous(breaks = 0:6, limits = c(0, 6)) +
  labs(
    title = "Cruise Passenger Bookings",
    subtitle = "Before: Aug. 7–Sept. 2 | After: Sept. 3–29, 2026",
    x = NULL,
    y = "Passengers booked",
    caption = "Data as of Sept. 29. Timing does not establish campaign attribution."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

print(booking_chart)

ggsave(
  "cruise_email_click_rates.png",
  plot = click_chart,
  width = 9, height = 6, dpi = 300,
  bg = "white"
)

ggsave(
  "cruise_passenger_bookings.png",
  plot = booking_chart,
  width = 9, height = 6, dpi = 300,
  bg = "white"
)

# Show where the images were saved
getwd()