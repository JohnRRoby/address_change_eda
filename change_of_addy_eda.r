library(tidyverse)

# this is restricted to year 2023 as proof of concept. specify class type to keep leading 0s in ZIP codes.

y2023 <- read.csv("y2023_for_eda.csv", colClasses = c(zipcode = "character"), stringsAsFactors = FALSE)
str(y2023)
head(y2023)

# clean up city capitalization

y2023$city <- str_to_title(y2023$city)


# remove not-states. shld be 51 (inc. DC) and hackily recode the month. This works only because I know there's just 2 values


length(unique(y2023$state))

y2023 <- filter(y2023, state != "AA" & state != "VI" & state != "AE" & state != "AP" & state != "GU" & state != "MP" & state != "PR")

length(unique(y2023$state))
head(y2023)

unique(y2023$month)

y2023$month <- ifelse(y2023$month == 202301, "Jan.", "Feb.")

unique(y2023$month)


str(y2023)
head(y2023)


# Yes, we can easily calculate net change by ZIP codes or cities, monthly, annually, and/or over time.

y2023 %>%
    group_by(zipcode, city, state) %>%
    summarize(
    net_change = sum(total_to_less_biz) - sum(total_from_less_biz)
    ) %>%
    arrange(desc(net_change)) %>%
    head(25)


