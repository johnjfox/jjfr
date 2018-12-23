library(tidyverse)

ral_standard <- read_csv("ral_standard.csv")

cDF <- ral_standard %>%
	select(name=English, rgbStr = RGB, HEX) %>%
	mutate(name = sort(tolower(name))) %>%
	separate(rgbStr,into=c("red","green","blue"),sep="-")

cDF %>% write_csv("colors.csv")
