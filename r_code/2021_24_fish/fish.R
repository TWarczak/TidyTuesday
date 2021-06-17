library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(extrafont)
library(colorspace)
library(showtext)
library(lubridate)
library(ggimage)
library(gganimate)
library(gifski)
library(png)
library(timetk)

tuesdata <- tidytuesdayR::tt_load(2021, 24)
readme(tuesdata)

fishing <- tuesdata$fishing
stocked <- tuesdata$stocked

# Consolidate the species into families
fish_type_tbl <- fishing %>%
  mutate(Fish = ifelse(species %in% c("Alewife",
                                      "Cisco",
                                      "Cisco and Chubs",
                                      "Chubs",
                                      "Cisco and chubs",
                                      "Cisco and Chub",
                                      "Herring",
                                      "Gizzard Shad"), "Herring",
                       ifelse(species %in% c("Rock Bass",
                                             "Rock Bass and Crappie",
                                             "Smallmouth Bass",
                                             "Sunfish",
                                             "White Bass",
                                             "White bass",
                                             "White Perch",
                                             "Crappie",
                                             "Crappies"), "Bass",
                              ifelse(species %in% c("Yellow Perch",
                                                    "Blue Pike",
                                                    "Bowfin",
                                                    "Sauger",
                                                    "Walleye",
                                                    "Walleye and Blue Pike",
                                                    "Northern Pike"), "Perch/Pike",
                                     ifelse(species %in% c("Lake Trout",
                                                           "Lake Trout - siscowet"), "Trout",
                                            ifelse(species %in% c("Round Whitefish",
                                                                  "Lake Whitefish",
                                                                  "Pacific Salmon",
                                                                  "Pacific salmon",
                                                                  "Chinook Salmon",
                                                                  "Coho Salmon"), "Salmon",
                                                   ifelse(species %in% c("American Eel",
                                                                         "Amercian Eel"), "Eel",
                                                          ifelse(species %in% c("Channel catfish",
                                                                                "Channel Catfish",
                                                                                "Channel Catfish and Bullheads",
                                                                                "Bullhead",
                                                                                "Bullheads"), "Catfish",
                                                                 ifelse(species %in% c("Buffalo",
                                                                                       "Quillback",
                                                                                       "Suckers"), "Sucker",
                                                                        ifelse(species %in% c("Carp",
                                                                                              "Minnows",
                                                                                              "Goldfish"), "Carp",
                                                                               ifelse(species %in% c("Drum",
                                                                                                     "Freshwater Drum",
                                                                                                     "Sheepshead"), "Drum",
                                                                                      ifelse(species %in% c("Rainbow Smelt"), "Smelt",
                                                                                             ifelse(species %in% c("Lake Sturgeon"), "Sturgeon",
                                                                                                    ifelse(species %in% c("Burbot"), "Burbot",
                                                                                                           NA ))))))))))))))
# Need to consolidate production but remove redundant production
fish_type_sum_tbl <- fish_type_tbl %>%
                     filter(region %in% c("U.S. Total",
                                          "Canada",
                                          "Total Canada (ONT)",
                                          "U.S. Total (NY)",
                                          "U.S. Total (MI)",
                                          "MI State Total")) %>%
                     mutate(year = as.factor(year)) %>%
                     group_by(Fish, year) %>%
                     replace_na(list(values = 0, grand_total = 0)) %>%
                     summarise(total_val = sum(values))

# Was thinking about using some other metric like percent_production.  Didn't use in end
fish_type_sum_tbl_2 <- fish_type_sum_tbl %>%
                       mutate(year_date = ymd(year, truncated = 2L)) %>%
                       group_by(year) %>%
                       mutate(total_val_year = sum(total_val),
                              perc_prod = round(total_val/total_val_year, digits = 2)) %>%
                       ungroup()

# pad empty data with 0s and filter out the lowest producing fish
fish_type_sum_tbl_3 <- fish_type_sum_tbl_2 %>%
                       group_by(Fish) %>%
                       timetk::pad_by_time(.date_var = year_date,
                                           .by = "year",
                                           .pad_value = 0,
                                           .start_date = "1867-01-01",
                                           .end_date = "2015-01-01") %>%
                       filter(!(Fish %in% c("Eel", "Burbot", "Catfish", "Drum")))
# facet plot EDA
ggplot(fish_type_sum_tbl_2, aes(x     = year_date,
                                y     = total_val,
                                color = Fish)) +
  geom_line() +
  facet_wrap(~Fish) +
  theme(text = element_text(family = "Literata"))

#rolling averages to smooth out the animation
rolling_periods <- c(10,25)

# rolling averages and only plot 1915-2015
fish_practice <- fish_type_sum_tbl_3 %>%
                 filter(Fish %in% c("Salmon", "Trout", "Herring", "Perch/Pike", "Carp", "Smelt")) %>%
                 group_by(Fish) %>%
                 tk_augment_slidify(.value   = total_val,
                                    .f       = ~ mean(.x, na.rm = TRUE),
                                    .period  = rolling_periods,
                                    .partial = TRUE,
                                    .align   = 'right') %>%
                 filter_by_time(.date_var = year_date,
                                .start_date = "1915-01-01",
                                .end_date = "2015-01-01") %>%
                 rename(`10 Year` = total_val_roll_10,
                        `25 Year` = total_val_roll_25)

# pivot_longer the different rolling averages
fish_practice_long <- fish_practice %>%
                      pivot_longer(cols      = c(`10 Year`,`25 Year`),
                                   names_to  = "Rolling Average",
                                   values_to = "production")

# main plot for GIF
fish_plot <- ggplot(fish_practice_long, aes(x     = year_date,
                                            y     = production,
                                            color = Fish)) +
             geom_line(aes(linetype = `Rolling Average`),
                       alpha = 0.8) +
             geom_image(data  = . %>% filter(`Rolling Average` == "10 Year"),
                        image = "images/2021_24_fish/fish2.png") +
             geom_image(data  = . %>% filter(`Rolling Average` == "25 Year"),
                        image = "images/2021_24_fish/fish2.png") +
             transition_reveal(year_date) +
             theme_minimal() +
             theme() +
             theme(plot.background    = element_rect(fill = "#168aad"),
                   line               = element_line(color = "white"),
                   text               = element_text(color = "white",
                                                     family = "Literata"),
                   panel.grid.minor   = element_blank(),
                   panel.grid.major.x = element_blank(),
                   panel.grid.major.y = element_blank(),
                   axis.title.y       = element_text(color = "white",
                                                     size = 12,
                                                     face = "bold"),
                   axis.title.x       = element_blank(),
                   axis.text.y        = element_text(color = "#e5d8bd",
                                                     size = 11,
                                                     angle = 90,
                                                     vjust = -1.5,
                                                     hjust = 0.5,
                                                     face = "bold"),
                   axis.text.x        = element_text(color = "#e5d8bd",
                                                     size = 13),
                   legend.position    = "bottom") +
             geom_hline(yintercept = c(0,25000,50000),
                        color      = "#e5d8bd",
                        size       = 0.5,
                        alpha      = 0.2) +
             labs(title   = "Great Lakes Commercial Fishing: Yearly Catch",
                  caption = "Graphic: Todd Warczak | Source: Great Lakes Fishery Commission",
                  y       = 'Total Weight of Fish Caught (lbs)') +
             scale_y_continuous(limits = c(0, 55000),
                                breaks = c(0,25000,50000),
                                expand = c(0, 0)) +
             scale_x_date(expand = c(0, 0)) +
             scale_color_todd()

animate(fish_plot, duration = 10, fps = 10, width = 1600, height = 1400, res = 200,
        renderer = gifski_renderer())
anim_save("fish2.gif")

# line colors
scale_color_todd <- function(...){
  library(scales)
  discrete_scale("color","todd",
                 manual_pal(values=c("#ffbd00","#ffffcc","#9de25f",
                                     "#d708fb","#b7b7a4","#f94144")), ...)
}


# Extra pretty plot for twitter banner
ggplot(fish_practice_long) +
  geom_line(aes(x = year_date, y = production, color = Fish),
            size = 2.6, linetype = 1, show.legend = FALSE) +
  geom_line(aes(x = year_date, y = production, color = Fish),
            size = 1, linetype = 3, show.legend = FALSE) +
  coord_cartesian() +
  theme_void() +
  theme(plot.background = element_rect(fill = "#168aad")) +
  scale_color_todd()

ggsave("roll_avg4.png", width = 40, height = 13.5, dpi = 600)
ggsave("roll_avg.png", width = 40, height = 40, dpi = 400)


# blues <-  colorRampPalette(c("#a9d6e5", "#012a4a"))(100)
# g <- rasterGrob(blues, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
#
# ggplot(fish_practice_long) +
#   geom_line(aes(x = year_date, y = production, color = Fish),
#             size = 0.8, linetype = 1, show.legend = FALSE) +
#   geom_line(aes(x = year_date, y = production, color = Fish),
#             size = 0.8, linetype = 1, show.legend = FALSE) +
#   theme_void() +
#   theme(plot.background = element_blank()) +
#   scale_color_todd()
# ggsave("roll_avg.png", plot = p, width = 20, height = 40, dpi = 600)
#
#
# png("roll_avg.png", width = 200, height = 200, res = "retina")
# grid.newpage()
# grid.draw(g)
# print(p, newpage = FALSE)
# dev.off()
