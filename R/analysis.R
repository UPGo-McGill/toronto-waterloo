#### TORONTO-WATERLOO COMMUTING ANALYSIS #######################################

# Attach packages ---------------------------------------------------------

library(tidyverse)
library(sf)
library(cancensus)
library(readxl)
library(patchwork)
library(ggrepel)


# Import data -------------------------------------------------------------

commutes <- read_xlsx("data/commutes.xlsx", sheet = "Flows")

# cancensus::list_census_regions("CA16") %>% filter(name == "Ontario")
# cancensus::list_census_regions("CA16") %>% filter(name == "Toronto")
# cancensus::list_census_regions("CA16") %>% filter(str_detect(name, "Waterloo"))
# cancensus::list_census_regions("CA16") %>% filter(str_detect(name, "Kitchener"))

CT_all <- get_census("CA16", list(PR = 35), "CT", geo_format = "sf")
PR <- get_census("CA16", list(PR = 35), geo_format = "sf")
CMA <- get_census("CA16", list(CMA = c("35535", "35541")), geo_format = "sf")
cities <- get_census("CA16", list(CSD = c("3520005", "3530013", "3530016")), 
                     geo_format = "sf")


# Wrangle PR, CMA and cities tables ---------------------------------------

PR$label <- "No data available"

CMA <- CMA %>% mutate(name = c("Toronto CMA", 
                               "Kitchener-Cambridge-Waterloo CMA"),
                      label = "CMA boundary")
cities <- cities %>% mutate(name = c("Toronto", "Kitchener", "Waterloo"),
                            label = "City boundary")


# Wrangle CT table --------------------------------------------------------

CT <- 
  CT_all %>% 
  mutate(CMA = case_when(
    CMA_UID == "35535" ~ "Toronto", 
    CMA_UID == "35541" ~ "Kitchener-Cambridge-Waterloo",
    TRUE               ~ "Other"),
         city = case_when(
           CSD_UID == "3520005" ~ "Toronto", 
           CSD_UID %in% c("3530013", "3530016") ~ "Kitchener-Waterloo",
           TRUE ~ "Other")
    )

CT_all <- summarize(CT_all) %>% mutate(label = "No commuters")


# Wrangle commutes table --------------------------------------------------

# Destination CMA and city
commutes <- 
  CT %>% 
  st_drop_geometry() %>% 
  select(GeoUID, CMA, city) %>% 
  left_join(commutes, ., by = c("To" = "GeoUID")) %>% 
  mutate(dest_CMA = CMA, dest_city = city) %>% 
  select(-CMA, -city)

# Remove rows with missing data
commutes <- 
  commutes %>% 
  filter(!is.na(dest_CMA))

# CMA destinations
commutes_CMA <- 
  commutes %>% 
  group_by(From, dest_CMA) %>% 
  summarize(total_commute = sum(
    `Total - Industry - North American Industry Classification System (NAICS) 2007`
  )) %>% 
  mutate(percent_commute = total_commute / sum(total_commute)) %>% 
  ungroup()

commutes_CMA <-
  commutes_CMA %>% 
  left_join(select(CT, GeoUID, geometry), by = c("From" = "GeoUID")) %>% 
  st_as_sf()

# City destinations
commutes_city <- 
  commutes %>% 
  group_by(From, dest_city) %>% 
  summarize(total_commute = sum(
    `Total - Industry - North American Industry Classification System (NAICS) 2007`
  )) %>% 
  mutate(percent_commute = total_commute / sum(total_commute)) %>% 
  ungroup()

commutes_city <-
  commutes_city %>% 
  left_join(select(CT, GeoUID, geometry), by = c("From" = "GeoUID")) %>% 
  st_as_sf()


# Labelling and highlighting setup ----------------------------------------

highlight <- 
  rbind(select(cities, name, geometry), select(CMA, name, geometry)) %>% 
  mutate(
    label = "Destination",
    id = c(
      "D. City of Toronto",
      "C. Cities of Kitchener and Waterloo",
      "C. Cities of Kitchener and Waterloo",
      "B. Toronto CMA",
      "A. Kitchener-Cambridge-Waterloo CMA"),
    x = st_coordinates(st_centroid(geometry))[,1],
    y = st_coordinates(st_centroid(geometry))[,2]) %>% 
  as_tibble() %>% 
  st_as_sf()

# Replace centroid with vertex for CMAs
new_TO <- 
  st_coordinates(CMA[1,]$geometry)[which(
    st_coordinates(CMA[1,]$geometry)[,2] == 
      max(st_coordinates(CMA[1,]$geometry)[,2])), 1:2]

new_KCW <- 
  st_coordinates(CMA[2,]$geometry)[which(
    st_coordinates(CMA[2,]$geometry)[,2] == 
      max(st_coordinates(CMA[2,]$geometry)[,2])), 1:2]

highlight[4,]$x <- new_TO[[1]]
highlight[4,]$y <- new_TO[[2]]
highlight[5,]$x <- new_KCW[[1]]
highlight[5,]$y <- new_KCW[[2]]


# Map template ------------------------------------------------------------

map_template <- 
  function(title, city, nudge) {
    list(
      geom_sf(aes(colour = label), data = PR, fill = "grey90"),
      
      geom_sf(aes(colour = label), data = CT_all, fill = "grey70"),
      
      geom_sf(aes(fill = percent_commute), colour = "transparent"),
      
      geom_sf(aes(colour = label), data = CMA, fill = "transparent"),
      
      geom_sf(aes(colour = label), data = cities, fill = "transparent",
              linetype = "dashed"),
      
      geom_sf(aes(colour = label), 
              data = filter(highlight, id == title), 
              linetype = if (city) "dashed" else "solid",
              fill = "transparent"),
      
      geom_text_repel(
        aes(x = x, y = y, label = name),
        filter(highlight, id == title),
        nudge_x = nudge[[1]],
        nudge_y = nudge[[2]],
        point.padding = 0.5,
        box.padding = 0.5
        ),
      
      scale_fill_viridis_c(name = "Commute share",
                           option = "D", 
                           labels = scales::percent,
                           guide = guide_colourbar(order = 1)),
      
      scale_colour_manual(name = "Other",
                          values = c("No data available" = "transparent",
                                     "No commuters" = "transparent",
                                     "CMA boundary" = "black",
                                     "City boundary" = "black",
                                     "Destination" = "red"
                                     ),
                          breaks = c("No commuters", "No data available",
                                     "City boundary", "CMA boundary",
                                     "Destination"),
                          guide = guide_legend(
                            order = 2,
                            title = NULL,
                            override.aes = list(
                              fill = c("grey70", "grey90", "transparent", 
                                       "transparent", "transparent"),
                              linetype = c("solid", "solid", "dashed", "solid",
                                           "solid")
                              ))),
      
      coord_sf(xlim = c(-80.9, -78.4), ylim = c(42.9, 44.6), expand = FALSE),
      
      ggtitle(title),
      
      theme_void(),
      
      theme(panel.border = element_rect(fill = "transparent", colour = "black"),
            plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
    )
}
  

# Maps --------------------------------------------------------------------

# KW CMA
KW_CMA <-
  commutes_CMA %>% 
  filter(dest_CMA == "Kitchener-Cambridge-Waterloo") %>% 
  ggplot() +
  map_template("A. Kitchener-Cambridge-Waterloo CMA", FALSE, list(-.1, .55))

# Toronto CMA
Toronto_CMA <-
  commutes_CMA %>% 
  filter(dest_CMA == "Toronto") %>% 
  ggplot() +
  map_template("B. Toronto CMA", FALSE, list(.3, 0)) +
  theme(legend.position = "none")

# KW city
KW_city <-
  commutes_city %>% 
  filter(dest_city == "Kitchener-Waterloo") %>% 
  ggplot() +
  map_template("C. Cities of Kitchener and Waterloo", TRUE, list(c(0, 0),
                                                                 c(-.3, .35))) +
  theme(legend.position = "none")

# Toronto city
Toronto_city <-
  commutes_city %>% 
  filter(dest_city == "Toronto") %>% 
  ggplot() +
  map_template("D. City of Toronto", TRUE, list(.3, -.15)) +
  theme(legend.position = "none")

figure <- 
  KW_CMA + Toronto_CMA + 
  KW_city + Toronto_city +
  plot_annotation(
    title = 
      "2016 daily commuting in Southern Ontario by city/region of destination",
    caption = "Data: Statistics Canada custom tabulation from 2016 Census",
    theme = theme(plot.title = element_text(size = 18, face = "bold"))) + 
  plot_layout(guides = "collect") 

ggsave("figure.png", figure, dpi = 400, width = 12, height = 11)
