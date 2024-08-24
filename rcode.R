#### Draft for generate Biscale Map ####
# 1 Set Seed and WD -------------------------------------------------------

set.seed(1234)
WD <- getwd()

# 2 Install and Lib Packages ----------------------------------------------

library(tidyverse)
library(readxl)
library(spdep)
library(ggpubr)
library(biscale)


# 3 Load Data -------------------------------------------------------------

data_fake <- read_excel("Data/Fake data for Emily Ranger project.xlsx") %>%
  mutate(SA3_CODE = as.character(SA3_CODE)) %>%
  mutate(SA3_CODE = replace(SA3_CODE, SA3_CODE == "31095", "31905"),
         # correct some wrong SA3CODE
         SA3_NAME = replace(SA3_NAME,
                            SA3_NAME == "Bowen Basin – North",
                            "Bowen Basin - North"),

         SA3_NAME = replace(SA3_NAME,
                            SA3_NAME == "Cairns – South",
                            "Cairns - South"),
         SA3_NAME = replace(SA3_NAME,
                            SA3_NAME == "Darling Downs – East",
                            "Darling Downs - East"),
         SA3_NAME = replace(SA3_NAME,
                            SA3_NAME == "Outback – South",
                            "Outback - South")
         # correct some wrong SA3NAME
  ) %>%
  rename(SA3_NAME21 = SA3_NAME,
         SA3_CODE21 = SA3_CODE)

SA3_map <- st_read("Maps/SA3_2021_AUST_GDA2020.shp",
                   quiet = TRUE) %>%
  mutate(GCC_ShortName = recode(factor(GCC_NAME21),
                                "Greater Sydney" = "Sydney",
                                "Greater Melbourne" = "Melbourne",
                                "Greater Brisbane" = "Brisbane",
                                "Greater Adelaide" = "Adelaide",
                                "Greater Hobart" = "Hobart",
                                "Greater Perth" = "Perth",
                                "Greater Darwin" = "Darwin",
                                "Australian Capital Territory" = "Canberra")) %>%
  left_join(data_fake)


# 4 Draw Map -------------------------------------------------------------
# Generate bi-class categories variable ----

map_data <- SA3_map %>%
  mutate(Count_cat = factor(ntile(Count, 4),
                            labels = c("Q1 ", "Q2", "Q3", "Q4")),
         MMM_cat = if_else(MMM == 1, NA, MMM),
         MMM_cat = cut(MMM_cat,
                       breaks = c(-Inf, 3, 5, 6, Inf),
                       labels = c("2-3", "4-5", "6", "7"))) %>%
  bi_class(x = Count_cat, y = MMM_cat, dim = 4)

quantile(map_data$Count,
         probs = c(0, 0.25, 0.5, 0.75, 1),
         na.rm = TRUE)


# Function for map ----
FUN_Map_Biscale <- function(Area = NA,
                            Colors = "PinkGrn"){
  Area_Title <- Area
  if(is.na(Area)){
    Area <- c("Sydney","Melbourne", "Brisbane", "Adelaide",
              "Hobart", "Perth", "Darwin", "Canberra",
              "Rest of NSW", "Rest of Vic.", "Rest of Qld",
              "Rest of SA", "Rest of WA", "Rest of Tas.",
              "Rest of NT")
    Area_Title <- ""
  }

  Map <- map_data %>%
    filter(GCC_ShortName %in% Area) %>%
    ggplot() +
    geom_sf(aes(fill = bi_class),
            color = "black",
            show.legend = FALSE) +
    bi_scale_fill(pal = Colors, dim = 4,
                  na.value = NA) +
    labs(title = Area_Title) +
    bi_theme() +
    theme(plot.title = element_text(size = 8,
                                    face = NULL,
                                    family = "sans"))


  legend <- bi_legend(pal = Colors,
                      dim = 4,
                      xlab = "Count",
                      ylab = "MMM",
                      size = 8)
  if(length(Area) > 1){
    Map <- ggarrange(Map, legend,
                     nrow = 1, ncol = 2,
                     widths = c(5,1))
  }

  return(Map)
}

FUN_Map_Uniscale <- function(Area = NA,
                             Colors = "Oranges",
                             Variable = "MMM_cat"){
  Area_Title <- Area
  if(is.na(Area)){
    Area <- c("Sydney","Melbourne", "Brisbane", "Adelaide",
              "Hobart", "Perth", "Darwin", "Canberra",
              "Rest of NSW", "Rest of Vic.", "Rest of Qld",
              "Rest of SA", "Rest of WA", "Rest of Tas.",
              "Rest of NT")
    Area_Title <- ""
  }

  Map <- map_data %>%
    filter(GCC_ShortName %in% Area) %>%
    ggplot() +
    geom_sf(aes(fill = !!sym(Variable)),
            color = "black",
            show.legend = TRUE) +
    scale_fill_brewer(palette = Colors,
                      na.value = NA) +
    labs(title = Area_Title,
         fill = str_pad(str_sub(Variable, end = -5), width =6, "right")) +
    theme_void() +
    theme(plot.title = element_text(size = 8,
                                    face = NULL,
                                    family = "sans"))

  return(Map)
}


# Biscale Map ----

MainMap <- FUN_Map_Biscale(Area = NA, Color = "BlueOr")
ggsave(plot = MainMap,
       filename = str_c("Output/Map_Main.png"),
       height = 16,
       width = 16,
       units = "cm")

# Uni-scale Maps ----
Map_MMM <- FUN_Map_Uniscale(Area = NA, Color = "Blues",
                            Variable = "MMM_cat")
Map_Count <- FUN_Map_Uniscale(Area = NA, Color = "Oranges",
                              Variable = "Count_cat")

ggsave(plot = Map_MMM,
       filename = str_c("Output/Map_MapMMM.png"),
       height = 16,
       width = 16,
       units = "cm")

ggsave(plot = Map_Count,
       filename = str_c("Output/Map_MapCount.png"),
       height = 16,
       width = 16,
       units = "cm")

# End of the code ---------------------------------------------------------

