setwd("C:/Users/bensc/OneDrive/Documents/Empirical laws, old and new, in the nutrition transition/")
library(tidyverse)
library(ggrepel)
library(gganimate)

shape_vec <- c(21:24, 4)
point_size <- 1.5
smallPoint_size <- 1
label_size <- 2.5
smallLabel_size <- 2
title_size <- 8
subtitle_size <- 7
legendText_size <- 7
axisText_size <- 6
axisTitle_size <- 7
facetTitle_size <- 7

color_vec <- c("#E073C3", "#8E8AD9", "#DCDE55", "#7EB3D0", "#89E2C9")
#==========================================================================
# Highlight some countries in plot
# label_Africa <- c("Tanzania", "Uganda", "Ethiopia", "South Africa",
#                   "Ghana")
# label_NAfricaWAsia <- c("Turkey", "Saudi Arabia")
# label_Asia <- c("India", "Iran", "China, mainland", "Vietnam", "Japan",
#                 "South Korea")
# label_NAmEurAusNZ <- c("Italy", "France", "Australia")
# label_LAC <- c("Nicaragua", "Colombia", "Brazil")
# labelThese_vec <- c(label_Africa, label_NAfricaWAsia, label_Asia,
#                     label_NAmEurAusNZ, label_LAC)
label_Africa <- c("Tanzania", "South Africa")
label_NAfricaWAsia <- c("Turkey")
label_Asia <- c("India", "China, mainland")
label_NAmEurAusNZ <- c("France")
label_LAC <- c("Brazil")
labelThese_vec <- c(label_NAmEurAusNZ, label_NAfricaWAsia, label_Asia, label_LAC, label_Africa)


#============================================================================
#============================================================================
#============================================================================
#============================================================================
#============================================================================
# Create animation of Bennett's law
#============================================================================
df_anim <- read.csv("Animation data_Bennets law.csv", stringsAsFactors = F)
df_anim <- df_anim[, c("Area", "Year", "Region",
                       "GDP.capita..USD...logged",
                       "Cereals...starchy.roots....of.diet...logged")]
colnames(df_anim)[4:5] <- c("GDP/capita (USD), logged",
                            "Cereals & starchy roots (% of diet), logged")
#---------------------------------------------------------------------------
df_anim$label_these <- NA
u <- df_anim$Area
df_anim$label_these[which(u %in% labelThese_vec)] <- df_anim$Area[which(u %in% labelThese_vec)]

gg <- ggplot(df_anim, aes(x = `GDP/capita (USD), logged`,
                          y = `Cereals & starchy roots (% of diet), logged`,
                          group = Region, fill = Region,
                          shape = Region,
                          #size = `GDP / capita (USD)`,
                          label = label_these))
gg <- gg + geom_smooth(aes(group = NULL, fill = NULL, shape = NULL), method = lm, se = F)
gg <- gg + geom_point(alpha = 0.8, size = 2.5, color = "black", stroke = 0.5)
gg <- gg + scale_fill_manual(values = color_vec)
gg <- gg + scale_shape_manual(values = shape_vec)
gg <- gg + geom_text_repel(color = "black", size = 4,
                           point.padding = 2,
                           box.padding = 1,
                           nudge_x = 1.5,
                           nudge_y = 1)#, size = label_size)
gg <- gg + labs(title = "Year: {frame_time}",
                subtitle = "Bennett's law, animated")
gg <- gg + guides(fill = guide_legend(nrow = 2, byrow = T, override.aes = list(linetype = 0)),
                  color = guide_legend(override.aes = list(linetype = 0)),
                  shape = guide_legend(override.aes = list(linetype = 0)))
gg <- gg + theme(legend.position = "bottom",
                 legend.spacing.x = unit(0.25, 'cm'),
                 legend.title = element_blank(),
                 #legend.text = element_text(size = legendText_size),
                 legend.background = element_rect(color = NA))#,
#plot.title = element_text(size = title_size),
#plot.subtitle = element_text(size = subtitle_size),
#axis.title = element_text(size = axisTitle_size),
#axis.text = element_text(size = axisText_size))
gg <- gg + transition_time(Year) + ease_aes("linear")
#gg
gg <- animate(gg, fps = 5)
anim_save("Bennetts law animation.gif", fps = 5)


#===========================================================================
#===========================================================================
#===========================================================================
#===========================================================================
# Create animation of Carbohydrate law
#===========================================================================
#===========================================================================
df_anim <- read.csv("Animation data_Carbohydrate law.csv", stringsAsFactors = F)
df_anim <- df_anim[, c("Area", "Year", "Region",
                       "GDP.capita..USD...logged",
                       "Diet.share......logged")]
colnames(df_anim)[4:5] <- c("GDP/capita (USD), logged",
                            "Carbohydrate diet share (%), logged")
df_anim$label_these <- NA
u <- df_anim$Area
df_anim$label_these[which(u %in% labelThese_vec)] <- df_anim$Area[which(u %in% labelThese_vec)]
#---------------------------------------------------------------------------
gg <- ggplot(df_anim, aes(x = `GDP/capita (USD), logged`,
                          y = `Carbohydrate diet share (%), logged`,
                          group = Region, fill = Region,
                          shape = Region,
                          #size = `GDP / capita (USD)`,
                          label = label_these))
gg <- gg + geom_smooth(aes(group = NULL, fill = NULL, shape = NULL), method = lm, se = F)
gg <- gg + geom_point(alpha = 0.8, size = 2.5, color = "black", stroke = 0.5)
gg <- gg + scale_fill_manual(values = color_vec)
gg <- gg + scale_shape_manual(values = shape_vec)
gg <- gg + geom_text_repel(color = "black", size = 4,
                           point.padding = 2,
                           box.padding = 1,
                           nudge_x = 1.5,
                           nudge_y = 1)#, size = label_size)
gg <- gg + labs(title = "Year: {frame_time}",
                subtitle = "Carbohydrate law, animated")
gg <- gg + guides(fill = guide_legend(nrow = 2, byrow = T, override.aes = list(linetype = 0)),
                  color = guide_legend(override.aes = list(linetype = 0)),
                  shape = guide_legend(override.aes = list(linetype = 0)))
gg <- gg + theme(legend.position = "bottom",
                 legend.spacing.x = unit(0.25, 'cm'),
                 legend.title = element_blank(),
                 #legend.text = element_text(size = legendText_size),
                 legend.background = element_rect(color = NA))#,
#plot.title = element_text(size = title_size),
#plot.subtitle = element_text(size = subtitle_size),
#axis.title = element_text(size = axisTitle_size),
#axis.text = element_text(size = axisText_size))
gg <- gg + transition_time(Year) + ease_aes("linear")
#gg
gg <- animate(gg, fps = 5)
anim_save("Carbohydrate law animation.gif")

#===========================================================================
#===========================================================================
#===========================================================================
#===========================================================================
# Create animation of Fat-carb frontier
#===========================================================================
#===========================================================================
df_anim <- read.csv("Animation data_Fat carb frontier.csv", stringsAsFactors = F)
df_anim <- df_anim[, c("Area", "Year", "Region",
                       "GDP.capita..USD.",
                       "Carbohydrate....of.diet.",
                       "Fat....of.diet.")]
colnames(df_anim)[4:6] <- c("GDP/capita (USD)",
                            "Carbohydrate (% of diet)",
                            "Fat (% of diet)")
df_anim$label_these <- NA
u <- df_anim$Area
df_anim$label_these[which(u %in% labelThese_vec)] <- df_anim$Area[which(u %in% labelThese_vec)]
#---------------------------------------------------------------------------

gg <- ggplot(df_anim, aes(x = `Carbohydrate (% of diet)`,
                          y = `Fat (% of diet)`,
                          group = Region, fill = Region,
                          shape = Region,
                          size = `GDP/capita (USD)`,
                          label = label_these))
gg <- gg + geom_smooth(aes(group = NULL, fill = NULL, shape = NULL), method = lm, se = F)
gg <- gg + geom_point(alpha = 0.8, color = "black", stroke = 0.5)
gg <- gg + scale_fill_manual(values = color_vec)
gg <- gg + scale_shape_manual(values = shape_vec)
gg <- gg + geom_text_repel(color = "black", size = 4,
                           point.padding = 2,
                           box.padding = 1,
                           nudge_x = 1.5,
                           nudge_y = 1)#, size = label_size)
gg <- gg + labs(title = "Year: {frame_time}",
                subtitle = "Fat-carb frontier, animated\n(Point size corresponds to GDP/capita)")
gg <- gg + theme(legend.position = "bottom",
                 legend.spacing.x = unit(0.25, 'cm'),
                 legend.title = element_blank(),
                 #legend.text = element_text(size = legendText_size),
                 legend.background = element_rect(color = NA))#,
#plot.title = element_text(size = title_size),
#plot.subtitle = element_text(size = subtitle_size),
#axis.title = element_text(size = axisTitle_size),
#axis.text = element_text(size = axisText_size))
gg <- gg + guides(fill = guide_legend(nrow = 2, byrow = T, override.aes = list(linetype = 0)),
                  color = guide_legend(override.aes = list(linetype = 0)),
                  shape = guide_legend(override.aes = list(linetype = 0)),
                  size = "none")
gg <- gg + transition_time(Year) + ease_aes("linear")
#gg
gg <- animate(gg, fps = 5)
anim_save("Fat carb frontier animation.gif")

#===========================================================================
#===========================================================================
#===========================================================================
#===========================================================================
# Create animation of Engel's law
#===========================================================================
#===========================================================================
df_anim <- read.csv("Animation data_Engels law.csv", stringsAsFactors = F)
df_anim <- df_anim[, c("Country", "Year", "Region",
                       "GDP.capita..USD...logged",
                       "Food.demand..kcal.capita.day...logged")]
colnames(df_anim)[4:5] <- c("GDP/capita (USD), logged",
                            "Food demand (kcal/capita/day), logged")
df_anim$label_these <- NA
u <- df_anim$Country
df_anim$label_these[which(u %in% labelThese_vec)] <- df_anim$Country[which(u %in% labelThese_vec)]
#---------------------------------------------------------------------------


gg <- ggplot(df_anim, aes(x = `GDP/capita (USD), logged`,
                          y = `Food demand (kcal/capita/day), logged`,
                          group = Region, fill = Region,
                          shape = Region,
                          #size = `GDP / capita (USD)`,
                          label = label_these))
gg <- gg + geom_smooth(aes(group = NULL, fill = NULL, shape = NULL), method = lm, se = F)
gg <- gg + geom_point(alpha = 0.8, size = 2.5, color = "black", stroke = 0.5)
gg <- gg + scale_fill_manual(values = color_vec)
gg <- gg + scale_shape_manual(values = shape_vec)
gg <- gg + geom_text_repel(color = "black", size = 4,
                           point.padding = 2,
                           box.padding = 1,
                           nudge_x = 1,
                           nudge_y = -1)#, size = label_size)
gg <- gg + labs(title = "Year: {frame_time}",
                subtitle = "Engel's law, animated")
gg <- gg + guides(fill = guide_legend(nrow = 2, byrow = T, override.aes = list(linetype = 0)),
                  color = guide_legend(override.aes = list(linetype = 0)),
                  shape = guide_legend(override.aes = list(linetype = 0)))
gg <- gg + theme(legend.position = "bottom",
                 legend.spacing.x = unit(0.25, 'cm'),
                 legend.title = element_blank(),
                 #legend.text = element_text(size = legendText_size),
                 legend.background = element_rect(color = NA))#,
#plot.title = element_text(size = title_size),
#plot.subtitle = element_text(size = subtitle_size),
#axis.title = element_text(size = axisTitle_size),
#axis.text = element_text(size = axisText_size))
gg <- gg + transition_time(Year) + ease_aes("linear")
#gg
gg <- animate(gg, fps = 5)
anim_save("Engels law animation.gif")
