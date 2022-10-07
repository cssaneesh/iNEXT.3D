
# install.packages----
# install.packages("remotes")
# remotes::install_github("KaiHsiangHu/iNEXT.3D")

library(tidyverse)
library(patchwork)
library(iNEXT.3D)
library(viridis)
# Data----
transect_prep_iNext  <- read.csv('https://raw.githubusercontent.com/cssaneesh/iNEXT.3D/main/transect_prep_iNext.csv')

transect_prep_iNext <- transect_prep_iNext %>%
  mutate(Presence = as.numeric(1)) %>% 
  mutate(Group = factor(Group)) %>% # to order Groups while ploting
  mutate(Group = fct_relevel(Group, c("Group-i","Group-ii","Group-iii")))

# First list
transect.list <- transect_prep_iNext %>%
  split(.$Group)
# Second list
transect.matrix.list <- purrr::map(
  transect.list,
  ~ .x %>%
    select(Species, Site, Presence) %>%
    distinct() %>%
    spread(key = Site, value = Presence) %>%
    replace(is.na(.), 0) %>%
    column_to_rownames(var = "Species")
)

#  Taxonomic diversity-----
TD_treat_out <-
  iNEXT3D(
    data = transect.matrix.list,
    diversity = 'TD',
    q = c(0, 1, 2),
    datatype = 'incidence_raw',
    size = c(1:40),
    nboot = 0
  )

TD_treat_out$DataInfo
# Assemblage = the Group or groups
# T = Reference sample size
# U = Total number of incidents
# S.obs = Observed Species richness
# SC = Sample coverage

TD_treat_out$AsyEst # to see the asymptotic diversity estimates

# Make data frame for ggplot----
transect.TD.df <- TD_treat_out %>%
  purrr::pluck("iNextEst", "size_based")

transect_info <- transect_prep_iNext %>%
  distinct() %>% mutate(Assemblage = as.character(Group))

transect.hill.TD <- transect.TD.df %>% left_join(transect_info) %>%
  mutate(Order.q  = case_when(Order.q  == "0" ~ "q = 0", # q=0 Species richness
                              Order.q == "1" ~ "q = 1", # q=1 Shannon diversity
                              Order.q == "2" ~ "q = 2")) %>% # q=2 Simpson diversity or evenness
  filter(!Order.q == "q = 1")

df.point <-
  transect.hill.TD[which(transect.hill.TD$Method == "Observed"), ]
df.line <-
  transect.hill.TD[which(transect.hill.TD$Method != "Observed"), ]
df.line$Method <- factor(df.line$Method,
                         c("Rarefaction", "Extrapolation"))

# Plot----
transect.TD.fig_op1 <- ggplot(transect.hill.TD ,
                              aes(x = nt, y = qD,   color = Group)) +
  facet_wrap( ~ Order.q) +
  geom_point(aes(),
             shape = 1,
             size = 2,
             data = df.point) +
  geom_line(aes(linetype = Method), lwd = 0.75, data = df.line) +
  labs(
    x = "Number of sampling units",
    y = "Taxonomic diversity",
    title =
      "Sample - based diversity accumulation ") +
  scale_color_manual(values = c(
    "Group-i" = "red",
    "Group-ii" = "blue",
    "Group-iii" = "green"
  )) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size = 8)) +
  guides(col = guide_legend(ncol = 15)) +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(
    panel.grid.major = element_line(colour = "gray86", size = 0.1),
    panel.background = element_rect(fill = "white")
  )


plot <-   transect.TD.fig_op1 +
  theme(plot.caption = element_text(size = 8, face = "italic",
                                    hjust = 0.5)) +
  theme(legend.position = 'bottom', )  +
  theme(legend.background = element_rect(fill = NA))

plot+ 
  theme(legend.position = c(0.9, 0.7)) # you can over write this.

transect_prep_iNext  %>%
  group_by(Group) %>%
  distinct(Species) %>%
  summarise(Species = n()) # note the number of Species and run the following code

TD_treat_out$DataInfo # S.obs= Observed Species richness will be the same, this is shown on the y axis of q=0 

TD_treat_out$AsyEst # to see the asymptotic diversity estimates note the Simpson diversity and compare with q=2

