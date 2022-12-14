# install.packages----
# library(devtools)
# install_github("KaiHsiangHu/iNEXT.3D")
# install.packages('tidyverse')
# install.packages('purrr')

# libraries----
library(iNEXT.3D)
library(tidyverse)
library(purrr)

# Data----
transect_prep_iNext  <- read.csv('https://raw.githubusercontent.com/cssaneesh/iNEXT.3D/main/transect_prep_iNext.csv')

# Data dictionary----
# Group= can be different treatments, habitat types, elevations, etc.
# Site= within groups site or plots
# Species= individual species or observation

str(transect_prep_iNext ) # make the grouping variable a factor

transect_prep_iNext <- transect_prep_iNext %>%
  mutate(Presence = as.numeric(1)) %>% 
  mutate(Group = factor(Group)) %>% # to order Groups while making graph
  mutate(Group = fct_relevel(Group, c('Group-i','Group-ii', 'Group-iii')))

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
    size = c(1:250),
    nboot = 0
  )

TD_treat_out$DataInfo
# Assemblage = the Group or groups
# T = Reference sample size
# U = Total number of incidents
# S.obs = Observed Species richness
# SC = Sample coverage

TD_treat_out$AsyEst # to see the asymptotic diversity estimates note the Simpson diversity and compare with q=2

transect_prep_iNext  %>%
  group_by(Group) %>%
  distinct(Species) %>%
  summarise(Species = n()) # note the number of Species and run the following code

TD_treat_out$DataInfo # S.obs= Observed Species richness will be the same, this is shown on the y axis of q=0 

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
ggplot(transect.hill.TD ,
       aes(x = nt, y = qD,   color = Group)) +
  facet_wrap(~ Order.q) +
  geom_point(aes(),
             shape = 1,
             size = 2,
             data = df.point) +
  geom_line(aes(linetype = Method), lwd = 0.75, data = df.line, alpha=0.8) +
  labs(x = "Number of sampling units",
       y = "Taxonomic diversity",
       title =
         "Sample - based diversity accumulation ")
