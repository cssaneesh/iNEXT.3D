# Install packages----
# install.packages("remotes")
# remotes::install_github("KaiHsiangHu/iNEXT.3D")

# Library----
library(tidyverse)
library(patchwork)
library(iNEXT.3D)
library(viridis)
library(skimr)

# Data----
transect_prep_iNext <- data.frame(#Site= as.character(seq(1:200)),
  Treatment= factor(c(rep('Control', 60), 
                      rep('Treatment-1', 40), 
                      rep('Treatment-2', 100))),
  Transect= as.character(seq(1:200)),
  Family=c(rep('Acanthaceae', 20), 
           rep('Amaranthaceae', 20),
           rep('Euphorbiaceae', 10),
           rep('Amaranthaceae', 40),
           rep('Acanthaceae', 10),
           rep('Acanthaceae', 10),
           rep('Asteraceae', 10),
           rep('Caryophyllaceae', 10),
           rep('Commelinaceae', 10),
           rep('Convolvulaceae', 20),
           rep('Amaranthaceae', 10),
           rep('Euphorbiaceae', 10),
           rep('Cyperaceae', 20))) 
names(transect_prep_iNext)

# relevel treatment and add presence of species as numeric
transect_prep_iNext<-transect_prep_iNext %>%  
  mutate(Treatment = fct_relevel(Treatment, c("Control","Treatment-1","Treatment-2"))) %>% 
  mutate(pres= as.numeric(1))

# names(transect_prep_iNext)
# View(transect_prep_iNext)
# glimpse(transect_prep_iNext)

# make lists----

transect.list <- transect_prep_iNext %>%
  split(.$Treatment)

transect.matrix.list <- purrr::map(
  transect.list,
  ~ .x %>%
    select(Family, Transect, pres) %>%
    distinct() %>%
    spread(key = Transect, value = pres) %>%
    replace(is.na(.), 0) %>%
    column_to_rownames(var = "Family")
)

# Taxonomic diversity----
TD_treat_out <-
  iNEXT3D(
    data = transect.matrix.list,
    diversity = 'TD',
    q = c(0, 1, 2),
    datatype = 'incidence_raw', 
    size = c(1:180), # number of sampling units for which diversity be computed.
    nboot = 0
  )

TD_treat_out$DataInfo
# Assemblage = the treament or groups
# T = Reference sample size
# U = Total number of incidents
# S.obs = Observed species richness
# SC = Sample coverage

TD_treat_out$AsyEst # to see the asymptotic diversity estimates

transect.TD.df <- TD_treat_out %>%
  purrr::pluck("iNextEst", "size_based")

transect_info <- transect_prep_iNext %>%
  distinct() %>% mutate(Assemblage = as.character(Treatment))

transect.hill.TD <- transect.TD.df %>% left_join(transect_info) %>%
  mutate(Order.q  = case_when(Order.q  == "0" ~ "q = 0", # q0= Species richness
                              Order.q == "1" ~ "q = 1",
                              Order.q == "2" ~ "q = 2")) %>% # q2= Species eveness
  filter(!Order.q == "q = 1")

df.point <-
  transect.hill.TD[which(transect.hill.TD$Method == "Observed"),]
df.line <-
  transect.hill.TD[which(transect.hill.TD$Method != "Observed"),]
df.line$Method <- factor(df.line$Method,
                         c("Rarefaction", "Extrapolation"))

# Plot----
ggplot(transect.hill.TD ,
       aes(x = nt, y = qD,   color = Treatment)) +
  facet_wrap( ~ Order.q) +
  geom_point(aes(),
             shape = 1,
             size = 2,
             data = df.point) +
  geom_line(aes(linetype = Method), lwd = 0.75, data = df.line) +
  labs(x = "Number of sampling units", y = "Taxonomic diversity", title =
         "Sample - based diversity accumulation ")

# save figure----

# ggsave('Sampling curve.jpg', width = 10, height = 6, dpi = 300, path = "Drive Letter:/Folder/Folder")

transect_prep_iNext  %>% 
  group_by(Treatment) %>% 
  distinct(Family) %>% 
  summarise(Family=n()) # note the number of Family and run the following code

TD_treat_out$DataInfo # S.obs= Observed species richness will be the same, this is the q=0 in the plot

# Enjoy----