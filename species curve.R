# Install packages----
# install.packages("remotes")
# remotes::install_github("KaiHsiangHu/iNEXT.3D")

# Library----
library(tidyverse)
library(patchwork)
library(iNEXT.3D)
library(viridis)

# Data----
fake_data <- data.frame(
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

names(fake_data)
View(fake_data)

# relevel treatment and add presence of species as numeric
transect_prep_iNext <- fake_data %>%
  mutate(Treatment = fct_relevel(Treatment, c("Control", "Treatment-1", "Treatment-2"))) %>%
  mutate(presence = as.numeric(1))

levels(transect_prep_iNext$Treatment)

# View(transect_prep_iNext)
# glimpse(transect_prep_iNext)

# make a list----

transect.list <- transect_prep_iNext %>%
  split(.$Treatment) # a list of data frames based on the Treatment

transect.matrix.list <- purrr::map(
  transect.list,
  ~ .x %>%
    select(Family, Transect, presence) %>%
    distinct() %>%
    spread(key = Transect, value = presence) %>%
    replace(is.na(.), 0) %>%
    column_to_rownames(var = "Family")
)

# Now, a list of matrices from a list of data frames (Treatment), 
# where each matrix represenceents the presence of certain species in 
# different transects.

transect.matrix.list <- purrr::map(
  transect.list, # Iterate over each element in the transect.list
  ~ .x %>% # do the following operations to each data frame (.x)
    select(Family, Transect, presence) %>% # Select only Family, Transect, and presence
    distinct() %>% # if there are duplicates, remove duplicate rows
    spread(key = Transect, value = presence) %>% # Spread the presence values across Transect columns
    replace(is.na(.), 0) %>% # replace missing values (NA) with 0
    column_to_rownames(var = "Family") # make the Family column as row names
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

TD_treat_out$TDInfo
# Assemblage = the treatment or groups
# T = Reference sample size
# U = Total number of incidents
# S.obs = Observed species richness
# SC = Sample coverage

TD_treat_out$TDAsyEst # to see the asymptotic diversity estimates

names(TD_treat_out)

TD_treat_out$TDiNextEst$coverage_based

transect.TD.df <- TD_treat_out %>%
  purrr::pluck("TDiNextEst", 
               "size_based", # also known as sample-size based
               # 'coverage_based'
               )
# read about types of extrapolation: https://cran.r-project.org/web/packages/iNEXT/vignettes/Introduction.pdf

transect_info <- transect_prep_iNext %>%
  distinct() %>% mutate(Assemblage = as.character(Treatment))

names(transect.hill.TD)

transect.hill.TD <- transect.TD.df %>% left_join(transect_info) %>%
  mutate(Order.q  = case_when(Order.q  == "0" ~ "q = 0", # q0= Species richness
                              Order.q == "1" ~ "q = 1",
                              Order.q == "2" ~ "q = 2")) %>% # q2= Species evenness
  filter(!Order.q == "q = 1")

df.point <-
  transect.hill.TD[which(transect.hill.TD$Method == "Observed"),]
df.line <-
  transect.hill.TD[which(transect.hill.TD$Method != "Observed"),]
df.line$Method <- factor(df.line$Method,
                         c("Rarefaction", "Extrapolation"))

# Plot----
names(df.point)

ggplot(transect.hill.TD ,
       aes(x = mT, y = qTD,   color = Treatment)) +
  facet_wrap( ~ Order.q) +
  geom_point(aes(),
             shape = 1,
             size = 2,
             data = df.point) +
  geom_line(aes(linetype = Method), lwd = 0.75, data = df.line) +
  scale_color_viridis_d() +
  labs(x = "Number of sampling units", y = "Taxonomic diversity", title =
         "Sample - based diversity accumulation ")

# save figure----

# ggsave('Sampling curve.jpg', width = 10, height = 6, dpi = 300, path = "Drive Letter:/Folder/Folder")

transect_prep_iNext  %>% 
  group_by(Treatment) %>% 
  distinct(Family) %>% 
  summarise(Family=n()) # note the number of Family and run the following code

TD_treat_out$TDInfo # S.obs= Observed species richness will be the same, this is the q=0 in the plot

# library(spelling)
# spell_check_files(path = 'species_curve.R')

# Enjoy----
