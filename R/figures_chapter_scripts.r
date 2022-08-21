# Code for the figures in the "Figures" chapter

#### load libraries ####
suppressPackageStartupMessages({
  library(ggplot2)
  library(cowplot)
  library(tidyr)
  library(forcats)
  library(tibble)
  library(ggforce)
  library(dplyr)
  library(purrr)
  library(glue)
})
theme_set(theme_cowplot())
set.seed(20923) # to keep random numbers consistent between runs

#### Set-up some regressions ####
# Load the penguin data
penguins = suppressMessages( # quietly...
  readr::read_csv(
  "https://raw.githubusercontent.com/allisonhorst/palmerpenguins/master/inst/extdata/penguins.csv")) |> 
  # change some units
  mutate(flipper_length_cm = flipper_length_mm / 10,
         body_mass_kg = body_mass_g / 1000)
#### Calculate regression values
# These will be referenced in the figures
# Flipper length vs. body mass
# Fit the regression:
penguin_lm = lm(flipper_length_cm ~ body_mass_kg, penguins)
penguin_eqn = penguin_lm |> 
  # Get coeficients in the right format
  coef() |> as.list() |> 
  # Round them
  map(~round(.x, 1)) |> 
  # Format w/ text
  glue_data("(Flipper Length) = {`(Intercept)`} + {body_mass_kg}*(Body Mass)")

# Basic summary stats
penguin_smry = summary(penguin_lm)
# p-value
penguin_pval = anova(penguin_lm)[["Pr(>F)"]]

# Bill length vs bill depth
bill_lm = lm(bill_depth_mm ~ bill_length_mm + species, penguins) 
# Reformat these intercepts for each species
bill_coefs = bill_lm |> 
  coef() |> as.list()
bill_spp_coefs = 
  with(bill_coefs, # with lets us work directly with elements inbill_coefs
       list(
    # lm() uses dummy coding for categories, where the (Intercept) parameter
      # matches the first category (in this case, Adelie), and the 
      # other category parameters (speciesChinstrap & speciesGentoo)
      # are the difference between that category's intercept and the 
      # first category's intercept.  To get species-level intercepts,
      # merely add them back together
    Adelie = `(Intercept)`,
    Chinstrap = `(Intercept)` + speciesChinstrap,
    Gentoo = `(Intercept)` + speciesGentoo,
    bill_length_mm = bill_length_mm
  )) |>map(~round(.x, 1)) 
# Since we have multiple 
bill_eqn = 
  glue_data(bill_spp_coefs, glue_collapse(c(
    # separate equations each species
  "(Bill Depth) = ",
    "{Adelie} + {bill_length_mm}&ast;(Bill Length) for Adelie, ",
    "{Gentoo} + {bill_length_mm}&ast;(Bill Length) for Gentoo, and ",
    "{Chinstrap} + {bill_length_mm}&ast;(Bill Length) for Chinstrap")))
bill_smry = summary(bill_lm)
bill_pvals = anova(bill_lm)[["Pr(>F)"]][1:2] # bill length, species

#### Bad figure formatting: ####
# Don't try this at home

# This defines the position of the equation in the bad penguin plot
bad_penguin_eqn_df = 
  data.frame(flipper_length_cm = 23.5, body_mass_kg = .2)

penguin_plt_bad = ggplot(penguins) +
  # Set X and Y variables
  aes(x = body_mass_kg, y = flipper_length_cm) + 
  # Points, colored by species
  geom_point(aes(color = species)) + 
  ylim(0, NA) + # Force y axis through 0
  xlim(0, NA) + 
  # Make it ugly
  theme(panel.grid.major.y  = element_line(color = grey(.8), size = .7),
        panel.grid.minor.y  = element_line(color = grey(.8), size = .2), 
        plot.title = element_text(hjust = .5)) +
  # Add regression line
  geom_smooth(aes(), method = "lm", se = FALSE, formula = y~x,
              color = "black") + 
  # Insert test onto the plot
  geom_text(label = penguin_eqn, hjust = 0.05, data = bad_penguin_eqn_df) +
  ggtitle("Penguin Flipper Length (cm) vs. Body Mass (kg)")

### The good version ####

# Define colors that will print in black & white and are colorblind friendly
species_colors = c(
  "#a06f00", "#004368", "#e0a1c4")
penguin_plt_good = ggplot(penguins) +
  # Set X and Y variables
  aes(x = body_mass_kg, y = flipper_length_cm) + 
  # Points, species separated by color and shape
  geom_point(aes(color = species, shape = species)) + 
  # Axis labels
  xlab("Body Mass (kg)") + 
  ylab("Flipper Length (cm)") +
  # Add regression line
  geom_smooth(aes(), method = "lm", se = FALSE, formula = y~x,
              color = "black") +
  # Move the legend into lower right corner
  theme(legend.position = c(.8, .2)) + # c(x, y) relative positions
  # Define colors & shapes for species
  scale_shape_manual(name = "Species", 
                     values = c(15, 19, 17))+ # use ?pch to see the list of shapes
  scale_color_manual(values = species_colors, 
                     name = "Species") # Capitalize the legend title

# You can check the colors with the colorblindr package
# (You'll need to google the package & read the readme to install it)
# colorblindr::cvd_grid(plot = penguin_plt_good) # Uncomment to run check

### Panel plot #### 
# Create a data frame for facet label positions
facet_letter_df = tibble(
  bill_length_mm = 32, bill_depth_mm = 21, # These are the same for all species
  species = unique(penguins$species),
  label = c("A", "B", "C")
)
penguin_facet_plt = penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() + 
  facet_grid(species~.) + 
  geom_smooth(aes(), method = "lm",
              formula = y~x,  # see note below
              se = FALSE) + 
  ylab("Bill Depth (mm)") + xlab("Bill Length (mm)") +
  # Make facet names look nicer
  theme(strip.background = element_blank()) + 
  # Add facet labels
  geom_text(aes(label = label), fontface = "bold",
            data = facet_letter_df) + 
  # Prevent the position of the labels from expanding the plot axis limits
  coord_cartesian(xlim = c(32.5, NA))
# NOTE: This formula is not technically the same as the one the regression model is fitting
# This is fitting a separate bill_depth ~ bill_length regression
# for each facet (i.e., species), instead of fitting a single
# bill_depth ~ bill_length + species regression
# In theory, hat meas that the slope of the regression may be different
# for each species in the graph, while our model keeps them the same
# Practically, it doesn't look any different here, but 
# if you have a different dataset or a more complicated model
# you may wish to avoid geom_smooth() and instead manually
# build the line with geom_abline() or geom_segment()

#### dynamite plot ####
 # DO not make these, they are bad
penguin_dyn = penguins |> group_by(species) |> 
  summarize(y = mean(body_mass_kg, na.rm = TRUE), 
            se = sd(body_mass_kg, na.rm = TRUE)/sqrt(n()))
dynam_plot = ggplot(penguin_dyn, aes(x = species, y )) + 
  geom_col(fill = "white", color = "black") + 
  geom_errorbar(aes(ymin = y-se, ymax = y+se), width = .3) +
  ylab("Body Mass (kg)") 

#### Box plot, histogram, violin plot ####
box_plot = penguins |>
  ggplot(aes(x = species, y = body_mass_kg )) + 
  geom_boxplot() + 
  ylab("Body Mass (kg)") 
histo_plot = penguins |>
  ggplot(aes(x = body_mass_kg )) + 
  geom_histogram(fill = "white", color = "black", binwidth = .1) + 
  facet_grid(species~., switch = "both") + 
  xlab("Body Mass (kg)") + 
  theme(strip.text = element_text(face = "italic"),
        strip.background = element_blank(),
        strip.placement = "outside") + ylab("")
violin_plot = penguins |>
  ggplot(aes(x = species, y = body_mass_kg )) + 
  geom_violin() +  # Add the violin shape
  geom_sina(color = grey(.4)) + # this is from the ggforce package; adds the dots
  ylab("Body Mass (kg)") + 
  # Add mean + SE
  geom_errorbar(aes(y = y, ymin = y - se, ymax = y + se), 
                color = "red", data = penguin_dyn, width = .15)

#### Make the anole data ####
# Synthesize some anolis data for categorical examples

anole_classes = tribble(
  ~"Species",          ~"Ecomorph",      
  "A. chlorocyanus",   "Trunk-crown",
  "A. cristatellus",   "Trunk-ground",
  "A. cybotes",        "Trunk-ground",
  "A. distichus",      "Trunk",
  "A. equestris",      "Crown Giant",
  "A. garmani",        "Crown Giant",
  "A. carolinensis",   "Trunk-crown",
  "A. sagrei",         "Trunk-ground",
  "A. trinitatus",     "Ground-ish")
set.seed(123); 
anole_dat = anole_classes |> 
  distinct(Ecomorph) |> 
  mutate(ground_prob = c(.25, .75, .5, .1, .9)) |> 
  right_join(anole_classes, by = "Ecomorph") |> 
  mutate(count = rpois(9, 3.5)*5 + rpois(9, 6)) |> 
  mutate(Trunk = rbinom(9, count, ground_prob),
         Canopy = count - Trunk) |>
  select(Species, Ecomorph, Trunk, Canopy) |> 
  # Add in trunk bias
  mutate(Trunk = round(Trunk * 2.5)) |> 
  gather(key = "Perch", value = "Count", Trunk, Canopy) |> 
  mutate(Perch = factor(Perch, levels = c("Trunk", "Canopy")))
#### Bad Categorical Figure ####
anole_bad_plt = 
  anole_dat |> 
  ggplot(aes(x = Species, y = Count, group = Perch)) + 
  geom_col(aes(fill = Perch), color = "black", position = position_dodge()) +
  scale_fill_manual(values = c(grey(.95), grey(.6)), 
                    name = "Perch Position") +
  theme(axis.text.x = element_text(face = "italic", 
                                   angle = -20, hjust = 0.5, vjust = -.2),
        legend.position = c(0.05,1), #legend.direction = "horizontal", 
        legend.justification = c(0,1))
#### Stacked Counts figure ####

# We want to arrange the levels by the total count (per-species)
anole_stacked_plt = anole_dat |> 
  # First, compute the total count
  group_by(Species) |> # group by species
  mutate(total_count = sum(Count)) |> # total count (per species, due to grouping)
  ungroup() |> # Remove grouping so that the following sorting step doesn't use it
  # THen, sort the data by total count
  arrange(desc(total_count)) |> # desc() makes it go highest-to-lowest (default is low-to-high)
  # alternatively, try one of these instead:
    # arrange(Perch, desc(Count)) |> 
    # arrange(desc(Perch), desc(Count)) |> 
  # Encode the sorting in the Species column:
  mutate(Species = fct_inorder(Species)) |> 
  # fct_inorder() tells species assign species underlying values based on 
    # its existing ordering in the data
    # This will make ggplot arrange the bar graph in that order
    # Try commenting this line out and seeing how different it looks
  ggplot(aes(x = Species, y = Count, group = Perch)) + 
  # bar graph
  geom_col(aes(fill = Perch), color = "black",) +
  # solid objects use 'fill' instead of color; color is the outside lines
  scale_fill_manual(values = c(grey(.95), grey(.6)), 
                    name = "Perch Position") +
  theme(axis.text.x = element_text(face = "italic", 
                                   angle = -20, hjust = 0.5, vjust = -.2),
        legend.position = c(1,1), legend.direction = "horizontal", 
        legend.justification = c(1,1))

#### Dodged Counts figure

# for this case, we want to arrange in decreasing order by 
  # Trunk counts
anole_dodge_plt = anole_dat |> 
  arrange(Perch, desc(Count)) |> 
  mutate(Species = fct_inorder(Species)) |> 
  ggplot(aes(x = Species, y = Count, group = Perch)) + 
  geom_col(aes(fill = Perch), color = "black",
           position = position_dodge()) + # position_dodge() makes the bars go next to each other
  scale_fill_manual(values = c(grey(.95), grey(.6)), 
                    name = "Perch Position") +
  theme(axis.text.x = element_text(face = "italic", 
                                   angle = -20, hjust = 0.5, vjust = -.2),
        legend.position = c(1,1), legend.direction = "horizontal", 
        legend.justification = c(1,1))

#### Anole Perch & Species frequency plots ####

# Calculate frequencies by perch
anole_freq_perch = anole_dat |> group_by(Perch) |> 
  mutate(Frequency = Count/sum(Count)) |> ungroup() |> 
  arrange(Perch, desc(Frequency) )
anole_perch_freq_plt = anole_freq_perch |>  
  mutate(Species = fct_inorder(Species)) |> 
  ggplot(aes(x = Species, y = Frequency, group = Perch)) + 
  geom_col(aes(fill = Perch), color = "black", position = position_dodge()) +
  scale_fill_manual(values = c(grey(.95), grey(.6)), 
                    name = "Perch Position") +
  theme(axis.text.x = element_text(face = "italic", 
                                   angle = -20, hjust = 0.5, vjust = -.2),
        legend.position = c(0.05,1), legend.direction = "horizontal", 
        legend.justification = c(.0,1)) +
  ylab("Proportion of species\nat each perch position")

anole_freq_spp = anole_dat |> group_by(Species) |> 
  mutate(Frequency = Count/sum(Count)) |> ungroup() |> 
  arrange(Perch, Frequency) 
anole_spp_freq_plt = anole_freq_spp |>  
  mutate(Species = fct_inorder(Species)) |> 
  ggplot(aes(x = Species, y = Frequency, group = Perch)) + 
  geom_col(aes(fill = Perch), color = "black") +
  scale_fill_manual(values = c(grey(.95), grey(.6)), 
                    name = "Perch\nPosition") +
  theme(axis.text.x = element_text(face = "italic", 
                                   angle = -20, hjust = 0, vjust = 1)) +
  ylab("Frequency of each species at perch position")

### Difference frequency plot ####

# Create a data frame where the lower & higher frequencies are
# defined as new columns, along with which_highest to identify which is which
anole_diff_dat = anole_freq_perch |> group_by(Species) |> 
  summarize(lowest = min(Frequency), highest = max(Frequency),
            which_higher = Perch[Frequency == highest]) |> 
  arrange(desc(lowest)) |> 
  mutate(Species = fct_inorder(Species)) 

anole_diff_plt = anole_diff_dat |>  
  ggplot(aes(x = Species)) + 
  # Create a column for the highest frequency, the background filled based on which perch has more lizards
  geom_col(aes(y = highest, fill = which_higher), color = "black") +
  # Create another column for the lowest frequency, which will be white;
  # this will be drawn on top of the previous column, since it is added later
  geom_col(aes(y = lowest), fill = "white", color = "black") +
  # define the colors of the column fill
  scale_fill_viridis_d(begin = .2, end = .81,
                       name = "More frequent\nperch position") +
  theme(axis.text.x = element_text(face = "italic", 
                                   angle = -20, hjust = 0.5, vjust = -.2),
        legend.position = c(1,1), legend.direction = "vertical", 
        legend.justification = c(1,1)) +
  ylab("Proportion of species \nat each perch position")


##### Table 1 ####

table_1_df = tibble::tibble(
  Group = LETTERS[1:3], N = c(10, 15, 12), 
  Mean = c(35.33, 42.61, 22.00), 
  `Std. Dev.` = c(3.53, 4.62, 2.97),
  Min. = c(30.74,36.36,17.99),
  Max. = c(37.02, 49.17, 26.38)
)
table_1_cap = "Standard length of three populations of rainbow trout (*Oncorhynchus mykiss*) in Southern Appalachian streams.  Group A was collected from the New River, group B from the Watauga River and group C from Winkler Creek."
