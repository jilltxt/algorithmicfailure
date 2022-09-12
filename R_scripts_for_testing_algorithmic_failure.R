# R scripts for the paper "Algorithmic Failure to Predict as a Humanities Methodology"
# by Jill Walker Rettberg
# 13 September 2022
# 
# Released under a CC-BY license.
# 
# How to run this --------------------------------------------------------
# 
# This code is written in R, using the Tidyverse and Class packages. It supports a
# short paper by Jill Walker Rettberg that has been accepted for publication in Big Data & Society, titled 
# "Algorithmic failure as a humanities methodology: using machine learning’s 
# mispredictions to identify rich cases for qualitative analysis in big datasets."
# 

# About the data used -----------------------------------------------------
# 
# This code analyses data from the Database of Machine Vision in Art, Games 
# and Narratives. A data paper documenting this dataset is available here:
# 
# Jill Walker Rettberg, Linda Kronman, Ragnhild Solberg, Marianne Gunderson, 
# Stein Magne Bjørklund, Linn Heidi Stokkedal, Kurdin Jacob, Gabriele de Seta, 
# Annette Markham. 2022. "Representations of machine vision technologies in 
# artworks, games and narratives: A dataset". Data in Brief, 42. 
# https://doi.org/10.1016/j.dib.2022.108319.
# 
# The dataset itself can be downloaded from DataverseNO:
# 
# Jill Walker Rettberg, Linda Kronman, Ragnhild Solberg, Marianne Gunderson, 
# Stein Magne Bjørklund, Linn Heidi Stokkedal, Kurdin Jacob, Gabriele de Seta, 
# Annette Markham, 2022, "A Dataset Documenting 
# Representations of Machine Vision Technologies in Artworks, Games and 
# Narratives", doi:10.18710/2G0XKN, DataverseNO
# 
# This version of the scripts assumes the data is in a folder called data in the
# working directory. You can either download the data from Dataverse (see above)
# or if you're using the Github repository it's in the data folder there:
# https://github.com/jilltxt/algorithmic_failure
# 
#

# Start of scripts --------------------------------------------------------

# These scripts require the packages tidyverse and class - install these packages if they're 
# not already installed.
# 
# install.packages(tidyverse)
# install.packages(class)

library(tidyverse)

# LOAD DATA ---------------------------------------------------------------

# 1. Import characters.csv
# 2. Convert "Unknown" values to NA. 
# 3. Simplify RaceorEthnicity and Species to make analysis easier.
# 4. Select relevant columns.
# 
# Note: this is set to import the file from GitHub. If you have downloaded the
# file already, you can change the path to where the file is on your computer.

Characters <- read_csv("https://raw.githubusercontent.com/jilltxt/algorithmic_failure/main/data/characters.csv") %>% 
        select(Character, Species, Gender, Sexuality, 
               RaceOrEthnicity, Age) %>% 
        na_if("Unknown") %>% 
        mutate(RaceOrEthnicity = recode(RaceOrEthnicity,  
                                        "Asian" = "Asian", 
                                        "Black" = "PoC", 
                                        "White" = "White", 
                                        "Person of Colour" = "PoC",
                                        "Indigenous" = "PoC",
                                        "Immigrant" = "PoC",
                                        "Complex"  = "PoC"),
               Species = recode(Species,
                                "Human" = "Human",
                                "Machine" = "Robot",
                                "Cyborg" = "Robot",
                                "Fictional" = "Fictional",
                                "Animal" = "Animal"),
               Gender = recode(Gender,
                               "Female" = "Cis Woman",
                               "Male" = "Cis Man",
                               "Non-binary or Other" = "Genderqueer", 
                               "Trans Woman" = "Trans Woman"))

# To view the data: 
view(Characters)
# This shows all the characters in the dataset that interact with machine 
# vision technologies, with traits to show their gender, sexuality, race, age and 
# species.

# So we can order the bar charts later we'll make a vector "traits" that has 
# all the traits in order. For some reason ggplot shows these in reverse order,
# so they're reversed.
# 
traits <- c("Child", "Young Adult", "Adult", "Elderly", 
            "White", "PoC", "Asian", "Other", "Bi-sexual", 
            "Heterosexual", "Homosexual",
            "Robot", "Human", "Fictional", "Animal",
            "Cis Man", "Cis Woman", "Trans Woman", 
            "Genderqueer")

# To figure out what these characters actually do with the machine vision we need 
# to load data about the Situations in which they interact with machine vision
# technologies in the creative works in our sample.
# 
# The following code imports data about the Situations from situations.csv, 
# sets the column types, and also tells R to skip the columns we’re not going 
# to need for this analysis.
# 
# If you haven't installed the 

Situations <- read_csv("https://raw.githubusercontent.com/jilltxt/algorithmic_failure/main/data/situations.csv",
        col_types = cols(
                SituationID = col_integer(),
                Situation = col_skip(),
                Genre = col_character(),
                Character = col_character(),
                Entity = col_character(),
                Technology = col_character(),
                Verb = col_character()
        )
)


# ADD ACTIVE/PASSIVE VARIABLE ---------------------------------------------

# The situations have VERBS that the research team entered to describe the actions 
# characters take with machine vision. Verbs are either passive (ending in -ed, as 
# in "she was hunted" or "he was targeted") or passive (ending in -ing, as in 
# "they were killing" or "she was searching").
# 
# We're going to use a simpile machine learning algorithm to predict whether
# an action is active or passive based only on the traits of the characters
# who use that actions.
# 
# The point is to see whether the actions the algorithm CAN'T predict are interesting
# for a qualitative analysis.  
# 
# Fist we need to add a column stating whether verb is active or passive.
# We'll call this column "target" since this is the target or outcome we want to 
# build a model to predict from the other variables. This variable is TRUE for active 
# verbs (ending in -ing) and FALSE for passive verbs (ending in -ed)

Situations <- Situations %>% 
        mutate(target = (str_detect(Verb, "ing"))) %>% # add new col called target
        filter(!is.na(Verb)) # remove any rows with no data in the Verb column

# The data frame Situations includes data about actions taken by entities (e.g. law
# enforcemnet, corporations) and by technologies. Those rows have missing data
# (represented as NA) in the Character variable as they're not referencing a character.
# For this analysis, we are only interested in actions taken by characters, so 
# next we will create a new dataframe that only includes rows where the Character 
# is not NA, i.e. where there is a character doing something.

Character_situations <- Situations %>% 
        select(SituationID, Genre, Character, Verb, target) %>% 
        filter(!is.na(Character))


# MERGE CHARACTERS AND SITUATIONS TO CREATE VERB TABLE --------------------

# Now we combine the two data frames using the Character 
# column as the shared information.

Character_verbs <- merge(
        x = Character_situations, y = Characters, 
        by = "Character") %>% 
        select(Character, SituationID, Genre, Verb, Species, Gender, 
               RaceOrEthnicity, Age, Sexuality, target)




# VISUALISE DISTRIBUTION OF TRAITS IN DATASET -----------------------------

# Before we run the ML algorithm let's look at what's actually in the dataset 
# by running some visualisastions of the distribution.

# Fig 1 - distribution of action proportional by trait --------------------

# Plot barcharts showing the proportion of active and passive verbs for each
# character trait. 

Character_verbs %>% 
	select(Genre, Species, Gender, 
               RaceOrEthnicity, Age, Sexuality, target) %>% 
        pivot_longer(!target, values_to = "value") %>%
        mutate(value = fct_relevel(value, traits)) %>% # reorder by trait vector
        drop_na() %>% 
        ggplot(aes(x=factor(value), fill=factor(target))) +
        scale_fill_manual(name="Action type",
                values=c("steelblue", "orangered1"),
                labels=c("Passive", "Active")) +
        geom_bar(position="fill", alpha=.7)+
        theme( axis.line = element_line(colour = "darkblue", 
                                        size = 1, linetype = "solid")) +
        theme_minimal() +
        labs(title ="Characters' interactions with machine vision technologies",
             subtitle="(Proportional)",
             y = "", 
             x = "") +
        theme(axis.text.x = element_blank(),
              text = element_text(size = 20)) +
        coord_flip() +
        facet_wrap(~name, scales="free")


# Fig2 - Distribution by count --------------------------------------------

# Same but using count, not proportion, which shows that some categories have small numbers.
# (To show absolute count instead of proportion, set position = "dodge" instead of
# postiion = "fill" in geom_bar()) 

Character_verbs %>% select(Genre, Species, Gender, 
                           RaceOrEthnicity, Age, Sexuality, target) %>% 
        pivot_longer(!target, values_to = "value") %>%
        drop_na() %>% 
        mutate(value = fct_relevel(value, traits)) %>% # reorder by trait vector
        ggplot(aes(x=factor(value), fill=factor(target))) +
        scale_fill_manual(name="Action type",
                          values=c("steelblue", "orangered1"),
                          labels=c("Passive", "Active")) +
        geom_bar(position = "dodge", alpha=.7) +
        theme( axis.line = element_line(colour = "darkblue", 
                                        size = 1, linetype = "solid")) +
        theme_minimal() +
        labs(title ="Character interactions with machine vision technologies",
             subtitle="(Absolute numbers)",
             y = "", 
             x = "") +
        theme(axis.text.x = element_text(size = 9, angle = 90, vjust=1, hjust = 1),
              text = element_text(size = 20)) +
        coord_flip() +
        facet_wrap(~name, scales="free")



# SET UP DATA FOR MACHINE LEARNING AND PREDICTION -------------------------

# OK, now we're getting to work preparing the data for the machine learning.

# TRANSFORM TO CONTINGENCY TABLE --------------------------------------------------

# Make a contingency table for the Character_verbs.
# If using existing data from a content analysis you might be able to start here.

Character_verbs_contingency <- Character_verbs %>% 
        select(Verb, Gender, Species, RaceOrEthnicity, Age, Sexuality) %>% 
        pivot_longer(cols= -Verb,
                     names_to = "variable", 
                     values_to = "value") %>% 
        drop_na() %>% 
        group_by(Verb, value) %>%
        summarise(n=n()) %>% 
        pivot_wider(names_from = "value", values_from = "n") %>% 
        mutate_all(~replace(., is.na(.), 0)) %>%  # convert NA to 0 since it's count 
        mutate(target = str_detect(Verb, "ing"), .after = Verb) # new col target

# If you want to take a look at the contingency table, use the following line:
view(Character_verbs_contingency)

# NORMALISE VALUES --------------------------------------------------------

library(class)

# Right now the contigency chart shows the actual count of occurances. We need
# to normalise the values so all are between 0 and 1. 
# 
# Define normalize function (from Lantz p. 80)
# 
# Lantz B (2019) Machine Learning with R: Expert Techniques for Predictive 
# Modeling. 3rd ed. Birmingham: Packt.
# Lantz also explains this in a Datacamp course:
# https://campus.datacamp.com/courses/supervised-learning-in-r-classification/k-nearest-neighbors-knn?ex=10
# 
# This normalises the values so they are all between 0 and 1.

normalise <- function(x) {
        return ((x - min(x)) / (max(x) - min(x)))
}

# convert the Verb column to rownames, since the normalising needs all numeric data 
# variables - but rownames are fine and used as labels in the plot.
Character_verbs_contingency_rownames <- Character_verbs_contingency %>% 
        column_to_rownames(var = "Verb")

# Create a new version of the contingency table with normalised values. 
#Character_verbs_contingency_norm <- as.data.frame(lapply(Character_verbs_contingency_temp, normalise))


# SPLIT DATASET INTO TRAINING AND TEST SUBSET -----------------------------

# Setting the seed so s the "random" rows selected
# will be the same each time if we run the prediction multiple times. 
set.seed(2022)

# Split dataset into subsets for training and testing.
split <- sample(1:nrow(Character_verbs_contingency),
                as.integer(0.7*nrow(Character_verbs_contingency)), F)
train <- Character_verbs_contingency_rownames[split,]
test <- Character_verbs_contingency_rownames[-split,]

# Storing a list of the actual verbnames in the order they are in the test data
# so that later we can add them back in and see which verb was predicted active
# or passive.
verbnames <- rownames(test)

# Normalise the data in train and test (do this AFTER the split so I know what 
# the row numbers are and can add the test_verbs back in)
train <- as.data.frame(lapply(train, normalise))
test <- as.data.frame(lapply(test, normalise))


# RUN THE kNN ALGORITHM ---------------------------------------------------

# This is the actual machine learning.
# 
# format:
# prediction <- knn(training data, test data, class to try to predict, how many k's)
# 
# We need to exclude the target variable (i.e. what we're trying to predict, whether 
# the verb is active or passive). So we remove first two columns (the verb and 
# whether or not it is active) from both the 
# training and test subsets. 

prediction <- knn(train = train[-c(1:2)], test = test[-c(1:2)], cl = train$target, k=1)


# ADD PREDICTIONS TO ORIGINAL DATA ----------------------------------------

# Now we have the predictions we can add them to the original contingency table and 
# the more detailed data about the characters and verbs to see the differences. 
#
# Add the predictions to the subset of the original data we used for the testing.
# This has 225 verbs.
# 
verbs <- cbind(test, prediction)
# Add the verbnames back in.
verbs$Verb <- verbnames

# Find false passives (i.e. verbs the prediction thought were passive (ending in -ed)
# but that were really active)

Verb_pred <- verbs %>% 
        mutate(Prediction_type = case_when(target == 1 & prediction == 0 ~ "False Passive",
                                           target == 0 & prediction == 1 ~"False Active",
                                           target == 0 & prediction == 0 ~ "Accurate",
                                           target == 1 & prediction == 1 ~ "Accurate",
                                           TRUE ~ "Other")) %>% 
        select(Verb, prediction, Prediction_type)

# Merge predictions with the full Character_verbs table.
Verb_pred1 <- merge(Verb_pred, Character_verbs, by = "Verb")

# But Character_verbs includes the training dataset, and we only have predictions
# for the 225 verbs in the test dataset. So we'll remove the verbs that don't 
# have predictions. This is a limitation of the method - we are only analysing
# 30% of our dataset. But if we're looking for the KINDS of actions that 
# are mispredicted, that can still be valuable.

Character_verb_predictions <- Verb_pred1 %>% 
        drop_na(prediction) 

# Add frequency count to compare predictions to how often an action is taken.

pred <- Character_verb_predictions %>% 
        add_count(Verb, name = "Count") %>% 
        select(Verb, Count, Prediction_type, target) %>% 
        distinct() %>% 
        arrange(desc(Count))

# To get an idea of how often each action is used:
summary(pred)
# shows that 75% of the actions are used 4 or less times (i.e. the count of
# how many times each verb is used is 4 for the 4rd quartile, which means 75% of
# the data)

# VISUALISE TRAITS OF UNPREDICTABLE ACTIONS ------------------------------------

# fig3: false predictions proportional ------------------------------------

# Plot barcharts showing the proportion of mispredicted  verbs for each
# character trait. 
# 
Character_verb_predictions %>% 
        filter(Prediction_type != "Accurate") %>% 
        select(Genre, Species, Gender, 
               RaceOrEthnicity, Age, Sexuality, Prediction_type) %>% 
        pivot_longer(!Prediction_type, values_to = "value") %>%
        drop_na() %>% 
        mutate(value = fct_relevel(value, traits)) %>% # reorder by trait vector
        ggplot(aes(x=factor(value), fill=factor(Prediction_type))) +
        scale_fill_manual(values=c("steelblue", "orangered1" )) +
        geom_bar(position="fill", alpha=.7)+
        theme(axis.line = element_line(colour = "darkblue", 
                                       size = 1, 
                                       linetype = "solid")) +
        theme_minimal() +
        labs(fill="Predicted action:", 
             title ="Traits of characters whose actions were falsely predicted",
             subtitle="(Proportional)",
             y = "", 
             x = "") +
        theme(axis.text.x = element_blank(),
              text = element_text(size = 20)) +
        coord_flip() +
        facet_wrap(~name, scales="free")


# fig4: False predictions count -------------------------------------------

Character_verb_predictions %>% 
        filter(Prediction_type != "Accurate") %>% 
        select(Genre, Species, Gender, 
               RaceOrEthnicity, Age, Sexuality, Prediction_type) %>% 
        pivot_longer(!Prediction_type, values_to = "value") %>%
        drop_na() %>% 
        mutate(value = fct_relevel(value, traits)) %>% # reorder by trait vector
        ggplot(aes(x=factor(value), fill=factor(Prediction_type))) +
        scale_fill_manual(values=c("steelblue", "orangered1" )) +
        geom_bar(position="dodge", alpha=.7)+
        theme(axis.line = element_line(colour = "darkblue", 
                                       size = 1, 
                                       linetype = "solid")) +
        theme_minimal() +
        labs(fill="Predicted action:", 
             title ="Traits of characters whose actions were falsely predicted",
             subtitle = "(Absolute numbers)",
             y = "", 
             x = "") +
        theme(axis.text.x = element_text(size = 9, angle = 90, vjust=1, hjust = 1),
              text = element_text(size = 20)) +
        coord_flip() +
        facet_wrap(~name, scales="free")


# LIST CORRECTLY PREDICTED VERBS -----------------------------------------------

Character_verb_predictions %>%  
        filter(Prediction_type == "Accurate") %>% 
        group_by(Verb) %>% 
        summarise(Count=n()) %>% 
        arrange(desc(Count))


# LIST FALSE PASSIVES ------------------------------------------------------

Character_verb_predictions %>%  
        filter(Prediction_type == "False Passive") %>% 
        group_by(Verb) %>% 
        summarise(Count=n()) %>% 
        arrange(desc(Count))


# LIST FALSE ACTIVES ------------------------------------------------------

Character_verb_predictions %>%  
        filter(Prediction_type == "False Active") %>% 
        group_by(Verb) %>% 
        summarise(Count=n()) %>% 
        arrange(desc(Count))


# Accuracy rate of 10 most common verbs: ----------------------------------

Character_verb_predictions %>%  
        select(Verb, prediction, Prediction_type, target) %>% 
        add_count(Verb) %>% # adds a column n with count of how many times Verb occurs
        distinct() %>% # remove duplicates
        arrange(desc(n)) %>% 
        top_n(10) %>% # only show the top 10 in n (i.e. 10 most frequently used verbs)
        group_by(Prediction_type) %>%
        summarise(proportion = n() / nrow(.) ) 
# Results are given as decimals adding up to 1

#  Calculate accuracy of all except top ten -------------------------------

Character_verb_predictions %>%  
        select(Verb, Prediction_type) %>% 
        add_count(Verb) %>% # adds a column n with count of how many times Verb occurs
        distinct() %>% # remove duplicates
        arrange(desc(n)) %>% 
        slice_tail(n = -10) %>%  # remove the top ten rows
        group_by(Prediction_type) %>%
        summarise(proportion = n() / nrow(.) ) 



             
        
