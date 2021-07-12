# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# Pre-crastination in Bonobos
# Natalie Schwob (ngschwob@gmail.com) 
# Amanda Epping, Jared Tagilalatela, Dan Weiss
# Data collection from July 2017 to March 2020
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 



# Reading in --------------------------------------------------------------

locations <- c("/Users/natalieschwob/Dropbox/PennState/Projects__PennState/PRCST_Precrastination/Data_files__PRCST", # Natalie iMac & Laptop
               "/Users/ngs16/Dropbox/PennState/Projects__PennState/PRCST_Precrastination/Data_files__PRCST") # Lab iMac

setwd(Find(dir.exists, locations))

rm(locations)

df <- read.csv("Data__PRCST.csv")







# -------------------------------------------------------------------------


# Phase 1: Tools ---------------------------------------------------------


# ~~~ Phase 1: Making DF -----------------------------------------------------

# Getting Phase 1  trials
  P1_df <- df[(df$Exp_Num == "1a"),]

# ~~~ Phase 1 Making a Freq DF -------------------------------------------

# Getting frequency of each type of choice
  invisible(P1_C <- xtabs(formula = ~ Ape + Choice_SL, data = P1_df))
  P1_C_df <- as.data.frame(P1_C)
  
  
# Changing ape names so it is in order of who did the most studies
  library(dplyr)
  P1_C_df <- P1_C_df %>%
    mutate(Ape = case_when(Ape == 'Kanzi' ~ 1,
                           Ape == 'Teco' ~ 2,
                           Ape == 'Maisha' ~ 3,
                           Ape == 'Elikya' ~ 4,
                           Ape == 'Nyota' ~ 5))
  
  
# Changing Selection names so Short & Both are next to each other
  library(dplyr)
  P1_C_df <- P1_C_df %>%
    mutate(Choice_SL = case_when(Choice_SL == "Both" ~ 2,
                                 Choice_SL == "Long" ~ 3,
                                 Choice_SL == "Short" ~ 1))  

# getting N_Trials by ape
  P1_C_df <- P1_C_df %>%
    group_by(Ape) %>%
    mutate(N_Trials = sum(Freq))
  
# ~~~ Phase 1: Visualizing Choice --------------------------------------------

# Calculating Proportion
  library(dplyr)
  Prop_P1_df <- P1_C_df %>%
    group_by(Ape) %>%
    dplyr::mutate(Prop = Freq / N_Trials)

   
  
# Visualizing
    
library(ggplot2)
    
P1_gg <- ggplot(Prop_P1_df, aes(x=factor(Ape), y = Prop, fill = factor(Choice_SL))) +
    geom_bar(stat = "identity", position = "stack") + 
    labs(y = "Proportion") +
    scale_y_continuous(breaks = seq(0,1,0.25)) + 
    theme_classic() + 
    scale_fill_manual(name = "Object Selection", labels = c("Short Appr", "Both", "Long Appr"), 
                      values = c("dodgerblue4", "deepskyblue4", "palegreen3")) +
    scale_x_discrete(labels=c("Kanzi", "Teco", "Maisha", "Elikya", "Nyota"), 
                     breaks=c("1", "2", "3", "4", "5")) +
    theme(axis.text.x = element_text(size = 18, color = "black"),
          axis.title.x = element_blank()) +
    theme(axis.title.y = element_text(size = 18, color = "black"), 
          axis.text.y = element_text(size = 15, color = "black")) +
    theme(legend.title = element_text(size = 15),
          legend.text = element_text(size = 13, hjust = 0.5)) +
    ggtitle("Phase 1: Tools") +
    theme(plot.title = element_text(size = 18, color = "black", face = "bold", hjust = 0.5)) +
    geom_text(data = subset(Prop_P1_df, Prop != 0), aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
              position = position_stack(vjust = .5),
              size = 5, color = "white", fontface = "bold") 
P1_gg    


# ~~~ Phase 1: Sample Info -------------------------------------------------------



addmargins(table(P1_df$Choice_SL))

  # Overall:
    # Short Only = 27 / 48
    # Both = 16 / 48
    # Long Only = 2 / 48
    # --- --- --- --- --- ---
    # Absolutre Pre = 27 / 48
    # Partial Pre = 16 / 48
    # Pre overall = (16+27) / 48 = 43 / 48
    # Pro = 5 / 48

    
addmargins(table(P1_df$Ape, P1_df$Choice_SL))

  # Kanzi:
    # Pre = 21 / 24
    # Pro = 3 / 24
    # --- --- --- --- --- ---
    # Absolute Pre = 21 / 24
    # Pro = 3 / 24
    # Partial Pre = 0 / 24
  # Teco:
    # Pre = 22 / 24
    # Pro = 2 / 24
    # --- --- --- --- --- ---
    # Absolute Pre = 6 / 24
    # Pro = 2 / 24
    # Partial Pre = 16 / 24
  

   

# ~~~ Phase 1: Binomial --------------------------------------------------

# --- --- --- --- --- --- 
# Binomial with Pre vs Pro 
# Pre = Short & Both 
# --- --- --- --- --- --- 

# Overall binomial:
# Overall:
  # Pre = 43 / 48
  # Pro = 5 / 48

binom.test(43, 48, 1/2)


# Did Kanzi select the first object more than chance?

# Kanzi:
  # Pre = 21 / 24
  # Pro = 3 / 24
  # --- --- --- --- --- --- 
  # Absolute Pre = 21 / 24
  # Pro = 3 / 24
  # Partial Pre = 0 / 24

  # Absolute PRE
    P1_KzAbs <- binom.test(21, 24, 0.25)
    P1_KzAbs
    
  # Patial PRECRST
    P1_KzPrt <- binom.test(0, 24, 0.25)
    P1_KzPrt
    
  # Pro
    P1_KzPro <- binom.test(3, 24, 0.25)
    P1_KzPro

# Did Teco select the first object more than chance?
    
# Teco:
  # Pre = 22 / 24
  # Pro = 2 / 24
  # --- --- --- --- --- --- 
  # Absolute Pre = 6 / 24
  # Pro = 2 / 24
  # Partial Pre = 16 / 24

  # Absolute PRE
   P1_TcAbs <-  binom.test(6, 24, 0.25)
   P1_TcAbs
   
  # Patial PRE
    P1_TcPrt <- binom.test(16, 24, 0.25)
    P1_TcPrt
    
  # Pro
    P1_TcPro <- binom.test(2, 24, 0.25)
    P1_TcPro


  
  
   
# -------------------------------------------------------------------------


# Phase 2: Nonfunctional -------------------------------------------------------

  
# ~~~ Phase 2: Making DF ----------------------------------------

# Getting trials where Exp_Num == 1b
  P2_df <- df[(df$Exp_Num == "1b"),]
 

# Getting frequency of each type of choice
  invisible(P2_C <- xtabs(formula = ~ Ape + Choice_SL, data = P2_df))
  P2_C_df <- as.data.frame(P2_C)
  
  
# Changing ape names so it is in order of who did the most studies 
  library(dplyr)
  P2_C_df <- P2_C_df %>%
    mutate(Ape = case_when(Ape == 'Kanzi' ~ 1,
                           Ape == 'Elikya' ~ 4,
                           Ape == 'Maisha' ~ 3,
                           Ape == 'Teco' ~ 2,
                           Ape == "Nyota" ~ 5))
  
  
# Changing Selection names so Short & Both are together
  library(dplyr)
  P2_C_df <- P2_C_df %>%
    mutate(Choice_SL = case_when(Choice_SL == "Both" ~ 2,
                                 Choice_SL == "Long" ~ 3,
                                 Choice_SL == "Short" ~ 1))  
  
# getting N_Trials by ape
  P2_C_df <- P2_C_df %>%
    group_by(Ape) %>%
    mutate(N_Trials = sum(Freq)) 
  

    
    
# ~~~ Phase 2: Visualizing Choice --------------------------------

# Calculating Proportion
  library(dplyr)
  Prop_P2_df <- P2_C_df %>%
    group_by(Ape) %>%
    dplyr::mutate(Prop = Freq / N_Trials)
  
  Prop_P2_df[is.na(Prop_P2_df)] <- 0
 
 
  library(ggplot2)
# Figure without sig indicators
  P2_gg <- ggplot(Prop_P2_df, aes(x=factor(Ape), y = Prop, fill = factor(Choice_SL))) +
    geom_bar(stat = "identity", position = "stack") + 
    labs(y = "Proportion") +
    scale_y_continuous(breaks = seq(0,1,0.25)) + 
    theme_classic() + 
    scale_fill_manual(name = "Object Selection", labels = c("Short Appr", "Both", "Long Appr"), 
                      values = c("dodgerblue4", "deepskyblue4", "palegreen3")) +
    scale_x_discrete(labels=c("Kanzi", "Teco","Maisha", "Elikya", "Nyota"), 
                     breaks=c("1", "2", "3", "4", "5")) +
    theme(axis.text.x = element_text(size = 18, color = "black"),
          axis.title.x = element_blank()) +
    theme(axis.title.y = element_text(size = 18, color = "black"), 
          axis.text.y = element_text(size = 15, color = "black")) +
    theme(legend.title = element_text(size = 15),
          legend.text = element_text(size = 13, hjust = 0.5)) +
    ggtitle("Phase 2: Non functional") +
    theme(plot.title = element_text(size = 18, color = "black", face = "bold", hjust = 0.5)) +
    geom_text(data = subset(Prop_P2_df, Prop != 0), aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
              position = position_stack(vjust = .5),
              size = 5, color = "white", fontface = "bold")
  P2_gg

  

# ~~~ Phase 2: Binomials, Grp & Indvd  -----------------------------------------

# Did the sample as a whole select the first object in Study 1B (control object)
  
# Overall:
  # Pre = 1 / 24
  # Pro = 23 / 24

  
  
# Did each individual select the first object in Study 1B (control object)
  
# Kanzi:
  # Pre = 0 / 12
  # Pro = 12 / 12
  # --- --- --- --- ---   
  # Absolutre Pre = 0 / 12
  # Partial Pre = 0 / 12
  # Pro = 12 / 12
  
  # Absolutre Pre
    binom.test(0, 12, 0.25)
  # Partial Pre
    binom.test(0, 12, 0.25)
  # Long Only
    binom.test(12, 12, 0.25)
    
# Teco:
  # Pre = 1 / 12
  # Pro = 11 / 12
  # --- --- --- --- ---   
  # Absolutre Prey = 0
  # Partial Pre = 1
  # Pro = 11
  
  # Absolutre Pre
    binom.test(0, 12, 0.25)
  # Partial Pre  
    binom.test(1, 12, 0.25)
  # Pro
    binom.test(11, 12, 0.25)
 
# -------------------------------------------------------------------------

  
# Phase 3: Cups ----------------------------------------------------------

  
# ~~~ Phase 3: Making DF -----------------------------------------------------

# renaming the original dataframe All_Trials_df
  All_Trials_df <- df  
  
# Getting Study 1C
  P3_df <- df[(df$Exp_Num == "1c"),]
    
    

# ~~~ Phase 3: Data information ----------------------------------------------

# How many trials per ape
  # Kanzi: 12
  # Elikya: 11
  # Nyota: 12
  # Maisha: 12
  # Teco: 8 

  table(P3_df$Ape)   

addmargins(table(P3_df$Ape, P3_df$Choice_SL))    
# Overall:
  # Pre = 38 / 43
  # Pro = 5 / 43  
  # Short Only = 21 / 43
  # Long Only = 5 / 43
  # Both = 17 / 43

# Kanzi:
  # Pre = 11 / 12
  # Pro = 1 / 12   
  # Short Only = 11 / 12
  # Long Only = 1 / 12
  # Both = 0 / 12
# Elikya:
  # Pre = 9 / 11
  # Pro = 3 / 11 
  # Short Only = 8 / 11
  # Long Only = 2 / 11
  # Both =  1 / 11 
# Nyota
  # Pre = 9 / 12
  # Pro = 3 / 12
  # Short Only = 6 / 12 
  # Long Only = 3 / 12
  # Both = 3 / 12
# Maisha:
  # Pre = 10 / 12
  # Prop = 2 / 12 
  # Short Only = 2 / 12 
  # Long Only = 2 / 12
  # Both =  8 / 12 
# Teco:
  # Pre = 8 / 8
  # Pro = 0 / 8   
  # Short Only = 0 / 8
  # Long Only = 0 / 8
  # Both = 8 / 8
    
  
# ~~~ Phase 3: Visualizing Choice --------------------------------------------

# Getting frequency of each type of choice
  invisible(P3_C <- xtabs(formula = ~ Ape + Choice_SL, data = P3_df))
  P3_C_df <- as.data.frame(P3_C)


# Changing ape names so it is in order of who did the most studies 
  library(dplyr)
  P3_C_df <- P3_C_df %>%
    mutate(Ape = case_when(Ape == 'Kanzi' ~ 1,
                           Ape == 'Elikya' ~ 4,
                           Ape == 'Maisha' ~ 3,
                           Ape == 'Teco' ~ 2,
                           Ape == 'Nyota' ~ 5))
  
  
# Changing Selecltion names so Short & Both are together
  library(dplyr)
  P3_C_df <- P3_C_df %>%
    mutate(Choice_SL = case_when(Choice_SL == "Both" ~ 2,
                                 Choice_SL == "Long" ~ 3,
                                 Choice_SL == "Short" ~ 1))

  
# Making it into proportion since Teco, EL don't have the same # of trials
  # getting N_Trials by ape
  P3_C_df <- P3_C_df %>%
    group_by(Ape) %>%
    mutate(N_Trials = sum(Freq))

  # Calculating Proportion
    library(dplyr)
    Prop_P3_df <- P3_C_df %>%
      group_by(Ape) %>%
      dplyr::mutate(Prop = Freq / N_Trials)


library(ggplot2)
  P3_gg <- ggplot(Prop_P3_df, aes(x=factor(Ape), y = Prop, fill = factor(Choice_SL))) +
    geom_bar(stat = "identity", position = "stack") + 
    labs(y = "Proportion") +
    scale_y_continuous(breaks = seq(0,1,0.2)) + 
    theme_classic() + 
    scale_fill_manual(name = "Cup Selection", labels = c("Absolute Pre", "Partial Pre", "Not Pre"), 
                      values = c("dodgerblue4", "deepskyblue4", "palegreen3")) +
    scale_x_discrete(labels=c("Kanzi", "Teco", "Maisha","Elikya", "Nyota"), 
                     breaks=c("1", "2", "3", "4", "5")) +
    theme(axis.text.x = element_text(size = 18, color = "black"),
          axis.title.x = element_blank()) +
    theme(axis.title.y = element_text(size = 18, color = "black"), 
          axis.text.y = element_text(size = 15, color = "black")) + 
    theme(legend.title = element_text(size = 15),
          legend.text = element_text(size = 13, hjust = 0.5)) +
    ggtitle("Phase 3: Cups") +
    theme(plot.title = element_text(size = 20, color = "black", face = "bold", hjust = 0.5)) +
    geom_text(data = subset(Prop_P3_df, Prop != 0), aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
              position = position_stack(vjust = .5),
              size = 5, color = "white", fontface = "bold")
  P3_gg

# ~~~ Phase 3: Binomials -------------------------------------------------

addmargins(table(P3_df$Ape, P3_df$Choice_SL))


# Overall:
  # Aboslute Pre = 27 / 55 --> 49%
  # Partial Pre = 20 / 55 --> 36.36%
  # Pro = 8 / 55 --> 14.55%
  
# Did each individual select the first object in Phase 3

# Kanzi:
  # Pre = 11 / 12
  # Pro = 1 / 12
  # --- --- --- --- --- ---
  # Absolutre Pre = 11 / 12
  # Pro = 1 / 12
  # Partial Pre = 0 / 12
  
  # Absolutre Pre
    binom.test(11, 12, 0.25)
  # Partial Pre
    binom.test(0, 12, 0.25)
  # Long Only
    binom.test(1, 12, 0.25)
 
    
# Elikya:
  # Pre = 9 / 11
  # Pro = 3 / 11
  # --- --- --- --- --- ---
  # Absolutre Pre = 8 / 11
  # Pro = 2 / 11
  # Partial Pre =  1 / 11 
    
  # Absolutre Pre
    binom.test(8, 11, 0.25)
  # Partial Pre  
    binom.test(1, 11, 0.25)
  # Pro
    binom.test(2, 11, 0.25)

  
 
# Maisha:
  # Pre = 10 / 12
  # Prop = 2 / 12
  # --- --- --- --- --- ---
  # Absolutre Pre = 2 / 12 
  # Pro = 2 / 12
  # Partial Pre =  8 / 12 
  
  # Absolutre Pre
    binom.test(2, 12, 0.25)
  # Partial Pre
    binom.test(8, 12, 0.25)
  # Pro
    binom.test(2, 12, 0.25)
  
      
# Nyota
  # Pre = 9 / 12
  # Pro = 3 / 12
  # --- --- --- --- --- ---
  # Absolutre Pre = 6 / 12
  # Pro = 3 / 12
  # Partial Pre = 3 / 12
    
  # Absolutre Pre
    binom.test(6, 12, 0.25)
    prop.test(6, 12, 0.25)
    prop.test(x = 6, n = 12, p = 0.25, alternative = "greater")
    fisher.test(x = 6, )
  # Partial Pre
    binom.test(3, 12, 0.25)
  # Pro
    binom.test(3, 12, 0.25)
  
    
# Teco:
  # Pre = 8 / 8
  # Pro = 0 / 8
  # --- --- --- --- --- ---
  # Absolutre Pre = 0 / 8
  # Pro = 0 / 8
  # Patial Pre = 8 / 8
    
  # Absolutre Pre
    binom.test(0, 8, 1/2)
  # Partial Pre
    binom.test(8, 8, 1/2) 
  # Pro
    binom.test(0, 8, 1/2)
  

  
# -------------------------------------------------------------------------

  
# Figures -----------------------------------------------------------------

  
# ~~~ All Study Choice Figure by Ape ---------------------------------------------

# Adding Study Number Column based on order I want them in 
  Prop_P1_df$Study_Num <- 1
  Prop_P2_df$Study_Num <- 2
  Prop_P3_df$Study_Num <- 3

Prop_All_df <- rbind(Prop_P1_df, Prop_P2_df)
Prop_All_df <- rbind(Prop_All_df, Prop_P3_df)



# changing NaN to 0
  Prop_All_df$Prop_Replace <- Prop_All_df$Prop
  Prop_All_df$Prop_Replace[is.nan(Prop_All_df$Prop_Replace)] <- 0
  


# Visualizing 
  library(ggplot2)

  # making labels for Study Numbers 
    study.labs <- c("Phase 1: Tools", "Phase 2: Nonfunctional", "Phase 3: Cups")
    names(study.labs) <- c(1,2,3)    
    library(ggtext)
    

  # Tall
   #tiff("Fig_All_Choice_Tall_wNy__PRCST.tiff", units = "in", width = 10, height = 16, res = 300)
      ggplot(Prop_All_df, aes(x = factor(Ape), y = Prop_Replace, fill = factor(Choice_SL))) + 
        geom_bar(stat = "identity", position = "stack") +
        labs(y = "Proportion") +
        scale_y_continuous(breaks = seq(0,1,0.25)) + 
        theme_classic() + 
        scale_fill_manual(name = "Object Selection", labels = c("Short Approach", "Both", "Long Approach"), 
                          values = c("dodgerblue4", "deepskyblue4","palegreen3")) +
        scale_x_discrete(labels=c("Kanzi", "Teco", "Maisha", "Elikya", "Nyota"), 
                         breaks=c("1", "2", "3", "4", "5")) +
        theme(axis.text.x = element_text(size = 16, color = "black", face = "bold"), # X axis
              axis.title.x = element_blank()) +
        theme(axis.title.y = element_text(size = 16, color = "black", face = "bold"), # Y axis
              axis.text.y = element_text(size = 14, color = "black")) +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0))) +
        theme(legend.title = element_text(size = 15, face = "bold"), # Legend
              legend.text = element_text(size = 14, hjust = 0.5),
              legend.position = "top") +
        facet_wrap("Study_Num", nrow = 3, labeller = labeller(Study_Num = study.labs)) +
        theme(strip.text = element_text(size = 14, face = "bold", color = "black")) + # Strip 
      geom_text(data = subset(Prop_All_df, Prop != 0), aes(label = sprintf("%0.2f", round(Prop, digits = 2))),
                position = position_stack(vjust = .5),
                size = 5, color = "white", fontface = "bold")
    #dev.off()
    