## packages

library(tidyverse)
library(stringr)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(emmeans)
library(gridExtra)


## data from 

## dataset

data <- read.csv("data/GodoyAnanias2022.csv", 
                      stringsAsFactors = TRUE)
str(data)

## columns
# participantes: participant ID
# par: image ID, identifying its pairs (A to F) and whether the image is pointy (pontudo) or round (redondo)
# nomes: names created by participants 
# transcript: phonological transcription of each name according to silac (Oushiro 2018)

###########################################
## creating variables: pair ID and shape ##
###########################################  

data <- mutate(data, forma = ifelse(grepl ("pontudo", par), "pontudo", "redondo"))

data <- mutate(data, 
                par_item = case_when(par == "parA.pontudo" ~ "parA",
                                     par == "parB.pontudo" ~ "parB",
                                     par == "parC.pontudo" ~ "parC",
                                     par == "parD.pontudo" ~ "parD",
                                     par == "parE.pontudo" ~ "parE",
                                     par == "parF.pontudo" ~ "parF",
                                     par == "parA.redondo" ~ "parA",
                                     par == "parB.redondo" ~ "parB",
                                     par == "parC.redondo" ~ "parC",
                                     par == "parD.redondo" ~ "parD",
                                     par == "parE.redondo" ~ "parE",
                                     par == "parF.redondo" ~ "parF"))  


str(data)

data$forma <- as.factor(data$forma)
data$par_item <- as.factor(data$par_item)

## new columns:

# forma: shape ID (pontudo = pointy; redondo = round)
# par_item: pair ID (A to F)


##################################
## number of pointy/round names ##
##################################

data%>%
  group_by(forma)%>%
  summarise(qtdd = n())


########################################
## checking for differences in length ##
########################################

data$syllable  = str_count(data$transcript, "-")+1
data$segments = str_count(data$transcript, "a|b|d|e|f|g|h|i|j|k|l|L|m|n|N|o|p|r|R|s|S|t|u|v|z|Z|w")


## comparing number of syllables

system.time(model.ztpois <- glmmTMB(syllable ~ forma + (1|participantes) + (1|par_item), data,family=truncated_poisson))

summary(model.ztpois)

## comparing number of segments

system.time(model.ztpois.seg <- glmmTMB(segments ~ forma + (1|participantes) + (1|par_item), data,family=truncated_poisson))

summary(model.ztpois.seg)


#################################
## creating response variables ##
#################################

#### CONSONANTS

# voicing
data$n.voiced <- str_count(data$transcript, "m|n|N|b|d|g|v|z|Z|l|L|r")
data$n.voiceless <- str_count(data$transcript, "p|k|t|f|s|S|h")

# manner
data$n.sonorants <- str_count(data$transcript, "m|n|l|L|r|N")
data$n.obstruents <- str_count(data$transcript, "p|t|k|b|d|g|f|s|S|h|v|z|Z")

# position
data$n.grave <- str_count(data$transcript, "m|p|k|b|g|f|h|v|N")
data$n.acute <- str_count(data$transcript, "n|t|d|s|z|S|Z|l|L|r")


#### VOWELS

# a, e, i, o, u
data$a <- str_count(data$transcript, "a")
data$e <- str_count(data$transcript, "e")
data$i <- str_count(data$transcript, "i")
data$o <- str_count(data$transcript, "o")
data$u <- str_count(data$transcript, "u")


####################################
### MODELS: POISSON DISTRIBUTION ###
####################################

## Model: logistic regression; poisson family

### VOICED

system.time(mdl.pois.voiced <- glmer(n.voiced ~ forma + (1|participantes), data = data,family=poisson(link=log)))

summary(mdl.pois.voiced)

pp.voiced <- summary(mdl.pois.voiced)$coefficients[2,4]

pph.voiced <- emmeans(mdl.pois.voiced, ~ forma, type = "response")

### VOICELESS

system.time(mdl.pois.voiceless <- glmer(n.voiceless ~ forma +  (1|participantes), data = data,family=poisson(link=log)))

summary(mdl.pois.voiceless)

pp.voiceless <- summary(mdl.pois.voiceless)$coefficients[2,4]

pph.voiceless <- emmeans(mdl.pois.voiceless, ~ forma, type = "response")

### SONORANT

system.time(mdl.pois.sonorants <- glmer(n.sonorants ~ forma + (1|participantes), data = data,family=poisson(link=log)))

summary(mdl.pois.sonorants)

pp.sonorants <- summary(mdl.pois.sonorants)$coefficients[2,4]

pph.sonorants <- emmeans(mdl.pois.sonorants, ~ forma, type = "response")

### OBSTRUENT

system.time(mdl.pois.obstruents <- glmer(n.obstruents ~ forma +  (1|participantes), data = data,family=poisson(link=log)))

summary(mdl.pois.obstruents)

pp.obstruents <- summary(mdl.pois.obstruents)$coefficients[2,4]

pph.obstruents <- emmeans(mdl.pois.obstruents, ~ forma, type = "response")

### GRAVE

system.time(mdl.pois.grave <- glmer(n.grave ~ forma + (1|par_item) + (1|participantes), data = data,family=poisson(link=log)))

summary(mdl.pois.grave)

pp.grave <- summary(mdl.pois.grave)$coefficients[2,4]

pph.grave <- emmeans(mdl.pois.grave, ~ forma, type = "response")

### ACUTE

system.time(mdl.pois.acute <- glmer(n.acute ~ forma + (1|par_item) + (1|participantes), data = data,family=poisson(link=log)))

summary(mdl.pois.acute)

pp.acute <- summary(mdl.pois.acute)$coefficients[2,4]

pph.acute <- emmeans(mdl.pois.acute, ~ forma, type = "response")


### A

system.time(mdl.pois.a <- glmer(a ~ forma + (1|participantes), data = data,family=poisson(link=log)))

summary(mdl.pois.a)

pp.a <- summary(mdl.pois.a)$coefficients[2,4]

pph.a <- emmeans(mdl.pois.a, ~ forma, type = "response")

### E

system.time(mdl.pois.e <- glmer(e ~ forma + (1|participantes), data = data,family=poisson(link=log)))

summary(mdl.pois.e)

pp.e <- summary(mdl.pois.e)$coefficients[2,4]

pph.e <- emmeans(mdl.pois.e, ~ forma, type = "response")


### I

system.time(mdl.pois.i <- glmer(i ~ forma + (1|participantes), data = data,family=poisson(link=log)))

summary(mdl.pois.i)

pp.i <- summary(mdl.pois.i)$coefficients[2,4]

pph.i <- emmeans(mdl.pois.i, ~ forma, type = "response")


### O

system.time(mdl.pois.o <- glmer(o ~ forma + (1|participantes), data = data,family=poisson(link=log)))

summary(mdl.pois.o)

pp.o <- summary(mdl.pois.o)$coefficients[2,4]

pph.o <- emmeans(mdl.pois.o, ~ forma, type = "response")


### U

system.time(mdl.pois.u <- glmer(u ~ forma + (1|participantes), data = data,family=poisson(link=log)))

summary(mdl.pois.u)

pp.u <- summary(mdl.pois.u)$coefficients[2,4]

pph.u <- emmeans(mdl.pois.u, ~ forma, type = "response")


### PADJUST

pp.values <- c(pp.voiced, pp.voiceless, pp.sonorants, pp.obstruents, pp.grave, pp.acute, pp.a, pp.e, pp.i, pp.o, pp.u)

p.adjust(pp.values, method = "fdr")

##############################
#### POST-HOC: OBSTRUENTS ####
##############################

### VOCING X MANNER

## create variable: number of voiceless stops
data$n.stopsvcls <- str_count(data$transcript, "p|t|k")

    ### model
    system.time(mdl.pois.stopsvcls <- glmer(n.stopsvcls ~ forma + (1|participantes)+      (1|par_item), data,family=poisson(link=log)))
    
    summary(mdl.pois.stopsvcls)
    
    pp.stopsvcls <- summary(mdl.pois.stopsvcls)$coefficients[2,4]
    
    pph.stopsvcls <- emmeans(mdl.pois.stopsvcls, ~ forma, type = "response")
    
    

## create variable: number of voiced stops
data$n.stopsvcd <- str_count(data$transcript, "b|d|g")

      ### model
      system.time(mdl.pois.stopsvcd <- glmer(n.stopsvcd ~ forma + (1|participantes) +      (1|par_item), data = data,family=poisson(link=log)))
      
      summary(mdl.pois.stopsvcd)
      
      pp.stopsvcd <- summary(mdl.pois.stopsvcd)$coefficients[2,4]
      
      pph.stopsvcd <- emmeans(mdl.pois.stopsvcd, ~ forma, type = "response")

      
      
## create variable: number of voiceless fricatives
data$n.fricativesvcls <- str_count(data$transcript, "f|s|S|h")
      
      ### model
      system.time(mdl.pois.fricativesvcls <- glm(n.fricativesvcls ~ forma, data = data     ,family=poisson(link=log)))
      
      summary(mdl.pois.fricativesvcls)
      
      pp.fricativesvcls <- summary(mdl.pois.fricativesvcls)$coefficients[2,4]
      
      pph.fricativesvcls <- emmeans(mdl.pois.fricativesvcls, ~ forma, type = "response")
      
      

## create variable: number of voiced fricatives
data$n.fricativesvcd <- str_count(data$transcript, "v|z|Z")

      ##model
      system.time(mdl.pois.fricativesvcd <- glmer(n.fricativesvcd ~ forma +       (1|participantes) + (1|par_item), data = data,family=poisson(link=log)))
      
      summary(mdl.pois.fricativesvcd)
      
      pp.fricativesvcd <- summary(mdl.pois.fricativesvcd)$coefficients[2,4]
      
      pph.fricativesvcd <- emmeans(mdl.pois.fricativesvcd, ~ forma, type = "response")


#############################
#### POST-HOC: SONORANTS ####
#############################
    

## create variable: number of nasals
data$n.nasals <- str_count(data$transcript, "m|N|n")

      ## model
      system.time(mdl.pois.nasals <- glmer(n.nasals ~ forma + (1|participantes) +       (1|par_item), data = data,family=poisson(link=log)))
      
      summary(mdl.pois.nasals)
      
      pp.nasals <- summary(mdl.pois.nasals)$coefficients[2,4]
      
      pph.nasals <- emmeans(mdl.pois.nasals, ~ forma, type = "response")


      
## create variable: lateral
data$n.lateral <- str_count(data$transcript, "l|L")

      ## model
      system.time(mdl.pois.lateral <- glm(n.lateral ~ forma, data = data,family=poisson     (link=log)))
      
      summary(mdl.pois.lateral)
      
      pp.lateral <- summary(mdl.pois.lateral)$coefficients[2,4]
      
      pph.lateral <- emmeans(mdl.pois.lateral, ~ forma, type = "response")
      

## create variable: approximants
data$n.r <- str_count(data$transcript, "r")

      ## model
      system.time(mdl.pois.r <- glmer(n.r ~ forma + (1|participantes), data =     data,family=poisson(link=log)))
      
      summary(mdl.pois.r)
      
      pp.r <- summary(mdl.pois.r)$coefficients[2,4]
      
      pph.r <- emmeans(mdl.pois.r, ~ forma, type = "response")
      
      
### PADJUST

      pp.values.posthoc <- c(pp.stopsvcls, pp.stopsvcd, pp.fricativesvcls, pp.fricativesvcd, pp.nasals, pp.lateral, pp.r)
      
      p.adjust(pp.values.posthoc, method = "fdr")
