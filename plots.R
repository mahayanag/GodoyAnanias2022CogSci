### run all analyses in analyses.R file


####################
##### FIGURE 1 #####
####################

### CREATE TABLE

### voiced

df.vozeada <- as.data.frame(pph.vozeada)

#levels(df.vozeada$forma) <- c("voiced / pointy", "voiced / rounded")

df.vozeada$parametro <- c("voiced", "voiced") 

df.vozeada

### voiced

df.desvozeada <- as.data.frame(pph.desvozeada)

#levels(df.desvozeada$forma) <- c("voiceless / pointy", "voiceless / rounded")

df.desvozeada$parametro <- c("voiceless", "voiceless") 

df.desvozeada

### sonorant
df.soante <- as.data.frame(pph.soante)

#levels(df.soante$forma) <- c("sonorant / pointy", "sonorant / rounded")

df.soante$parametro <- c("sonorant", "sonorant") 

df.soante

### obstruent

df.obstruinte <- as.data.frame(pph.obstruinte)

#levels(df.obstruinte$forma) <- c("obstruent / pointy", "obstruent / rounded")

df.obstruinte$parametro <- c("obstruent", "obstruent") 

df.obstruinte

### grave

df.grave <- as.data.frame(pph.grave)

#levels(df.grave$forma) <- c("grave / pointy", "grave / rounded")

df.grave$parametro <- c("grave", "grave") 

df.grave

### acute

df.acute <- as.data.frame(pph.acute)

#levels(df.acute$forma) <- c("acute / pointy", "acute / rounded")

df.acute$parametro <- c("acute", "acute")

df.acute

### TABELA CONSOANTES

consoantes <- rbind(df.vozeada, df.desvozeada, df.soante, df.obstruinte, df.grave, df.acute)

## ingles

consoantes %>%
  ggplot(aes(y = forma, x = rate)) +
  geom_point(size = 0.8)+
  scale_y_discrete(labels = c("pointy", "round"))+
  geom_errorbar(aes(xmin = asymp.LCL, xmax = asymp.UCL),
                size = 0.5, width = 0.1) +
  ylab('Shape\n') +
  xlab('\nNumber of occurences') +
  theme_bw() +
  theme(axis.text.x =
          element_text(face = 'bold', size = 10),
        axis.text.y =
          element_text(face = 'bold', size = 10),
        axis.title =
          element_text(face = 'bold', size = 15))+
  facet_wrap(~parametro, ncol = 2)+
  coord_flip()


##############################
##### FIGURE 3: POST HOC #####
##############################

### voiced stops

df.stopsvcd <- as.data.frame(pph.stopsvcd)

df.stopsvcd$parametro <- c("voiced stops", "voiced stops") 

df.stopsvcd

### voiceless stops

df.stopsvcls <- as.data.frame(pph.stopsvcls)

df.stopsvcls$parametro <- c("voiceless stops", "voiceless stops") 

df.stopsvcls

### voiced fricatives
df.fricativescvd <- as.data.frame(pph.fricativesvcd)

df.fricativescvd$parametro <- c("voiced fricatives", "voiced fricatives") 

df.fricativescvd

### voiceless fricatives

df.fricativesvcls <- as.data.frame(pph.fricativesvcls)

df.fricativesvcls$parametro <- c("voiceless fricatives", "voiceless fricatives") 

df.fricativesvcls


### TABELA POST HOC

post.hoc.obstruents <- rbind(df.stopsvcls, df.stopsvcd, df.fricativescvd, df.fricativesvcls)

post.hoc.obstruents$parametro <- factor(post.hoc.obstruents$parametro, levels = c("voiceless stops", "voiced stops", "voiceless fricatives", "voiced fricatives"))

# post.hoc.obstruents

post.hoc.obstruents %>%
  ggplot(aes(y = forma, x = rate)) +
  geom_point(size = 0.8)+
  scale_y_discrete(labels = c("pointy", "round"))+
  geom_errorbar(aes(xmin = asymp.LCL, xmax = asymp.UCL),
                size = 0.5, width = 0.1) +
  ylab('Shape\n') +
  xlab('\nNumber of occurences') +
  theme_bw() +
  theme(axis.text.x =
          element_text(face = 'bold', size = 10),
        axis.text.y =
          element_text(face = 'bold', size = 10),
        axis.title =
          element_text(face = 'bold', size = 15))+
  facet_wrap(~parametro, ncol = 2)+
  coord_flip()


##############################
##### FIGURE 4: POST HOC #####
##############################

### voiced stops

df.nasals <- as.data.frame(pph.nasals)
df.nasals$parametro <- c("nasals", "nasals") 
df.nasals

### voiceless stops

df.lateral <- as.data.frame(pph.lateral)
df.lateral$parametro <- c("lateral", "lateral") 
df.lateral

### voiced fricatives
df.r <- as.data.frame(pph.r)
df.r$parametro <- c("vibrant", "vibrant") 
df.r


### TABELA POST HOC

post.hoc.obstruents <- rbind(df.stopsvcls, df.stopsvcd, df.fricativescvd, df.fricativesvcls, df.nasals, df.lateral, df.r)

post.hoc.obstruents$parametro <- factor(post.hoc.obstruents$parametro, levels = c("voiceless stops", "voiced stops", "voiceless fricatives", "voiced fricatives", "nasals", "lateral", "vibrant"))

# post.hoc.obstruents

post.hoc.obstruents %>%
  ggplot(aes(y = forma, x = rate)) +
  geom_point(size = 0.8)+
  scale_y_discrete(labels = c("pointy", "round"))+
  geom_errorbar(aes(xmin = asymp.LCL, xmax = asymp.UCL),
                size = 0.5, width = 0.1) +
  ylab('Shape\n') +
  xlab('\nNumber of occurences') +
  theme_bw() +
  theme(axis.text.x =
          element_text(face = 'bold', size = 10),
        axis.text.y =
          element_text(face = 'bold', size = 10),
        axis.title =
          element_text(face = 'bold', size = 15))+
  facet_wrap(~parametro, ncol = 2)+
  coord_flip()