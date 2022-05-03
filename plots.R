### run all analyses in analyses.R file

####################
##### FIGURE 2 #####
####################

# prepare table

# a

df.a <- as.data.frame(pph.a)

df.a$parametro <- c("/a/", "/a/") 

# e

df.e <- as.data.frame(pph.e)

df.e$parametro <- c("/e/", "/e/")

# i

df.i <- as.data.frame(pph.i)

df.i$parametro <- c("/i/", "/i/") 

# o

df.o <- as.data.frame(pph.o)

df.o$parametro <- c("/o/", "/o/") 

# u

df.u <- as.data.frame(pph.u)

df.u$parametro <- c("/u/", "/u/") 


## Tabela vogais 

vogais <- rbind(df.a, df.e, df.i, df.o, df.u)

vogais %>%
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


####################
##### FIGURE 3 #####
####################

### CREATE TABLE

### voiced

df.vozeada <- as.data.frame(pph.voiced)

df.vozeada$parametro <- c("voiced", "voiced") 

### voiced

df.desvozeada <- as.data.frame(pph.voiceless)

df.desvozeada$parametro <- c("voiceless", "voiceless") 

### sonorant
df.soante <- as.data.frame(pph.sonorants)

df.soante$parametro <- c("sonorant", "sonorant") 

### obstruent

df.obstruinte <- as.data.frame(pph.obstruents)

df.obstruinte$parametro <- c("obstruent", "obstruent") 

### grave

df.grave <- as.data.frame(pph.grave)

df.grave$parametro <- c("grave", "grave") 

### acute

df.acute <- as.data.frame(pph.acute)

df.acute$parametro <- c("acute", "acute")

consoantes <- rbind(df.vozeada, df.desvozeada, df.soante, df.obstruinte, df.grave, df.acute)

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