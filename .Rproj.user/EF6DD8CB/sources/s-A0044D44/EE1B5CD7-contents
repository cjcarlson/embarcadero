
library(reshape)

# 1. Robertson 1929: plant-pollinator interactions

rob1929_raw <- read.csv("~/Github/brevity/Figure 1 Demo Networks/robertson1929.csv")
rob1929 <- rob1929_raw[,c('plant','poll')]
colnames(rob1929) <- c("Plant","Pollinator")

# 2. Schleuning 2010: seed-disperser interactions

sch2010_raw <- read.csv("~/Github/brevity/Figure 1 Demo Networks/schleuning2010.csv")
sch2010_raw[sch2010_raw>1] <- 1
sch2010 <- melt(sch2010_raw, id=c("Plant.species"))
sch2010 <- sch2010[sch2010$value==1,]
sch2010 <- sch2010[,c(2,1)]
colnames(sch2010) <- c("Plant","Disperser")

# 3. Toju 2018: plant-arbuscular mycorrhizae

tojuraw.1 <- read.csv('~/Github/brevity/Figure 1 Demo Networks/toju descriptors.csv')
tojuraw.2 <- read.csv('~/Github/brevity/Figure 1 Demo Networks/toju.csv')
otu.list <- unique(tojuraw.1[tojuraw.1$Category.In.This.Study=='Arbuscular_Mycorrhizal',]$OTU.code)

tojuraw.2$Plant <- gsub('_TES', '', tojuraw.2$Plant)
tojuraw.2$Plant <- gsub('_TOMA', '', tojuraw.2$Plant)
tojuraw.2$Plant <- gsub('_YKS', '', tojuraw.2$Plant)
tojuraw.2$Plant <- gsub('_SGD', '', tojuraw.2$Plant)
tojuraw.2$Plant <- gsub('_YSD', '', tojuraw.2$Plant)
tojuraw.2$Plant <- gsub('_YAKU', '', tojuraw.2$Plant)
tojuraw.2$Plant <- gsub('_YONA', '', tojuraw.2$Plant)
tojuraw.2$Plant <- gsub('_IRI', '', tojuraw.2$Plant)

toju <- aggregate(. ~ Plant, data=tojuraw.2, FUN=sum)
toju <- melt(toju, id=c("Plant"))
toju <- toju[toju$value>1,]
colnames(toju)[1:2] <- c("Plant","Microbe")
toju$Microbe <- gsub('X', '', toju$Microbe)
toju$Microbe <- gsub('\\.', ':', toju$Microbe)
toju <- toju[toju$Microbe %in% as.character(otu.list),]
toju <- toju[,c(1:2)]

# 4. Host-helminth relationships

helminths.raw <- read.csv('~/Github/brevity/Figure 1 Demo Networks/helminths.csv')
helminths <- helminths.raw[helminths.raw$group=='Nematoda',]
helminths <- helminths[helminths$hostgroup=='Mammalia',]
helminths <- na.omit(helminths)
helminths <- helminths[,c(2,3)]
helminths <- unique(helminths)

# Analyses for each

library(codependent)

df.poll <- curve.df(rob1929, 100)
df.disp <- curve.df(sch2010, 100)
df.myco <- curve.df(toju, 100)
df.helm <- curve.df(helminths, 100)


model.poll <- nls(n.par~b*n.host^z,start = list(b = 1, z = 0.5),data=df.poll)
predicted.poll <- data.frame(pred = predict(model.poll), host = df.poll$n.host)
g1 <- ggplot(df.poll, aes(n.host, n.par)) + xlim(0,500) + ylim(0,1500) + xlab('Plants') + ylab('Pollinators') + 
  geom_point(shape = 16, size = 2.5, show.legend = FALSE, alpha = .05, color = c('steelblue1')) + theme_bw() +
  geom_line(color='black',lwd=1,data = predicted.poll, aes(x=host, y=pred))



model.disp <- nls(n.par~b*n.host^z,start = list(b = 1, z = 0.5),data=df.disp)
predicted.disp <- data.frame(pred = predict(model.disp), host = df.disp$n.host)
g2 <- ggplot(df.disp, aes(n.host, n.par)) + xlim(0,35) + ylim(0,100) + xlab('Plants') + ylab('Seed dispersers') + 
  geom_point(shape = 16, size = 2.5, show.legend = FALSE, alpha = .05, color = c('red')) + theme_bw() +
  geom_line(color='black',lwd=1,data = predicted.disp, aes(x=host, y=pred))



model.myco <- nls(n.par~b*n.host^z,start = list(b = 1, z = 0.5),data=df.myco)
predicted.myco <- data.frame(pred = predict(model.myco), host = df.myco$n.host)
g3 <- ggplot(df.myco, aes(n.host, n.par)) + xlim(0,85) + ylim(0,600) + xlab('Plants') + ylab('Microbe OTUs') + 
  geom_point(shape = 16, size = 2.5, show.legend = FALSE, alpha = .05, color = c('orange')) + theme_bw() +
  geom_line(color='black',lwd=1,data = predicted.myco, aes(x=host, y=pred))



model.helm <- nls(n.par~b*n.host^z,start = list(b = 1, z = 0.5),data=df.helm)
predicted.helm <- data.frame(pred = predict(model.helm), host = df.helm$n.host)
g4 <- ggplot(df.helm, aes(n.host, n.par)) + xlim(0,875) + ylim(0,2400) + xlab('Hosts') + ylab('Helminths') + 
  geom_point(shape = 16, size = 2.5, show.legend = FALSE, alpha = .05, color = c('seagreen1')) + theme_bw() +
  geom_line(color='black',lwd=1,data = predicted.helm, aes(x=host, y=pred))

  
plot2by2 <- plot_grid(g2, g3, g1, g4,
                      labels=c("A", "B", "C", "D"), nrow=2, ncol = 2)


df.helm.10 <- df.helm[df.helm$n.host < 0.1*max(df.helm$n.host),]
df.helm.25 <- df.helm[df.helm$n.host < 0.25*max(df.helm$n.host),]
df.helm.50 <- df.helm[df.helm$n.host < 0.5*max(df.helm$n.host),]

model1 <- nls(n.par~b*n.host^z,start = list(b = 1, z = 0.5),data=df.helm)
model10 <- nls(n.par~b*n.host^z,start = list(b = 1, z = 0.5),data=df.helm.10)
model25 <- nls(n.par~b*n.host^z,start = list(b = 1, z = 0.5),data=df.helm.25)
model50 <- nls(n.par~b*n.host^z,start = list(b = 1, z = 0.5),data=df.helm.50)

predicted_10 <- data.frame(pred = predict(model10,df.helm), host = df.helm$n.host)
predicted_25 <- data.frame(pred = predict(model25,df.helm), host = df.helm$n.host)
predicted_50 <- data.frame(pred = predict(model50,df.helm), host = df.helm$n.host)
predicted_100 <- data.frame(pred = predict(model1,df.helm), host = df.helm$n.host)

g5 <- ggplot(df.helm, aes(n.host, n.par)) + xlim(0,900) + ylim(0,3500) + xlab('Mammal hosts') + ylab('Nematode parasites') + 
  geom_point(shape = 16, size = 2.5, show.legend = FALSE, alpha = .1, color = c('grey')) + theme_bw() +
  geom_line(color='black',lty='dotted',lwd=1.1,data = predicted_10, aes(x=host, y=pred))+
  geom_line(color='black',lty='dashed',lwd=1.1,data = predicted_25, aes(x=host, y=pred))+
  geom_line(color='black',lty='longdash',lwd=1.1,data = predicted_50, aes(x=host, y=pred))+
  geom_line(color='black',lwd=1.1,data = predicted_100, aes(x=host, y=pred))
  

plot_grid(plot2by2, g5, ncol=2, labels=c('','E'))



# BIG CI ON HELMINTHS

copredict(500,rob1929,100,1)
