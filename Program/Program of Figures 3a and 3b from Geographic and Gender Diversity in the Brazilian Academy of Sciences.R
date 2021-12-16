# Licensed under GNU General Public License v3.0.
# Program of two graphics that integrate the article "Geographic and Gender Diversity in the Brazilian Academy of Sciences".
# August 2018. All credits to Nathália C. Ferrari, Raquel Martell, Daniela H. Okido, Grasiele Romanzini, Viviane Magnan, Marcia C. Barbosa and Carolina Brito.
# Contact info: dhiromiokido@gmail.com

library("ggplot2"); library("RColorBrewer"); library("ggrepel"); library("gridExtra"); library("grid"); library("gtable")

tabela1 <- read.csv("tabela.csv", header = T, sep = ";", comment.char = "#", stringsAsFactors = T)
comlattes <- tabela1[which(tabela1$Possui_lattes == "S"),]
tabela2 <- comlattes[, c(2,3,6,14)]
areas <- levels(comlattes$Area)
mtx <- NULL

i = 1
for(i in 1:length(areas)){
  hhh <- which( tabela2$Genero == "M" & tabela2$Area == areas[i] & tabela2$Ano_Conclusao_Doutorado > 1900)
  mmm <- which( tabela2$Genero == "F" & tabela2$Area == areas[i] & tabela2$Ano_Conclusao_Doutorado > 1900)
  sh <- sum(tabela2$Artigos[hhh] / (2016 - tabela2$Ano_Conclusao_Doutorado[hhh]))
  sm <- sum(tabela2$Artigos[mmm] / (2016 - tabela2$Ano_Conclusao_Doutorado[mmm]))
  fh <- sh/length(hhh)
  fm <- sm/length(mmm)
  mtx <- cbind(mtx, c(fh, fm))
  colnames(mtx)[i] <- areas[i]
  rownames(mtx) <- c("Homens", "Mulheres")
}

mtx <- cbind(mtx[1,],mtx[2,])
colnames(mtx) <- c("Média_homens", "Média_mulheres")
matrixnova <- NULL
Var1 = c("1 Agrarian","2 Biological","3 Engineering","4 Health","5 Earth Sciences","6 Physical","7 Math","8 Chemistry","9 Social Sciences")
numeros <- c("1","2","3","4","5","6","7","8","9")
matrixnova <- cbind(matrixnova, mtx, Var1, numeros)
matrixnova <- data.frame(matrixnova)
matrixnova$Média_homens <- as.numeric(as.character(matrixnova$Média_homens))
matrixnova$Média_mulheres <- as.numeric(as.character(matrixnova$Média_mulheres))

matrixnova$Média_homens <- round(matrixnova$Média_homens, digits=2);
matrixnova$Média_mulheres <- round(matrixnova$Média_mulheres, digits=2);

sapply(matrixnova, class)

remove(tabela1); remove(tabela2); remove(comlattes); remove(mtx)

png(file = "figure_publicationsperyear.png", width = 960, height = 560)

ggplot(matrixnova, aes(x=Média_homens, y=Média_mulheres, colour = factor(Var1))) +
  geom_point(shape = 18, size = 15) +
  labs(x = "Male - Publications per year", y = "Female - Publications per year", colour = "Per subject") +
  geom_abline(intercept = 0, linetype = 1, colour = "darkgray") +
  xlim(0,13) +
  ylim(0,13) +
  geom_text_repel(aes(label = numeros), show.legend=F, box.padding = unit(1.15, 'lines'), size = 15) +
  scale_color_brewer(type = "qual", palette = 3) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        axis.title.x = element_text(size = 30, margin = margin(15,0,0,0)),
        axis.title.y = element_text(size = 30, margin = margin(0,5,0,0)),
        axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        panel.grid.minor = element_line(size = 0.5),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25))

dev.off()


###############################################


ABC <- read.csv("tabela.csv", sep=";", comment.char="#", stringsAsFactors = T)
ABC$Doutorado<-as.numeric(as.character(ABC$Doutorado))
ABC$Mestrado<- as.numeric(as.character(ABC$Mestrado))
ABC$Pos.Doutorado<-as.numeric(as.character(ABC$Pos.Doutorado))
ABC$IC<-as.numeric(as.character(ABC$IC))
ABC$IC<-(ABC$IC+ABC$Mestrado+ABC$Doutorado+ABC$Pos.Doutorado)
ABC$Ano_Conclusao_Doutorado <- 2015-as.numeric(as.character(ABC$Ano_Conclusao_Doutorado))
ABC$IC <- (ABC$IC/ABC$Ano_Conclusao_Doutorado)
areas <- levels(ABC$Area)

m2 <- c(1:9); h2 <- c(1:9)
a.h <- c(1:9); a.m <- c(1:9)

for (i in 1:length(areas)){
  m2[i] <- length(ABC$Membros[which(ABC$Genero=="F" & ABC$IC!="NA" & ABC$Area==areas[i])])
  h2[i] <- length(ABC$Membros[which(ABC$Genero=="M" & ABC$IC!="NA" & ABC$Area==areas[i])])
  a.h[i] <- sum(ABC$IC[which(ABC$Genero=="M" & ABC$Area==areas[i] & ABC$IC!="NA")]) 
  a.m[i] <- sum(ABC$IC[which(ABC$Genero=="F" & ABC$Area==areas[i] & ABC$IC!="NA")])
  a.h[i] <- (a.h[i]/h2[i])
  a.m[i] <- (a.m[i]/m2[i])
}


a.m <- round(a.m, digits=2);
a.h <- round(a.h, digits=2)
c <- c(1:9);
Var1 = c("1 Agrarian","2 Biological","3 Engineering","4 Health","5 Earth Sciences","6 Physical","7 Math","8 Chemistry","9 Social Sciences")

tab <- cbind(Var1, a.m, a.h, c)
tab <- as.data.frame(tab)
tab$a.m <- as.double(tab$a.m) 
tab$a.h <- as.double(tab$a.h)

sapply(tab,class)

remove(ABC)

png(file = "figure_studentsperyear.png", width = 960, height = 560)

ggplot(tab, aes(x=a.h, y=a.m, colour = factor(Var1))) +
  geom_point(shape = 18, size = 15, show.legend=FALSE) +
  labs(x = "Male - Students per year", y = "Female - Students per year") +
  geom_abline(intercept = 0, linetype = 1, colour = "darkgray") +
  xlim (0,5) + 
  ylim(0,5) +
  geom_text_repel(aes(label = c), show.legend=FALSE, box.padding = unit(1.2, 'lines'), size = 15) +
  scale_color_brewer(type = "qual", palette = 3) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 30, margin = margin(15,0,0,0)),
        axis.title.y = element_text(size = 30, margin = margin(0,20,0,0)),
        axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        panel.grid.minor = element_line(size = 0.5)) +
  guides(scale = "none", fill = "none")

dev.off()


###############################################

q <- ggplot(matrixnova, aes(x=Média_homens, y=Média_mulheres, colour = factor(Var1))) +
  geom_point(shape = 18, size = 15) +
  labs(x = "Male - Publications per year", y = "Female - Publications per year", colour = "Per subject") +
  geom_abline(intercept = 0, linetype = 1, colour = "darkgray") +
  xlim(0,13) +
  ylim(0,13) +
  geom_text_repel(aes(label = numeros), show.legend=F, box.padding = unit(1.15, 'lines'), size = 15) +
  scale_color_brewer(type = "qual", palette = 3) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        axis.title.x = element_text(size = 30, margin = margin(15,0,0,0)),
        axis.title.y = element_text(size = 30, margin = margin(0,5,0,0)),
        axis.text.x = element_text(size = 28, color = "#000000"),
        axis.text.y = element_text(size = 28, color = "#000000"),
        panel.grid.minor = element_line(size = 0.5),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25))

q1 <- ggplot(tab, aes(x=a.h, y=a.m, colour = factor(Var1))) +
  geom_point(shape = 18, size = 15, show.legend=FALSE) +
  labs(x = "Male - Students per year", y = "Female - Students per year") +
  geom_abline(intercept = 0, linetype = 1, colour = "darkgray") +
  xlim (0,5) + 
  ylim(0,5) +
  geom_text_repel(aes(label = c), show.legend=F, box.padding = unit(1.2, 'lines'), size = 15) +
  scale_color_brewer(type = "qual", palette = 3) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 30, margin = margin(15,0,0,0)),
        axis.title.y = element_text(size = 30, margin = margin(0,20,0,0)),
        axis.text.x = element_text(size = 28, color = "#000000"),
        axis.text.y = element_text(size = 28, color = "#000000"),
        panel.grid.minor = element_line(size = 0.5)) +
  guides(scale = "none")

q2 <- ggplot(matrixnova, aes(x=Média_homens, y=Média_mulheres, colour = factor(Var1))) +
  geom_point(shape = 18, size = 15, show.legend=FALSE) +
  labs(x = "Male - Publications per year", y = "Female - Publications per year", colour = "Per subject") +
  geom_abline(intercept = 0, linetype = 1, colour = "darkgray") +
  xlim(0,13) +
  ylim(0,13) +
  geom_text_repel(aes(label = numeros), show.legend=F, box.padding = unit(1.15, 'lines'), size = 15) +
  scale_color_brewer(type = "qual", palette = 3) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        axis.title.x = element_text(size = 30, margin = margin(15,0,0,0)),
        axis.title.y = element_text(size = 30, margin = margin(0,15,0,0)),
        axis.text.x = element_text(size = 28, color = "#000000"),
        axis.text.y = element_text(size = 28, color = "#000000"),
        panel.grid.minor = element_line(size = 0.5),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25)) +
  guides(scale = "none")

# Extracxt the legend from p1
legend = gtable_filter(ggplotGrob(q), "guide-box") 
blank<-rectGrob(gp=gpar(col="white"))

jpeg(file = "figure_doublegraph.jpeg", width = 1980, height = 660)
grid.arrange(q1, blank, q2, blank, legend, ncol=5, nrow=1, widths=c(1.5, 0.07, 1.5, 0.07, 0.5))
dev.off()
