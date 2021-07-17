#setwd("~/Analytics/R/SUPOR-dash-Dados")

library(tidyverse)
library(gganimate)
library(plotly)
#devtools::install_github("hrbrmstr/streamgraph", dep=FALSE)
library(streamgraph)



# Dados
desp_df <- read.csv2("despesa.csv")
rec_df <- read.csv2("receita.csv")
gg_df <- read.csv2("GG.csv")
ggl1_df <- read.csv2("GG-L1-outros.csv")
gd_df <- read.csv2("GD.csv")
func_df <- read.csv2("Funcoes.csv")

# Prepara Despesa
desp_df2 <-as_tibble(desp_df)
desp_df2 <- desp_df2 %>%
  pivot_longer(-Exercício,
               names_to = "Tipo",
               values_to = "Valor")

# Prepara Receita
rec_df2 <-as_tibble(rec_df)
rec_df2 <- rec_df2 %>%
  pivot_longer(-Exercício,
               names_to = "Tipo",
               values_to = "Valor")

# Prepara Grupos de Gasto
gg_df2 <-as_tibble(gg_df)
gg_df2 <- gg_df2 %>%
  select(c(Exercício, GG, Dotação_Inicial))

  #  pivot_longer(-c(Exercício, GG),
  #               names_to = "Desp",
  #               values_to = "Valor")%>%
  #gg_df2$Tipo <- paste(gg_df2$GG, "-",gg_df2$Tipo)
  #gg_df2$GG <- NULL
  head(gg_df2)

# Prepara L1 x Outros GG
ggl1_df2 <-as_tibble(ggl1_df)
ggl1_df2 <- ggl1_df2 %>%
  select(c(Exercício, GG, Rel_L1xOutros))
ggl1_df2$Rel_L1xOutros <- round(ggl1_df2$Rel_L1xOutros,2)

  #  pivot_longer(-c(Exercício, GG),
  #               names_to = "Desp",
  #               values_to = "Valor")%>%
  #gg_df2$Tipo <- paste(gg_df2$GG, "-",gg_df2$Tipo)
  #gg_df2$GG <- NULL
  head(ggl1_df2)

  # Prepara Grupos de Despesa
gd_df2 <-as_tibble(gd_df)
gd_df2 <- gd_df2 %>%
  select(c(Exercício, GD, Dotação_Inicial))
gd_df2$GD <- as.factor(gd_df2$GD)
#gd_df2$Dot_Inicial <- as.numeric(gd_df2$Dot_Inicial)
head(gd_df2)

# Prepara Funções
func_df2 <-as_tibble(func_df)
func_df2 <- func_df2 %>%
  select(c(Exercício, Func, Dotação_Inicial))
func_df2$Func <- as.factor(func_df2$Func)
head(func_df2)



# Plota Funções
plot(func_df2$Exercício, func_df2$Dotação_Inicial)

ggplot(func_df2, aes(x=Exercício, y=Dotação_Inicial, group=Func)) +
  geom_line(aes(color=Func, linetype=Func))+
  #  geom_point(aes(color=GG))+
  scale_color_brewer(palette="Dark2")+
  labs(#title = "GG por Dotação Inicial",
    x = "Exercícios (1999 a 2020)",
    y = "Despesa (em Bilhões)",
    colour = "Func")+
  theme(legend.position="bottom") +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = NULL
  )


# Plota Grupos de Despesa
ggplot(gd_df2, aes(x=Exercício, y=Dotação_Inicial, group=GD)) +
  geom_line(aes(color=GD, linetype=GD))+
#  geom_point(aes(color=GD))+
#  scale_color_brewer(palette="Dark2")+
  labs(#title = "GD por Dotação Inicial",
    x = "Exercícios (1999 a 2020)",
    y = "Despesa (em Bilhões)",
    colour = "GD")+
  theme(legend.position="bottom") #+
  scale_y_continuous(
    breaks = seq(-10, 100, 10),
    minor_breaks = NULL
  )
  

# Plota L1 x Outros GG
ggplot(ggl1_df2, aes(x=Exercício, y=Rel_L1xOutros, group=GG)) +
    geom_line(aes(color=GG, linetype=GG))+
    geom_point(aes(color=GG))+
    scale_color_brewer(palette="Dark2")+
    labs(title = "GG-L1 x Outros GG",
         x = "Exercícios (2001 a 2020)",
         y = "Percentual",
         colour = "GG",
         caption = "Fontes: SIAFEM e SIAFE")+
    theme(legend.position="bottom",
    text = element_text(family = "Helvetica Neue"),
#    title = element_text(color = "blue"),
#    plot.caption = element_text(color = "gray"),
    #plot.subtitle = element_text(size = 12)
  )
  #  scale_linetype_manual(values=c("solid", "solid","dotted"))
  

# Plota Grupos de Gasto
ggplot(gg_df2, aes(x=Exercício, y=Dotação_Inicial, group=GG)) +
  geom_line(aes(color=GG, linetype=GG))+
#  geom_point(aes(color=GG))+
  scale_color_brewer(palette="Dark2")+
  labs(#title = "GG por Dotação Inicial",
       x = "Exercícios (1999 a 2020)",
       y = "Despesa (em Bilhões)",
       colour = "GG")+
  theme(legend.position="bottom") +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = NULL
  )
#  scale_linetype_manual(values=c("solid", "solid","dotted"))

# Plota Despesa
ggplot(desp_df2, aes(x=Exercício, y=Valor, group=Tipo)) +
  geom_line(aes(color=Tipo, linetype=Tipo))+
#  geom_point(aes(color=Tipo))+
  scale_color_brewer(palette="Dark2")+
  labs(title = "Dotação Inicial e Despesa Liquidada",
       x = "Exercícios (1999 a 2020)",
       y = "Despesa (em Bilhões)",
       colour = "Tipo")+
  theme(legend.position="bottom")+
  scale_linetype_manual(values=c("solid", "solid","dotted"))+
  labs(caption = "Fontes: SIAFEM e SIAFE")+
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    minor_breaks = NULL
  )
# Here comes the gganimate code
#  transition_manual(Exercício) #+
#  ease_aes('linear')


# Plota streamgraph Grupos de Despesa

# Create data:
dataGD <- data.frame(
  year=gd_df2$Exercício,
  name=gd_df2$GD,
  value=gd_df2$Dotação_Inicial
)
cores <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "gray40", "black")

# Stream graph with a legend
GDstream <- streamgraph(dataGD, key="name", 
                        value="value", offset = "expand", date="year",
                        height="200px", width="800px"
) %>%
  sg_axis_x(2, "year", "%Y") %>%
  sg_fill_tableau(cores) %>%
  sg_legend(show=TRUE, label="Grupo de Despesa: ")

GDstream


# Plota Receita
ggplot(rec_df2, aes(x=Exercício, y=Valor, group=Tipo)) +
  geom_line(aes(color=Tipo, linetype=Tipo))+
  #  geom_point(aes(color=Tipo))+
  scale_color_brewer(palette="Dark2")+
  labs(title = "Receita Inicial e Receita Realizada",
       caption = "Fontes: SIAFEM, SIAFE e IBGE",
       x = "Exercícios (1999 a 2020)",
       y = "Receita (em Bilhões)",
       colour = "Tipo")+
  theme(legend.position="bottom")+
  scale_linetype_manual(values=c("dotted", "dotted", "solid", "solid"))+
  scale_y_continuous(
    breaks = seq(0, 110, 10),
    minor_breaks = NULL
  )


# Plota GD streamgraph

# Library
library(streamgraph)

# Create data:
dataGD <- data.frame(
  year=gd_df2$Exercício,
  name=gd_df2$GD,
  value=gd_df2$Dotação_Inicial
)

# Stream graph with a legend
GDstream <- streamgraph(dataGD, key="name", 
                  value="value",offset = "zero", date="year",
                  height="250px", width="800px"
                  ) %>%
  sg_axis_x(2, "year", "%Y") %>%
  sg_fill_tableau("cyclic") %>%
  sg_legend(show=TRUE, label="Grupo de Despesa: ")

GDstream
# save the widget
#library(htmlwidgets)
#saveWidget(GDstream, file=paste0( getwd(), "/GDstreamgraph.html"))




# Plota Funções (Barplot)

dataTeste <- data.frame(Func = func_df$TitFunc,    # data
                        Valor = func_df2$Dotação_Inicial,
                        Exercício = func_df2$Exercício)

theTable <- dataTeste  %>%       
  filter(Exercício == "1999")

theTable %>%
  ggplot(aes(Func, Valor, fill = Func)) + 
  geom_bar(stat = "identity")+
  scale_x_discrete(limits = ordemFunc) +
  coord_flip()+
  #  scale_color_brewer(palette="Dark2")+
  theme(legend.position="")+
  labs(title = theTable$Exercício,
    x = element_blank(),
    y = "Despesa (em Bilhões)",
    colour = "Func")


#Using scale_x_discrete (limits = ...) to specify the order of bars.
ordemFunc <- c("Comunicações","Energia","Organização Agrária","Desporto e Lazer","Trabalho","Indústria","Urbanismo","Habitação","Cultura","Direitos da Cidadania","Assistência Social","Saneamento","Agricultura","Comércio e Serviços","Ciência e Tecnologia","Reserva de Contingência","Gestão Ambiental","Transporte","Legislativa","Essencial à Justiça","Judiciária","Administração","Saúde","Educação","Segurança Pública","Encargos Especiais","Previdência Social"
)
positions <- c("9", "6", "10", "28", "12", "1","2", "3", "4","5","7","8",
               "11","13","14","15", "16","17","18", "19","20", "21", "22",
               "23", "24", "25", "26", "27", "99")
p <- ggplot(theTable, aes(x = Position)) + scale_x_discrete(limits = positions)


# Plota streamgraph Funções

# Create data:
dataFunc <- data.frame(
  year=func_df$Exercício,
  name=func_df$TitFunc,
  value=func_df$Dotação_Inicial
)
cores <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "gray40", "black")

# Stream graph with a legend
FuncStream <- streamgraph(dataFunc, key="name", 
                        value="value", offset = "expand", date="year",
                        height="200px", width="800px") %>%
  sg_axis_x(2, "year", "%Y") %>%
  sg_fill_tableau(cores) %>%
  sg_legend(show=TRUE, label="Funções: ")

FuncStream

# Create vertical barplot in ggplot2
# dataTeste         
dataTeste <- data.frame(Func = LETTERS[1:15],    # Create example data
                        Valor = 15:1)

ggp <- ggplot(dataTeste, aes(Func, Valor)) +    
  geom_bar(stat = "identity", fill = "Blue")+                                       # Horizontal barplot in ggplot2
  coord_flip()+
  #  scale_color_brewer(palette="Dark2")+
  labs(#title = "GG por Dotação Inicial",
    x = "Funções",
    y = "Despesa (em Bilhões)",
    colour = "Func")#+
#  theme(legend.position="bottom")

renderPlotly(ggp)

