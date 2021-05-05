##Comparativa de los tres  candidatos más importantes (Jiles, Jadue y Lavin) y del sector indeciso ##

#Creación de la BBDD con la variable

voto <-filter(Pulso, P11_COD%in% c(21,36,45,99))


saveRDS(voto, file = "Original Data/BBDD_voto.rds")


### [Cambio de nombres de etiquetas] ### ------------------------------------------


## Cambio de nombre a  Candidatos ##

voto$P11_COD <-as.numeric(voto$P11_COD)

voto <-mutate(voto, Candi = recode(voto$P11_COD, "21" = "Daniel Jadue", "36" = "Joaquín Lavín",
                                     "45" = "Pamela Jiles", "99" = "No sé"))

## Posición política ##

voto$P13_1 <-as.numeric(voto$P13_1)
voto <-mutate(voto, PosPol = recode(voto$P13_1, "1" = "Izquierda", "2" = "Centro Izquierda", "3" = "Centro",
                                    "4" = "Centro Derecha", "5" = "Derecha", "6" = "Sin posición política",
                                    "7" = "No sé"))

## Partidario u opositor

voto$P29XPARTIDARIO <-as.numeric(voto$P29XPARTIDARIO)

voto <-filter(voto, P29XPARTIDARIO%in% c(1,2,3))

voto<-mutate(voto, Parti = recode(voto$P29XPARTIDARIO, "1" = "Partidario", "2" = "Opositor", "3" = "Independiente"))

table(voto$Parti)

## Situación Laboral

voto$P69 <- as.numeric(voto$P69)

voto <-mutate(voto, Labor = recode(voto$P69, "1" = "Trabajo presencial", "2" = "Teletrabajo", "3" = "Cesante",
                                   "4" = "Sólo estudiando", "5" = "Jefe de Hogar", "6" = "Jubilado"))

voto$P162 <-as.numeric(voto$P162)

voto <-filter(voto, P162%in% c(1,2,3))
voto <-mutate(voto, Moni = recode(voto$P162, "1" = "No alcanza", "2" = "Alcanza justo", "3" = "Alcanza y sobra"))

table(voto$Moni)

## Niveles de felicidad ##

voto$P15 <-as.numeric(voto$P15)
voto <-mutate(voto,Felici = recode(voto$P15, "1" = "Nada Feliz", "2" = "Poco Feliz", "3" = "Medianamente feliz",
                                   "4" = "Feliz", "5" = "Muy Feliz"))


# Caracterización socioeconómica de los cuatro grupos -----------------


##Edades ##

EdadPlot <-ggplot(data = subset(voto, !is.na(Edadrec)),
                  aes(x = factor(Edadrec),
                      y = prop.table(stat(count)),
                      weight = PONDERADOR,
                      fill = factor(Candi),
                      label = scales::percent(prop.table(stat(count)),1))) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Grupo etáreo', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_grid(~Candi)
plot(EdadPlot)

ggsave(EdadPlot, filename = "Results/EdadPlot.png",
       dpi = 400, width = 8, height = 7)


##Género ##

SexoPlot <-ggplot(data = subset(voto, !is.na(Candi)),
                 aes(x = factor(Candi),
                     y = prop.table(stat(count)),
                     weight = PONDERADOR,
                     fill = factor(SexoRecod),
                     label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  labs(title = "Sexo de votantes",
       x = "Sexo", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  xlab("Sexo") + ylab("Porcentaje") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Sexo', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic"))
plot(SexoPlot)

ggsave(SexoPlot, filename = "Results/SexoPlot.png",
       dpi = 400, width = 8, height = 7)

#GSE ##

GSEPlot <-ggplot(data = subset(voto, !is.na(GSERecod)),
                   aes(x = factor(GSERecod),
                       y = prop.table(stat(count)),
                       weight = PONDERADOR,
                       fill = factor(Candi),
                       label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") + 
  labs(title = "GSE de votantes",
       x = "GSE", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  xlab("GSE") + ylab("Porcentaje") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'GSE', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_grid(~Candi)
plot(GSEPlot)

ggsave(GSEPlot, filename = "Results/GSEPlot.png",
       dpi = 400, width = 8, height = 7)


# Características laborales -----------------------------------------------

LaborPlot <-ggplot(data = subset(voto, !is.na(Candi)),
                 aes(x = factor(Candi),
                     y = prop.table(stat(count)),
                     weight = PONDERADOR,
                     fill = factor(Labor),
                     label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") + 
  labs(title = "Situación laboral según candidato",
       x = "GSE", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  xlab("Candidato") + ylab("Porcentaje") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Candidato', fill = 'Situación Laboral') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~Labor, nrow = 2)
plot(LaborPlot)

ggsave(GSEPlot, filename = "Results/LaborPlot.png",
       dpi = 400, width = 8, height = 7)


# Dinero

MoniPlot <-ggplot(data = subset(voto, !is.na(Candi)),
                  aes(x = factor(Candi),
                      y = prop.table(stat(count)),
                      weight = PONDERADOR,
                      fill = factor(Moni),
                      label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Situación Laboral', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic"))
plot(MoniPlot)

ggsave(EdadPlot, filename = "Results/MoniPlot.png",
       dpi = 400, width = 8, height = 7)



# Posición política de los votantes ---------------------------------------


PolitPlot <-ggplot(data = subset(voto, !is.na(PosPol)),
                   aes(x = factor(PosPol),
                       y = prop.table(stat(count)),
                       weight = PONDERADOR,
                       fill = factor(Candi),
                       label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") +
    labs(title = "Posición política, según voto de candidatos",
       x = "Posición Política", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Posición política', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~Candi)
plot(PolitPlot)

ggsave(EdadPlot, filename = "Results/PolitPlot.png",
       dpi = 400, width = 8, height = 7)

## Oposición o no a piñera

PartiPlot <-ggplot(data = subset(voto, !is.na(Candi)),
                   aes(x = factor(Candi),
                       y = prop.table(stat(count)),
                       weight = PONDERADOR,
                       fill = factor(Parti),
                       label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") +
  labs(title = "Posición política, según voto de candidatos",
       x = "Posición Política", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Posición política', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic"))
plot(PartiPlot)

ggsave(EdadPlot, filename = "Results/PartiPlot.png",
       dpi = 400, width = 8, height = 7)



# Problemas de chile ------------------------------------------------------

#Principal problema

#Buena info. hay que depurar: sólo dejar los más importantes (5 primeros) y que se vean las barras.

Problem1Plot <-ggplot(data = subset(voto, !is.na(P10_1)),
                   aes(x = factor(P10_1),
                       y = prop.table(stat(count)),
                       weight = PONDERADOR,
                       fill = factor(Candi),
                       label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") +
  labs(title = "Problema principal del País, según voto",
       x = "Problema Principal", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  xlab("Problema principal") + ylab("Porcentaje") +
  scale_fill_manual("Candidato",
                    values = c("#FF6666", "#00CC66", "#CC0000", "#FF9999"),
                    labels = c("Daniel Jadue", "Joaquin lavin", "Pamela Jiles", "No sé")) +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Problema principal', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~Candi, nrow = 4)
plot(Problem1Plot)

ggsave(Problem1Plot, filename = "resultados/GrafSex.png",
       dpi = 400, width = 8, height = 7)

# Segundo más importante
Problem2Plot <-ggplot(data = subset(voto, !is.na(P10_1)),
                     aes(x = factor(P10_1),
                         y = prop.table(stat(count)),
                         weight = PONDERADOR,
                         fill = factor(Candi),
                         label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") +
  labs(title = "Segundo problema principal del País, según voto",
       x = "Problema Principal", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  xlab("Problema principal") + ylab("Porcentaje") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Problema principal', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~Candi, nrow = 4)
plot(Problem2Plot)

ggsave(Problem2Plot, filename = "resultados/GrafSex.png",
       dpi = 400, width = 8, height = 7)

# Tercer más importante

Problem3Plot <-ggplot(data = subset(voto, !is.na(P10_3)),
                      aes(x = factor(P10_1),
                          y = prop.table(stat(count)),
                          weight = PONDERADOR,
                          fill = factor(Candi),
                          label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") +
  labs(title = "Tercer problema principal del país, según voto",
       x = "Problema Principal", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  xlab("Problema principal") + ylab("Porcentaje") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Problema principal', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~Candi, nrow = 4)
plot(Problem3Plot)

ggsave(Problem2Plot, filename = "resultados/GrafSex.png",
       dpi = 400, width = 8, height = 7)



# Felicidad ---------------------------------------------------------------


HappyPlot <-ggplot(data = subset(voto, !is.na(Felici)),
                      aes(x = factor(Felici),
                          y = prop.table(stat(count)),
                          weight = PONDERADOR,
                          fill = factor(Candi),
                          label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") +
  labs(title = "Grado de felicidad, según votante",
       x = "Grado de felicidad", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Grado de Felicidad', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~Candi, nrow = 4)
plot(HappyPlot)

ggsave(HappyPlot, filename = "Results/HappyPlot.png",
       dpi = 400, width = 8, height = 7)



## Tablas con resultados ##

## Sexo de Votantes ##

Sexo<-table(voto$SexoRecod)

SexoCandi <-voto%>%
  group_by(Candi)%>%
  summarise(Género = round((prop.table(Sexo)*100),2))

## GSE de votantes ##

GSE <-table(voto$GSERecod)

GSECandi <-voto%>%
  group_by(Candi)%>%
  summarise(GSE = round((prop.table(GSE)*100),2))

ctable()



# Tablas Socioeconómicas ------------------------------------------------------------------

## GSE ###

TablaGSE <-ctable(voto$GSERecod, voto$Candi, prop = "c", weights = voto$PONDERADOR, style = 'rmarkdown',
                  headings = F, report.nas = FALSE)

## Sexo ##

TablaSexo <-ctable(voto$SexoRecod, voto$Candi, prop = "c", weights = voto$PONDERADOR, style = 'rmarkdown',
                   chisq = T, headings = F, report.nas = FALSE)
view(TablaSexo)

## Grupo etáreo ##

TablaEdad <-ctable(voto$Edadrec, voto$Candi, prop = "c", weights = voto$PONDERADOR, style = 'rmarkdown',
                   chisq = T, headings = F, report.nas = FALSE)
view(TablaEdad)





# Tablas de situación económica -------------------------------------------

TablaLabor <-ctable(voto$Labor, voto$Candi, prop = "c", weights = voto$PONDERADOR, style = 'rmarkdown',
                    chisq = T, headings = F, report.nas = FALSE)

TablaMoni <-ctable(voto$Moni, voto$Candi, prop = "c", weights = voto$PONDERADOR, style = 'rmarkdown',
                   chisq = T, headings = F, report.nas = FALSE)

# Tablas de posición política ---------------------------------------------

TablaPosPol <-ctable(voto$PosPol, voto$Candi, prop = "c", weights = voto$PONDERADOR, style = 'rmarkdown',
                     headings = F, report.nas = FALSE)
view(TablaPosPol)

TablaParti <-ctable(voto$Parti, voto$Candi, prop = "c", weights = voto$PONDERADOR, style = 'rmarkdown',
                    chisq = T, headings = F, report.nas = FALSE)
view(TablaParti)


# Tablas de emociones -----------------------------------------------------


TablaFelici <-ctable(voto$Felici, voto$Candi, prop = "c", weights = voto$PONDERADOR, style = 'rmarkdown',
                     headings = F, report.nas = FALSE)
view(TablaFelici)


#Agrupar tablas y guardarlas

tablas <-list(TablaEdad, TablaGSE, TablaSexo, TablaLabor, TablaMoni, TablaPosPol, TablaParti, TablaFelici)

saveRDS(tablas, file = "Results/tablas.rds")
