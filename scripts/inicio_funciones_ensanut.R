# automatizando

# plot 1
# este plot permite evaluar la frecuencia de alimentos por semana
# para cada estado

ensanut_limpia %>%
  filter(estado == "Aguascalientes") %>%
  ggplot(aes(x = as.factor(frec_semana), fill = as.factor(frec_semana)))+
  geom_bar()+
  xlab("Frecuencia a la semana")+
  ylab("")+
  ggtitle("consumo de alimentos a la semana por estado")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(.~alimentos)+
  theme(legend.position="none")

# plot 2
# plot de frecuencia de consumo de alimentos al dia por estado

ensanut_limpia %>%
  filter(estado == "Aguascalientes") %>%
  ggplot(aes(x = as.factor(frec_dia), fill = as.factor(frec_dia)))+
  geom_bar()+
  xlab("Frecuencia al dia")+
  ylab("")+
  ggtitle("consumo de alimentos al dia por estado")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(.~alimentos)+
  theme(legend.position="none")

#plot 3
# plot de frecuencia de consumo de botanas a la semana a nivel nacional
# las botanas se pueden modificar por cualquier elemento de la columna de alimentos

ensanut_limpia %>%
  filter(alimentos == "botanas") %>%
  ggplot(aes(x = as.factor(frec_semana), fill = as.factor(frec_semana)))+
  geom_bar()+
  xlab("Frecuencia a la semana")+
  ylab("")+
  ggtitle("consumo de botanas a la semana a nivel nacional")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")

# plot 4
# plot de frecuencia de consumo de botanas a la semana por estado
# se puede modificar el estado y el alimento

ensanut_limpia %>%
  filter(alimentos == "botanas",
         estado == "Aguascalientes") %>%
  ggplot(aes(x = as.factor(frec_semana), fill = as.factor(frec_semana)))+
  geom_bar()+
  xlab("Frecuencia a la semana")+
  ylab("")+
  ggtitle("consumo de botanas a la semana en Aguascalientes")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")


#plot 5
# plot de frecuencia de consumo de botanas al dia a nivel nacional
# las botanas se pueden modificar por cualquier elemento de la columna de alimentos

ensanut_limpia %>%
  filter(alimentos == "botanas") %>%
  ggplot(aes(x = as.factor(frec_dia), fill = as.factor(frec_dia)))+
  geom_bar()+
  xlab("Frecuencia al dia")+
  ylab("")+
  ggtitle("consumo de botanas al dia a nivel nacional")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")


# plot 6
# plot de frecuencia de consumo de botanas al dia por estado
# se puede modificar el estado y el alimento

ensanut_limpia %>%
  filter(alimentos == "botanas",
         estado == "Aguascalientes") %>%
  ggplot(aes(x = as.factor(frec_dia), fill = as.factor(frec_dia)))+
  geom_bar()+
  xlab("Frecuencia al dia")+
  ylab("")+
  ggtitle("consumo de botanas al dia en Aguascalientes")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")

# plot 7
# consumo de botanas a la semana a nivel nacional por dominio urbano
# se puede modificar el alimento y dominio

ensanut_limpia %>%
  filter(alimentos == "botanas",
         dominio == "urbano") %>%
  ggplot(aes(x = as.factor(frec_semana), fill = as.factor(frec_semana)))+
  geom_bar()+
  xlab("Frecuencia a la semana")+
  ylab("")+
  ggtitle("consumo de botanas a la semana a nivel nacional por dominio urbano")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")


# plot 8
# consumo de botanas a la semana a por estado por dominio urbano
# se puede modificar el alimento, dominio y estado

ensanut_limpia %>%
  filter(alimentos == "botanas",
         dominio == "urbano",
         estado == "Aguascalientes") %>%
  ggplot(aes(x = as.factor(frec_semana), fill = as.factor(frec_semana)))+
  geom_bar()+
  xlab("Frecuencia a la semana")+
  ylab("")+
  ggtitle("consumo de botanas a la semana en Aguascalientes por dominio urbano")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")

# plot 9
# frecuencia de consumo de alimentos al dia a nivel nacional por dominio urbano

ensanut_limpia %>%
  filter(alimentos == "botanas",
         dominio == "urbano") %>%
  ggplot(aes(x = as.factor(frec_dia), fill = as.factor(frec_dia)))+
  geom_bar()+
  xlab("Frecuencia al dia")+
  ylab("")+
  ggtitle("consumo de botanas al dia a nivel nacional por dominio urbano")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")

# plot 10
# consumo de alimentos al dia a por estado por dominio urbano
# se puede modificar el alimento, dominio y estado

ensanut_limpia %>%
  filter(alimentos == "botanas",
         dominio == "urbano",
         estado == "Aguascalientes") %>%
  ggplot(aes(x = as.factor(frec_dia), fill = as.factor(frec_dia)))+
  geom_bar()+
  xlab("Frecuencia al dia")+
  ylab("")+
  ggtitle("consumo de botanas al dia en Aguascalientes por dominio urbano")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")

# plot 11
# consumo de botanas a la semana a nivel nacional por dominio urbano, por sexo
# se puede modificar el alimento y dominio

ensanut_limpia %>%
  filter(alimentos == "botanas",
         dominio == "urbano",
         sexo == "hombre") %>%
  ggplot(aes(x = as.factor(frec_semana), fill = as.factor(frec_semana)))+
  geom_bar()+
  xlab("Frecuencia a la semana")+
  ylab("")+
  ggtitle("consumo de botanas a la semana a nivel nacional por dominio urbano en hombres")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")

# plot 12
# consumo de botanas a la semana a por estado por dominio urbano por sexo
# se puede modificar el alimento, dominio y estado

ensanut_limpia %>%
  filter(alimentos == "botanas",
         dominio == "urbano",
         estado == "Aguascalientes",
         sexo == "hombre") %>%
  ggplot(aes(x = as.factor(frec_semana), fill = as.factor(frec_semana)))+
  geom_bar()+
  xlab("Frecuencia a la semana")+
  ylab("")+
  ggtitle("consumo de botanas a la semana en Aguascalientes por dominio urbano en hombres")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")

# plot 13
# frecuencia de consumo de alimentos al dia a nivel nacional por dominio urbano por sexo

ensanut_limpia %>%
  filter(alimentos == "botanas",
         dominio == "urbano",
         sexo == "hombre") %>%
  ggplot(aes(x = as.factor(frec_dia), fill = as.factor(frec_dia)))+
  geom_bar()+
  xlab("Frecuencia al dia")+
  ylab("")+
  ggtitle("consumo de botanas al dia a nivel nacional por dominio urbano en hombres")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")

# plot 14
# consumo de alimentos al dia a por estado por dominio urbano por sexo
# se puede modificar el alimento, dominio y estado

ensanut_limpia %>%
  filter(alimentos == "botanas",
         dominio == "urbano",
         estado == "Aguascalientes",
         sexo == "hombre") %>%
  ggplot(aes(x = as.factor(frec_dia), fill = as.factor(frec_dia)))+
  geom_bar()+
  xlab("Frecuencia al dia")+
  ylab("")+
  ggtitle("consumo de botanas al dia en Aguascalientes por dominio urbano en hombres")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")


# plot 15
# consumo de botanas a la semana a nivel nacional por dominio urbano, por sexo, por edad
# se puede modificar el alimento, dominio, sexo y edad categorica

ensanut_limpia %>%
  filter(alimentos == "botanas",
         dominio == "urbano",
         sexo == "hombre",
         edad_categorica == "preescolares") %>%
  ggplot(aes(x = as.factor(frec_semana), fill = as.factor(frec_semana)))+
  geom_bar()+
  xlab("Frecuencia a la semana")+
  ylab("")+
  ggtitle("consumo de botanas a la semana a nivel nacional por dominio urbano en hombres preescolares")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")

# plot 16
# consumo de botanas a la semana a por estado por dominio urbano por sexo por edad
# se puede modificar el alimento, dominio, estado, sexo y edad categorica

ensanut_limpia %>%
  filter(alimentos == "botanas",
         dominio == "urbano",
         estado == "Aguascalientes",
         sexo == "hombre",
         edad_categorica == "preescolares") %>%
  ggplot(aes(x = as.factor(frec_semana), fill = as.factor(frec_semana)))+
  geom_bar()+
  xlab("Frecuencia a la semana")+
  ylab("")+
  ggtitle("consumo de botanas a la semana en Aguascalientes por dominio urbano en hombres preescolares")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")


# plot 17
# frecuencia de consumo de alimentos al dia a nivel nacional por dominio urbano por sexo
# por edad

ensanut_limpia %>%
  filter(alimentos == "botanas",
         dominio == "urbano",
         sexo == "hombre",
         edad_categorica == "preescolares") %>%
  ggplot(aes(x = as.factor(frec_dia), fill = as.factor(frec_dia)))+
  geom_bar()+
  xlab("Frecuencia al dia")+
  ylab("")+
  ggtitle("consumo de botanas al dia a nivel nacional por dominio urbano en hombres preescolares")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")


# plot 18
# consumo de alimentos al dia a por estado por dominio urbano por sexo por edad
# se puede modificar el alimento, dominio, estado, sexo y edad

ensanut_limpia %>%
  filter(alimentos == "botanas",
         dominio == "urbano",
         estado == "Aguascalientes",
         sexo == "hombre",
         edad_categorica == "preescolares") %>%
  ggplot(aes(x = as.factor(frec_dia), fill = as.factor(frec_dia)))+
  geom_bar()+
  xlab("Frecuencia al dia")+
  ylab("")+
  ggtitle("consumo de botanas al dia en Aguascalientes por dominio urbano en hombres preescolares")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")


# plot 19
# frecuencia de consumo de alimentos en el pais, sin normalizar


ensanut_limpia %>%
  group_by(alimentos, estado) %>%
  summarise_at(vars(frec_semana),
               list(name = sum)) %>%
  ggplot(aes(x=alimentos, y=name, fill = alimentos))+
  geom_bar(stat = "identity")+
  facet_wrap(.~estado)+
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10),
        panel.border=element_blank(),
        strip.background=element_rect(colour="white", fill="white"))+
  ggtitle("consumo de alimentos en el pais")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  xlab("alimentos")+
  ylab("frecuencia absoluta acumulada")



# plot 20
# sin normalizar
# consumo de alimentos filtrado por dominio, sexo, edad_categorica

ensanut_limpia %>%
  select(alimentos, estado, dominio, sexo, edad_categorica, frec_semana) %>%
  filter(dominio == "urbano",
         sexo == "hombre",
         edad_categorica == "preescolares") %>%
  group_by(alimentos, estado) %>%
  summarise_at(vars(frec_semana),
               list(name = sum)) %>%
  ggplot(aes(x=alimentos, y=name, fill = alimentos))+
  geom_bar(stat = "identity")+
  facet_wrap(.~estado)+
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10),
        panel.border=element_blank(),
        strip.background=element_rect(colour="white", fill="white"))+
  ggtitle("consumo de alimentos en el pais")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  xlab("alimentos")+
  ylab("frecuencia absoluta acumulada")

# plot 21
# sin normalizar
# consumo de alimentos filtrado por dominio, sexo, edad_categorica, region

ensanut_limpia %>%
  select(alimentos, estado, dominio, sexo, edad_categorica, frec_semana, region) %>%
  filter(dominio == "urbano",
         sexo == "hombre",
         edad_categorica == "preescolares",
         region == "centro") %>%
  group_by(alimentos, estado) %>%
  summarise_at(vars(frec_semana),
               list(name = sum)) %>%
  ggplot(aes(x=alimentos, y=name, fill = alimentos))+
  geom_bar(stat = "identity")+
  facet_wrap(.~estado)+
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10),
        panel.border=element_blank(),
        strip.background=element_rect(colour="white", fill="white"))+
  ggtitle("consumo de alimentos en zona centro")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  xlab("alimentos")+
  ylab("frecuencia absoluta acumulada")


# plot 22
# frecuencia de consumo de alimentos por estado, sin normalizar


ensanut_limpia %>%
  filter(estado == "Aguascalientes") %>%
  group_by(alimentos, estado) %>%
  summarise_at(vars(frec_semana),
               list(name = sum)) %>%
  ggplot(aes(x=alimentos, y=name, fill = alimentos))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10),
        panel.border=element_blank(),
        strip.background=element_rect(colour="white", fill="white"))+
  ggtitle("consumo de alimentos en Aguascalientes")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  xlab("alimentos")+
  ylab("frecuencia absoluta acumulada")




# plot 23
# sin normalizar
# consumo de alimentos por estado filtrado por dominio, sexo, edad_categorica

ensanut_limpia %>%
  select(alimentos, estado, dominio, sexo, edad_categorica, frec_semana) %>%
  filter(estado == "Aguascalientes",
         dominio == "urbano",
         sexo == "hombre",
         edad_categorica == "preescolares") %>%
  group_by(alimentos, estado) %>%
  summarise_at(vars(frec_semana),
               list(name = sum)) %>%
  ggplot(aes(x=alimentos, y=name, fill = alimentos))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10),
        panel.border=element_blank(),
        strip.background=element_rect(colour="white", fill="white"))+
  ggtitle("consumo de alimentos en Aguascalientes en zona urbana en hombres preescolares")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  xlab("alimentos")+
  ylab("frecuencia")


### a normalizar


ensanut_limpia %>%
  select(alimentos, estado, frec_semana) %>%
  filter(estado == "Aguascalientes",
         alimentos == "bebidas") %>%
  group_by(estado) %>%
  count() %>%
  summarise(total = sum(freq))

table(ensanut_limpia['estado'])

ensanut_limpia %>%
  select(alimentos, estado, frec_semana) %>%
  group_by(estado) %>%
  summarise_at(vars(estado),
               list(name = sum))
