### Funciones plot ###

# ANTES QUE NADA, CARGA ESTE OBJETO

ensanut_limpia <- readRDS("clean_data/ensanut_limpia.rds")

# funcion para plot 1
# este plot permite evaluar la frecuencia de consumo de alimentos por semana
# para cada estado

week_state_intake <- function(x = ensanut_limpia, state, x_name = "Frecuencia a la semana",
                              y_name = "", title_name, title_alig = 0.5){
  x %>%
    filter(estado == state) %>%
    ggplot(aes(x = as.factor(frec_semana), fill = as.factor(frec_semana)))+
    geom_bar()+
    xlab(x_name)+
    ylab(y_name)+
    ggtitle(title_name)+
    theme(plot.title = element_text(hjust = title_alig))+
    facet_wrap(.~alimentos)+
    theme(legend.position="none")
}



# funcion para plot 2
# este plot permite evaluar frecuencia de consumo de alimentos al dia por estado

daily_state_intake <- function(x = ensanut_limpia, state, x_name = "Frecuencia al dia",
                               y_name = "", title_name, title_alig = 0.5){
  x %>%
    filter(estado == state) %>%
    ggplot(aes(x = as.factor(frec_dia), fill = as.factor(frec_dia)))+
    geom_bar()+
    xlab(x_name)+
    ylab(y_name)+
    ggtitle(title_name)+
    theme(plot.title = element_text(hjust = title_alig))+
    facet_wrap(.~alimentos)+
    theme(legend.position="none")
}


ensanut_limpia %>%
  filter(alimentos == "bebidas",
         estado == "Aguascalientes") %>%
  select(estado, frec_semana)  %>%
  count()


# funcion para plot 3
# este plot permite evaluar frecuencia de consumo de botanas a la semana a nivel nacional
# las botanas se pueden modificar por cualquier elemento de la columna de alimentos

week_food_intake <- function(x = ensanut_limpia, food, x_name = "frecuencia a la semana",
                             y_name = "", title_name, title_alig = 0.5){
  x %>%
    filter(alimentos == food) %>%
    ggplot(aes(x = as.factor(frec_semana), fill = as.factor(frec_semana)))+
    geom_bar()+
    xlab(x_name)+
    ylab(y_name)+
    ggtitle(title_name)+
    theme(plot.title = element_text(hjust = title_alig))+
    theme(legend.position="none")
}


# funcion para plot 4
# plot de frecuencia de consumo de botanas a la semana por estado
# se puede modificar el estado y el alimento

week_food_n_state_intake <- function(x = ensanut_limpia, food, state,
                                     x_name = "Frecuencia a la semana",
                                     y_name = "", title_name, title_alig = 0.5){
  x %>%
    filter(alimentos == food,
           estado == state) %>%
    ggplot(aes(x = as.factor(frec_semana), fill = as.factor(frec_semana)))+
    geom_bar()+
    xlab(x_name)+
    ylab(y_name)+
    ggtitle(title_name)+
    theme(plot.title = element_text(hjust = title_alig))+
    theme(legend.position="none")
}


# funcion para plot 5
# plot de frecuencia de consumo de botanas al dia a nivel nacional
# las botanas se pueden modificar por cualquier elemento de la columna de alimentos

daily_food_intake <- function(x = ensanut_limpia, food, x_name = "Frecuencia al dia",
                              y_name = "", title_name, title_alig = 0.5){
  x %>%
    filter(alimentos == food) %>%
    ggplot(aes(x = as.factor(frec_dia), fill = as.factor(frec_dia)))+
    geom_bar()+
    xlab(x_name)+
    ylab(y_name)+
    ggtitle(title_name)+
    theme(plot.title = element_text(hjust = title_alig))+
    theme(legend.position="none")
}


# funcion para plot 6
# plot de frecuencia de consumo de botanas al dia por estado
# se puede modificar el estado y el alimento

daily_food_n_state_intake <- function(x = ensanut_limpia, food, state,
                                      x_name = "Frecuencia al dia",
                                      y_name = "", title_name, title_alig = 0.5){
  x %>%
    filter(alimentos == food,
           estado == state) %>%
    ggplot(aes(x = as.factor(frec_dia), fill = as.factor(frec_dia)))+
    geom_bar()+
    xlab(x_name)+
    ylab(y_name)+
    ggtitle(title_name)+
    theme(plot.title = element_text(hjust = title_alig))+
    theme(legend.position="none")
}


# funcion para plot 16
# consumo de botanas a la semana a por estado por dominio, por sexo por edad
# se puede modificar el alimento, dominio, estado, sexo y edad categorica

week_intake <- function(x = ensanut_limpia, food, state, domain, sex, age,
                        x_name = "Frecuencia a la semana",
                        y_name = "", title_name, title_alig = 0.5){
  x %>%
    filter(alimentos == food | dominio == domain | estado == state | sexo == sex | edad_categorica == age) %>%
    ggplot(aes(x = as.factor(frec_semana), fill = as.factor(frec_semana)))+
    geom_bar()+
    xlab(x_name)+
    ylab(y_name)+
    ggtitle(title_name)+
    theme(plot.title = element_text(hjust = title_alig))+
    theme(legend.position="none")
}



week_intake(food = "bebidas", state = "Aguascalientes", domain = NULL, sex = NULL, age = NULL,
            title_name = "prueba222",
            title_alig = 1)

# plot 19
# frecuencia de consumo de alimentos en el pais

country_intake <- function(x = ensanut_limpia, title_name, title_alig = 0.5,
                           x_name = "alimentos",
                           y_name = "frecuencia absoluta acumulada"){
  x %>%
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
    ggtitle(title_name)+
    theme(plot.title = element_text(hjust = title_alig))+
    theme(legend.position="none")+
    xlab(x_name)+
    ylab(y_name)

}


# funcion para plot 21
# consumo de alimentos filtrado por dominio, sexo, edad_categorica

region_intake <- function(x = ensanut_limpia, zone, domain, sex, age,
                          title_name, title_alig = 0.5,
                          x_name = "alimentos",
                          y_name = "frecuencia absoluta acumulada"){
  x %>%
    select(alimentos, estado, dominio, sexo, edad_categorica, frec_semana, region) %>%
    filter(region == zone,
           dominio == domain,
           sexo == sex,
           edad_categorica == age) %>%
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
    ggtitle(title_name)+
    theme(plot.title = element_text(hjust = title_alig))+
    theme(legend.position="none")+
    xlab(x_name)+
    ylab(y_name)
}



# plot 22
# frecuencia absoluta acumulada de consumo de alimentos por estado

abs_week_intake <- function(x = ensanut_limpia, state,
                            title_name, title_alig = 0.5,
                            x_name = "alimentos",
                            y_name = "frecuencia absoluta acumulada"){
  x %>%
    filter(estado == state) %>%
    group_by(alimentos, estado) %>%
    summarise_at(vars(frec_semana),
                 list(name = sum)) %>%
    ggplot(aes(x=alimentos, y=name, fill = alimentos))+
    geom_bar(stat = "identity")+
    theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
          axis.text.y = element_text(size = 10),
          panel.border=element_blank(),
          strip.background=element_rect(colour="white", fill="white"))+
    ggtitle(title_name)+
    theme(plot.title = element_text(hjust = title_alig))+
    theme(legend.position="none")+
    xlab(x_name)+
    ylab(y_name)
}



