### Funciones plot ###

# ANTES QUE NADA, CARGA ESTE OBJETO

ensanut_limpia <- readRDS("clean_data/ensanut_limpia.rds")

# funcion para plot 1
# este plot permite evaluar la frecuencia de consumo de alimentos por semana
# para cada estado

week_intake_state <- function(x = ensanut_limpia, state, x_name = "Frecuencia a la semana",
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

daily_intake_state <- function(x = ensanut_limpia, state, x_name = "Frecuencia al dia",
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

week_intake_food <- function(x = ensanut_limpia, food, x_name = "frecuencia a la semana",
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

week_intake_food_n_state <- function(x = ensanut_limpia, food, state,
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


