getwd()

# Carregando o dataset
df <- read.csv('radares.csv', sep = ';')
df

# Renomeando a Coluna Latitude...Longitude
colnames(df)[1]<-'LongLat'
df

# Efetua a limpeza de todos os carater de A a Z contidos nas variáveis/objetos LatLong
df$LongLat <- gsub('[A-Z]', '', df$LongLat)
df


# Efetua a limpeza dos parenteses "()" contidos nas variáveis/objetos LatLong
df$LongLat <- gsub('[:():]', '', df$LongLat)
df

# Dividi as coordenadas da coluna LongLat pelo (-)

#strsplit - divide os elementos de um vetor de caracteres x em substrings de acordo com as
# correspondencias para substring split dentro deles

# rbind - combina vetor, matriz ou dataframe por linhas

# do.call constrói e executa uma chamada de função a partir  de um nome ou uma função a uma lista
# de argumentos a serem passadas
df = data.frame(df, do.call(rbind, strsplit(df$LongLat, '-', FALSE)))
View(df)

# Retira do Dataset a coluna X1 que não contém valores
df$X1 <- NULL
df

options(digits = 16) # Definir as casas decimais, se necessário

# Converte para numerico as variáveis X2 e X3
df$X2 <- as.numeric(as.character(df$X2))
df$X3 <- as.numeric(as.character(df$X3))

# Concatena as variáveis X2 e X3 com o sinal de menos(-)
df$X2 <- -df$X2
df$X3 <- -df$X3
df

# Altera os nomes das variaveis
df$Long <- df$X2
df$Lat <- df$X3

# Exclui as variáveis X2 e X3 e LongLat
df$X2 <- NULL
df$X3 <- NULL
df$LongLat <- NULL
df

# Visualização em formato de tabela
View(df)

# Agora vou renomear algumas variaveis e inverter as ordens das mesmas
# df_ordenado <- df[c('Codigo.Prodam', 'LOCAL', 'Sentido', 'REFERENCIA', 'Lote', 'Velocidade',
                   # 'EQUIPAMENTO.TIPO', 'Captura', 'Enquadramento', 'Publicado', 'Lat', 'Long')]
View(df)
df

# Agora vou renomear algumas variaveis e inverter as ordens das mesmas
library(dplyr)
df_ordenado <- df %>%
  select(Lote, Lat, Long, Codigo.Prodam, LOCAL, REFERENCIA, Sentido, Velocidade,
                    EQUIPAMENTO.TIPO, Captura, Enquadramento, Publicado)
View(df_ordenado)

df <- df_ordenado
str(df)

library(data.table)

table(df$Lote)

# Criação de um gráfico simples
barplot(table(df$Lote), col = 'blue')

Lote_table <- table(df$Lote)
Lote_table <- prop.table(Lote_table) * 100 # valores em porcentagem
Lote_table<-round(Lote_table, digits = 1)
Lote_table

library(ggplot2)

# Verificando o percentual por lote
df_Lote0 <- data.frame(Lote_table)
df_Lote1 <- setnames(df_Lote0,"Var1","Lotes")
df_Lote <-setnames(df_Lote1,"Freq","Percentual")
df_Lote

# Plotando gráfico Básico com o qplot()
library(ggplot2)
qplot(Lote, data = df)

# Grafico um pouco mais elaborado, mas ainda básico
ggplot(df, aes(x = as.factor(Lote), fill = as.factor(Lote)))+
  geom_bar()+
  labs(fill = 'Lote')

# Verificando a classe do objeto df
class(df)

str(df)

# Unindo as variáveis LOCAL e REFERENCIA, pois as duas juntas será o endereço completo do
# equipamento e será usado no popup do mapa.

df$End = paste(df$LOCAL, df$REFERENCIA)
View(df)

install.packages('leaflet')
library(leaflet)

# Comando Básico para geração do Mapa
map <- leaflet(df) %>%  addTiles() %>%  
   addMarkers(~Long, ~Lat, popup = df$End)

# Imprimindo o mapa
map

# Customizando os marcadores de acordo com o lote pertencente

markColors <- function(df){
  sapply(df$Lote, function(Lote){
    if(Lote == 1) {
      'green'
    } else if(Lote == 2){
      'red'
    }else if(Lote == 3){
      'orange'
    }else{
      'blue'
    }  })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'white', 
  library = 'ion',
  markerColor = markColors(df) # markerColor é um atributo do método awesomeIcons e não uma variável
)

# Este trecho configuramos os popups que são as informações exibidas ao clicar
# nos marcadores e também definimos as colunas lat e long para gerar os marcadores
# A idéia do popup foi retirada do site: 
# https://stackoverflow.com/questions/31562383/using-leaflet-library-to-output-multiple-popup-values
# pois ee precisava que mostrasse mais informações sobre cada marcador.

map1 <- leaflet(df) %>% addTiles()%>%
  addAwesomeMarkers(~Long, ~Lat, icon = icons, 
                    popup = paste('Local:', df$End, ' | Latitude:', df$Lat,
                                  'Longitude:', df$Long))
                
# Imprimindo o mapa
map1

# Adicionando legendas para cada cor de icone
map1 %>%
  addLegend(
    position = 'topright',
    colors = c('green', 'red', 'orange', 'blue'),
    labels = c('Lote 1', 'Lote 2', 'Lote 3', 'Lote 4'),
    opacity = 0.75,
    title = 'LEGENDA'
  )
