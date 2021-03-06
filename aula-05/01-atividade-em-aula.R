# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse
library(tidyverse)

# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 
read_csv("aula-05/data/ted_main.csv.gz") -> dF_Aula5

# Visualize o resumo dos dados do dataframe. 
# Verifique os mínimos, máximos, médias e medianas das variáveis numéricas.
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?
summary(dF_Aula5)
dF_Aula5 %>%
  select(duration, film_date, published_date)

# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..
dF_Aula5 <- dF_Aula5 %>%
  mutate(duration = as.duration(duration),
         film_date = as_datetime(film_date),
         published_date = as_datetime(published_date))

# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation
dF_Aula5 <- dF_Aula5 %>%
  mutate(event = as.factor(event),
         speaker_occupation = as.factor(speaker_occupation))

# Retire do dataframe a variável name
dF_Aula5 <- dF_Aula5 %>%
  select(- name)

# Visualize novamente o resumo dos dados do dataframe. 
# Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. 
# Verifique as contagens das variáveis categóricas
summary(dF_Aula5)

# Verifique quais registros possuem a menor quantidade de línguas. 
# Corrija para que possuam no mínimo 1 idioma.
dF_Aula5 <- dF_Aula5 %>%
  mutate(languages = if_else(languages == 0, 1L, languages))

# Verifique os 15 registros com menor data de filmagem. 
dF_Aula5 %>%
  arrange(desc(film_date)) %>%
  tail(15) %>%
  View()

# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo
df_year <- dF_Aula5 %>%
  mutate(AnoFilmagem = year(film_date)) %>%
  group_by(AnoFilmagem) %>%
  count(AnoFilmagem) %>%
  ungroup()

# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.
Quantis <- quantile(df_year$n, probs = seq(from = 0.1, to = 1, by = 0.1))

df_year %>%
  filter(n > 33) %>%
  pull(AnoFilmagem) -> AnosFiltrados

dF_Aula5 %>%
  filter(year(film_date) %in% AnosFiltrados) -> dF_Aula5

# Verifique novamente o resumo dos dados do dataframe
summary(dF_Aula5)

# Verifique os 10 registros com maior duração.
dF_Aula5 %>%
  arrange(desc(duration)) %>%
  head(10) %>%
  View()
  
# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas
mean(dF_Aula5$duration) + (sd(dF_Aula5$duration) * 3)
dF_Aula5 %>%
  filter(duration > as.duration((mean(duration) + (sd(duration) * 3)))) %>%
  View()

# Calcule os 4 quartis e o IQR da duração das apresentações.
# Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil
quantile(as.numeric(dF_Aula5$duration),
         probs = seq(from = 0, to = 1, by = 0.25)) -> Quartil

IQR(dF_Aula5$duration) -> IQR

dF_Aula5 %>%
  filter(as.numeric(duration) > ((IQR * 1.5) + Quartil[4])) %>%
  View()

# Visualize os 10 quantis da quantidade de visualizações
quantile(dF_Aula5$views, probs = seq(from = 0.1, to = 1, by = 0.1))

# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?
mean(dF_Aula5$views) > median(dF_Aula5$views) #Media maior
median(abs(dF_Aula5$views - median(dF_Aula5$views))) > sd(dF_Aula5$views) #Desvio Padrão maior
IQR(dF_Aula5$views) / median(abs(dF_Aula5$views - median(dF_Aula5$views))) #2.192295

#Posso concluir que não estão distribuídos de forma simétrica.

# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações
dF_Aula5 %>%
  arrange(desc(views)) %>%
  head(abs(nrow(dF_Aula5)) / 10) -> Grupo1
mean(Grupo1$languages)
sd(Grupo1$languages)
median(Grupo1$languages)
IQR(Grupo1$languages)

dF_Aula5 %>%
  arrange(desc(views)) %>%
  tail(abs(nrow(dF_Aula5)) / 10) -> Grupo2
mean(Grupo2$languages)
sd(Grupo2$languages)
median(Grupo2$languages)
IQR(Grupo2$languages)

# Determine a quantidade de apresentações por evento cujo nome inicie com TED. 
# Utilize a função str_detect para este filtro
dF_Aula5 %>%
  mutate(TED = str_detect(event, "^TED")) %>%
  filter(TED == TRUE) %>%
  count(event) %>%
  View()

# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de línguas das apresentações
#   * o desvio padrão da quantidade de línguas
#   * o coeficiente de variação da quantidade de línguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES
dF_Aula5 %>%
  group_by(event) %>%
  filter(str_detect(event, "^TED") == TRUE) %>%
  filter(views > median(views)) %>%
  summarise(Quantidade_Apresentações = n(),
            Ano_Evento = min(year(published_date)),
            Média_Linguas = mean(languages),
            Desvio_Padrão_Linguas = sd(languages),
            Coeficiente_Variação = Desvio_Padrão_Linguas / Média_Linguas) %>%
  ungroup() %>%
  filter(Quantidade_Apresentações > 10) %>%
  View()

# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas
#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de línguas
cor(x = dF_Aula5$views, y = dF_Aula5$languages) # Correlação fraca, 0.3758641
cor(x = dF_Aula5$views, y = dF_Aula5$duration) # Correlação desprezível, 0.0592439
cor(x = dF_Aula5$views, y = dF_Aula5$comments) # Correlação moderada, 0.5763873
cor(x = dF_Aula5$comments, y = dF_Aula5$languages) # Correlação fraca, 0.3386114

# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas
dF_Aula5 %>%
  filter(duration < as.duration((mean(duration) + (sd(duration) * 3)))) -> NovoDF
cor(x = NovoDF$views, y = NovoDF$languages) # Correlação fraca, 0,378641
cor(x = NovoDF$views, y = NovoDF$duration) # Correlação desprezível, 0.592439
cor(x = NovoDF$views, y = NovoDF$comments) # Correlação moderada, 0.5769718
cor(x = NovoDF$comments, y = NovoDF$languages) # Correlação fraca, 0.3409573

# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. 
# Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado
read_csv("aula-05/data/ted_main.csv.gz") -> OriginalDF

OriginalDF %>%
  group_by(AnoFilmagem = year(as_datetime(film_date))) %>%
  summarise(MedianaDuração = median(as.duration(duration))) %>%
  ungroup() -> DiferenteDF

cor(x = DiferenteDF$AnoFilmagem, y = DiferenteDF$MedianaDuração)