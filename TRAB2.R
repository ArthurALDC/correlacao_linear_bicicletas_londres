#Pacotes
library(dplyr) #Manipulação de dados
library(moments) #Usa para assimetria e curtose 
library(kableExtra) #Tabelas
library(ggplot2)
library(lmtest)
library(knitr)
library(kableExtra)
library(tidyverse)
library(e1071)            # Carregar o pacote

#Dados de bicicletas
dados <- read.csv("D:/PJS/Trab2BiRstudio/london_merged.csv")
#Remover valores NA na coluna t2 antes de calcular (NÃO ESTÁ CARREGANDO N/A)
dados_t2_clean <- dados$t2[!is.na(dados$t2)]

#Calcular quartis novamente
Q1_t2 <- quantile(dados_t2_clean, 0.25)
Q3_t2 <- quantile(dados_t2_clean, 0.75)

#Função para calcular as estatísticas univariadas

calcular_estatisticas <- function(var) {
  c(
    Minimo = min(var, na.rm = TRUE),
    Q1 = quantile(var, 0.25, na.rm = TRUE),
    Mediana = median(var, na.rm = TRUE),
    Q3 = quantile(var, 0.75, na.rm = TRUE),
    Maximo = max(var, na.rm = TRUE),
    Media = mean(var, na.rm = TRUE),
    Desvio_Padrao = sd(var, na.rm = TRUE),
    Assimetria = skewness(var, na.rm = TRUE),
    Curtose = kurtosis(var, na.rm = TRUE),
    Desvio_Interquartilico = IQR(var, na.rm = TRUE)
  )
}

#Calcular as estatísticas para t2 e cnt
estatisticas_t2 <- calcular_estatisticas(dados$t2)
estatisticas_cnt <- calcular_estatisticas(dados$cnt)

#Organizar as estatísticas em uma tabela
estatisticas <- data.frame(
  Variavel = c("t2", "cnt"),
  Minimo = c(estatisticas_t2[1], estatisticas_cnt[1]),
  Q1 = c(estatisticas_t2[2], estatisticas_cnt[2]),
  Mediana = c(estatisticas_t2[3], estatisticas_cnt[3]),
  Q3 = c(estatisticas_t2[4], estatisticas_cnt[4]),  
  Maximo = c(estatisticas_t2[5], estatisticas_cnt[5]),
  Media = c(estatisticas_t2[6], estatisticas_cnt[6]),
  Desvio_Padrao = c(estatisticas_t2[7], estatisticas_cnt[7]),
  Assimetria = c(estatisticas_t2[8], estatisticas_cnt[8]),
  Curtose = c(estatisticas_t2[9], estatisticas_cnt[9]),
  Desvio_Interquartilico = c(estatisticas_t2[10], estatisticas_cnt[10])
)

#Exibir a tabela com kable
estatisticas %>%
  kable("html", caption = "Estatísticas Descritivas das Variáveis t2 e cnt") %>%
  kable_styling(full_width = TRUE)



#Calcular os quartis manualmente
quantile(dados$t2, probs = c(0.25, 0.75), na.rm = TRUE)

##ESCORES Z e OUTLIERS PARA T2
# Calcula a média e o desvio padrão de t2
media_t2 <- mean(dados$t2, na.rm = TRUE)  
desvio_padrao_t2 <- sd(dados$t2, na.rm = TRUE) 

#Calculando os escores Z para t2
escore_z_t2 <- (dados$t2 - media_t2) / desvio_padrao_t2

#Verificando os escores Z de t2
head(escore_z_t2)

#Identificando os outliers (valores com escore Z maior que 3 ou menor que -3)
outliers_t2 <- subset(dados, abs(escore_z_t2) > 3)

#Adicionando os escores Z ao dataframe
dados$escore_z_t2 <- escore_z_t2

#Exibindo um resumo estatístico dos escores Z
summary(escore_z_t2) 
resumo_escores <- summary(escore_z_t2)

#Criando uma tabela com o resumo estatístico dos escores Z
tabela_resumo <- data.frame(
  Estatística = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo"),
  Valor = as.numeric(resumo_escores)
)

#Exibindo a tabela resumo
kable(tabela_resumo, caption = "Resumo Estatístico dos Escores Z para t2")

#Exibe a tabela de estatísticas descritivas das variáveis t2 e cnt
estatisticas %>%
  kable("html", caption = "Estatísticas Descritivas das Variáveis t2 e cnt") %>%
  kable_styling(full_width = TRUE)

#Criando o histograma dos escores Z para t2
ggplot(data = dados, aes(x = escore_z_t2)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histograma dos Escores Z de t2", x = "Escores Z", y = "Frequência") +
  theme_minimal()



##ESCORESZ e OUTLIERS PARA CNT
media_cnt <- mean(dados$cnt, na.rm = TRUE)  # Calcula a média, ignorando valores NA
desvio_padrao_cnt <- sd(dados$cnt, na.rm = TRUE)  # Calcula o desvio padrão
escore_z_cnt <- (dados$cnt - media_cnt) / desvio_padrao_cnt

head(dados$escore_z_cnt)
outliers <- subset(dados, abs(escore_z_cnt) > 3)
dados$escore_z_cnt <- escore_z_cnt
summary(escore_z_cnt) 
resumo_escores <- summary(escore_z_cnt)

#Criando uma tabela com o resumo

tabela_resumo <- data.frame(
  Estatística = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo"),
  Valor = as.numeric(resumo_escores)

)
kable(tabela_resumo, caption = "Resumo Estatístico dos Escores Z")

#Exibe a tabela
estatisticas %>%
  kable("html", caption = "Estatísticas Descritivas das Variáveis t2 e cnt") %>%
  kable_styling(full_width = TRUE)


#Criando o histograma cnt com 
ggplot(data = dados, aes(x = escore_z_cnt)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histograma dos Escores Z de cnt", x = "Escores Z", y = "Frequência") +
  theme_minimal()


#Boxplot usando t2 ggplot2
ggplot(data = dados, aes(y = t2)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.colour = "red") +
  labs(title = "Boxplot da Sensação Térmica (t2)", y = "Temperatura (t2)") +
  theme_minimal()

#Boxplot usando cnt 
ggplot(dados, aes(y = cnt)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.colour = "red", outlier.shape = 16) +
  labs(title = "Boxplot de Número de Viagens (cnt)", y = "Número de Viagens (cnt)") +
  theme_minimal()





######## BIVARIADA #########

# Diagrama de dispersão com linha de tendência entre t2 (sensação térmica) e cnt (número de viagens)
ggplot(dados, aes(x = t2, y = cnt)) +
  geom_point(alpha = 0.5, color = "blue") +  # Pontos do gráfico
  geom_smooth(method = "lm", col = "red", se = TRUE) +  # Linha de tendência linear
  labs(
    title = "Relação entre Sensação Térmica (t2) e Número de Viagens (cnt)",
    x = "Sensação Térmica (°C)",
    y = "Número de Viagens"
  ) +
  theme_minimal()

# Calcular correlação entre t2 e cnt
correlacao_t2_cnt <- cor(dados$t2, dados$cnt, use = "complete.obs")
print(paste("Correlação entre t2 e cnt:", round(correlacao_t2_cnt, 2)))


# Calcular a correlação entre t1 e t2
correlacao_t1_t2 <- cor(dados$t1, dados$t2, use = "complete.obs")
print(paste("Correlação entre t1 e t2:", round(correlacao_t1_t2, 2)))

## t1 e t2: 0.99 


######## Teste de significância para a correlação entre t2 e cnt ######
teste_correlacao <- cor.test(dados$t2, dados$cnt, method = "pearson", use = "complete.obs")

print(teste_correlacao)

### REGRESSÃO LINEAR #####

modelo <- lm(cnt ~ t2, data = dados)
summary(modelo)
#Coeficientes da reta
coeficientes <- coef(modelo)
print(paste("Intercepto (β0):", coeficientes[1]))
print(paste("Coeficiente angular (β1):", coeficientes[2]))

#Diagrama de dispersão com a reta de regressão
ggplot(dados, aes(x = t2, y = cnt)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, col = "red") +
  labs(
    title = "Reta de Regressão: Relação entre t2 e cnt",
    x = "Sensação Térmica (°C)",
    y = "Número de Viagens"
  ) +
  theme_minimal()

#Análise de Variância do modelo
modelo <- lm(cnt ~ t2, data = dados)
anova_result <- anova(modelo)
anova_result
summary(modelo)

####Obter os resíduos####

residuos <- residuals(modelo)
# Resumo estatístico dos resíduos
summary(residuos)
# Histograma dos resíduos
ggplot(data = data.frame(residuos), aes(x = residuos)) +
  geom_histogram(binwidth = 50, fill = "lightblue", color = "black") +
  labs(
    title = "Histograma dos Resíduos",
    x = "Resíduos",
    y = "Frequência"
  ) +
  theme_minimal()

###Durbin Watson###
# Realizar o teste de Durbin-Watson
dw_test <- dwtest(modelo) 

# Exibir o resultado do teste
print(dw_test)
residuos <- residuals(modelo)
plot(residuos, type = "o", col = "blue", 
     main = "Resíduos do Modelo de Regressão", 
     xlab = "Índice", ylab = "Resíduos")
abline(h = 0, col = "red", lty = 2)  # Linha horizontal em 0

# Gráfico de autocorrelação dos resíduos (ACF)
acf(residuos, main = "Função de Autocorrelação dos Resíduos")

###KOLMOGROV-SMIRNOV### 

# Teste de Kolmogorov-Smirnov para normalidade, porém como há dados repetidos é preciso fazer um ajuste
# como é mostrado na saída da mensagem # ties should not be present for the Kolmogorov-Smirnov test

# Adicionar ruído pequeno aos resíduos para evitar valores repetidos
set.seed(123)  # Questão de reprodutibilidade para não ficar gerando números aleatorios diferentes toda vez
residuos_jitter <- residuos + runif(length(residuos), -1e-6, 1e-6)

# Realizar o teste KS com os resíduos ajustados
ks_test <- ks.test(residuos_jitter, "pnorm", mean = mean(residuos), sd = sd(residuos))

# Exibir os resultados do teste
print(ks_test)

##Breusch-Pagan##
modelo <- lm(cnt ~ t2, data = dados)
summary(modelo)


bp_test <- bptest(modelo)

# Exibir o resultado do teste
print(bp_test)

#### TABELAS ##### 

# Informações para a tabela
resultado_correlacao <- c(0.369, 0.356, 0.381)  # Correlation coefficient e intervalo de confiança
resultado_regressao <- c(445.697, 60.534, 1.155) # Intercepto, coeficiente de t2 e erro padrão

# Criar um dataframe com as informações
tabela_resultados <- data.frame(
  Estatística = c("Coeficiente de Correlação de Pearson", "Limite Inferior do Intervalo de Confiança (95%)", "Limite Superior do Intervalo de Confiança (95%)",
                  "Intercepto (Modelo de Regressão)", "Coeficiente de t2 (Modelo de Regressão)", "Erro Padrão (Coeficiente de t2)"),
  Valor = c(resultado_correlacao[1], resultado_correlacao[2], resultado_correlacao[3], 
            resultado_regressao[1], resultado_regressao[2], resultado_regressao[3])
)

# Criar a tabela no formato bonito
tabela_resultados %>%
  kable("html", caption = "Resultados da Correlação de Pearson e Modelo de Regressão Linear") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)



# Transformar os resultados em um data frame, sem a coluna "Variável"
anova_df <- as.data.frame(anova_result)

# Renomear as colunas para maior clareza
colnames(anova_df) <- c("Graus de Liberdade (Df)", "Soma dos Quadrados (Sum Sq)",
                        "Quadrado Médio (Mean Sq)", "Valor F (F value)", "p-valor (Pr(>F))")

# Criar a tabela formatada
anova_df %>%
  kable("html", caption = "Análise de Variância (ANOVA) para cnt e t2") %>%
  kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))





# Obter os valores do modelo
r2 <- summary(modelo)$r.squared
r2_ajustado <- summary(modelo)$adj.r.squared

# Criar a tabela
tabela_r2 <- data.frame(
  `Tipo_de_Coeficiente` = c("R²", "R² Ajustado"),
  `Valor` = c(r2, r2_ajustado)
)
tabela_r2 %>%
  kable("html", caption = "Coeficientes de Determinação do Modelo") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))


# Calcular os resíduos do modelo
residuos <- residuals(modelo)

# Calcular as estatísticas
estatisticas_residuos <- c(
  "Média" = mean(residuos),
  "Mediana" = median(residuos),
  "Mínimo" = min(residuos),
  "Máximo" = max(residuos),
  "1º Quartil" = quantile(residuos, 0.25),
  "3º Quartil" = quantile(residuos, 0.75)
)

# Criar uma tabela com as estatísticas
tabela_residuos <- data.frame(
  Estatística = names(estatisticas_residuos),
  Valor = estatisticas_residuos
)

# Mostrar a tabela
library(kableExtra)
tabela_residuos %>%
  kable("html", caption = "Estatísticas dos Resíduos do Modelo de Regressão") %>%
  kable_styling(full_width = TRUE)

