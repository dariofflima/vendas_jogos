# Vamos iniciar verificando a versão do R que estamos usando para nosso estudo:
R.version

# Inicialmente vamos instalar e carregar os pacotes que iremos utilizar:
# O "tidyverse", que é uma coleção de pacotes R para manipulação de dados. 
# Ele inclui outros pacotes que utilizaremos, o "dplyr" e o "ggplot2". 
# O "dplyr" também é usado para manipular e filtrar dados, e o "ggplot2" 
# é para plotagem de gráficos. O "caret" é um pacote de funções auxiliares 
# de machine learning, e o "caTools" é útil para gerar amostrar e fazer 
# a separação de bases, por exemplo. E o "kableExtra" serve para criar
# tabelas
pacotes <- c("tidyverse","caret", "caTools", "car", "stats", 
             "kableExtra")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregando o dataset e conhecendo sua estrtutura:
vendas_jogos <- read.csv("data/Video_Games_Sales_as_at_22_Dec_2016.csv")

# Exibe a estrutura do dataset
str(vendas_jogos)


# Resumo dos dados:
summary(vendas_jogos)

# Espiando os primeiros registros:
head(vendas_jogos, 10)

# Limpeza da base de dados:
vendas_jogos <- vendas_jogos[!(vendas_jogos$Year_of_Release=="N/A" 
                               | vendas_jogos$Year_of_Release=="1985" 
                               | vendas_jogos$Year_of_Release=="1988" 
                               | vendas_jogos$Year_of_Release=="1992" 
                               | vendas_jogos$User_Score=="tbd" 
                               | vendas_jogos$User_Score=="" 
                               | vendas_jogos$Critic_Score=="" 
                               | vendas_jogos$Developer=="" 
                               | vendas_jogos$Rating==""),]

# Agora vamos tirar os demais dados "n/a"
vendas_jogos <- na.omit(vendas_jogos)

# Agora, vamos trabalhar os dados já existentes e enriquecer a nossa base:
# Vamos criar uma váriavel que mostre a quantidade de plataformas para quais
# o jogo foi lançado
qtd_plataformas <- as.data.frame(table(vendas_jogos$Name), 
                                 col.names = c("Name", 
                                               "Qtd_Plataformas"))
qtd_plataformas <- data.frame(setNames(qtd_plataformas, c("Name", 
                                                          "Qtd_Plataformas")))
vendas_jogos <- merge(vendas_jogos, qtd_plataformas, by = "Name")

# Corrigindo o tipo das variáveis
vendas_jogos$Platform <- as.factor(vendas_jogos$Platform)
vendas_jogos$Year_of_Release <- as.numeric(vendas_jogos$Year_of_Release)
vendas_jogos$Genre <- as.factor(vendas_jogos$Genre)
vendas_jogos$Publisher <- as.factor(vendas_jogos$Publisher)
vendas_jogos$Developer <- as.factor(vendas_jogos$Developer)
vendas_jogos$Rating <- as.factor(vendas_jogos$Rating)
vendas_jogos$User_Score <- as.numeric(vendas_jogos$User_Score)

# Vamos tirar as variáveis desncessárias (as relacionadas as vendas 
# em partes específicas do mundo)
vendas_jogos <- select(vendas_jogos,
                       Name,
                       Platform,
                       Year_of_Release,
                       Genre,
                       Publisher,
                       Global_Sales,
                       Critic_Score,
                       Critic_Count,
                       User_Score,
                       User_Count,
                       Developer,
                       Rating,
                       Qtd_Plataformas)

# Renomeando as colunas para o português:
colnames(vendas_jogos) <- c("Nome",
                            "Plataforma",
                            "Ano_De_Lancamento",
                            "Genero",
                            "Editora",
                            "Vendas_Globais",
                            "Nota_Criticos",
                            "Qtd_Criticos",
                            "Nota_Usuarios",
                            "Qtd_Usuarios",
                            "Desenvolvedora",
                            "Classificacao",
                            "Qtd_Plataformas")

# Agoras vamos plotar um gráfico apresentando as vinte maiores Publishers,
# em volume de vendas
maiores_publishers <- vendas_jogos %>% 
  group_by(Editora) %>% 
  summarize(Vendas_Totais = sum(Vendas_Globais)) %>% 
  arrange(desc(Vendas_Totais)) %>% 
  slice(1:20)

ggplot(maiores_publishers, aes(x = reorder(Editora, Vendas_Totais), 
                               y = Vendas_Totais, fill = Vendas_Totais)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "20 Maiores Publishers em Volume de Vendas",
       x = "Publisher",
       y = "Vendas Totais (em milhões)") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", guide = "none")

# E veremos também as 20 maiores Desenvolvedoras. A primeira posição é 
# ocupada pelo Nintendo, com folga.
maiores_desenvolvedoras <- vendas_jogos %>% 
  group_by(Desenvolvedora) %>% 
  summarize(Vendas_Totais = sum(Vendas_Globais)) %>% 
  arrange(desc(Vendas_Totais)) %>% 
  slice(1:20)

ggplot(maiores_desenvolvedoras, aes(x = reorder(Desenvolvedora, Vendas_Totais), 
                                    y = Vendas_Totais, fill = Vendas_Totais)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "20 Maiores Desenvolvedoras em Volume de Vendas",
       x = "Desenvolvedora",
       y = "Vendas Totais (em milhões)") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", guide = "none")

#aproveitando os dados que utilizamos para os gráficos anterioes, vamos criar 
# variáveis binárias que dizem se uma empresa faz parte do top20
vendas_jogos <- vendas_jogos %>% 
  mutate(Maiores_Publishers = ifelse(Editora %in% 
                                       maiores_publishers$Editora, 
                                       TRUE, FALSE),
         Maiores_Desenv = ifelse(Desenvolvedora %in% 
                                   maiores_desenvolvedoras$Desenvolvedora, 
                                   TRUE, FALSE))

# Agora vamos importar uma segunda base, com as maiores vendas em hardware 
# (consoles) da história
vendas_consoles <- read.csv("data/console.csv")

# Segue a estrutura dessa nova base:
str(vendas_consoles)

# E as primeiras observações:
head(vendas_consoles)

# Podemos observar que os consoles aqui estão com nome completo. 
# Precisamos transformar eles em abreviaçao (que está entre parenteses) 
# para ficar no mesmo padrão do outro dataset, e assim fazermos um merge

# Criando a função que usa expressão regular para encontrar uma substring 
# dentro da string e extraí-la
ext_abv_cons <- function(texto) {
  abreviacao <- regmatches(texto, regexpr("\\((.*?)\\)", texto))[[1]]
  abreviacao <- gsub("\\(|\\)", "", abreviacao) 
  return(abreviacao)
}

# Fazendo um loop para alterar cada uma das observações
for (i in 1:nrow(vendas_consoles)) {
  vendas_consoles$console[i] <- ext_abv_cons(vendas_consoles$console)
}

# Vamos retirar dos primeiros consoles o NS (Switch) e o GB (GameBoy), 
# Pois ambos existiram em períodos não-contemplados por nossa base de jogos 
# (o primeiro veio depois, e o segundo, antes)
vendas_consoles <- vendas_consoles[!(vendas_consoles$console == "NS"
                                     | vendas_consoles$console == "GB"),]

# Agora vamos separar os dez primeiros
vendas_consoles <- slice(vendas_consoles, 1:10)

# E renomear a variável
vendas_consoles <- rename(vendas_consoles, Plataforma = console)

# Vamos gerar o gráfico de disperção Jogos vs Consoles

#primeiro criamos uma tabela resumo com a quantidade de vendas de 
# consoles por plataforma
vendas_consoles <- aggregate(Global ~ Plataforma, data = vendas_consoles, sum)

#agora uma com a quantidade de vendas de jogos por plataforma
vendas_jogos_plataforma <- aggregate(Vendas_Globais ~ Plataforma, 
                                     data = vendas_jogos, sum)

#plotando o gráfico de disperção
ggplot(data = merge(vendas_jogos_plataforma, vendas_consoles, 
                    by = "Plataforma")) +
  aes(x = Vendas_Globais, y = Global, label = Plataforma) +
  geom_point() +
  geom_text(vjust = -1) +
  labs(x = "Vendas de jogos", 
       y = "Vendas de consoles", 
       title = "Vendas de jogos vs. Vendas de consoles por plataforma") +
  theme_bw() +
  geom_smooth(method = lm)

# Por fim, vamos criar um gráfico de barras empilhadas para mostrar 
# as vendas globais por plataforma e gênero.  Podemos ver que o gênero de ação
# é o mais vendido em todas as plataformas, seguido pelo gênero de esportes
vendas_por_plataforma_e_genero <- vendas_jogos %>% 
  group_by(Plataforma, Genero) %>% 
  summarize(Vendas_Totais = sum(Vendas_Globais), .groups = 'drop') %>% 
  arrange(desc(Vendas_Totais))

ggplot(vendas_por_plataforma_e_genero, aes(x = Plataforma, 
                                           y = Vendas_Totais, fill = Genero)) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_fill_brewer(palette = "Set3", "Gênero", 
                    breaks=c("Action",
                             "Adventure", 
                             "Racing", 
                             "Sports", 
                             "Strategy",
                             "Fighting",
                             "Plataform",
                             "Puzzle",
                             "Role-Playing",
                             "Simulation",
                             "Shooter",
                             "Misc"), 
                    labels=c("Ação",
                             "Aventura",
                             "Corrida",
                             "Esporte",
                             "Estratégia",
                             "Luta",
                             "Plataforma",
                             "Quebra-cabeças",
                             "RPG",
                             "Simulação",
                             "Tiro",
                             "Diversos")) + 
  labs(title = "Vendas Globais por Plataforma e Gênero",
       x = "Plataforma",
       y = "Vendas Globais (em milhões)")

# Por fim, um gráfico com os jogos mais vendidos
maiores_jogos <- vendas_jogos %>% 
  group_by(Nome) %>% 
  summarize(Vendas_Totais = sum(Vendas_Globais)) %>% 
  arrange(desc(Vendas_Totais)) %>% 
  slice(1:10)  

ggplot(maiores_jogos, aes(x = reorder(Nome, Vendas_Totais), 
                          y = Vendas_Totais)) +
  geom_col(fill = "#076fa2", width = 0.6) +
  coord_flip() +
  geom_text(aes(label = Vendas_Totais), color = "white", hjust = 1.2) +
  labs(title = "10 Jogos Mais Vendidos",
       x = "Jogo",
       y = "Vendas Totais (em milhões)")

# Agora vamos criar um novo dataset só com as variáveis que interessam ao
# modelo
dataset_modelo <- select(vendas_jogos, Plataforma, Ano_De_Lancamento, 
                         Genero, Vendas_Globais, Nota_Criticos, Nota_Usuarios,
                         Classificacao, Qtd_Plataformas, Editora, 
                         Desenvolvedora)

# ***********************************
# * CRIANDO E ANALISANDO OS MODELOS *
# ***********************************

# ********* Modelo inicial
regressao_linear <- lm(Vendas_Globais ~., data = dataset_modelo)

# Resultados
summary(regressao_linear)

# ********* Modelo Box-Cox

# Calculando o lambda
lambda_bc <- powerTransform(dataset_modelo$Vendas_Globais)

# Inserindo o lambda de Box-Cox no dataset, para fazer a estimação 
# do novo modelo
dataset_modelo$Vendas_Globais_bc <- (((dataset_modelo$Vendas_Globais 
                                    ^ lambda_bc$lambda) - 1) / 
                                    lambda_bc$lambda)

# Rodando o modelo com variável dependente transformada por Box-Cox
regressao_linear_bc <- lm(formula = Vendas_Globais_bc ~ . - Vendas_Globais, 
                          data = dataset_modelo)

# Resultados
summary(regressao_linear_bc)

# ********* Modelo Box-Cox com Stepwise

# Aplicando o procedimento Stepwise
regressao_linear_bc_stepwise <- step(regressao_linear_bc, k = 3.841459)

# Resultados
summary(regressao_linear_bc_stepwise)

# ********* Modelo Inicial com Procedimento 1,5x FIQ

# Definindo o primeiro e terceiro quartil do dataset
quartis <- quantile(dataset_modelo$Vendas_Globais, probs=c(.25, .75))

# Calculando a Faixa Interquantil
fiq <- IQR(dataset_modelo$Vendas_Globais)

# Dataset do modelo 1,5x FIQ
dataset_modelo_fiq <- subset(dataset_modelo, dataset_modelo$Vendas_Globais >
                               (quartis[1] - 1.5*fiq) & 
                               dataset_modelo$Vendas_Globais < 
                               (quartis[2]+1.5*fiq))

# Rodando o Modelo
regressao_linear_fiq <- lm(Vendas_Globais ~., data = dataset_modelo_fiq)

# Resultados
summary(regressao_linear_fiq)

# ********* Modelo Box-Cox sobre o Modelo 1,5x FIQ

# Calculando o lambda
lambda_bc_fiq <- powerTransform(dataset_modelo_fiq$Vendas_Globais)

# Inserindo o lambda de Box-Cox no dataset, para fazer a estimação 
# do novo modelo
dataset_modelo_fiq$Vendas_Globais_bc_fiq <- (((dataset_modelo_fiq$Vendas_Globais 
                                               ^ lambda_bc_fiq$lambda) - 1) / 
                                               lambda_bc_fiq$lambda)

# Rodando o modelo com variável dependente transformada por Box-Cox
regressao_linear_bc_fiq <- lm(formula = Vendas_Globais_bc_fiq ~ . 
                              - Vendas_Globais, data = dataset_modelo_fiq)

# Resultados
summary(regressao_linear_bc_fiq)

# ********* Modelo Box-Cox com Stepwise sobre o Modelo 1,5x FIQ

# Aplicando o procedimento Stepwise
regressao_linear_bc_fiq_stepwise <- step(regressao_linear_bc_fiq, k = 3.841459)

# Resultados
summary(regressao_linear_bc_fiq_stepwise)

# Montando a tabela de resultados em forma de dataframe
tab_resultados <- data.frame(modelo = c("R. Linear", "R. Linear c/ Box-Cox", 
                                        "R. Linear c/ BC e SW", 
                                        "R. Linear 1,5x FIQ",
                                        "R. Linear 1,5x FIQ c/ BC",
                                        "R. Linear 1,5x FIQ c/ BC e SW"),
                         erro_padrao_residual = c(summary(regressao_linear)$sigma,
                                                  summary(regressao_linear_bc)$sigma,
                                                  summary(regressao_linear_bc_stepwise)$sigma,
                                                  summary(regressao_linear_fiq)$sigma, 
                                                  summary(regressao_linear_bc_fiq)$sigma, 
                                                  summary(regressao_linear_bc_fiq_stepwise)$sigma),
                         r_quadrado_multiplo = c(summary(regressao_linear)$r.squared,
                                                 summary(regressao_linear_bc)$r.squared,
                                                 summary(regressao_linear_bc_stepwise)$r.squared,
                                                 summary(regressao_linear_fiq)$r.squared, 
                                                 summary(regressao_linear_bc_fiq)$r.squared, 
                                                 summary(regressao_linear_bc_fiq_stepwise)$r.squared),
                         estatística_f = c(summary(regressao_linear)$fstatistic[1],
                                           summary(regressao_linear_bc)$fstatistic[1],
                                           summary(regressao_linear_bc_stepwise)$fstatistic[1],
                                           summary(regressao_linear_fiq)$fstatistic[1], 
                                           summary(regressao_linear_bc_fiq)$fstatistic[1], 
                                           summary(regressao_linear_bc_fiq_stepwise)$fstatistic[1]))

# Exibindo a tabela de resultados
tab_resultados %>%
  kbl() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)