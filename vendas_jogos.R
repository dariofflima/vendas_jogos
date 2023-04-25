# Vamos iniciar verificando a versão do R que estamos usando para nosso estudo:
R.version

# Inicialmente vamos instalar e carregar os pacotes que iremos utilizar:
# O "tidyverse", que é uma coleção de pacotes R para manipulação de dados. 
# Ele inclui outros pacotes que utilizaremos, o "dplyr" e o "ggplot2". 
# O "dplyr" também é usado para manipular e filtrar dados, e o "ggplot2" 
# é para plotagem de gráficos. O "caret" é um pacote de funções auxiliares 
# de machine learning, e o "caTools" é útil para gerar amostrar e fazer 
# a separação de bases, por exemplo."kableExtra" serve para criar
# tabelas. "ggrepel" para organizar as labels nos gráficos."nortest"
# para realizar testes de normalidade. E "olsrr" para testar
# multicolinearidade
pacotes <- c("tidyverse","caret", "caTools", "car", "stats", 
             "kableExtra", "ggrepel", "nortest", "olsrr")

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
vendas_jogos <- read.csv("data/vgsales.csv")

# Exibe a estrutura do dataset
str(vendas_jogos)

# Resumo dos dados:
summary(vendas_jogos)

# Espiando os primeiros registros:
head(vendas_jogos, 10)

# Limpeza da base de dados:
vendas_jogos <- vendas_jogos[!(vendas_jogos$Year=="N/A" 
                               | vendas_jogos$Year>="2017" 
),]

# Agora, vamos trabalhar os dados já existentes e enriquecer a nossa base:
# Vamos criar uma váriavel que mostre a quantidade total de vendas em todas as 
# plataformas
vendas_agr_tds_plat <- setNames(aggregate(vendas_jogos$Global_Sales,
                                          by = list(vendas_jogos$Name),
                                          FUN = sum), c("Name",
                                                        "Vendas_Agr_Tds_Plat"))
vendas_jogos <- merge(vendas_jogos, vendas_agr_tds_plat, by = "Name")

# Agora vamos criar uma váriavel que mostre a quantidade de plataformas para quais
# o jogo foi lançado
qtd_plataformas <- as.data.frame(table(vendas_jogos$Name), 
                                 col.names = c("Name", 
                                               "Qtd_Plataformas"))
qtd_plataformas <- data.frame(setNames(qtd_plataformas, c("Name", 
                                                          "Qtd_Plataformas")))
vendas_jogos <- merge(vendas_jogos, qtd_plataformas, by = "Name")

# Corrigindo o tipo das variáveis e alterando o formato da coluna Year
vendas_jogos$Platform <- as.factor(vendas_jogos$Platform)
vendas_jogos$Year <- 2016 - as.numeric(vendas_jogos$Year)
vendas_jogos$Genre <- as.factor(vendas_jogos$Genre)
vendas_jogos$Publisher <- as.factor(vendas_jogos$Publisher)

# Vamos colocar uma observação por jogo, transpondo as plataformas para
# colunas. Vamos aproveitar e organizar a plataforma por fabricante

# Criando uma tabela de correspondência plataforma/fabricante
tab_correspondencia <- data.frame(Platform = c("XB", "X360", "XOne", "PS",
                                               "PS2", "PS3", "PS4", "PSP",
                                               "PSV", "3DS", "DS", "GB",
                                               "GBA", "GC", "N64", "NES",
                                               "SNES", "Wii", "WiiU", "PC",
                                               "2600", "3DO", "DC", "GEN",
                                               "GG", "NG", "PCFX", "SAT", 
                                               "SCD", "TG16", "WS"),
                                  Fabricante = c("Microsoft", "Microsoft", 
                                                 "Microsoft", "Sony", "Sony",
                                                 "Sony", "Sony", "Sony", "Sony", 
                                                 "Nintendo", "Nintendo", "Nintendo",
                                                 "Nintendo", "Nintendo", "Nintendo",
                                                 "Nintendo", "Nintendo", "Nintendo",
                                                 "Nintendo", "PC", "Outros", 
                                                 "Outros", "Outros", "Outros", 
                                                 "Outros", "Outros", "Outros", 
                                                 "Outros", "Outros", "Outros", 
                                                 "Outros"))

# Mergear as tabelas e incluir a variável "Fabricante"
adiciona_marca_consoles <- merge(vendas_jogos, 
                                 tab_correspondencia)[,
                                                      union(names(vendas_jogos),
                                                            names(tab_correspondencia))]

# Agrupar os dados por jogo e plataforma
jogos_e_plataformas <- adiciona_marca_consoles %>%
  group_by(Name, Fabricante) %>%
  summarize(lancado = TRUE)

# Pivotar a tabela para ter uma coluna por plataforma
jogos_plat_colunas <- jogos_e_plataformas %>%
  pivot_wider(names_from = Fabricante, values_from = lancado, values_fill = FALSE)

# Mergear as tabelas
vendas_jogos <- merge(vendas_jogos, jogos_plat_colunas, by = "Name")

# Renomeando as colunas para o português:
colnames(vendas_jogos) <- c("Nome",
                            "Rank",
                            "Plataforma",
                            "Anos_Desde_Lancado",
                            "Genero",
                            "Editora",
                            "Vendas_AN",
                            "Vendas_EU",
                            "Vendas_JP",
                            "Vendas_Outros",
                            "Vendas_Globais",
                            "Vendas_Agr_Tds_Plat",
                            "Qtd_Plataformas",
                            "Sony",
                            "Microsoft",
                            "PC",
                            "Nintendo",
                            "Outros")

# Plotando as vendas em um histograma
hist(vendas_jogos$Vendas_Agr_Tds_Plat, main = "Vendas Globais de Jogos de Videogame", 
     xlab = "Vendas Globais", ylab = "Frequência", breaks = 180)

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

# Vamos retirar dos primeiros consoles o NS (Switch), pois não existia no
# período contemplado em nossa base (veio depois)
vendas_consoles <- vendas_consoles[!(vendas_consoles$console == "NS"),]

# Agora vamos separar os dez primeiros
vendas_consoles <- slice(vendas_consoles, 1:10)

# E renomear a variável
vendas_consoles <- rename(vendas_consoles, Plataforma = console)

# Vamos gerar o gráfico de dispersão Jogos vs Consoles

# Primeiro criamos uma tabela resumo com a quantidade de vendas de 
# consoles por plataforma
vendas_consoles <- aggregate(Global ~ Plataforma, data = vendas_consoles, sum)

# Agora uma com a quantidade de vendas de jogos por plataforma
vendas_jogos_plataforma <- aggregate(Vendas_Globais ~ Plataforma, 
                                     data = vendas_jogos, sum)

# Plotando o gráfico de dispersão
ggplot(data = merge(vendas_jogos_plataforma, vendas_consoles, 
                    by = "Plataforma")) +
  aes(x = Vendas_Globais, y = Global, label = Plataforma) +
  geom_point() +
  geom_text_repel() +
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

dataset_modelo <- select(vendas_jogos, Anos_Desde_Lancado, Genero,
                         Vendas_Agr_Tds_Plat, Qtd_Plataformas, Outros, 
                         Sony, Microsoft, Nintendo, PC)

# Retirar os registros duplicados
dataset_modelo <- subset(dataset_modelo, !duplicated(dataset_modelo))

# ***********************************
# * CRIANDO E ANALISANDO OS MODELOS *
# ***********************************

# ********* Modelo inicial
regressao_linear <- lm(Vendas_Agr_Tds_Plat ~., data = dataset_modelo)

# Resultados
summary(regressao_linear)

# Teste shapiro-francia, para testar a hipóstese de não-aderência à 
# normalidade, conforme o gráfico visto no começo. selecionamos 5000 registro,
# pois é teste é feito com uma amostra máxima de 5000 registros
sf.test(regressao_linear$residuals[2500:7499])

# ********* Modelo Box-Cox

# Calculando o lambda
lambda_bc <- powerTransform(dataset_modelo$Vendas_Agr_Tds_Plat)

# Inserindo o lambda de Box-Cox no dataset, para fazer a estimação 
# do novo modelo
dataset_modelo$Vendas_Agr_Tds_Plat_bc <- (((dataset_modelo$Vendas_Agr_Tds_Plat 
                                            ^ lambda_bc$lambda) - 1) / 
                                            lambda_bc$lambda)

# Rodando o modelo com variável dependente transformada por Box-Cox
regressao_linear_bc <- lm(formula = Vendas_Agr_Tds_Plat_bc ~ . - Vendas_Agr_Tds_Plat, 
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
quartis <- quantile(dataset_modelo$Vendas_Agr_Tds_Plat, probs=c(.25, .75))

# Calculando a Faixa Interquantil
fiq <- IQR(dataset_modelo$Vendas_Agr_Tds_Plat)

# Dataset do modelo 1,5x FIQ
dataset_modelo_fiq <- subset(dataset_modelo, dataset_modelo$Vendas_Agr_Tds_Plat >
                               (quartis[1] - 1.5 * fiq) & 
                               dataset_modelo$Vendas_Agr_Tds_Plat < 
                               (quartis[2] + 1.5 * fiq))

# Rodando o Modelo
regressao_linear_fiq <- lm(Vendas_Agr_Tds_Plat ~. - Vendas_Agr_Tds_Plat_bc,
                           data = dataset_modelo_fiq)

# Resultados
summary(regressao_linear_fiq)

# ********* Modelo Box-Cox sobre o Modelo 1,5x FIQ

# Calculando o lambda
lambda_bc_fiq <- powerTransform(dataset_modelo_fiq$Vendas_Agr_Tds_Plat)
dataset_modelo_fiq_bc <- dataset_modelo_fiq

# Inserindo o lambda de Box-Cox no dataset, para fazer a estimação 
# do novo modelo
dataset_modelo_fiq_bc$Vendas_Agr_Tds_Plat <- (((dataset_modelo_fiq_bc$Vendas_Agr_Tds_Plat 
                                                    ^ lambda_bc_fiq$lambda) - 1) / 
                                                    lambda_bc_fiq$lambda)

# Rodando o modelo com variável dependente transformada por Box-Cox
regressao_linear_bc_fiq <- lm(formula = Vendas_Agr_Tds_Plat ~ .
                              - Vendas_Agr_Tds_Plat_bc,
                              data = dataset_modelo_fiq_bc)

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

# ***********************
# * TESTANDO OS MODELOS *
# ***********************

# Testando se há overfitting

# Gerando amostra e splitando a base
set.seed(555)
amostra <- createDataPartition(dataset_modelo_fiq_bc$Vendas_Agr_Tds_Plat, p = 0.7, list = FALSE)
base_de_treino <- dataset_modelo_fiq_bc[amostra, ]
base_de_teste <- dataset_modelo_fiq_bc[-amostra, ]

# Rodando a regressão na base de treino
regressao_treino <- lm(Vendas_Agr_Tds_Plat ~ .
                       - Vendas_Agr_Tds_Plat_bc,
                       data = base_de_treino)
regressao_treino_stepwise <- step(regressao_treino, k = 3.841459)

# Rodando as previsões
previsao_treino <- predict(regressao_treino_stepwise, newdata = base_de_treino)
previsao_teste <- predict(regressao_treino_stepwise, newdata = base_de_teste)

# Calculando os resultados

# Calculo do r-quadrado
r_quadrado_treino <- summary(regressao_treino_stepwise)$r.squared
r_quadrado_teste <- cor(base_de_teste$Vendas_Agr_Tds_Plat, previsao_teste) ^ 2

# Calculo do rmse
rmse_treino <- sqrt(mean((base_de_treino$Vendas_Agr_Tds_Plat - previsao_treino) ^ 2))
rmse_teste <- sqrt(mean((base_de_teste$Vendas_Agr_Tds_Plat - previsao_teste) ^ 2))

# Exibindo os resultados
r_quadrado_treino
r_quadrado_teste
rmse_treino
rmse_teste

# Teste de Multicolinearidade
ols_vif_tol(regressao_linear_bc_fiq_stepwise)