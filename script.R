# Instalando pacotes -----------------------
pacotes <- c("tidyverse","knitr","kableExtra","car","rgl","gridExtra",
             "PerformanceAnalytics","reshape2","rayshader","psych","pracma",
             "polynom","rqPen","ggrepel", "plotly","factoextra")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)  
} else {
  sapply(pacotes, require, character = T) 
}


# Carregando a base de dados --------------------
dados <- read.csv2("BaseMunicipioMensal.csv")
dados

# Preparando os dados ------------------

# Filtrando o ano e descartando algumas variáveis categóricas
dados <- dados %>%
  filter(ano == 2020) %>%
  select(-fmun_cod,-ano, -mes,-mes_ano, -regiao, -fase)

# Somando as linhas para cada município já que o group_by é um pau no cu
dados <- aggregate(.~fmun, data = dados, FUN= sum)
save(dados,file="dados.Rda")

# Escolhendo as variáveis ------
# Olhando a soma das colunas
teste <- data.frame(mapply(sum,dados[,-1]))
colnames(teste) <- "soma"
teste %>% arrange(desc(soma))

teste <-
  teste %>% summarize(total = sum(soma)) %>%
  mutate(prop = total/sum(total))
teste[1,]
# Observando a base de dados
dados %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12) %>% 
  kable_paper() %>%
  scroll_box(width ="500px", height = "200px")

dados %>%
  kable_paper() %>%
  scroll_box()
# Testando a possibilidade de uma PCA ----------

rho <- cor(dados[,2:54])

# Tentando ver as correlações
chart.Correlationdados[,2:54]

# Construindo um mapa de calor a partir das correlações
ggplotly(
rho %>% 
  melt() %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(x = Var1, y = Var2, label = round(x = value, digits = 3)),
            size = 4) +
  labs(x = NULL,
       y = NULL,
       fill = "Correlações") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))
)
# Teste de esferecidade de Bartlet
cortest.bartlett(R = rho)


# Transformando a variavel fmun em indice para rodar a pca pelo pshych
dados_std <- dados %>%
  column_to_rownames("fmun") %>%
  scale() %>%
  data.frame

afpca <- prcomp(dados_std)
summary(afpca)
afpca$sdev^2

# Screeplot 
ggplotly(
  fviz_eig(X= afpca,
           ggtheme = theme_bw(),
           barcolor = "black",
           barfill = "dodgerblue4",
           linecolor = "darkgoldenrod4")
)
# Sumarizando os dados importantes
relatorio <-  data.frame(eigenvalue = afpca$sdev ^2,
           var_compartilhada = summary(afpca)$importance[2,],
           var_cumulativa = summary(afpca)$importance[3,])
relatorio %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

# Visualizando os eigen vectors que cada variavel tem com o componente principal 
#obtido pela PCA

ggplotly(
  data.frame(afpca$rotation) %>%
    mutate(var = names(dados[2:54])) %>%
    melt(id.vars = "var") %>%
    mutate(var = factor(var)) %>%
    ggplot(aes(x=var, y = value, fill = var)) +
    geom_bar(stat = "identity", color = "black") +
    facet_wrap(~variable) +
    labs(x = NULL, y = NULL, fill = "Legenda:")+
    scale_fill_viridis_d()+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))
)

# Extraindo as cargas fatoriais
k <- sum((afpca$sdev ^2 ) >1 )
cargas_fatoriais <- afpca$rotation[, 1:k] %*% diag(afpca$sdev[1:k])


relatorio_cargas_fatoriais <- data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width= T, 
                font_size = 12)

# Visualizando as comunalidades
data.frame(rowSums(cargas_fatoriais ^ 2 )) %>%
  rename(comunalidades = 1) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)



# Visualizando as cargas fatoriais e comunalidades
data.frame(cargas_fatoriais) %>%
  rename(F1=X1,
         F2=X2) %>%
  mutate(comunalidades = rowSums(cargas_fatoriais ^2 )) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

# Plotando as cargas fatoriais
data.frame(cargas_fatoriais) %>%
  ggplot(aes(x = X1, y = X2)) +
  geom_point(color = "dodgerblue4") +
  geom_hline(yintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_text_repel(label = row.names(cargas_fatoriais)) +
  labs(x = paste("F1", paste0("(",
                              round(summary(afpca)$importance[2,1] * 100,
                                    digits = 2),
                              "%)")),
       y = paste("F2", paste0("(",
                              round(summary(afpca)$importance[2,2] * 100,
                                    digits = 2),
                              "%)"))) +
  theme_bw()


# Construção do ranking ------