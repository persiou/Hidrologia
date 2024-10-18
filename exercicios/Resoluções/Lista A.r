##### MA01 #####


dados=data.frame(
    Mês= c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"),
    "1980"= c(114.4, 133.5, 175.7, 86.6, 25.8, 87.1, 199.3, 82.3, 172.3, 163.7, 51.7, 314.6),
    "1981"= c(161.6, 28.3, 63.0, 72.1, 43.8, 17.2, 25.4, 57.2, 71.3, 126.0, 136.9, 145.1),
    "1982"= c(20.9, 279.1, 67.5, 43.3, 69.8, 250.1, 117.1, 56.8, 19.0, 226.3, 258.9, 199.6),
    "1983"= c(267.8, 77.2, 107.8, 148.8, 330.7, 227.0, 264.6, 5.2, 239.1, 77.9, 45.0, 199.6),
    "1984"= c(145.9, 72.3, 160.9, 126.9, 117.9, 149.3, 63.1, 200.9, 108.6, 40.9, 170.7, 130.9),
    check.names = FALSE)

max = apply(dados[,-1], 1, max)
dados_max= data.frame(Mês = dados$Mês, Máximo = max)

min= apply(dados[,-1], 1, min)
dados_min= data.frame(Mês = dados$Mês, Mínimo = min)

med= apply(dados[,-1], 1, mean)
dados_med= data.frame(Mês = dados$Mês, Média = med)

library(ggplot2)

ggplot() +
    geom_line(data = dados_max, aes(x = Mês, y = Máximo, color = "Máximo", group = 1), size = 0.7) +
    geom_line(data = dados_min, aes(x = Mês, y = Mínimo, color = "Mínimo", group = 1), size = 0.7) +
    geom_step(data = dados_med, aes(x = Mês, y = Média, color = "Média", group = 1), size = 0.7) +
    labs(title = "Valores mensais", x = "Mês", y = "Precipitação (mm)") +
    scale_color_manual(name = "Legenda", values = c("Máximo" = "blue", "Mínimo" = "red", "Média" = "green")) +
    scale_x_discrete(limits = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

med_anual=sum(dados_med$Média)

dados_med_perc=data.frame(Mês=dados_med$Mês, P=dados_med$Média*100/med_anual)

ggplot()+
    geom_step(data = dados_med_perc, aes(x = Mês, y = P, group = 1), size = 0.7) +
    geom_text(data = dados_med_perc, aes(x = Mês, y = P, label = round(P, 1)), vjust = -0.5, hjust=-0.7) +
    labs(title = "Pluviograma", x = "Mês", y = "%P") +
    scale_x_discrete(limits = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")) +
    scale_y_continuous(limits = c(0, 20)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))


##### MA02 #####


dados = data.frame(
  ANO = c(1985, 1986, 1987, 1988, 1989),
  A = c(1600, 1500, 1700, 1800, 1300),
  B = c(1200, 1300, 1500, 1200, 1800),
  C = c(1400, 1500, 1300, 1200, 1600),
  D = c(1800, 1200, 1900, 1700, 1500))

estacoes=dados[,-1]

estacoes_difA=rowMeans(estacoes[, -1])
estacoes_difB=rowMeans(estacoes[, -2])
estacoes_difC=rowMeans(estacoes[, -3])
estacoes_difD=rowMeans(estacoes[, -4])

df_A=data.frame(cumsum(dados$A), cumsum(estacoes_difA))
colnames(df_A)=c("Posto", "Outros")

df_B=data.frame(cumsum(dados$B), cumsum(estacoes_difB))
colnames(df_B)=c("Posto", "Outros")

df_C=data.frame(cumsum(dados$A), cumsum(estacoes_difC))
colnames(df_C)=c("Posto", "Outros")

df_D=data.frame(cumsum(dados$D), cumsum(estacoes_difD))
colnames(df_D)=c("Posto", "Outros")

library(ggplot2)

ggplot() +
    geom_point(data = df_A, aes(x = Posto, y = Outros, color = "A")) +
    geom_smooth(data = df_A, aes(x = Posto, y = Outros, color = "A"), method = "lm", se = FALSE, size = 0.7) +
    geom_point(data = df_B, aes(x = Posto, y = Outros, color = "B")) + 
    geom_smooth(data = df_B, aes(x = Posto, y = Outros, color = "B"), method = "lm", se = FALSE, size = 0.7) +
    geom_point(data = df_C, aes(x = Posto, y = Outros, color = "C")) +
    geom_smooth(data = df_C, aes(x = Posto, y = Outros, color = "C"), method = "lm", se = FALSE, size = 0.7) +
    geom_point(data = df_D, aes(x = Posto, y = Outros, color = "D")) +
    geom_smooth(data = df_D, aes(x = Posto, y = Outros, color = "D"), method = "lm", se = FALSE, size = 0.7) +
    labs(title = "Curva duplo acumulativa", x = "Acumulado no posto", y = "Média acumulada") +
    scale_color_manual(name = "Legenda", values = c("A" = "blue", "B" = "red", "C" = "green", "D"= "orange")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))


##### MA03 #####


dados = data.frame(
  ANO = c(1985, 1986, 1987, 1988, 1989),
  A = c(1600, 1500, 1700, 1800, 1300),
  B = c(1200, 1300, 1500, 200, 1800),
  C = c(1400, 1500, 1300, 1200, 1600),
  D = c(1800, 1200, 1900, 1700, 1500))

estacoes=dados[,-1]

estacoes_difA=rowMeans(estacoes[, -1])
estacoes_difB=rowMeans(estacoes[, -2])
estacoes_difC=rowMeans(estacoes[, -3])
estacoes_difD=rowMeans(estacoes[, -4])

df_A=data.frame(cumsum(dados$A), cumsum(estacoes_difA))
colnames(df_A)=c("Posto", "Outros")

df_B=data.frame(cumsum(dados$B), cumsum(estacoes_difB))
colnames(df_B)=c("Posto", "Outros")

df_C=data.frame(cumsum(dados$A), cumsum(estacoes_difC))
colnames(df_C)=c("Posto", "Outros")

df_D=data.frame(cumsum(dados$D), cumsum(estacoes_difD))
colnames(df_D)=c("Posto", "Outros")

library(ggplot2)

ggplot() +
    geom_point(data = df_A, aes(x = Posto, y = Outros, color = "A")) +
    geom_smooth(data = df_A, aes(x = Posto, y = Outros, color = "A"), method = "lm", se = FALSE, size = 0.7) +
    geom_point(data = df_B, aes(x = Posto, y = Outros, color = "B")) + 
    geom_smooth(data = df_B, aes(x = Posto, y = Outros, color = "B"), method = "lm", se = FALSE, size = 0.7) +
    geom_point(data = df_C, aes(x = Posto, y = Outros, color = "C")) +
    geom_smooth(data = df_C, aes(x = Posto, y = Outros, color = "C"), method = "lm", se = FALSE, size = 0.7) +
    geom_point(data = df_D, aes(x = Posto, y = Outros, color = "D")) +
    geom_smooth(data = df_D, aes(x = Posto, y = Outros, color = "D"), method = "lm", se = FALSE, size = 0.7) +
    labs(title = "Curva duplo acumulativa", x = "Acumulado no posto", y = "Média acumulada") +
    scale_color_manual(name = "Legenda", values = c("A" = "blue", "B" = "red", "C" = "green", "D"= "orange")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

df_B$Posto

quebra= df_B[3,1]

df_B_antes= df_B[df_B$Posto <= quebra, ]
df_B_depois= df_B[df_B$Posto > quebra, ]

ggplot() +
  geom_point(data = df_B_antes, aes(x = Posto, y = Outros, color = "B1")) +
  geom_smooth(data = df_B_antes, aes(x = Posto, y = Outros, color = "B1"), method = "lm", se = FALSE, size = 0.7) +
  
  geom_point(data = df_B_depois, aes(x = Posto, y = Outros, color = "B2")) +
  geom_smooth(data = df_B_depois, aes(x = Posto, y = Outros, color = "B2"), method = "lm", se = FALSE, size = 0.7) +
  
  labs(title = "Curva duplo acumulativa", x = "Acumulado no posto", y = "Média acumulada", color = "Legenda") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


##### MA04 #####


dados= data.frame(
  Ano = c(1983, 1984, 1985, 1986, 1987, 1988),
  X = c(1620, 1340, 1410, 1530, 1600, 1740),
  Y = c(1377, 1139, 1198, 1300, 1360, 1479),
  Z = c(1539, 1273, 1340, 1454, 1520, 1653))

dados_px=dados
dados_px[dados_px$Ano == 1983, "X"]= NA
medX=mean(dados_px$X, na.rm = TRUE)
medY=mean(dados_px$Y, na.rm = TRUE)
medZ=mean(dados_px$Z, na.rm = TRUE)
px=((dados_px[is.na(dados_px$X),"Y"]*medX/medY)+(dados_px[is.na(dados_px$X),"Z"]*medX/medZ))/2
px

dados_py=dados
dados_py[dados_py$Ano == 1985, "Y"]= NA
medX=mean(dados_py$X, na.rm = TRUE)
medY=mean(dados_py$Y, na.rm = TRUE)
medZ=mean(dados_py$Z, na.rm = TRUE)
py=((dados_py[is.na(dados_py$Y),"X"]*medY/medX)+(dados_py[is.na(dados_py$Y),"Z"]*medY/medZ))/2
py

dados_pz=dados
dados_pz[dados_pz$Ano == 1988, "Z"]= NA
medX=mean(dados_pz$X, na.rm = TRUE)
medY=mean(dados_pz$Y, na.rm = TRUE)
medZ=mean(dados_pz$Z, na.rm = TRUE)
pz=((dados_pz[is.na(dados_pz$Z),"X"]*medZ/medX)+(dados_pz[is.na(dados_pz$Z),"Y"]*medZ/medY))/2
pz


##### MA05 #####


dados = data.frame(
  Ano = c(1980, 1981, 1982, 1983, 1984),
  A = c(1600, 1500, 1700, 1800, 1300),
  B = c(1200, 1300, 1500, 1200, 1800),
  C = c(1400, 1500, 1300, 1200, 1600),
  D = c(1800, 1200, 1900, 1700, 1500))

p=vector("numeric", (ncol(dados)-1))
for (i in 1:(ncol(dados)-1)){
    dados2=dados
    dados2[dados2$Ano == 1983, i+1]= NA

    med=vector("numeric", (ncol(dados2)-1))
    for (j in 1:(ncol(dados2)-1)) {
        med[j]=mean(dados2[,j+1], na.rm = TRUE)}

    p_aux=vector("numeric", (ncol(dados2)-1))
    for (k in 1:(ncol(dados2)-1)) {
        if (k == i) {
            next}
        p_aux[[k]]=(dados2[is.na(dados2[,i+1]),(k+1)]*med[[i]]/med[[k]])}
            
    p[i]=sum(p_aux)/(ncol(dados2)-2)}

p

estacoes=dados[,-1]

estA=dados
estA[estA$Ano == 1983, "A"]= p[1]
estacoes_difA=rowMeans(estacoes[, -1])
estB=dados
estB[estB$Ano == 1983, "B"]= p[2]
estacoes_difB=rowMeans(estacoes[, -2])
estC=dados
estC[estC$Ano == 1983, "C"]= p[3]
estacoes_difC=rowMeans(estacoes[, -3])
estD=dados
estD[estD$Ano == 1983, "D"]= p[4]
estacoes_difD=rowMeans(estacoes[, -4])

df_A=data.frame(cumsum(estA$A), cumsum(estacoes_difA))
colnames(df_A)=c("Posto", "Outros")

df_B=data.frame(cumsum(estB$B), cumsum(estacoes_difB))
colnames(df_B)=c("Posto", "Outros")

df_C=data.frame(cumsum(estC$C), cumsum(estacoes_difC))
colnames(df_C)=c("Posto", "Outros")

df_D=data.frame(cumsum(estD$D), cumsum(estacoes_difD))
colnames(df_D)=c("Posto", "Outros")

library(ggplot2)

ggplot() +
    geom_point(data = df_A, aes(x = Posto, y = Outros, color = "A")) +
    geom_smooth(data = df_A, aes(x = Posto, y = Outros, color = "A"), method = "lm", se = FALSE, size = 0.7) +
    geom_point(data = df_B, aes(x = Posto, y = Outros, color = "B")) + 
    geom_smooth(data = df_B, aes(x = Posto, y = Outros, color = "B"), method = "lm", se = FALSE, size = 0.7) +
    geom_point(data = df_C, aes(x = Posto, y = Outros, color = "C")) +
    geom_smooth(data = df_C, aes(x = Posto, y = Outros, color = "C"), method = "lm", se = FALSE, size = 0.7) +
    geom_point(data = df_D, aes(x = Posto, y = Outros, color = "D")) +
    geom_smooth(data = df_D, aes(x = Posto, y = Outros, color = "D"), method = "lm", se = FALSE, size = 0.7) +
    labs(title = "Curva duplo acumulativa", x = "Acumulado no posto", y = "Média acumulada") +
    scale_color_manual(name = "Legenda", values = c("A" = "blue", "B" = "red", "C" = "green", "D"= "orange")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))



##### MA06 #####


dados= data.frame(
  Ano= c(1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974),
  Região= c(1030, 980, 990, 1040, 990, 1000, 990, 1020),
  Posto= c(1050, 970, 950, 1070, 1210, 1180, 1230, 1180))

df_acumulado=data.frame(cumsum(dados$Posto), cumsum(dados$Região))
colnames(df_acumulado)=c("Posto", "Outros")

library(ggplot2)
ggplot() +
    geom_point(data = df_acumulado, aes(x = Posto, y = Outros)) +
    geom_smooth(data = df_acumulado, aes(x = Posto, y = Outros), method = "lm", se = FALSE, size = 0.7) +
    labs(title = "Curva duplo acumulativa", x = "Acumulado no posto", y = "Média acumulada", color = "Legenda") +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))

quebra=df_acumulado[4,1]

df_antes= df_acumulado[df_acumulado$Posto <= quebra, ]
df_depois= df_acumulado[df_acumulado$Posto > quebra, ]

Ma=(df_antes[nrow(df_antes),1] - df_antes[1,1]) / (df_antes[nrow(df_antes),2] - df_antes[1,2])
Mo=(df_depois[nrow(df_depois),1] - df_antes[nrow(df_depois),1]) / (df_depois[nrow(df_depois),2] - df_antes[nrow(df_antes),2])
Pa_asterisco=df_antes[nrow(df_antes),1]

df_depois_ajustado=vector("numeric", nrow(df_depois))
for (i in 1:nrow(df_depois)){
df_depois_ajustado[i] = Pa_asterisco + (df_depois[i,1]-Pa_asterisco)*Ma/Mo

}

precipitacoes_ajustadas=vector("numeric", length(df_depois_ajustado))
auxiliar=df_acumulado[4,1]
for (i in 1:length(df_depois_ajustado)){
precipitacoes_ajustadas[i]= df_depois_ajustado[i] - auxiliar 
auxiliar=auxiliar+precipitacoes_ajustadas[i]}

dados_ajustados=dados
for (i in 1:length(precipitacoes_ajustadas)){
    dados_ajustados[(4+i),3]=precipitacoes_ajustadas[i]}

dados_ajustados$Posto = round(dados_ajustados$Posto, 2)

dados_ajustados


##### MA07 #####


dados= data.frame(
  Ano = c(1960, 1961, 1962, 1963, 1964),
  A = c(1600, 1500, 1700, 1700, 1300),
  B = c(1200, 1300, 1500, 1200, 1800),
  C = c(1400, NA, 1300, 1200, 1600),
  D= c(1800, 1200, 1900, 1700, 1500))

medA=mean(dados$A, na.rm = TRUE)
medB=mean(dados$B, na.rm = TRUE)
medC=mean(dados$C, na.rm = TRUE)
medD=mean(dados$D, na.rm = TRUE)
pc=((dados[is.na(dados$C),"A"]*medC/medA)+(dados[is.na(dados$C),"B"]*medC/medB)+(dados[is.na(dados$C),"D"]*medC/medD))/3
pc

dados[is.na(dados$C),"C"]=pc

estacoes=dados[,-1]
estacoes_difA=rowMeans(estacoes[, -1])
estacoes_difB=rowMeans(estacoes[, -2])

df_A=data.frame(cumsum(dados$A), cumsum(estacoes_difA))
colnames(df_A)=c("Posto", "Outros")

df_B=data.frame(cumsum(dados$B), cumsum(estacoes_difB))
colnames(df_B)=c("Posto", "Outros")

ggplot() +
    geom_point(data = df_A, aes(x = Posto, y = Outros, color = "A")) +
    geom_smooth(data = df_A, aes(x = Posto, y = Outros, color = "A"), method = "lm", se = FALSE, size = 0.7) +
    geom_point(data = df_B, aes(x = Posto, y = Outros, color = "B")) + 
    geom_smooth(data = df_B, aes(x = Posto, y = Outros, color = "B"), method = "lm", se = FALSE, size = 0.7) +
    labs(title = "Curva duplo acumulativa", x = "Acumulado no posto", y = "Média acumulada") +
    scale_color_manual(name = "Legenda", values = c("A" = "blue", "B" = "red")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))


##### MA08 #####


pontos = data.frame(
  x = c(0, 0, 8),
  y = c(0, 6, 0),
  chuva = c(1200, 960, 1040)
)

tri_coords= rbind(c(0, 0), c(0, 6), c(8, 0), c(0, 0))

library(deldir)

thiessen= deldir(pontos$x, pontos$y)
poligonos= tile.list(thiessen) #extrai os poligonos de thiessen para uma lista

library(sf)

#converter o triangulo em objeto sf
tri_sf = st_polygon(list(tri_coords)) %>% st_sfc()

#função que converte os poligonos de thiessen em objetos `sf`
thiessen_para_sf = function(polygon) {
  coords = cbind(polygon$x, polygon$y)
  
  #fechar os polígonos
  if (!all(coords[1, ] == coords[nrow(coords), ])) {
    coords = rbind(coords, coords[1, ])
  }
  
  st_polygon(list(coords)) %>% st_sfc()
}

#area onde há intersecção dos polígonos com o perimetro limite
areas = sapply(poligonos, function(polygon) {
  poly_sf = thiessen_para_sf(polygon)
  
  interseccao = st_intersection(poly_sf, tri_sf)
  
  if (length(interseccao) > 0) {
    return(as.numeric(st_area(interseccao)))
  } else {
    return(0)
  }
})

#soma das áreas de influencia
area_total = as.numeric(st_area(tri_sf))

chuva_media = sum(areas * pontos$chuva) / area_total
chuva_media

plot(thiessen, wlines = "tess", main="Polígonos de Thiessen", xlim = c(-1, 9), ylim = c(-1, 7)) #linhas de tesselação
plot(st_geometry(tri_sf), border="blue", lwd=2, add=TRUE)
points(pontos$x, pontos$y, col="red", pch=19)


##### MA09 #####


pontos = data.frame(
  x = c(-49, -50, -50, -49, -49.5),
  y = c(-25, -25, -26, -26, -25.5),
  chuva = c(1200, 1250, 1300, 1350, NA)
)

coords_limites= rbind(c(-49, -25), c(-50, -25), c(-50, -26), c(-49, -26), c(-49, -25))

library(deldir)

thiessen= deldir(pontos$x, pontos$y)
poligonos= tile.list(thiessen) #extrai os polígonos de thiessen para uma lista

library(sf)

#convertendo o perimetro limite em objeto sf
limites_sf = st_polygon(list(coords_limites)) %>% st_sfc()

#função que converte os polígonos de thiessen em objetos sf
thiessen_para_sf = function(polygon) {
  coords = cbind(polygon$x, polygon$y)
  
  #fechar os polígonos
  if (!all(coords[1, ] == coords[nrow(coords), ])) {
    coords = rbind(coords, coords[1, ])
  }

  st_polygon(list(coords)) %>% st_sfc()
}

#area onde há intersecção dos polígonos com o perimetro limite
areas = sapply(poligonos, function(polygon) {
  poly_sf = thiessen_para_sf(polygon)
  
  interseccao = st_intersection(poly_sf, limites_sf)
  
  if (length(interseccao) > 0) {
    return(as.numeric(st_area(interseccao)))
  } else {
    return(0)
  }
})

#soma das areas de influencia
area_total= as.numeric(st_area(limites_sf))

chuva_media=1300

pontos_aux=pontos[-nrow(pontos),]
areas_aux=areas[-length(areas)]

chuva_centro= (chuva_media*area_total - sum(areas_aux*pontos_aux$chuva)) / areas[length(areas)]
chuva_centro

plot(thiessen, wlines = "tess", main="Polígonos de Thiessen")
plot(st_geometry(limites_sf), border="blue", lwd=2, add=TRUE)
points(pontos$x, pontos$y, col="red", pch=19)


##### MA10 #####


pontos = data.frame(
  x = c(0, 20*cos(pi/4), 0, -20*cos(pi/4), 0),
  y = c(0, 20*sin(pi/4), 40*sin(pi/4), 20*sin(pi/4), 20*sin(pi/4)),
  chuva = c(1100, 1320, 1200, 1180, 1400)
)

coords_limites= rbind(c(0, 0), c(20*cos(pi/4), 20*sin(pi/4)), c(0, 40*sin(pi/4)), c(-20*cos(pi/4), 20*sin(pi/4)), c(0, 0))

library(deldir)

thiessen= deldir(pontos$x, pontos$y)
poligonos= tile.list(thiessen) #extrai os polígonos de thiessen para uma lista

library(sf)

#convertendo o perimetro limite em objeto sf
limites_sf = st_polygon(list(coords_limites)) %>% st_sfc()

#função que converte os polígonos de thiessen em objetos sf
thiessen_para_sf = function(polygon) {
  coords = cbind(polygon$x, polygon$y)
  
  #fechar os polígonos
  if (!all(coords[1, ] == coords[nrow(coords), ])) {
    coords = rbind(coords, coords[1, ])
  }

  st_polygon(list(coords)) %>% st_sfc()
}

#area onde há intersecção dos polígonos com o perimetro limite
areas = sapply(poligonos, function(polygon) {
  poly_sf = thiessen_para_sf(polygon)
  
  interseccao = st_intersection(poly_sf, limites_sf)
  
  if (length(interseccao) > 0) {
    return(as.numeric(st_area(interseccao)))
  } else {
    return(0)
  }
})

#soma das areas de influencia
area_total= as.numeric(st_area(limites_sf))

chuva_media= sum(areas*pontos$chuva)/area_total
chuva_media

plot(thiessen, wlines = "tess", main="Polígonos de Thiessen")
plot(st_geometry(limites_sf), border="blue", lwd=2, add=TRUE)
points(pontos$x, pontos$y, col="red", pch=19)


##### MA11 #####


dados=data.frame(
    raio= c(2, 4, 6, 8, 10),
    chuva= c(80, 60, 40, 20, 0))

area_total= pi*100
chuva_centro= 100

areas=vector("numeric",nrow(dados))
areas[1]=pi*dados[1,1]^2
for (i in 2:nrow(dados)) {
    areas[i]=pi*dados[i,1]^2-pi*dados[i-1,1]^2
}

precip_med_isoietas=vector("numeric",nrow(dados))
precip_med_isoietas[1]=(dados[1,2]+chuva_centro)/2
for (i in 2:nrow(dados)){
    precip_med_isoietas[i]=(dados[i,2]+dados[i-1,2])/2
}

chuva_media= sum(areas*precip_med_isoietas)/area_total
chuva_media



##### MA12 #####


chuva=99:0

var_raio=10/length(chuva)

raio=vector("numeric",length(chuva))
raio[1]=var_raio
for (i in 2:length(chuva)){
    raio[i]=raio[i-1]+var_raio
}

area_total= pi*100
chuva_centro= 100

areas=vector("numeric",nrow(dados))
areas[1]=pi*raio[1]^2
for (i in 2:length(raio)) {
    areas[i]=pi*raio[i]^2-pi*raio[i-1]^2
}

precip_med_isoietas=vector("numeric",nrow(dados))
precip_med_isoietas[1]=(chuva[1]+chuva_centro)/2
for (i in 2:length(chuva)){
    precip_med_isoietas[i]=(chuva[i]+chuva[i-1])/2
}

chuva_media= sum(areas*precip_med_isoietas)/area_total
chuva_media


##### MA13 #####


l = 1

pontos = data.frame(
  x = c(0, 0, l/2, l, l, l, l/2, 0, l/2),
  y = c(l/2, l, l, l, l/2, 0, 0, 0, l/2),
  chuva = c(1632, 1600, 1616, 1552, 1568, 1552, 1584, 1568, 1800)
)

coords_limites= rbind(c(0, 0), c(0, l), c(l, l), c(l, 0), c(0, 0))

library(deldir)

thiessen= deldir(pontos$x, pontos$y)
poligonos= tile.list(thiessen) #extrai os polígonos de thiessen para uma lista

library(sf)

#convertendo o perimetro limite em objeto sf
limites_sf = st_polygon(list(coords_limites)) %>% st_sfc()

#função que converte os polígonos de thiessen em objetos sf
thiessen_para_sf = function(polygon) {
  coords = cbind(polygon$x, polygon$y)
  
  #fechar os polígonos
  if (!all(coords[1, ] == coords[nrow(coords), ])) {
    coords = rbind(coords, coords[1, ])
  }

  st_polygon(list(coords)) %>% st_sfc()
}

#area onde há intersecção dos polígonos com o perimetro limite
areas = sapply(poligonos, function(polygon) {
  poly_sf = thiessen_para_sf(polygon)
  
  interseccao = st_intersection(poly_sf, limites_sf)
  
  if (length(interseccao) > 0) {
    return(as.numeric(st_area(interseccao)))
  } else {
    return(0)
  }
})

#soma das areas de influencia
area_total= as.numeric(st_area(limites_sf))

chuva_media= sum(areas*pontos$chuva)/area_total
chuva_media

plot(thiessen, wlines = "tess", main="Polígonos de Thiessen")
plot(st_geometry(limites_sf), border="blue", lwd=2, add=TRUE)
points(pontos$x, pontos$y, col="red", pch=19)


##### MA14 #####


A=sum(c(103.8, 59.1, 115.8, 188.6, 266.6, 230.5, 336.5, 37.2, 166.5, 76.0, 31.7, 194.6))
B=sum(c(217.5, 62.4, 109.7, 171.8, 291.6, 208.8, 235.6, 36.0, 221.1, 51.6, 54.9, 251.3))
C=sum(c(85.6, 34.2, 125.8, 168.2, 250.0, 166.2, 324.0, 30.6, 123.0, 105.4, 128.4, 99.2))
D=sum(c(162.6, 126.8, 90.8, 175.6, 268.2, 206.2, 304.6, 25.0, 207.0, 76.0, 44.2, 173.2))

pontos = data.frame(
  x = c(-49, -49, -(49+10/60), -(49+10/60)),
  y = c(-(25+30/60), -(25+25/60), -(25+20/60), -(25+25/60)),
  chuva = c(A, B, C, D)
)

library(deldir)

thiessen = deldir(pontos$x, pontos$y, rw=c(-49.5, -48.5, -26, -25)) #rw para extender a projeção dos poligonos, uma vez que os limites estão mais distantes dos postos 
poligonos = tile.list(thiessen) #extrai os polígonos de thiessen para uma lista

library(sf)

#shapefile da bacia
bacia = st_read("bacia/watershed.shp")

perimetro = st_boundary(bacia)

coordenadas = st_coordinates(perimetro)

coords_limites = coordenadas[, -3]

#convertendo o perimetro limite em objeto sf
limites_sf = st_polygon(list(coords_limites)) %>% st_sfc()

#função que converte os polígonos de thiessen em objetos sf
thiessen_para_sf = function(polygon) {
  coords = cbind(polygon$x, polygon$y)
  
  #fechar os polígonos
  if (!all(coords[1, ] == coords[nrow(coords), ])) {
    coords = rbind(coords, coords[1, ])
  }

  st_polygon(list(coords)) %>% st_sfc()
}

#area onde há intersecção dos polígonos com o perimetro limite
areas = sapply(poligonos, function(polygon) {
  poly_sf = thiessen_para_sf(polygon)
  
  interseccao = st_intersection(poly_sf, limites_sf)
  
  if (length(interseccao) > 0) {
    return(as.numeric(st_area(interseccao)))
  } else {
    return(0)
  }
})

#soma das areas de influencia
area_total= as.numeric(st_area(limites_sf))

chuva_media= sum(areas*pontos$chuva)/area_total
chuva_media

plot(st_geometry(limites_sf), border="blue", lwd=2)
plot(thiessen, wlines = "tess", main="Polígonos de Thiessen", add=TRUE)
points(pontos$x, pontos$y, col="red", pch=19)


##### MA15 #####


A=c(103.8, 59.1, 115.8, 188.6, 266.6, 230.5, 336.5, 37.2, 166.5, 76.0, 31.7, 194.6)
B=c(217.5, 62.4, 109.7, 171.8, 291.6, 208.8, 235.6, 36.0, 221.1, 51.6, 54.9, 251.3)
C=c(85.6, 34.2, 125.8, 168.2, 250.0, 166.2, 324.0, 30.6, 123.0, 105.4, 128.4, 99.2)
D=c(162.6, 126.8, 90.8, 175.6, 268.2, 206.2, 304.6, 25.0, 207.0, 76.0, 44.2, 173.2)

mes=1

pontos = data.frame(
  x = c(-49, -49, -(49+10/60), -(49+10/60)),
  y = c(-(25+30/60), -(25+25/60), -(25+20/60), -(25+25/60)),
  chuva = c(A[mes], B[mes], C[mes], D[mes])
)

coords_limites= rbind(c(-49, -(25+30/60)), c(-49, -(25+25/60)), c(-(49+10/60), -(25+20/60)), c(-(49+10/60), -(25+25/60)), c(-49, -(25+30/60)))

library(deldir)

thiessen = deldir(pontos$x, pontos$y, rw=c(-49.5, -48.5, -26, -25)) #rw para extender a projeção dos poligonos, uma vez que os limites estão mais distantes dos postos 
poligonos = tile.list(thiessen) #extrai os polígonos de thiessen para uma lista

library(sf)

#shapefile da bacia
bacia = st_read("bacia/watershed.shp")

perimetro = st_boundary(bacia)

coordenadas = st_coordinates(perimetro)

coords_limites = coordenadas[, -3]

#convertendo o perimetro limite em objeto sf
limites_sf = st_polygon(list(coords_limites)) %>% st_sfc()

#função que converte os polígonos de thiessen em objetos sf
thiessen_para_sf = function(polygon) {
  coords = cbind(polygon$x, polygon$y)
  
  #fechar os polígonos
  if (!all(coords[1, ] == coords[nrow(coords), ])) {
    coords = rbind(coords, coords[1, ])
  }

  st_polygon(list(coords)) %>% st_sfc()
}

#area onde há intersecção dos polígonos com o perimetro limite
areas = sapply(poligonos, function(polygon) {
  poly_sf = thiessen_para_sf(polygon)
  
  interseccao = st_intersection(poly_sf, limites_sf)
  
  if (length(interseccao) > 0) {
    return(as.numeric(st_area(interseccao)))
  } else {
    return(0)
  }
})

#soma das areas de influencia
area_total= as.numeric(st_area(limites_sf))

chuva_media= sum(areas*pontos$chuva)/area_total
chuva_media

plot(st_geometry(limites_sf), border="blue", lwd=2)
plot(thiessen, wlines = "tess", main="Polígonos de Thiessen", add=TRUE)
points(pontos$x, pontos$y, col="red", pch=19)

mes=7

pontos = data.frame(
  x = c(-49, -49, -(49+10/60), -(49+10/60)),
  y = c(-(25+30/60), -(25+25/60), -(25+20/60), -(25+25/60)),
  chuva = c(A[mes], B[mes], C[mes], D[mes])
)

chuva_media= sum(areas*pontos$chuva)/area_total
chuva_media

mes=12

pontos = data.frame(
  x = c(-49, -49, -(49+10/60), -(49+10/60)),
  y = c(-(25+30/60), -(25+25/60), -(25+20/60), -(25+25/60)),
  chuva = c(A[mes], B[mes], C[mes], D[mes])
)

chuva_media= sum(areas*pontos$chuva)/area_total
chuva_media


##### MA16 #####


A = sum(c(103.8, 59.1, 115.8, 188.6, 266.6, 230.5, 336.5, 37.2, 166.5, 76.0, 31.7, 194.6))
B = sum(c(217.5, 62.4, 109.7, 171.8, 291.6, 208.8, 235.6, 36.0, 221.1, 51.6, 54.9, 251.3))
C = sum(c(85.6, 34.2, 125.8, 168.2, 250.0, 166.2, 324.0, 30.6, 123.0, 105.4, 128.4, 99.2))
D = sum(c(162.6, 126.8, 90.8, 175.6, 268.2, 206.2, 304.6, 25.0, 207.0, 76.0, 44.2, 173.2))

pontos <- data.frame(
  x = c(-49, -49, -49 - 10/60, -49 - 10/60),
  y = c(-(25 + 30/60), -(25 + 25/60), -(25 + 20/60), -(25 + 25/60)),
  chuva = c(A, B, C, D)
)

library(sp)

#converter os pontos em um objeto SpatialPointsDataFrame
coordinates(pontos) = ~x + y

x_range = range(pontos$x) #limites de x e de y
y_range = range(pontos$y)

#expandir os limites do grade
x_expanded = seq(x_range[1] - 0.2 * diff(x_range), x_range[2] + 0.7 * diff(x_range), 
                  length.out = 100) #numero de elementos da sequencia
y_expanded = seq(y_range[1] - 0.2 * diff(y_range), y_range[2] + 0.2 * diff(y_range), 
                  length.out = 100)

grd = expand.grid(x = x_expanded, y = y_expanded)
coordinates(grd) = ~x + y
gridded(grd) = TRUE

library(gstat)

semivariograma = vgm(psill = 1, model = "Sph", range = 10, nugget = 0)

krigagem= krige(chuva ~ 1, pontos, grd, model = semivariograma)

#convertendo os dados da krigagem em uma matriz para uso no filled.contour
krigagem_matrix = matrix(krigagem$var1.pred, nrow = (length(krigagem$var1.pred))^0.5, ncol = (length(krigagem$var1.pred))^0.5)

#ordenar de forma crescente para utilizar a função filled.contour
x_vals = sort(unique(grd@coords[, 1]))
y_vals = sort(unique(grd@coords[, 2]))

filled.contour(x_vals, y_vals, krigagem_matrix, color.palette = terrain.colors, main = "Krigagem", xlab = "Latitude", ylab = "Longitude")
points(pontos$x, pontos$y, pch = 19, col = "red")
text(pontos$x, pontos$y, labels = pontos$chuva, pos = 3, col = "black")

A = c(103.8, 59.1, 115.8, 188.6, 266.6, 230.5, 336.5, 37.2, 166.5, 76.0, 31.7, 194.6)
B = c(217.5, 62.4, 109.7, 171.8, 291.6, 208.8, 235.6, 36.0, 221.1, 51.6, 54.9, 251.3)
C = c(85.6, 34.2, 125.8, 168.2, 250.0, 166.2, 324.0, 30.6, 123.0, 105.4, 128.4, 99.2)
D = c(162.6, 126.8, 90.8, 175.6, 268.2, 206.2, 304.6, 25.0, 207.0, 76.0, 44.2, 173.2)

precipitacao = A + B + C + D
max_precipitacao = which.max(precipitacao)

pontos <- data.frame(
  x = c(-49, -49, -49 - 10/60, -49 - 10/60),
  y = c(-(25 + 30/60), -(25 + 25/60), -(25 + 20/60), -(25 + 25/60)),
  chuva = c(A[max_precipitacao], B[max_precipitacao], C[max_precipitacao], D[max_precipitacao])
)

library(sp)

#converter os pontos em um objeto SpatialPointsDataFrame
coordinates(pontos) = ~x + y

x_range = range(pontos$x) #limites de x e de y
y_range = range(pontos$y)

#expandir os limites do grade
x_expanded = seq(x_range[1] - 0.2 * diff(x_range), x_range[2] + 0.7 * diff(x_range), 
                  length.out = 100) #numero de elementos da sequencia
y_expanded = seq(y_range[1] - 0.2 * diff(y_range), y_range[2] + 0.2 * diff(y_range), 
                  length.out = 100)

grd = expand.grid(x = x_expanded, y = y_expanded)
coordinates(grd) = ~x + y
gridded(grd) = TRUE

library(gstat)

semivariograma = vgm(psill = 1, model = "Sph", range = 10, nugget = 0)

krigagem= krige(chuva ~ 1, pontos, grd, model = semivariograma)

#convertendo os dados da krigagem em uma matriz para uso no filled.contour
krigagem_matrix = matrix(krigagem$var1.pred, nrow = (length(krigagem$var1.pred))^0.5, ncol = (length(krigagem$var1.pred))^0.5)

#ordenar de forma crescente para utilizar a função filled.contour
x_vals = sort(unique(grd@coords[, 1]))
y_vals = sort(unique(grd@coords[, 2]))

filled.contour(x_vals, y_vals, krigagem_matrix, color.palette = terrain.colors, main = "Krigagem", xlab = "Latitude", ylab = "Longitude")
points(pontos$x, pontos$y, pch = 19, col = "red")
text(pontos$x, pontos$y, labels = pontos$chuva, pos = 3, col = "black")


##### MA17 #####


A = sum(c(103.8, 59.1, 115.8, 188.6, 266.6, 230.5, 336.5, 37.2, 166.5, 76.0, 31.7, 194.6))
B = sum(c(217.5, 62.4, 109.7, 171.8, 291.6, 208.8, 235.6, 36.0, 221.1, 51.6, 54.9, 251.3))
C = sum(c(85.6, 34.2, 125.8, 168.2, 250.0, 166.2, 324.0, 30.6, 123.0, 105.4, 128.4, 99.2))
D = sum(c(162.6, 126.8, 90.8, 175.6, 268.2, 206.2, 304.6, 25.0, 207.0, 76.0, 44.2, 173.2))

pontos <- data.frame(
  x = c(-49, -49, -49 - 10/60, -49 - 10/60),
  y = c(-(25 + 30/60), -(25 + 25/60), -(25 + 20/60), -(25 + 25/60)),
  chuva = c(A, B, C, D)
)

library(sp)

#converter os pontos em um objeto SpatialPointsDataFrame
coordinates(pontos) = ~x + y

x_range = range(pontos$x) #limites de x e de y
y_range = range(pontos$y)

#expandir os limites do grade
x_expanded = seq(x_range[1] - 0.4 * diff(x_range), x_range[2] + 0.8 * diff(x_range), 
                  length.out = 100) #numero de elementos da sequencia
y_expanded = seq(y_range[1] - 0.4 * diff(y_range), y_range[2] + 0.4 * diff(y_range), 
                  length.out = 100)

grd = expand.grid(x = x_expanded, y = y_expanded)
coordinates(grd) = ~x + y
gridded(grd) = TRUE

library(gstat)

semivariograma = vgm(psill = 1, model = "Sph", range = 10, nugget = 0)

krigagem= krige(chuva ~ 1, pontos, grd, model = semivariograma)

library(sf)

bacia = st_read("bacia/watershed.shp")

library(raster)

krigagem_raster = raster(krigagem) #converter para raster

bacia_sp = as(bacia, "Spatial")  #converter o objeto de sf para sp
krigagem_recortada = mask(krigagem_raster, bacia_sp)  #recortar o raster com base na bacia

#ordenar de forma crescente para utilizar a função filled.contour
x_vals = sort(unique(grd@coords[, 1]))
y_vals = sort(unique(grd@coords[, 2]))

#convertendo os dados da krigagem em uma matriz para uso no filled.contour
krigagem_matrix = matrix(krigagem$var1.pred, nrow = (length(krigagem$var1.pred))^0.5, ncol = (length(krigagem$var1.pred))^0.5)

filled.contour(x_vals, y_vals, krigagem_matrix, color.palette = terrain.colors, main = "Krigagem", xlab = "Latitude", ylab = "Longitude")
plot(bacia_sp, border = "blue", lwd = 2, add = TRUE)
points(pontos$x, pontos$y, pch = 19, col = "red")
text(pontos$x, pontos$y, labels = pontos$chuva, pos = 3, col = "black")

prec_media = cellStats(krigagem_recortada, stat = "mean")
prec_media

A = c(103.8, 59.1, 115.8, 188.6, 266.6, 230.5, 336.5, 37.2, 166.5, 76.0, 31.7, 194.6)
B = c(217.5, 62.4, 109.7, 171.8, 291.6, 208.8, 235.6, 36.0, 221.1, 51.6, 54.9, 251.3)
C = c(85.6, 34.2, 125.8, 168.2, 250.0, 166.2, 324.0, 30.6, 123.0, 105.4, 128.4, 99.2)
D = c(162.6, 126.8, 90.8, 175.6, 268.2, 206.2, 304.6, 25.0, 207.0, 76.0, 44.2, 173.2)

precipitacao = A + B + C + D
max_precipitacao = which.max(precipitacao)

pontos <- data.frame(
  x = c(-49, -49, -49 - 10/60, -49 - 10/60),
  y = c(-(25 + 30/60), -(25 + 25/60), -(25 + 20/60), -(25 + 25/60)),
  chuva = c(A[max_precipitacao], B[max_precipitacao], C[max_precipitacao], D[max_precipitacao])
)

library(sp)

#converter os pontos em um objeto SpatialPointsDataFrame
coordinates(pontos) = ~x + y

x_range = range(pontos$x) #limites de x e de y
y_range = range(pontos$y)

#expandir os limites do grade
x_expanded = seq(x_range[1] - 0.4 * diff(x_range), x_range[2] + 0.8 * diff(x_range), 
                  length.out = 100) #numero de elementos da sequencia
y_expanded = seq(y_range[1] - 0.4 * diff(y_range), y_range[2] + 0.4 * diff(y_range), 
                  length.out = 100)

grd = expand.grid(x = x_expanded, y = y_expanded)
coordinates(grd) = ~x + y
gridded(grd) = TRUE

library(gstat)

semivariograma = vgm(psill = 1, model = "Sph", range = 10, nugget = 0)

krigagem= krige(chuva ~ 1, pontos, grd, model = semivariograma)

library(sf)

bacia = st_read("bacia/watershed.shp")

library(raster)

krigagem_raster = raster(krigagem) #converter para raster

bacia_sp = as(bacia, "Spatial")  #converter o objeto de sf para sp
krigagem_recortada = mask(krigagem_raster, bacia_sp)  #recortar o raster com base na bacia

#ordenar de forma crescente para utilizar a função filled.contour
x_vals = sort(unique(grd@coords[, 1]))
y_vals = sort(unique(grd@coords[, 2]))

#convertendo os dados da krigagem em uma matriz para uso no filled.contour
krigagem_matrix = matrix(krigagem$var1.pred, nrow = (length(krigagem$var1.pred))^0.5, ncol = (length(krigagem$var1.pred))^0.5)

filled.contour(x_vals, y_vals, krigagem_matrix, color.palette = terrain.colors, main = "Krigagem", xlab = "Latitude", ylab = "Longitude")
plot(bacia_sp, border = "blue", lwd = 2, add = TRUE)
points(pontos$x, pontos$y, pch = 19, col = "red")
text(pontos$x, pontos$y, labels = pontos$chuva, pos = 3, col = "black")

prec_media = cellStats(krigagem_recortada, stat = "mean")
prec_media


##### MA18 #####


Tr = c(10, 25, 50)
t = seq(5, 120, by = 0.1)

data=data.frame()
for (i in 1:length(Tr)){
  intens=vector("numeric", length(t))
  for (j in 1:length(t)){
    intens[j]= 5950 * Tr[i]^0.217 / (t[j]+26)^1.15
  }
  data_aux = data.frame(Tr = as.factor(Tr[i]), t = t, i = intens) #as.factor faz com que Tr seja interpretado como uma variavel categorica
  data = rbind(data, data_aux)
}

library(ggplot2)

ggplot(data, aes(x = t, y = i, color = Tr)) +
geom_line(size = 1.2) +
labs(title = "Intensidade em função do tempo para diferentes Tr", x = "Tempo (minutos)", y = "Intensidade (mm/h)", color = "Tr (anos)") +
theme_minimal() +
theme(plot.title = element_text(hjust = 0.5))

#Tr= tempo de recorrência (ano)
#i= intensidade da chuva (mm/h)
#t= tempo de duração da chuva (minutos) (é baseado no tempo de concentração da bacia, uma vez que a chuva com duração igual ao tc gera a maior vazão possível. isso acontece pois toda a bacia contribui para a vazao quando esse tempo é atingido)

#limitação: 5<=t<=120


##### MA19 #####


minutos= c(0, 15, 30, 45, 60, 75)
precipitacao_acumulada = c(0, 1.0, 5.0, 7.0, 10.5, 14.0)

df = data.frame(Minutos = minutos, Precipitacao = precipitacao_acumulada)

intervalos= c(15, 22.5, 30)

intensidades_max= c()
for (i in 1:length(intervalos)){
    minutos_interpolacao = minutos + intervalos[i]
    minutos_interpolacao = minutos_interpolacao[minutos_interpolacao <= max(minutos)]  #para nao ultrapasse o ultimo valor

    precipitacao_acumulada_interpolada = approx(df$Minutos, df$Precipitacao, xout = minutos_interpolacao)$y

    intervalo_horas = intervalos[i]/60
    intensidades = c()
    for (j in 1:(length(precipitacao_acumulada_interpolada))) {
        intensidade= (precipitacao_acumulada_interpolada[j]-precipitacao_acumulada[j])/intervalo_horas
        intensidades= rbind(intensidades, intensidade)
    }

intensidades_max[i]= round(max(intensidades), 3)
}

intensidades_max


##### MA20 #####


minutos= c(0, 20, 43, 135, 142, 180, 186, 220, 225,
232, 235, 272, 285, 291, 320, 380, 390, 425)
precipitacao_acumulada = c(0.00, 0.18, 1.15, 1.15, 1.75, 1.80, 1.92, 1.92, 6.78,
8.49, 9.40,10.10, 12.10, 19.70, 27.30, 28.20, 28.70, 29.20)

df = data.frame(Minutos = minutos, Precipitacao = precipitacao_acumulada)

intervalos= c(5, 10, 30, 60, 120)

intensidades_max= c()
for (i in 1:length(intervalos)){
    minutos_interpolacao = minutos + intervalos[i]
    minutos_interpolacao = minutos_interpolacao[minutos_interpolacao <= max(minutos)]

    precipitacao_acumulada_interpolada = approx(df$Minutos, df$Precipitacao, xout = minutos_interpolacao)$y

    intervalo_horas = intervalos[i]/60
    intensidades = c()
    for (j in 1:(length(precipitacao_acumulada_interpolada))) {
        intensidade= (precipitacao_acumulada_interpolada[j]-precipitacao_acumulada[j])/intervalo_horas
        intensidades= rbind(intensidades, intensidade)
    }

intensidades_max[i]= round(max(intensidades), 3)
}

intensidades_max


##### MA21 #####


minutos= c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
precipitacao_acumulada = c(2, 6, 8, 8, 9, 10, 13, 15, 17, 17)

df = data.frame(Minutos = minutos, Precipitacao = precipitacao_acumulada)

intervalos= c(10, 15, 20, 30, 60)

intensidades_max= c()
for (i in 1:length(intervalos)){
    minutos_interpolacao = minutos + intervalos[i]
    minutos_interpolacao = minutos_interpolacao[minutos_interpolacao <= max(minutos)] 

    precipitacao_acumulada_interpolada = approx(df$Minutos, df$Precipitacao, xout = minutos_interpolacao)$y

    intervalo_horas = intervalos[i]/60
    intensidades = c()
    #intensidades = c(precipitacao_acumulada_interpolada[1]/intervalo_horas)
    for (j in 1:(length(precipitacao_acumulada_interpolada))) {
        intensidade= (precipitacao_acumulada_interpolada[j]-precipitacao_acumulada[j])/intervalo_horas
        intensidades= rbind(intensidades, intensidade)
    }

intensidades_max[i]= round(max(intensidades), 3)
}

intensidades_max

df_plot = data.frame(Intervalos = intervalos, Intensidades = intensidades_max)

library(ggplot2)

ggplot(df_plot, aes(x = Intervalos, y = Intensidades)) +
geom_point() +
geom_smooth(method = "lm", formula = y ~ poly(x^(-1.01)), se = FALSE, size = 1) +
labs(x = "Intervalo (minutos)", y = "Intensidade Máxima (mm/h)") +
theme_minimal()


##### MA22 #####


Tr = c(2, 10, 25, 50)
t = seq(5, 120, by = 0.1)

data=data.frame()
for (i in 1:length(Tr)){
  intens=vector("numeric", length(t))
  for (j in 1:length(t)){
    intens[j]= 5950 * Tr[i]^0.217 / (t[j]+26)^1.15
  }
  data_aux = data.frame(Tr = as.factor(Tr[i]), t = t, i = intens) #as.factor faz com que Tr seja interpretado como uma variavel categorica
  data = rbind(data, data_aux)
}

tabela= data.frame(Tr= c(2,10,25,50),
"5"= data[data$t == 5, "i"],
"30"= data[data$t == 30, "i"],
"60"= data[data$t == 60, "i"],
"120"= data[data$t == 120, "i"],
check.names = FALSE)
tabela

library(ggplot2)
ggplot(data, aes(x = t, y = i, color = Tr)) +
geom_line(size = 1.2) +
labs(title = "Intensidade em função do tempo para diferentes Tr", x = "Tempo (minutos)", y = "Intensidade (mm/h)", color = "Tr (anos)") +
theme_minimal() +
theme(plot.title = element_text(hjust = 0.5))


##### MA23 #####


t = c(10, 20, 30, 40)

data=data.frame()
intens=vector("numeric", length(t))
for (i in 1:length(t)){
    intens[i]= 9800 / (t[i] + 26)^1.15
}
data_aux = data.frame(t = t, i = intens) #as.factor faz com que Tr seja interpretado como uma variavel categorica
data = rbind(data, data_aux)

intensidade_ordenada = sort(unique(data$i))

x= intensidade_ordenada[length(intensidade_ordenada)-1]*20/60 - intensidade_ordenada[length(intensidade_ordenada)]*10/60
x
y= intensidade_ordenada[length(intensidade_ordenada)]*10/60
y
z= intensidade_ordenada[length(intensidade_ordenada)-2]*30/60 - intensidade_ordenada[length(intensidade_ordenada)-1]*20/60
z
w= intensidade_ordenada[length(intensidade_ordenada)-3]*40/60 - intensidade_ordenada[length(intensidade_ordenada)-2]*30/60
w


##### MA24 #####


A= 0.1^2
C= 0.5

Tr = c(2, 10, 25, 50)
t = c(5, 30, 60, 120)

data=data.frame()
for (i in 1:length(Tr)){
  intens=vector("numeric", length(t))
  for (j in 1:length(t)){
    intens[j]= 5950 * Tr[i]^0.217 / (t[j]+26)^1.15
  }
  data_aux = data.frame(Tr = as.factor(Tr[i]), t = t, i = intens) #as.factor faz com que Tr seja interpretado como uma variavel categorica
  data = rbind(data, data_aux)
}

vazao= data.frame(Tr=Tr,
"5"= (C*data[data$t == 5, "i"]*A/3.6)*1000,
"30"= (C*data[data$t == 30, "i"]*A/3.6)*1000,
"60"= (C*data[data$t == 60, "i"]*A/3.6)*1000,
"120"= (C*data[data$t == 120, "i"]*A/3.6)*1000,
check.names = FALSE)

vazao


##### MA25 #####


#o balanço hídrico é expresso pela seguinte equação: precipitação= evapotranspiração + escoamento superficial + infiltração
#por sua vez, o método racional foca na parcela do escoamento do balanço hídrico, utilizando a equação Q = C*i*A / 3,6 , onde:
#Q= vazão máxima instantânea (m³/s) (valor da vazão imaginando que o volume precipitado chega ao rio no mesmo momento em que cai na área observada. esse valor representa a vazão no seu momento de máxima, o que coincide com o tempo de duração da chuva para a intensidade utilizada, caso o tempo seja suficiente para que a precipitação em qualquer ponto da bacia chegue à seção do rio que está sendo observada)
#C= coeficiente de escoamento (coeficiente que indica o quanto da precipitação irá se tornar escoamento superficial. observando a equação do balanço hídrico, (1-C) pode ser interpretado como uma aproximação das outras duas parcelas que compõe a equação)
#i= intensidade da precipitação (mm/h) (geralmente calculada utilizando um tempo de evento igual ao tempo de concentração da bacia para que se tenha o )
#A= área de drenagem (km²)
#divisão por 3.6= conversão das unidades de km²*mm/h para m³/s (1000000/1000)/(60*60)
