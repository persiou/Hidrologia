##### MC01 #####


C= 1
A= 2
Tr= 10
tc= 30

t=tc
i= 5950*Tr^0.217/(t+26)^1.15 #curitiba - parigot de souza

Q= C*i*A/3.6
Q


##### MC02 #####


tc= c(15, 30, 60, 120, 240)
mm_paranagua= c(44, 61, 86, 116, 156)
mm_curitiba= c(44, 63, 85, 93, 95)
mm_pontagrossa= c(38, 61, 71, 78, 118)
mm_saopaulo= c(41, 52, 54, 59, 62)

A= 4.5
C= 0.4

Q= data.frame("Tempo"= tc, 
    "Paranaguá"= C*(mm_paranagua*60/tc)*A/3.6,
    "Curitiba"= C*(mm_curitiba*60/tc)*A/3.6,
    "Ponta Grossa"= C*(mm_pontagrossa*60/tc)*A/3.6,
    "São Paulo"= C*(mm_saopaulo*60/tc)*A/3.6)
Q


##### MC03 #####


H= 30
L= 1
h30= 40
A= 0.2
C= 0.3

t1= 30
i1= h30*60/t1


#encontrando a cidade que mais se aproxima do valor de intensidade para o tempo apresentado inicialmente através das constantes a, b, n e m
parametros_cidades= data.frame("Aracaju"= c(834.205, 0.179, 15, 0.726),
    "Manaus"= c(1136.504, 0.158, 10, 0.764),
    "Belém"= c(1085.508, 0.156, 12, 0.758),
    "Porto Alegre"= c(1297.9, 0.171, 11.619, 0.85),
    "Belo Horizonte"= c(1477.87, 0.1, 20, 0.84),
    "Porto Velho"= c(1181.378, 0.159, 11, 0.757),
    "Brasília"= c(1574.7, 0.207, 8, 0.844),
    "Rio Branco"= c(1419.345, 0.162, 18, 0.795),
    "Cuiabá"= c(1016.453, 0.133, 7.5, 0.739),
    "Rio de Janeiro"= c(1239, 0.15, 20, 0.74),
    "Florianópolis"= c(222, 0.1648, 0, 0.3835),
    "São Luiz"= c(1519.371, 0.161, 28, 0.777),
    "Fortaleza"= c(2345.29, 0.173, 28.31, 0.904),
    "São Paulo"= c(3462.6, 0.172, 22, 1.025),
    "Goiânia"= c(920.45, 0.1422, 12, 0.7599),
    "Teresina"= c(1248.856, 0.177, 10, 0.769),
    "Curitiba"= c(5950, 0.217, 26, 1.15))

indice= c()
diferenca= c()
for (j in 1:ncol(parametros_cidades)){
    a= parametros_cidades[1, j]
    b= parametros_cidades[2, j]
    n= parametros_cidades[3, j]
    m= parametros_cidades[4, j]

    i=a*10^b/(t1+n)^m
    h=i*t1/60

    indice[j]= j
    diferenca[j]= abs(h-h30)
}
diferença_i_i30= data.frame(indice=indice, diferenca=diferenca)
diferença_i_i30= diferença_i_i30[order(diferença_i_i30$diferenca), ]
menor_dif= diferença_i_i30[,1]
indice_menor_dif= menor_dif[1]

cidade=names(parametros_cidades)[indice_menor_dif]
cidade

a= parametros_cidades[1, indice_menor_dif]
b= parametros_cidades[2, indice_menor_dif]
n= parametros_cidades[3, indice_menor_dif]
m= parametros_cidades[4, indice_menor_dif]

tc= 57*(L^3/H)^0.385
t2= tc
i2=a*10^b/(t2+n)^m

Q=C*i2*A/3.6
Q


##### MC04 #####


Tr= 25
A= 3.6
C= 0.5
tc= 50

tempo= c(15, 30, 60)
h= c(40, 55, 85)
i= h*60/tempo

dados= data.frame(tempo= tempo, i= i)

library(minpack.lm)
modelo = nlsLM(
  i ~ a/(tempo+b)^c, data = dados, #i= a*Tr^b/(t+n)^m ---> como a*Tr^b = constante, foi substituido por "a"
  start = list(a = 1000, b = 20, c= 0.9),
  lower = c(a = 0, b = 0, c= 0),
  upper = c(a = Inf, b = Inf, c= Inf),
  control = nls.lm.control(maxiter = 100))

i_pred= predict(modelo)
variancia_residuosn= sum((dados$i - i_pred)^2)
variancia_serien= sum((dados$i - mean(dados$i))^2)
R2= 1 - variancia_residuosn/variancia_serien
R2

tempo_pred= c(50)
i_pred= predict(modelo, data.frame(tempo = tempo_pred))

i_pred


Q=C*i_pred*A/3.6
Q

#considerando que a duração e a precipitação variam linearmente
h_tc= approx(tempo, h, xout = tc)$y
i_tc= h_tc*60/tc

Q=C*i_tc*A/3.6
Q

40*60/15
55*60/30
85*60/60

75*60/50


##### MC05 #####


A= 0.1*0.3
C= 1

Tr1=25
Tr2=50
Tr3=35

t1= 30
#ponto A
h1= 84
h2= 102
#ponto B
h3= 74
h4= 96
#ponto C
h5= 77
h6= 93

#i= a*Tr^b/(t+n)^m   e   i=h*t/60

#h*t/60= a*Tr^b/(t+n)^m
#h= 60*a*Tr^b/(t*(t+n)^m)
#tempo constante e tr variavel:
#const= 60*a/(t*(t+n)^m)  ---> h= const*Tr^b

#h1= const*Tr1^b
#h2= const*Tr2^b

#h2= h1*Tr2^b/Tr1^b = h1*(Tr2/Tr1)^b
#h2/h1= (Tr2/Tr1)^b  ---> ln(h2/h1)= b*ln(Tr2/Tr1)
#b= ln(h2/h1)/ln(Tr2/Tr1)

b= log(h2/h1)/log(Tr2/Tr1)
hA3530= h1*(Tr3/Tr1)^b
hA3530

b= log(h4/h3)/log(Tr2/Tr1)
hB3530= h3*(Tr3/Tr1)^b
hB3530

b= log(h6/h5)/log(Tr2/Tr1)
hC3530= h5*(Tr3/Tr1)^b
hC3530


#i= a*Tr^b/(t+n)^m
#tr constante e tempo variavel:
#const= a*Tr^b  ---> i= const/(t+n)^m
#i1= const/(t1+n)^m  ---> const= i1*(t1+n)^m
#i2= const/(t2+n)^m
#i2= i1*(t1+n)^m/(t2+n)^m= i1*((t1+n)/(t2+n))^m

#é possível aproximar a equação para: i2= i1*(t1/t2)^z
#ln(i2/i1)= z*ln(t1/t2)  ---> z= ln(i2/i1)/ln(t1/t2)


#como o exercicio nao possui dados com 2 tempos diferentes, estimando um valor para z com base na média de valores conhecidos
parametros_cidades= data.frame("Aracaju"= c(834.205, 0.179, 15, 0.726),
    "Manaus"= c(1136.504, 0.158, 10, 0.764),
    "Belém"= c(1085.508, 0.156, 12, 0.758),
    "Porto Alegre"= c(1297.9, 0.171, 11.619, 0.85),
    "Belo Horizonte"= c(1477.87, 0.1, 20, 0.84),
    "Porto Velho"= c(1181.378, 0.159, 11, 0.757),
    "Brasília"= c(1574.7, 0.207, 8, 0.844),
    "Rio Branco"= c(1419.345, 0.162, 18, 0.795),
    "Cuiabá"= c(1016.453, 0.133, 7.5, 0.739),
    "Rio de Janeiro"= c(1239, 0.15, 20, 0.74),
    "Florianópolis"= c(222, 0.1648, 0, 0.3835),
    "São Luiz"= c(1519.371, 0.161, 28, 0.777),
    "Fortaleza"= c(2345.29, 0.173, 28.31, 0.904),
    "São Paulo"= c(3462.6, 0.172, 22, 1.025),
    "Goiânia"= c(920.45, 0.1422, 12, 0.7599),
    "Teresina"= c(1248.856, 0.177, 10, 0.769),
    "Curitiba"= c(5950, 0.217, 26, 1.15))

z= c()
for (j in 1:ncol(parametros_cidades)){
    a= parametros_cidades[1, j]
    b= parametros_cidades[2, j]
    n= parametros_cidades[3, j]
    m= parametros_cidades[4, j]

    t1=30
    t2=20
    #i=a*Tr^b/(t1+n)^m
    i1=a*Tr3^b/(t1+n)^m
    i2=a*Tr3^b/(t2+n)^m

    #z= ln(i2/i1)/ln(t1/t2)
    z[j]= log(i2/i1)/log(t1/t2)
}

Z=mean(z)

t2=20

iA3520= (hA3530*60/t1)*(t1/t2)^Z
iA3520
iB3520= (hB3530*60/t1)*(t1/t2)^Z
iB3520
iC3520= (hC3530*60/t1)*(t1/t2)^Z
iC3520

QA3520= C*iA3520*A/3.6
QA3520

QB3520= C*iB3520*A/3.6
QB3520

QC3520= C*iC3520*A/3.6
QC3520


##### MC06 #####


Tr= 100
A= 1
C= 0.3
t= tc= 20

i= 5950*Tr^0.217/(t+26)^1.15

Q=C*i*A/3.6
Q


##### MC07 #####


#o metodo considera precipitação e coeficiente de escoamento uniformes na bacia e, por isso, é um método que funciona melhor para áreas menores


##### MC08 #####


#estimando a área em 1.5 km²
#estimando um C de 0.4
A= 1.5
C= 0.4
Tr= 10

#teste para diferentes tempos de concentração
t= seq(5, 120, 1)

i= c()
Q= c()
for (j in 1:length(t)){
    i[j]= 5950*Tr^0.217/(t[j]+26)^1.15
    Q[j]=C*i[j]*A/3.6
}
median(Q)
mean(Q)
min(Q)
max(Q)


##### MC09 #####


i_Tr10= data.frame("Duracao"= c(15, 30, 60, 120, 240),
    "Blumenau"= c(31, 50, 72, 80, 81),
    "São Francisco do Sul"= c(35, 47, 73, 97, 113),
    "Paranaguá"= c(36, 51, 70, 94, 122),
    "Curitiba"= c(36, 50, 67, 71, 77),
    "Ponta Grossa"= c(31, 47, 56, 63, 87),
    "Santos"= c(39, 63, 95, 119, 135),
    "São Paulo"= c(34, 39, 46, 51, 56),
    "Ubatuba"= c(40, 60, 76, 119, 209),
    "Jacarezinho"= c(33, 48, 58, 74, 77))
    
i_Tr25= data.frame("Duracao"= c(15, 30, 60, 120, 240),
    "Blumenau"= c(37, 65, 97, 106, 101),
    "São Francisco do Sul"= c(43, 59, 94, 130, 152),
    "Paranaguá"= c(44, 61, 86, 116, 156),
    "Curitiba"= c(44, 63, 85, 93, 95),
    "Ponta Grossa"= c(38, 61, 71, 79, 110),
    "Santos"= c(48, 63, 129, 159, 176),
    "São Paulo"= c(41, 52, 54, 59, 62),
    "Ubatuba"= c(52, 78, 90, 142, 290),
    "Jacarezinho"= c(39, 59, 71, 92, 95))

i_Tr50= data.frame("Duracao"= c(15, 30, 60, 120, 240),
    "Blumenau"= c(42, 79, 121, 131, 141),
    "São Francisco do Sul"= c(51, 70, 113, 165, 192),
    "Paranaguá"= c(52, 70, 100, 139, 190),
    "Curitiba"= c(61, 74, 98, 102, 112),
    "Ponta Grossa"= c(44, 74, 86, 93, 144),
    "Santos"= c(58, 101, 162, 200, 220),
    "São Paulo"= c(49, 50, 60, 66, 70),
    "Ubatuba"= c(66, 96, 100, 168, 370),
    "Jacarezinho"= c(44, 69, 81, 109, 112))

cidade= "Santos"
A= 4
C= 0.45

Q_Tr10=C*(i_Tr10[,cidade]*60/i_Tr10[,"Duracao"])*A/3.6
Q_Tr10
Q_Tr25=C*(i_Tr25[,cidade]*60/i_Tr25[,"Duracao"])*A/3.6
Q_Tr25
Q_Tr50=C*(i_Tr50[,cidade]*60/i_Tr50[,"Duracao"])*A/3.6
Q_Tr50


##### MC10 #####


#h1= f
#h2= g
#h3= h

t1= 15
t2= 30
t3= 60

C= 0.5
A= 3.6

Q1=(C*A*(60/t1)/3.6) #*h1
Q1#*h1

Q2=(C*A*(60/t2)/3.6) #*h2
Q2#*h2

Q3=(C*A*(60/t3)/3.6) #*h3
Q3#*h3


##### MC11 #####


A= 1058
h= 61.47

Dia= seq(10, 14.75, 0.25)
Qtotal= c(11.1, 17.2, 28, 42, 57, 64.5, 53, 48.6, 44.4, 35.5, 29.9, 27.8, 26.2, 23.2, 20.5, 19.2, 18.3, 17.5, 16.8, 16.2)
Qsub= c(11.1, 11, 10, 10.6, 11, 11.3, 11.6, 12, 12.4, 12.7, 13, 13.4, 13.8, 14.1, 14.4, 14.8, 15.2, 15.5, 15.8, 16.2)

Qsuperf= Qtotal-Qsub

Vtotal= A*h*10^3
Vsuperf= sum(Qsuperf*6*60*60)

hef= h*Vsuperf/Vtotal
hef

Qunit= Qsuperf*10/hef
Qunit

dados= data.frame(Dia= Dia, Vazão= Qunit)

library(ggplot2)
ggplot() +
    geom_line(data= dados, aes(x= Dia, y= Vazão)) +
    labs(title= "Hidrograma unitário", x= "Dias", y= "Vazão (m³/s)")+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.45))


##### MC12 #####


Dia= seq(4, 15)
Qtotal= c(11, 30, 59, 68, 55, 34, 25, 20, 17, 14, 11, 8)
Qsub= c(11, 10, 9, 8, 11, 14, 13, 12, 11, 10, 9, 8)

Qsuperf= Qtotal-Qsub

Vsuperf= sum(Qsuperf*24*60*60)

h= 80
C= 0.25
hef= h*C

#Vsuperf= A*hef*10^3 
A= Vsuperf/(hef*10^3)
A

Qunit= Qsuperf*10/hef
Qunit

dados= data.frame(Dia= Dia, Vazão= Qunit)

library(ggplot2)
ggplot() +
    geom_line(data= dados, aes(x= Dia, y= Vazão)) +
    labs(title= "Hidrograma unitário", x= "Dias", y= "Vazão (m³/s)")+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.45))


##### MC13 #####


Dia= seq(5, 9, 0.25)
Qtotal= c(60, 210, 360, 510, 660, 610, 560, 510, 460, 410, 360, 310, 260, 210, 160, 110, 60)
Qsub= rep(60, length(Qtotal))

A= 4147.2

Qsuperf= Qtotal-Qsub
Vsuperf= sum(Qsuperf*6*60*60)

#Vsuperf= A*hef*10^3
hef= Vsuperf/(A*10^3)
hef

Qunit= Qsuperf*10/hef
Qunit

dados= data.frame(Dia=Dia, Vazão=Qsuperf)

library(ggplot2)
ggplot() +
    geom_line(data= dados, aes(x= Dia, y= Vazão)) +
    labs(title= "Hidrograma unitário", x= "Dias", y= "Vazão (m³/s)")+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.45))


##### MC14 #####


Dia= seq(1, 5.75, 0.25)
Qunit= c(0, 17, 47.5, 86.5, 127, 147, 114, 101, 88, 63, 46, 39.5, 34, 25, 17, 12, 8, 5.5, 2.5, 0)

h= 47
C= 0.21

hef= h*C

Qsuperf= Qunit*hef/10
Qsuperf


##### MC15 #####


Tempo= seq(0, 100, 10)
Qtotal= c(31.25, 25, 20, 130, 102.5, 75, 47.5, 20, 16, 12.8, 10.24)
dados= data.frame(Tempo=Tempo, Vazão=Qtotal)

C= 0.4

#separação do escoamento
vazao_div= Qtotal[-1]/Qtotal[-length(Qtotal)]

aux=c()
for (i in 1:(length(vazao_div)-1)){
    if (abs(vazao_div[i+1]-vazao_div[i]) > 0.01){
        aux=c(aux,i)
    }
}
indice_pA= aux[1]+1
tempo_pA= Tempo[indice_pA]

aux= c()
for (i in length(vazao_div):2){
    if (abs(vazao_div[i]-vazao_div[i-1]) > 0.01){
    aux= c(aux, i)
    }
}
indice_pB= aux[1]
tempo_pB= Tempo[indice_pB]

dados_modelo= data.frame(Tempo=c(tempo_pA, tempo_pB), Vazão=c(Qtotal[Tempo %in% c(tempo_pA, tempo_pB)]))
eq_linear= lm(Vazão ~ Tempo, dados_modelo)

Tempo_pred= seq(tempo_pA, tempo_pB, 10)
Qsub_pred= predict(eq_linear, data.frame(Tempo = Tempo_pred))
dados_pred= data.frame(Tempo=Tempo_pred, Vazão=Qsub_pred)

Qsub= dados
Qsub[Qsub$Tempo %in% dados_pred$Tempo, "Vazão"]= dados_pred$Vazão[match(Qsub$Tempo[Qsub$Tempo %in% dados_pred$Tempo], dados_pred$Tempo)]

Qsuperf= Qtotal-Qsub$Vazão
Vsuperf= sum(Qsuperf*10*60*60)

#Vsuperf= A*hef*10^3
#hef= P*C ---> Vsuperf= A*(P*C)*10^3
A=Vsuperf/(C*10^3) #/P
A#/P


##### MC16 #####


Tempo= seq(0, 54, 6)
Qunit= c(0, 12, 24, 36, 30, 24, 18, 12, 6, 0)

h1= 25
h2= 50
C1= 0.2
C2= 0.3
hef1= h1*C1
hef2= h2*C2

Qsuperf1= Qunit*hef1/10
Qsuperf2= Qunit*hef2/10

dist_eventos= 18 #horas

lag= dist_eventos/(Tempo[2]-Tempo[1])
Qsuperf1= c(Qsuperf1, rep(0, lag))
Qsuperf2= c(rep(0, lag), Qsuperf2)

Qsuperf= Qsuperf1+Qsuperf2
Qsuperf


##### MC17 #####


Tempo= seq(0, 96, 12)
Qunit= c(0, 15, 30, 25, 20, 15, 10, 5, 0)

hef= 10

Qsuperf= Qunit*hef/10

dist_eventos= 12 #horas

lag= dist_eventos/(Tempo[2]-Tempo[1])
Qsuperf1= c(Qsuperf, rep(0, 2*lag))
Qsuperf2= c(rep(0, lag), Qsuperf, rep(0, lag))
Qsuperf3= c(rep(0, 2*lag), Qsuperf)

Qsuperftot= Qsuperf1+Qsuperf2+Qsuperf3
Qsuperftot

dados= data.frame(Tempo= seq(0, (length(Qsuperftot)-1)*12, 12), Vazão= Qsuperftot)

library(ggplot2)
ggplot()+
    geom_line(data=dados, aes(x=Tempo, y=Vazão))+
    labs(title= "Hidrograma unitário", x= "Dias", y= "Vazão (m³/s)")+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.45))


##### MC18 #####


Tempo= seq(0, 96, 6)
Qunit= c(0, 60, 120, 180, 240, 220, 200, 180, 160, 140, 120, 100, 80, 60, 40, 20, 0)

hef1= 15
hef2= 20
hef3= 15

Qsuperf1= Qunit*hef1/10
Qsuperf2= Qunit*hef2/10
Qsuperf3= Qunit*hef3/10

dist_eventos1= 12 #horas
dist_eventos2= 12

lag1= dist_eventos1/(Tempo[2]-Tempo[1])
lag2= dist_eventos2/(Tempo[2]-Tempo[1])
Qsuperf1= c(Qsuperf1, rep(0, lag1), rep(0, lag2))
Qsuperf2= c(rep(0, lag1), Qsuperf2, rep(0, lag2))
Qsuperf3= c(rep(0, lag1), rep(0, lag2), Qsuperf3)

Qsuperftot= Qsuperf1+Qsuperf2+Qsuperf3
Qsuperftot



##### MC19 #####


Tempo= seq(0, 96, 6)
Qunit= c(0, 150, 300, 450, 600, 550, 500, 450, 400, 350, 300, 250, 200, 150, 100, 50, 0)

hef1= 15
hef2= 20
hef3= 15/2
hef4= 15/2

Qsuperf1= Qunit*hef1/10
Qsuperf2= Qunit*hef2/10
Qsuperf3= Qunit*hef3/10
Qsuperf4= Qunit*hef4/10

dist_eventos1= 36 #horas
dist_eventos2= 6
dist_eventos3= 6

lag1= dist_eventos1/(Tempo[2]-Tempo[1])
lag2= dist_eventos2/(Tempo[2]-Tempo[1])
lag3= dist_eventos3/(Tempo[2]-Tempo[1])
Qsuperf1= c(Qsuperf1, rep(0, lag1), rep(0, lag2), rep(0, lag3))
Qsuperf2= c(rep(0, lag1), Qsuperf2, rep(0, lag2), rep(0, lag3))
Qsuperf3= c(rep(0, lag1), rep(0, lag2), Qsuperf3, rep(0, lag3))
Qsuperf4= c(rep(0, lag1), rep(0, lag2), rep(0, lag3), Qsuperf4)

Qsuperftot= Qsuperf1+Qsuperf2+Qsuperf3+Qsuperf4
Qsuperftot


##### MC20 #####


Tempo= seq(0, 5, 0.5)

qu_1_5=c()
for (i in 1:9){
    qu_1_5[i]= 4.44-(4.44/8)*(i-1)
}
Qunit= c(0, 2.22, qu_1_5)

Tr= 10
t= 60
i= 5950*Tr^0.217/(t+26)^1.15
h= i*t/60

C= 0.3
hef= h*C
hef1= hef/2
hef2= hef/2

Qsuperf1= Qunit*hef1/10
Qsuperf2= Qunit*hef2/10

dist_eventos= 0.5 #30 min
lag= dist_eventos/(Tempo[2]-Tempo[1])
Qsuperf1= c(Qsuperf1, rep(0, lag))
Qsuperf2= c(rep(0, lag), Qsuperf2)

Qsuperftot= Qsuperf1+Qsuperf2
Qsuperftot


##### MC21 #####


Tempo= seq(0, 5, 0.5)

qu_1_5=c()
for (i in 1:9){
    qu_1_5[i]= 4-(4/8)*(i-1)
}
Qunit= c(0, 2, qu_1_5)

Tr= 25
t= 90
i= 5950*Tr^0.217/(t+26)^1.15
h= i*t/60

C= 0.3
hef= h*C
hef1= hef/3
hef2= hef/3
hef3= hef/3

Qsuperf1= Qunit*hef1/10
Qsuperf2= Qunit*hef2/10
Qsuperf3= Qunit*hef3/10

dist_eventos= 0.5 #30 min
lag= dist_eventos/(Tempo[2]-Tempo[1])
Qsuperf1= c(Qsuperf1, rep(0, 2*lag))
Qsuperf2= c(rep(0, lag), Qsuperf2, rep(0, lag))
Qsuperf3= c(rep(0, 2*lag), Qsuperf3)

Qsuperftot= Qsuperf1+Qsuperf2+Qsuperf3
Qsuperftot


##### MC22 #####


Tempo= seq(0, 9, 1)
qu_3_9=c()
for (i in 1:7){
    qu_3_9[i]= 12-(12/6)*(i-1)
}
Qunit= c(0, 4, 8, qu_3_9)

dist_eventos= 1
lag= dist_eventos/(Tempo[2]-Tempo[1])

Qunit2h1= c(Qunit, rep(0, 1))
Qunit2h2= c(rep(0, 1), Qunit)

#Qunit---> h= 10 mm     mantendo a mesma área e as mesmas vazoes, o h para Qunit2h1+Qunit2h2 é igual a 20 mm. assim, a soma deve ser dividida por 2
Qunit2h= (Qunit2h1+Qunit2h2)/2
Qunit2h

Qunit4h1= c(Qunit, rep(0, 3))
Qunit4h2= c(rep(0, 1), Qunit, rep(0, 2))
Qunit4h3= c(rep(0, 2), Qunit, rep(0, 1))
Qunit4h4= c(rep(0, 3), Qunit)

Qunit4h= (Qunit4h1+Qunit4h2+Qunit4h3+Qunit4h4)/4
Qunit4h

#Vqunit= A*hunit*10^3
Vqunit= sum(Qunit*1*60*60)
hunit=10

A= Vqunit/(hunit*10^3)
A


##### MC23 #####


Tempo= seq(0, 36, 6)
Qsuperftot= c(0, 5, 20, 50, 40, 5, 0)

h1= 40
h2= 25
C1= 0.5
C2= 0.8
hef1= h1*C1
hef2= h2*C2

#Qsuperftot= Qsuperf1+Qsuperf2

#como Qsuperf1 é a primeira precipitação e elas ocorrem com um intervalo de 6 horas de diferença, o primeiro valor do hidrograma (desconsiderando o 0) pertence apenas a Qsuperf1 e o último apenas a Qsuperf2. a partir do primeiro valor e do escoamento superficial total, é possível descobrir os componentes de Qunit
#Qsuperf1_1= Qsuperftot_1= 0
#Qsuperf1_2= Qsuperftot_2= 5 , enquanto que Qsuperf2_1= 0
#Qsuperf= Qunit*hef/10  ---> Qunit_1= 0   e   Qunit_2= Qsuperf1_2*10/hef1
#Qsuperftot_3= Qunit_3*hef1/10 + Qunit_2*hef2/10  ---> Qunit_3= (Qsuperftot_3-Qunit_2*hef2/10)/(hef1/10)

Qunit= c(0)
for (i in 2:(length(Qsuperftot)-1)){
    Qunit[i]= (Qsuperftot[i] - Qunit[i-1]*hef2/10)/(hef1/10)
}

Qunit


##### MC24 #####


Tempo= seq(0, 17, 1)
Qsuperftot= c(0, 10, 25, 40, 65, 76, 80, 84, 74, 64, 54, 44, 34, 24, 14, 8, 4, 0)

A= 25.2
hef1= 40
hef2= 20
hef3= 0
hef4= 40

#Qsuperftot_2= Qunit1= 0
#Qsuperftot_2= Qunit2*hef1/10  --> Qunit2= Qsuperftot_2*10/hef1
#Qsuperftot_3= Qunit3*hef1/10 + Qunit2*hef2/10  ---> Qunit3= (Qsuperftot_3 - Qunit2*hef2/10)/(hef1/10)
#Qsuperftot_4= Qunit4*hef1/10 + Qunit3*hef2/10 + Qunit2*hef3/10
#Qsuperftot_5= Qunit5*hef1/10 + Qunit4*hef2/10 + Qunit3*hef3/10 + Qunit2*hef4/10
#Qunit5= (Qsuperftot_5 - Qunit4*hef2/10 - Qunit3*hef3/10 - Qunit2*hef4/10) / (hef1/10)

Qunit= c(0, Qsuperftot[2]*10/hef1)
Qunit= c(Qunit, (Qsuperftot[3]-Qunit[2]*hef2/10)/(hef1/10))
for (i in 4:(length(Qsuperftot)-3)){
    Qunit[i]= (Qsuperftot[i] - Qunit[i-1]*hef2/10 - Qunit[i-2]*hef3/10 - Qunit[i-3]*hef4/10)/(hef1/10)
}

Qunit



##### MC25 #####


A= 100
L= 20
Lg= 10

Ct_segurança= 1.8
Ct_economia= 2.2
Cp_segurança= 0.69
Cp_economia= 0.56

tp1= Ct_segurança*(L*Lg)^0.3/1.33
Qp_segurança= 2.76*Cp_segurança*A/tp1
Qp_segurança

tp2= Ct_economia*(L*Lg)^0.3/1.33
Qp_economia= 2.76*Cp_economia*A/tp2
Qp_economia

Variaçao_vazoes= (Qp_economia-Qp_segurança)*100/Qp_segurança
Variaçao_vazoes


##### MC26 #####


#qmax= 2.76*Cpmax*A/(Ctmin*(L*Lg)^0.3/1.33)  ---> const= 2.76*A/(((L*Lg)^0.3)/1.33)
#qmin= 2.76*Cpmin*A/(Ctmax*(L*Lg)^0.3/1.33)
#qmax/qmin= (Cpmax/Ctmin)/(Cpmin/Ctmax)= Cpmax*Ctmax/(Cpmin*Ctmin)

Ctmax= 2.2
Ctmin= 1.8
Cpmax= 0.69
Cpmin= 0.56

qmax_div_qmin= Cpmax*Ctmax/(Cpmin*Ctmin)
qmax_div_qmin


##### MC27 #####


Qunit_4_p= c(12, 22, 37, 52, 58.7, 55, 50, 44, 38) #4 valores de vazao antes e depois do pico

h1= 10
h2= 30
h3= 0
h4= 20
C=0.5
hef1=h1*C
hef2=h2*C
hef3=h3*C
hef4=h4*C

Qsuperfteste= c()
for(i in 1:(length(Qunit_4_p)-3)){
    Qsuperfteste[i]= Qunit_4_p[i+3]*hef1/10 + Qunit_4_p[i+2]*hef2/10 + Qunit_4_p[i+1]*hef3/10 + Qunit_4_p[i]*hef4/10
}

Qsuperfmax= max(Qsuperfteste)
Qsuperfmax

Tp= (4+which.max(Qsuperfteste))*1.5
Tp

dist_eventos= 3
tb= 33+dist_eventos*1.5
tb


##### MC28 #####


med= 2000
dp= 1000
Tr= 100
gamma= 0.5772 #constante de euler-mascheroni

Kt= (-log(-log(1-1/Tr))-gamma)/(pi/6^0.5)

Q= med+Kt*dp
Q


##### MC29 #####


Qtr10= 9915
Qtr100= 15411

gamma= 0.5772
K10= (-log(-log(1-1/10))-gamma)/(pi/6^0.5)
K100= (-log(-log(1-1/100))-gamma)/(pi/6^0.5)
K10000= (-log(-log(1-1/10000))-gamma)/(pi/6^0.5)

#Q= med+Kt*dp
#Qtr10= med+K10*dp  ---> med= Qtr10-K10*dp
#Qtr100= med+K100*dp= (Qtr10-K10*dp)+K100*dp  ----> dp= (Qtr100-Qtr10)/(K100-K10)
dp= (Qtr100-Qtr10)/(K100-K10)
dp
med= Qtr10-K10*dp
med

Qtr10000= med+K10000*dp
Qtr10000


##### MC30 #####


K1.1=-log(-log(1-1/1.1))
K5=-log(-log(1-1/5))
K10=-log(-log(1-1/10))
K100=-log(-log(1-1/100))
K1000=-log(-log(1-1/1000))

d1.1_5= 5

d1.1_10= (K10-K1.1)*d1.1_5/(K5-K1.1)
d1.1_10
d1.1_100= (K100-K1.1)*d1.1_5/(K5-K1.1)
d1.1_100
d1.1_1000= (K1000-K1.1)*d1.1_5/(K5-K1.1)
d1.1_1000
d10_100= (K100-K10)*d1.1_5/(K5-K1.1)
d10_100
d100_1000= (K1000-K100)*d1.1_5/(K5-K1.1)
d100_1000


##### MC31 #####


Q= c(272, 278, 62, 178, 272, 133, 380, 274, 251, 56, 172, 169, 135, 146, 299)
med= mean(Q)
dp= sd(Q)
gamma= 0.5772 #constante de euler-mascheroni

loc= med-(gamma*dp*6^0.5/pi) #posição do topo da curva (localização)
scale= dp*6^0.5/pi #largura da curva (escala)

vazoes= seq(min(Q), max(Q), length.out = 100)
densidade_de_probabilidade= (1/scale) * exp(-((vazoes - loc)/scale + exp(-(vazoes-loc)/scale)))

dados= data.frame(vazoes = vazoes, densidade_de_probabilidade = densidade_de_probabilidade)

library(ggplot2)
ggplot(dados, aes(x = vazoes, y = densidade_de_probabilidade)) +
  geom_line() +
  labs(title = "Distribuição de Gumbel", x = "Vazão (m³/s)", y = "Densidade de probabilidade") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.45))

K100= (-log(-log(1-1/100))-gamma)/(pi/6^0.5)
K1000= (-log(-log(1-1/1000))-gamma)/(pi/6^0.5)

Qtr100= med+K100*dp
Qtr100

Qtr1000= med+K1000*dp
Qtr1000

#Q= med+Kt*dp
Kt1= (326-med)/dp

#Kt= (-log(-log(1-1/Tr))-gamma)/(pi/6^0.5)
#-log(-log(1-1/Tr))= Kt*(pi/6^0.5)+gamma
#-log(1-1/Tr)= e^(-Kt*(pi/6^0.5)-gamma)
#1-1/Tr= e^(-e^(-Kt*(pi/6^0.5)-gamma))
#1/Tr= -e^(-e^(-Kt*(pi/6^0.5)-gamma))+1  ---> Tr= 1/(-e^(-e^(-Kt*(pi/6^0.5)-gamma))+1)
Tr1= 1/(-exp(-exp(-Kt1*(pi/6^0.5)-gamma))+1)
Tr1

Kt2= (612-med)/dp
Tr2= 1/(-exp(-exp(-Kt2*(pi/6^0.5)-gamma))+1)
Tr2



##### MC32 #####


Q= c(1270, 894, 1000, 258, 258, 769, 425, 884, 509, 1400, 1000, 724, 817, 1140, 829, 625, 1080, 1390, 1080, 956)
med= mean(Q)
dp= sd(Q)
gamma= 0.5772

K100= (-log(-log(1-1/100))-gamma)/(pi/6^0.5)
K1000= (-log(-log(1-1/1000))-gamma)/(pi/6^0.5)

Qtr100= med+K100*dp
Qtr100

Qtr1000= med+K1000*dp
Qtr1000


##### MC33 #####


Qtr10= 330.46
Qtr100= 513.67

gamma= 0.5772
K10= (-log(-log(1-1/10))-gamma)/(pi/6^0.5)
K100= (-log(-log(1-1/100))-gamma)/(pi/6^0.5)
K10000= (-log(-log(1-1/10000))-gamma)/(pi/6^0.5)

#Q= med+Kt*dp
#Qtr10= med+K10*dp  ---> med= Qtr10-K10*dp
#Qtr100= med+K100*dp= (Qtr10-K10*dp)+K100*dp  ----> dp= (Qtr100-Qtr10)/(K100-K10)
dp= (Qtr100-Qtr10)/(K100-K10)
dp
med= Qtr10-K10*dp
med

Qtr10000= med+K10000*dp
Qtr10000


##### MC34 #####


K10=-log(-log(1-1/10))

K10000=-log(-log(1-1/10000))

Q10= 100
#Q10= med+K10*dp

#Q10000= med+K10000*dp
#Q10000= (Q10-K10*dp)+K10000*dp
#Q10000= Q10+(K10000-K10)*dp
Q10
K10000-K10

#Q10000= 100+6.9599*dp
#o valor de Q10000 depende do desvio padrão


##### MC35 #####


n= 20
med= 2000
dp= 1000

gamma= 0.5772
K10000= (-log(-log(1-1/10000))-gamma)/(pi/6^0.5)
Q= med+K10000*dp

Q21= 6000
#med= somatorio(xi)/n
med2= (med*n+Q21)/(n+1)
#dp= (somatorio((xi-med)^2)/(n-1))^0.5  ---> somatorio((xi-med)^2)= dp^2*(n-1)
dp2= ((dp^2*(n-1) + (Q21-med2)*(Q21-med))/n)^0.5

Q2= med2+K10000*dp2

deltaQ= (Q2-Q)*100/Q
deltaQ


##### MC36 #####


Qtr10= 3600
Qtr100= 6000

gamma= 0.5772
K10= (-log(-log(1-1/10))-gamma)/(pi/6^0.5)
K100= (-log(-log(1-1/100))-gamma)/(pi/6^0.5)

dp1= (Qtr100-Qtr10)/(K100-K10)

med1= Qtr10-K10*dp1

n1= 20
Q21= 8000

med2= (med1*n1+Q21)/(n1+1)
dp2= ((dp1^2*(n1-1)+(Q21-med2)*(Q21-med1))/n1)^0.5

Qtr10_2= med2+K10*dp2
Qtr10_2
Qtr100_2= med2+K100*dp2
Qtr100_2


##### MC37 #####


Q= c(6030, 4560, 2573, 3900, 3150, 3408, 1365, 4080, 5580, 3150, 2385, 2250, 3317, 2640, 4050)
med= mean(Q)
dp= sd(Q)

gamma= 0.5772
loc= med-(gamma*dp*6^0.5/pi) #posição do topo da curva (localização)
scale= dp*6^0.5/pi #largura da curva (escala)

vazoes= seq(min(Q), max(Q), length.out = 100)
densidade_de_probabilidade= (1/scale) * exp(-((vazoes - loc)/scale + exp(-(vazoes-loc)/scale)))

dados= data.frame(vazoes = vazoes, densidade_de_probabilidade = densidade_de_probabilidade)

library(ggplot2)
ggplot(dados, aes(x = vazoes, y = densidade_de_probabilidade)) +
  geom_line() +
  labs(title = "Distribuição de Gumbel", x = "Vazão (m³/s)", y = "Densidade de probabilidade") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.45))


K10= (-log(-log(1-1/10))-gamma)/(pi/6^0.5)
K100= (-log(-log(1-1/100))-gamma)/(pi/6^0.5)
K1000= (-log(-log(1-1/1000))-gamma)/(pi/6^0.5)
K10000= (-log(-log(1-1/10000))-gamma)/(pi/6^0.5)

Qtr10= med+K10*dp
Qtr10
Qtr100= med+K100*dp
Qtr100
Qtr1000= med+K1000*dp
Qtr1000
Qtr10000= med+K10000*dp
Qtr10000

Q2= c(Q, Qtr1000)
med2= mean(Q2)
dp2= sd(Q2)

gamma= 0.5772
loc2= med2-(gamma*dp2*6^0.5/pi) #posição do topo da curva (localização)
scale2= dp2*6^0.5/pi #largura da curva (escala)

vazoes2= seq(min(Q2), max(Q2), length.out = 100)
densidade_de_probabilidade2= (1/scale2) * exp(-((vazoes2 - loc2)/scale2 + exp(-(vazoes2-loc2)/scale2)))

dados2= data.frame(vazoes = vazoes2, densidade_de_probabilidade = densidade_de_probabilidade2)

library(ggplot2)
ggplot(dados2, aes(x = vazoes, y = densidade_de_probabilidade)) +
  geom_line() +
  labs(title = "Distribuição de Gumbel", x = "Vazão (m³/s)", y = "Densidade de probabilidade") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.45))


K10= (-log(-log(1-1/10))-gamma)/(pi/6^0.5)
K100= (-log(-log(1-1/100))-gamma)/(pi/6^0.5)
K1000= (-log(-log(1-1/1000))-gamma)/(pi/6^0.5)
K10000= (-log(-log(1-1/10000))-gamma)/(pi/6^0.5)

Qtr10_2= med2+K10*dp2
Qtr10_2
Qtr100_2= med2+K100*dp2
Qtr100_2
Qtr1000_2= med2+K1000*dp2
Qtr1000_2
Qtr10000_2= med2+K10000*dp2
Qtr10000_2

Qtr10_2/Qtr10
Qtr100_2/Qtr100
Qtr1000_2/Qtr1000
Qtr10000_2/Qtr10000

#as alterações nas vazões correspondentes aos tempos de recorrencia apresentaram variaçoes que vao de 25% a 43%, aumentando junto com o tempo de recorrencia. a diferença entre antes e depois do novo evento representa um acréscimo relevante no volume de água esperado e, caso os projetos existentes nao sejam readequados, eles nao terao a infraestrutura necessária para suportar as chuvas com o tempo de recorrencia que se esperava inicialmente


##### MC38 #####


med= 1200
dp= 400
K10= 1.625
K100= 3.836

#K(Tr)= a*ln(Tr)+b
#1.625= a*ln(10)+b
#3.836= a*ln(100)+b
a= (3.836-1.625)/(log(100)-log(10))
a
b= 1.625-a*log(10)
b

K1000= a*log(1000)+b

Qtr10= med+K10*dp
Qtr10
Qtr100= med+K100*dp
Qtr100
Qtr1000= med+K1000*dp
Qtr1000


##### MC39 #####


n= 30
med= 2000
dp= 1000

gamma= 0.5772
K10= (-log(-log(1-1/10))-gamma)/(pi/6^0.5)
K100= (-log(-log(1-1/100))-gamma)/(pi/6^0.5)

Qtr10_1= med+K10*dp
Qtr10_1
Qtr100_1= med+K100*dp
Qtr100_1


reid= data.frame(n= c(20, 30, 40), Tr10= c(1.625, 1.541, 1.495), Tr50= c(3.179, 3.026, 2.943), Tr100= c(3.836, 3.653, 3.554))

Qtr10_2= med+reid[reid$n==n, "Tr10"]*dp
Qtr10_2
Qtr100_2= med+reid[reid$n==n, "Tr100"]*dp
Qtr100_2


##### MC40 #####


Q= c(1300, 1450, 1500, 900, 650, 1100, 1300, 1200, 1400, 1500)
Q= sort(Q, decreasing=TRUE)

f=c()
for (i in 1:length(Q)){
    f[i]= i/(length(Q)+1)
}
Tr= 1/f

dados= data.frame(Q=Q, Tr= Tr)

min(dados[dados$Q==900, "Tr"])
min(dados[dados$Q==1500, "Tr"])


##### MC41 #####


Qtr10= 2305
Qtr100= 4137

#Qtr= med+kt*dp
gamma= 0.5772
K10= (-log(-log(1-1/10))-gamma)/(pi/6^0.5)
K100= (-log(-log(1-1/100))-gamma)/(pi/6^0.5)

#Qtr10= med+K10*dp
#Qtr100= med+K100*dp
dp= (Qtr100-Qtr10)/(K100-K10)
med= Qtr10-K10*dp

K1000= (-log(-log(1-1/1000))-gamma)/(pi/6^0.5)
Qtr1000= med+K1000*dp
Qtr1000


##### MC42 #####


Q= c(2010, 1520, 860, 1300, 1050, 1140, 460, 1360, 1860, 1050, 795, 750, 1100, 880, 1350)

med= 1166
dp= 416

gamma= 0.5772
loc= med-(gamma*dp*6^0.5/pi) #posição do topo da curva (localização)
scale= dp*6^0.5/pi #largura da curva (escala)

vazoes= seq(min(Q), max(Q), length.out = 100)
densidade_de_probabilidade= (1/scale) * exp(-((vazoes - loc)/scale + exp(-(vazoes-loc)/scale)))
densidade_de_probabilidade_pontos= (1/scale) * exp(-((Q - loc)/scale + exp(-(Q-loc)/scale)))

dados= data.frame(vazoes = vazoes, densidade_de_probabilidade = densidade_de_probabilidade)
dados2= data.frame(vazoes= Q, densidade_de_probabilidade=densidade_de_probabilidade_pontos)

library(ggplot2)
ggplot(dados, aes(x = vazoes, y = densidade_de_probabilidade)) +
  geom_line() +
  geom_point(data= dados2, aes(x=vazoes, y= densidade_de_probabilidade)) +
  labs(title = "Distribuição de Gumbel", x = "Vazão (m³/s)", y = "Densidade de probabilidade") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.45))


K10= (-log(-log(1-1/10))-gamma)/(pi/6^0.5)
K100= (-log(-log(1-1/100))-gamma)/(pi/6^0.5)
K1000= (-log(-log(1-1/1000))-gamma)/(pi/6^0.5)
K10000= (-log(-log(1-1/10000))-gamma)/(pi/6^0.5)

Qtr10= med+K10*dp
Qtr10
Qtr100= med+K100*dp
Qtr100
Qtr1000= med+K1000*dp
Qtr1000
Qtr10000= med+K10000*dp
Qtr10000


##### MC43 #####


Q= c(6030, 4560, 2573, 3900, 3150, 3408, 1366, 4080, 5580, 3150, 2385, 2250, 3317, 2640, 4050)

med= 3496
dp= 1251

gamma= 0.5772
loc= med-(gamma*dp*6^0.5/pi) #posição do topo da curva (localização)
scale= dp*6^0.5/pi #largura da curva (escala)

vazoes= seq(min(Q), max(Q), length.out = 100)
densidade_de_probabilidade= (1/scale) * exp(-((vazoes - loc)/scale + exp(-(vazoes-loc)/scale)))
densidade_de_probabilidade_pontos= (1/scale) * exp(-((Q - loc)/scale + exp(-(Q-loc)/scale)))

dados= data.frame(vazoes = vazoes, densidade_de_probabilidade = densidade_de_probabilidade)
dados2= data.frame(vazoes= Q, densidade_de_probabilidade=densidade_de_probabilidade_pontos)

library(ggplot2)
ggplot(dados, aes(x = vazoes, y = densidade_de_probabilidade)) +
  geom_line() +
  geom_point(data= dados2, aes(x= vazoes, y= densidade_de_probabilidade)) +
  labs(title = "Distribuição de Gumbel", x = "Vazão (m³/s)", y = "Densidade de probabilidade") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.45))


K10= (-log(-log(1-1/10))-gamma)/(pi/6^0.5)
K100= (-log(-log(1-1/100))-gamma)/(pi/6^0.5)
K1000= (-log(-log(1-1/1000))-gamma)/(pi/6^0.5)
K10000= (-log(-log(1-1/10000))-gamma)/(pi/6^0.5)

Qtr10= med+K10*dp
Qtr10
Qtr100= med+K100*dp
Qtr100
Qtr1000= med+K1000*dp
Qtr1000
Qtr10000= med+K10000*dp
Qtr10000

Q2= c(Q, Qtr1000)
med2= mean(Q2)
dp2= sd(Q2)

gamma= 0.5772
loc2= med2-(gamma*dp2*6^0.5/pi) #posição do topo da curva (localização)
scale2= dp2*6^0.5/pi #largura da curva (escala)

vazoes2= seq(min(Q2), max(Q2), length.out = 100)
densidade_de_probabilidade2= (1/scale2) * exp(-((vazoes2 - loc2)/scale2 + exp(-(vazoes2-loc2)/scale2)))
densidade_de_probabilidade_pontos2= (1/scale2) * exp(-((Q2 - loc2)/scale2 + exp(-(Q2-loc2)/scale2)))

dados_2= data.frame(vazoes = vazoes2, densidade_de_probabilidade = densidade_de_probabilidade2)
dados2_2= data.frame(vazoes= Q2, densidade_de_probabilidade=densidade_de_probabilidade_pontos2)

library(ggplot2)
ggplot(dados_2, aes(x = vazoes, y = densidade_de_probabilidade)) +
  geom_line() +
  geom_point(data= dados2_2, aes(x= vazoes, y= densidade_de_probabilidade)) +
  labs(title = "Distribuição de Gumbel", x = "Vazão (m³/s)", y = "Densidade de probabilidade") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.45))


K10= (-log(-log(1-1/10))-gamma)/(pi/6^0.5)
K100= (-log(-log(1-1/100))-gamma)/(pi/6^0.5)
K1000= (-log(-log(1-1/1000))-gamma)/(pi/6^0.5)
K10000= (-log(-log(1-1/10000))-gamma)/(pi/6^0.5)

Qtr10_2= med2+K10*dp2
Qtr10_2
Qtr100_2= med2+K100*dp2
Qtr100_2
Qtr1000_2= med2+K1000*dp2
Qtr1000_2
Qtr10000_2= med2+K10000*dp2
Qtr10000_2


Qtr10_2/Qtr10
Qtr100_2/Qtr100
Qtr1000_2/Qtr1000
Qtr10000_2/Qtr10000

#as alterações nas vazões correspondentes aos tempos de recorrencia apresentaram variaçoes que vao de 25% a 43%, aumentando junto com o tempo de recorrencia. a diferença entre antes e depois do novo evento representa um acréscimo relevante no volume de água esperado e, caso os projetos existentes nao sejam readequados, eles nao terao a infraestrutura necessária para suportar as chuvas com o tempo de recorrencia que se esperava inicialmente


##### MC44 #####


#área da bacia maior que 5 km²

#possui registro de vazoes dos ultimos 50 anos  ---> ajuste de distribuição de de probabilidades

#hidrogramas na seção do projeto e precipitações que deram origem aos hidrogramas  ---> hidrograma unitário

#não existem dados  ---> hidrograma unitário sintético


##### MC45 #####


x= 0.1
k= 1
intervalo_tempo= 1 #dia

Qentrada= c(2, 2, 3, 5, 6, 4, 3.5, 2, 2)

C1= (intervalo_tempo-2*k*x) / (2*k*(1-x)+intervalo_tempo)
C2= (intervalo_tempo+2*k*x) / (2*k*(1-x)+intervalo_tempo)
C3= (2*k*(1-x)-intervalo_tempo) / (2*k*(1-x)+intervalo_tempo)

Qsaida= c(Qentrada[1]) #inicio com escoamento permanente

for (i in 2:length(Qentrada)) {
  Qsaida[i]= C1*Qentrada[i] + C2*Qentrada[i-1] + C3*Qsaida[i-1]
}

Qsaida


##### MC46 #####


Qentrada= c(7, 6, 24, 50, 70, 53, 33.5, 22, 15.5, 12, 10)

Qsaida= c(8, 7.5, 11, 23, 40, 56.6, 53.5, 40, 29, 20, 15)

deltat= 1

#Qsaida2= C1*Qentrada2+C2*Qentrada1+C3*Qsaida1
#sistema com C1, C2 e C3:
#Qsaida[3]= C1*Qentrada[3]+C2*Qentrada[2]+C3*Qsaida[2]
#Qsaida[4]= C1*Qentrada[4]+C2*Qentrada[3]+C3*Qsaida[3]
#1= C1+C2+C3

coefs_sistema= rbind(c(Qentrada[3], Qentrada[2], Qsaida[2]), c(Qentrada[4], Qentrada[3], Qsaida[3]), c(1, 1, 1))
resultados_eqs= c(Qsaida[3], Qsaida[4], 1)

C1= solve(coefs_sistema, resultados_eqs)[1]
C1
C2= solve(coefs_sistema, resultados_eqs)[2]
C2
C3= solve(coefs_sistema, resultados_eqs)[3]
C3

k_v= seq(0, 10, by = 0.01)
x_v= seq(0, 10, by = 0.01)

K= 0
X= 0
erro= Inf

for (k in k_v) {
  for (x in x_v) {
    C1_teste= (deltat - 2 * k * x) / (2 * k * (1 - x) + deltat)
    C2_teste= (deltat + 2 * k * x) / (2 * k * (1 - x) + deltat)
    C3_teste= (2 * k * (1 - x) - deltat) / (2 * k * (1 - x) + deltat)

    erro_teste= (C1_teste - C1)^2 + (C2_teste - C2)^2 + (C3_teste - C3)^2
    
    if (erro_teste < erro) {
      erro= erro_teste
      K= k
      X= x
    }
  }
}

K

X


##### MC47 #####


Qentrada= c(10, 40, 25, 10, 10)

Qsaida= c(10, 20.5, 29.875)

#Qsaida[i]= C1*Qentrada[i] + C2*Qentrada[i-1] + C3*Qsaida[i-1]
#Qsaida[2]= C1*Qentrada[2] + C2*Qentrada[1] + C3*Qsaida[1]
#Qsaida[3]= C1*Qentrada[3] + C2*Qentrada[2] + C3*Qsaida[2]
#C1+C2+C3= 1  ---> C3= 1-C1-C2
#Qsaida[2]= C1*Qentrada[2] + C2*Qentrada[1] + (1-C1-C2)*Qsaida[1]= C1*Qentrada[2] + C2*Qentrada[1] + Qsaida[1] - C1*Qsaida[1] - C2*Qsaida[1]
#20.5= C1*40 + C2*10 + 10 - C1*10 - C2*10= 30*C1+10
C1=(20.5-10)/30
C1
#Qsaida[2]= C1*Qentrada[2] + C2*Qentrada[1] + Qsaida[1] - C1*Qsaida[1] - C2*Qsaida[1]
#Qsaida[3]= C1*Qentrada[3] + C2*Qentrada[2] + (1-C1-C2)*Qsaida[2]= C1*Qentrada[3] + C2*Qentrada[2] + Qsaida[2] - C1*Qsaida[2] - C2*Qsaida[2]
#Qsaida[3]= C1*Qentrada[3] + C2*Qentrada[2] + Qsaida[2] - C1*Qsaida[2] - C2*Qsaida[2]
C2= (Qsaida[3]-C1*Qentrada[3]-Qsaida[2]+C1*Qsaida[2])/(Qentrada[2]-Qsaida[2])
C2
C3= 1-C1-C2
C3

for (i in 4:length(Qentrada)) {
  Qsaida[i]= C1*Qentrada[i] + C2*Qentrada[i-1] + C3*Qsaida[i-1]
}

Qsaida


##### MC48 #####


Qentrada= c(12, 30, 42, 36, 36, 18, 18)

Qsaida= c(12, 15, 27)

#Qsaida[2]= C1*Qentrada[2] + C2*Qentrada[1] + C3*Qsaida[1]
#Qsaida[3]= C1*Qentrada[3] + C2*Qentrada[2] + C3*Qsaida[2]
#1= C1+C2+C3

coefs_sistema= rbind(c(Qentrada[2], Qentrada[1], Qsaida[1]), c(Qentrada[3], Qentrada[2], Qsaida[2]), c(1, 1, 1))
resultados_eqs= c(Qsaida[2], Qsaida[3], 1)

C1= solve(coefs_sistema, resultados_eqs)[1]
C1
C2= solve(coefs_sistema, resultados_eqs)[2]
C2
C3= solve(coefs_sistema, resultados_eqs)[3]
C3

for (i in 4:length(Qentrada)) {
  Qsaida[i]= C1*Qentrada[i] + C2*Qentrada[i-1] + C3*Qsaida[i-1]
}

Qsaida


##### MC49 #####


#V= 5.184*Qvert/30

deltat= 24*60*60

Qentrada= c(100, 350, 550, 400, 300, 200, 100)

#deltaV/deltat= Qentrada - Qsaida    #equação da continuidade
#deltaV/deltat= Qentrada - Qsaida  ---> (V_imais1 - V_i)/deltat= (Qentrada_i+Qentrada_imais1)/2 - (Qsaida_i+Qsaida_imais1)/2
#reorganizando a equação em incognitas e variaveis conhecidas:
#2*V_tmais1/deltat+Qsaida_imais1= Qentrada_i+Qentrada_imais1+2*V_i/deltat-Qsaida_i    #metodo de puls

#2*V_imais1*10^6/deltat + Qsaida_imais1 = Qentrada_i + Qentrada_imais1 + 2*V_i*10^6/deltat - Qsaida_i

#2*V_imais1*10^6/deltat + Qsaida_imais1
#2*(5.184*Qsaida_imais1/30)/deltat + Qsaida_imais1= (2*5.184*10^6+30*deltat)*Qsaida_imais1/(30*deltat)

Qsaida= c(Qentrada[1]) #regime inicial permanente
Vag= c(5.184*Qsaida/30)   

for (i in 1:(length(Qentrada)-1)){
    Qsaida[i+1]= (Qentrada[i] + Qentrada[i+1] + 2*Vag[i]*10^6/deltat - Qsaida[i])*(30*deltat)/(2*5.184*10^6+30*deltat)
    Vag[i+1]= 5.184*Qsaida[i+1]/30
}

max(Qsaida)


##### MC50 #####


V= c(0, 864, 1728, 2592, 3456)
Qvert= c(0, 10, 20, 30, 40)

deltat= 24*60*60

Qentrada= c(10, 20, 30, 25, 20, 15, 10)


#deltaV/deltat= Qentrada - Qsaida    #equação da continuidade
#deltaV/deltat= Qentrada - Qsaida  ---> (V_imais1 - V_i)/deltat= (Qentrada_i+Qentrada_imais1)/2 - (Qsaida_i+Qsaida_imais1)/2
#reorganizando a equação em incognitas e variaveis conhecidas:
#2*V_tmais1/deltat+Qsaida_imais1= Qentrada_i+Qentrada_imais1+2*V_i/deltat-Qsaida_i    #metodo de puls

#2*V_imais1/deltat + Qsaida_imais1 = Qentrada_i + Qentrada_imais1 + 2*V_i/deltat - Qsaida_i

#2*V_imais1/deltat + Qsaida_imais1
aux= 2*V*10^3/deltat + Qvert


Qsaida= c(Qentrada[1]) #regime inicial permanente
Vag= c(approx(Qvert, V, xout = Qsaida[1])$y)   

#Qentrada_i + Qentrada_imais1 + 2*V_i/deltat - Qsaida_i
aux2= c()
for (i in 1:(length(Qentrada)-1)){
    aux2[i]= Qentrada[i] + Qentrada[i+1] + 2*Vag[i]*10^3/deltat - Qsaida[i]
    Qsaida[i+1]= approx(aux, Qvert, xout = aux2[i])$y
    Vag[i+1]= approx(Qvert, V, xout = Qsaida[i+1])$y
}

Qsaida


##### MC51 #####


Qentrada= c(0.01, 0.21, 0.41, 0.31, 0.21, 0.11, 0.01)

A= 5*12
h_perm= 0.05

deltat= 300

Qsaida= c(Qentrada[1]) #regime inicial permanente
Vag= c(A*h_perm)


#V= A*h
#Qe= k'*h^1.5
klinha= Qsaida[1]/h_perm^1.5

V= seq(A*0.05, A*1, by= 0.1)
Qvert= klinha*(V/A)^1.5

aux= 2*V/deltat + Qvert


aux2= c()
for (i in 1:(length(Qentrada)-1)){
    aux2[i]= Qentrada[i] + Qentrada[i+1] + 2*Vag[i]/deltat - Qsaida[i]
    Qsaida[i+1]= approx(aux, Qvert, xout = aux2[i])$y
    Vag[i+1]= approx(Qvert, V, xout = Qsaida[i+1])$y
}
Qsaida
h_bordalivre= max(Vag)/A
h_bordalivre


##### MC52 #####


Qentrada= c(0.15, 0.5, 0.5, 0.5, 0.5, 0.15)/60

A= 1

V_perm= 1
h_perm= V_perm/A

deltat= 60

Qsaida= c(Qentrada[1]) #regime inicial permanente
Vag= c(A*h_perm)

#V= A*h
#Qe= Cd*A*(2*9.81*h)^0.5   #equação da vazão por um orificio
Cd= Qsaida[1]/(A*(2*9.81*h_perm)^0.5)

V= seq(A*1, A*3, by= 0.1)
Qori= Cd*A*(2*9.81*(V/A))^0.5

aux= 2*V/deltat + Qori


aux2= c()
for (i in 1:(length(Qentrada)-1)){
    aux2[i]= Qentrada[i] + Qentrada[i+1] + 2*Vag[i]/deltat - Qsaida[i]
    Qsaida[i+1]= approx(aux, Qori, xout = aux2[i])$y
    Vag[i+1]= approx(Qori, V, xout = Qsaida[i+1])$y
}

max(Qsaida)*60000 #l/min


##### MC53 #####


C0= 0.2
C1= 0.4
C2= 0.4

Qentrada= c(2, 3, 5, 6, 4, 3.5, 2)
Qsaida= c(Qentrada[1])

for (i in 1:(length(Qentrada)-1)) {
  Qsaida[i+1]= C0*Qentrada[i+1] + C1*Qentrada[i] + C2*Qsaida[i]
}

Qsaida

max(Qsaida)


##### MC54 #####


V= c(0, 1226000, 2477000, 3745000, 5026000, 6326000, 7649000, 8997000, 10369000, 11765000)
cota= c(0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8, 2.1, 2.4, 2.7)
Qvert= c(0, 14.9, 42.28, 77.48, 119.16, 166.83, 217.33, 272.94, 334.8, 405.73)

deltat= 6*60*60

Qentrada= c(42.56, 42.56, 45.4, 56.75, 87.96, 147.54, 272.38, 353.24, 343.31, 314.94, 289.4, 263.86, 241.17, 219, 198.61, 179, 161.72, 145, 133.35, 119, 110.65, 99, 90.79, 84, 79.44, 73, 68.02, 67, 68.42, 63, 56.75, 55, 53.91, 51, 50, 49, 48.23, 45, 44, 43, 42.56, 42, 41, 40, 39, 39.72)


#deltaV/deltat= Qentrada - Qsaida    #equação da continuidade
#deltaV/deltat= Qentrada - Qsaida  ---> (V_imais1 - V_i)/deltat= (Qentrada_i+Qentrada_imais1)/2 - (Qsaida_i+Qsaida_imais1)/2
#reorganizando a equação em incognitas e variaveis conhecidas:
#2*V_tmais1/deltat+Qsaida_imais1= Qentrada_i+Qentrada_imais1+2*V_i/deltat-Qsaida_i    #metodo de puls

#2*V_imais1/deltat + Qsaida_imais1 = Qentrada_i + Qentrada_imais1 + 2*V_i/deltat - Qsaida_i

#2*V_imais1/deltat + Qsaida_imais1
aux= 2*V/deltat + Qvert


Qsaida= c(Qentrada[1]) #regime inicial permanente
Vag= c(approx(Qvert, V, xout = Qsaida[1])$y)   

#Qentrada_i + Qentrada_imais1 + 2*V_i/deltat - Qsaida_i
aux2= c()
for (i in 1:(length(Qentrada)-1)){
    aux2[i]= Qentrada[i] + Qentrada[i+1] + 2*Vag[i]/deltat - Qsaida[i]
    Qsaida[i+1]= approx(aux, Qvert, xout = aux2[i])$y
    Vag[i+1]= approx(Qvert, V, xout = Qsaida[i+1])$y
}

max(Qsaida)
hmax= approx(Qvert, cota, xout = max(Qsaida))$y
hmax

V= c(0, 1226000, 2477000, 3745000, 5026000, 6326000, 7649000, 8997000, 10369000, 11765000)
cota= c(0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8, 2.1, 2.4, 2.7)
Qvert_antes= c(0, 14.9, 42.28, 77.48, 119.16, 166.83, 217.33, 272.94, 334.8, 405.73)
Qvert= Qvert_antes*2

deltat= 6*60*60

Qentrada= c(42.56, 42.56, 45.4, 56.75, 87.96, 147.54, 272.38, 353.24, 343.31, 314.94, 289.4, 263.86, 241.17, 219, 198.61, 179, 161.72, 145, 133.35, 119, 110.65, 99, 90.79, 84, 79.44, 73, 68.02, 67, 68.42, 63, 56.75, 55, 53.91, 51, 50, 49, 48.23, 45, 44, 43, 42.56, 42, 41, 40, 39, 39.72)


#deltaV/deltat= Qentrada - Qsaida    #equação da continuidade
#deltaV/deltat= Qentrada - Qsaida  ---> (V_imais1 - V_i)/deltat= (Qentrada_i+Qentrada_imais1)/2 - (Qsaida_i+Qsaida_imais1)/2
#reorganizando a equação em incognitas e variaveis conhecidas:
#2*V_tmais1/deltat+Qsaida_imais1= Qentrada_i+Qentrada_imais1+2*V_i/deltat-Qsaida_i    #metodo de puls

#2*V_imais1/deltat + Qsaida_imais1 = Qentrada_i + Qentrada_imais1 + 2*V_i/deltat - Qsaida_i

#2*V_imais1/deltat + Qsaida_imais1
aux= 2*V/deltat + Qvert


Qsaida= c(Qentrada[1]) #regime inicial permanente
Vag= c(approx(Qvert, V, xout = Qsaida[1])$y)   

#Qentrada_i + Qentrada_imais1 + 2*V_i/deltat - Qsaida_i
aux2= c()
for (i in 1:(length(Qentrada)-1)){
    aux2[i]= Qentrada[i] + Qentrada[i+1] + 2*Vag[i]/deltat - Qsaida[i]
    Qsaida[i+1]= approx(aux, Qvert, xout = aux2[i])$y
    Vag[i+1]= approx(Qvert, V, xout = Qsaida[i+1])$y
}

max(Qsaida)
hmax= approx(Qvert, cota, xout = max(Qsaida))$y
hmax


##### MC55 #####


V= c(0,  864, 1728, 2592, 3456)
Qvert= c(0, 10, 20, 30, 40)

deltat= 24*60*60

Qentrada= c(10, 20, 30, 25, 20, 15, 10)


#deltaV/deltat= Qentrada - Qsaida    #equação da continuidade
#deltaV/deltat= Qentrada - Qsaida  ---> (V_imais1 - V_i)/deltat= (Qentrada_i+Qentrada_imais1)/2 - (Qsaida_i+Qsaida_imais1)/2
#reorganizando a equação em incognitas e variaveis conhecidas:
#2*V_tmais1/deltat+Qsaida_imais1= Qentrada_i+Qentrada_imais1+2*V_i/deltat-Qsaida_i    #metodo de puls

#2*V_imais1/deltat + Qsaida_imais1 = Qentrada_i + Qentrada_imais1 + 2*V_i/deltat - Qsaida_i

#2*V_imais1/deltat + Qsaida_imais1
aux= 2*V*10^3/deltat + Qvert


Qsaida= c(Qentrada[1]) #regime inicial permanente
Vag= c(approx(Qvert, V, xout = Qsaida[1])$y)   

#Qentrada_i + Qentrada_imais1 + 2*V_i/deltat - Qsaida_i
aux2= c()
for (i in 1:(length(Qentrada)-1)){
    aux2[i]= Qentrada[i] + Qentrada[i+1] + 2*Vag[i]*10^3/deltat - Qsaida[i]
    Qsaida[i+1]= approx(aux, Qvert, xout = aux2[i])$y
    Vag[i+1]= approx(Qvert, V, xout = Qsaida[i+1])$y
}

max(Qsaida)


##### MC56 #####


C1= 0.16
C2= 0.4
C3= 0.44

Qentrada= c(2, 3, 5, 6, 4, 3.5, 2, 2)
Qsaida= c(Qentrada[1])

for (i in 1:(length(Qentrada)-1)) {
  Qsaida[i+1]= C1*Qentrada[i+1] + C2*Qentrada[i] + C3*Qsaida[i]
}

Qsaida


##### MC57 #####


Qentrada= c(4, 15, 10, 6, 3)

Qsaida= c(4, 6.2, 10.48)

#Qsaida[2]= C1*Qentrada[2] + C2*Qentrada[1] + C3*Qsaida[1]
#Qsaida[3]= C1*Qentrada[3] + C2*Qentrada[2] + C3*Qsaida[2]
#1= C1+C2+C3

coefs_sistema= rbind(c(Qentrada[2], Qentrada[1], Qsaida[1]), c(Qentrada[3], Qentrada[2], Qsaida[2]), c(1, 1, 1))
resultados_eqs= c(Qsaida[2], Qsaida[3], 1)

C1= solve(coefs_sistema, resultados_eqs)[1]

C2= solve(coefs_sistema, resultados_eqs)[2]

C3= solve(coefs_sistema, resultados_eqs)[3]


for (i in 4:length(Qentrada)) {
  Qsaida[i]= C1*Qentrada[i] + C2*Qentrada[i-1] + C3*Qsaida[i-1]
}

Qsaida


##### MC58 #####


Qentrada= c(12, 30, 41.4, 35.64, 30.144)

Qsaida= c(12, 21, 33)

#Qsaida[2]= C1*Qentrada[2] + C2*Qentrada[1] + C3*Qsaida[1]
#Qsaida[3]= C1*Qentrada[3] + C2*Qentrada[2] + C3*Qsaida[2]
#1= C1+C2+C3

coefs_sistema= rbind(c(Qentrada[2], Qentrada[1], Qsaida[1]), c(Qentrada[3], Qentrada[2], Qsaida[2]), c(1, 1, 1))
resultados_eqs= c(Qsaida[2], Qsaida[3], 1)

C1= solve(coefs_sistema, resultados_eqs)[1]

C2= solve(coefs_sistema, resultados_eqs)[2]

C3= solve(coefs_sistema, resultados_eqs)[3]


for (i in 4:length(Qentrada)) {
  Qsaida[i]= C1*Qentrada[i] + C2*Qentrada[i-1] + C3*Qsaida[i-1]
}

Qsaida
