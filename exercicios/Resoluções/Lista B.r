##### MB01 #####


C= 0.3
precipitacao= 1230
i= precipitacao/(365*24)

library(sf)

bacia = st_read("bacia/watershed.shp") #shapefile da bacia

st_crs(bacia)

area_m2 = as.numeric(st_area(bacia))
area_km2= area_m2/10^6

Q= C*i*area_km2/3.6
Q

q= Q*1000/area_km2
q


##### MB02 #####


A= 25
prec= 1200
evt= 800

Q= ((prec-evt)/1000)*A*10^6/(365*24*60*60)
Q


##### MB03 #####


A= 25
prec= 1200
evt= 800
x= 2 #exemplo
evp_res= 1100

Q= (prec/1000)*A*10^6/(365*24*60*60) - (evt/1000)*(A-x)*10^6/(365*24*60*60) - (evp_res/1000)*x*10^6/(365*24*60*60)
Q


##### MB04 #####


prec= 1500
evt= 1000

q= ((prec-evt)/(365*24*60*60))*10^6
q


##### MB05 #####


prec= 1500
evt= 1000
res= 0.15
evp_res= 1200

q= prec*10^6/(365*24*60*60) - (1-res)*evt*10^6/(365*24*60*60) - res*evp_res*10^6/(365*24*60*60)
q


##### MB06 #####


Q_antes= 19
Q_depois= 18
prec= 1500
evp_res= 1000

#19= prec*10^6/(365*24*60*60) - evt*10^6/(365*24*60*60)
evt= (prec*10^6/(365*24*60*60)-19) * (365*24*60*60) / 10^6

#18= prec*10^6/(365*24*60*60) - (1-res)*evt*10^6/(365*24*60*60) - res*evp_res*10^6/(365*24*60*60)
#18= prec*10^6/(365*24*60*60) - evt*10^6/(365*24*60*60) + res*evt*10^6/(365*24*60*60) - res*evp_res*10^6/(365*24*60*60)
res= (18 - prec*10^6/(365*24*60*60) + evt*10^6/(365*24*60*60)) / (evt*10^6/(365*24*60*60)-evp_res*10^6/(365*24*60*60))
res


##### MB07 #####


prec= 1300
evt= 850
Q= 30

A= Q*(1000*365*24*60*60)/((prec-evt)*10^6)
A


##### MB08 #####


prec= 1326
evt= 875
q= 14.3

q2= prec*10^6/(365*24*60*60) - evt*10^6/(365*24*60*60)
q2

#nao é possível descobrir a área


##### MB09 #####


q_antes= 15
q_depois= q_antes*0.8
res= 0.5
prec= 1500

#q_antes= prec*10^6/(365*24*60*60) - evt*10^6/(365*24*60*60)
evt= ((prec*10^6/(365*24*60*60))-q_antes) * (365*24*60*60) / 10^6

#q_depois= prec*10^6/(365*24*60*60) - (1-res)*evt*10^6/(365*24*60*60) - res*evp_res*10^6/(365*24*60*60)
evp_res= (prec*10^6/(365*24*60*60) - (1-res)*evt*10^6/(365*24*60*60) - q_depois) * 365*24*60*60 / (res*10^6)
evp_res


##### MB10 #####


q= 18
prec= 1100

evt= (prec*10^6/(365*24*60*60) - q) * 365*24*60*60 / 10^6
evt


##### MB11 #####


q_antes= 18
prec= 1100

evt= (prec*10^6/(365*24*60*60) - q_antes) * 365*24*60*60 / 10^6

res= 0.15
evp_res= 700

q_depois= prec*10^6/(365*24*60*60) - (1-res)*evt*10^6/(365*24*60*60) - res*evp_res*10^6/(365*24*60*60)
q_depois


##### MB12 #####


C1= 0.3
res= 0.05

#evt2= (evt1*(A-A_res) + evp_res*A_res) / A = evt1*(1-res) + 1.2*evt1*res = evt1 - evt1*res + 1.2*evt1*res = evt1 + 0.2*evt1*res = evt1 + 0.01*evt1
#evt2= 1.01*evt1

#admitindo C= (prec-evt)/prec

#C1= (prec-evt1)/prec
#0.3= (prec-evt1)/prec
#evt1= -0.3*prec + prec = 0.7*prec

#C2= (prec-1.01*evt1)/prec = (prec-1.01*0.7*prec)/prec = (prec-0.707*prec)/prec = 0.293

C2= 0.293

#i1 = i2
#A1 = A2
#(Q2-Q1)/Q2= (C2*i*A/3.6-C1*i*A/3.6)/(C1*i*A/3.6) = ((C2-C1)*i*A/3.6) / (C1*i*A/3.6) = (C2-C1)/C1
alteracao= (C2-C1)*100/C1
alteracao


##### MB13 #####


A_res= 5
prec= 1800
evt= prec*0.5
evp_res= 1485

#variacao= Q2-Q1 = (prec*A-evt*(A-A_res)-evp_res*A_res)*10^6/(365*24*60*60)-(prec*A - evt*A)*10^6/(365*24*60*60) = (prec*A - evt*A + evt*A_res - evp_res*A_res - prec*A + evt*A) * 10^6 / (365*24*60*60) = (evt*A_res - evp_res*A_res) * 10^6 / (365*24*60*60)
variacao= (evt*A_res - evp_res*A_res) * 10^6 / (365*24*60*60)
variacao


##### MB14 #####


Q= 50
A= 100

prec= Q*365*24*60*60 * 1000 / (A*10^6)
prec

#não é realista, pois seria necessário uma precipitação média anual de 15768 mm (sem nem considerar a evapotranspiração da bacia na equação do balanço hidrico simplificado)


##### MB15 #####


prec= c(1000, 1200, 1600, 1800)
C= 0.7

q= prec*C*10^6/(365*24*60*60)
q

evt= prec*(1-C)
evt


##### MB16 #####


Q_pico= 82.5
t= 50

Vol_esc_sup= Q_pico*(t*60*60) / 2 #área gerada pelo hidrograma de escoamento superficial

prec= c(150, 200, 250, 300)
C= c(0.35, 0.40, 0.45, 0.50)

A_aux= matrix(nrow = length(C), ncol = length(prec))
for (i in 1:length(C)) {
  for (j in 1:length(prec)) {
    A_aux[i, j] = (Vol_esc_sup / 10^9) / ((prec[j] / 10^6) * C[i])
  }
}

A= as.data.frame(A_aux)
colnames(A)= paste(prec)
rownames(A)= paste(C)
A


##### MB17 #####


#Q= (PRE*AREA - EVT*AREA) *10^3 / (365*24*60*60)

#Q= (PRE*AREA - EVT*(AREA-Z) - EVPR*Z) *10^3 / (365*24*60*60)


##### MB18 #####


#E1= P*A - EVT*A = (P - EVT) * A
#E2= P*A - EVT*A*(1-&) - EV*A*& = P*A - EVT*A + EVT*A*& - EV*A*& = (P - EVT + EVT*& - EV*&) * A

#E2-E1= (P - EVT + EVT*& - EV*&) * A - (P - EVT) * A = (EVT*& - EV*&) * A

#(E2-E1)/E1= ((EVT*& - EV*&) * A) / ((P - EVT) * A)
#(E2-E1)/E1= (EVT - EV) * &) / (P - EVT)


##### MB19 #####


vazao_saindo_fozdoareia= c(8080, 8030, 8120, 8190, 8060, 8200, 8180, 8070, 8090, 8000, 7660, 7760)
vazao_chegando_segredo= c(13170, 13650, 12960, 13180, 14140, 13210, 11920, 13506, 11350, 9600, 11570, 10830)
vazao_saindo_segredo= c(5560, 5770, 5990, 6090, 6160, 6270, 6340, 6420, 6480, 6500, 6580, 6600)

deltat= 2
area_incremental= 4000

deltaV= sum(vazao_chegando_segredo - vazao_saindo_segredo) * (deltat*60*60)
deltaV

deltaV_incremental= sum(vazao_chegando_segredo - vazao_saindo_fozdoareia) * (deltat*60*60)
precipitacao_efetiva= deltaV_incremental / (area_incremental*10^3)
precipitacao_efetiva

vazao_incremental_media= mean(vazao_chegando_segredo - vazao_saindo_fozdoareia)
q= vazao_incremental_media*10^3 / area_incremental
q


##### MB20 #####


Qi= 100
Qf= 94.1313
t= 7*24*60*60

#Q=-dV/dt
#Q=alpha*V ---> dV/dt= -alpha*V
#1/V dV= -alpha dt
#integrando a equação dos dois lados---> ln(V) = -alpha*t + c
#V(t)= e^(-alpha*t+c) = e^(-alpha*t) * e^c 
#e^c= constante que representa o V inicial
#V(t)= V0*e^(-alpha*t)

#Q=alpha*V ---> Q/alpha = (Q0/alpha)*e^(-alpha*t)
#Q=Q0*e^(-alpha*t) #equação de boussinessq
alpha= -(1/t)*log(Qf/Qi)

#V é igual a integral de Q(t) de 0 até t ---> (-Q0/alpha)*e^(-alpha*t) - (-Q0/alpha)*e^(-alpha*0) = -(Q0/alpha)*e^(-alpha*t) + Q0/alpha
V= -(Qi/alpha)*exp((-alpha*t)) + Qi/alpha
V


##### MB21 #####


A= 3500
Q= 46.5
prec= 1500
evt= 1000

Q01_74= 21.65
Q01_75= 50

t=31*24*60*60

P= prec*A*10^3
EVT= evt*A*10^3
D= Q*365*24*60*60 #defluvio ou escoamento total

deltaV= P-EVT-D

#Q=alpha*V ---> Q01_74= alpha*V01_74  e  Q01_75= alpha*V01_75
#Q01_75-Q01_74 = alpha*V01_75 - alpha*V01_74 = alpha * (V01_75-V01_74) = alpha*deltaV
alpha= (Q01_75-Q01_74)/deltaV

Q02_75= Q01_75*exp(-alpha*t)
Q02_75

#ao relacionar o volume da equação deltaV=P-D-EVT com a vazão da equação Q=Qo*e^(-alpha*t), a resolução considera que a variação do volume está toda concentrada no subterrâneo


##### MB22 #####


A= 9000
prec= 1800
Q= 174
Q_01_84= 90
Q_01_85= 95.6
A_res= 100
ev_r= 2000
alpha= 0.005

deltaV= Q_01_85-Q_01_84

#deltaV= P-D-EVT
#EVT= (evt*(A-A_res)+ev_r*A_res)/A
#deltaV= P - D - (evt*(A-A_res)+ev_r*A_res)/A ---> evt= ((P-D-deltaV)*A - ev_r*A_res) / (A-A_res)

P= prec
D= Q*365*24*60*60/(A*10^3)
deltaV= deltaV*365*24*60*60/(A*10^3)
evt= ((P-D-deltaV)*A - ev_r*A_res) / (A-A_res)
evt

EVT= (evt*(A-A_res)+ev_r*A_res)/A
EVT


##### MB23 #####


A= 1.3

prec= 131.6
Q= c(0, 1, 2, 1.5, 1, 0.5, 0)
V= sum(Q*60*60)

V_inf_ret= prec-V/(A*10^3)
V_inf_ret


##### MB24 #####


temp= c(23.9, 23.5, 22.4, 20.8, 17, 16, 16.9, 18.5, 20.3, 20.8, 23.1, 23)

fatores_correcao_lat= data.frame(
    Latitude = c(5, 0, -5, -10, -15, -20, -25, -30),
    jan = c(1.00, 1.02, 1.04, 1.08, 1.12, 1.14, 1.17, 1.20),
    fev = c(0.93, 0.94, 0.95, 0.97, 0.98, 1.10, 1.01, 1.03),
    mar = c(1.03, 1.04, 1.04, 1.05, 1.05, 1.05, 1.05, 1.06),
    abr = c(1.02, 1.01, 1.00, 0.99, 0.98, 0.97, 0.96, 0.95),
    mai = c(1.06, 1.01, 1.02, 1.01, 0.98, 0.96, 0.94, 0.92),
    jun = c(1.03, 1.01, 0.99, 0.96, 0.94, 0.91, 0.88, 0.85),
    jul = c(1.06, 1.04, 1.02, 1.00, 0.97, 0.95, 0.93, 0.90),
    ago = c(1.05, 1.04, 1.03, 1.01, 1.00, 0.99, 0.98, 0.96),
    set = c(1.01, 1.01, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00),
    out = c(1.03, 1.04, 1.05, 1.06, 1.07, 1.08, 1.10, 1.12),
    nov = c(0.99, 1.01, 1.03, 1.05, 1.07, 1.09, 1.11, 1.14),
    dez = c(1.02, 1.04, 1.06, 1.10, 1.12, 1.15, 1.18, 1.21))

latitude= -23.3

latitudes_adjacentes= fatores_correcao_lat$Latitude[order(abs(fatores_correcao_lat$Latitude - latitude))][1:2]

Fc= c()
for (i in 1:(ncol(fatores_correcao_lat)-1)) {
    #valor dos 2 Fcs adjacentes para o mês atual
    Fc_adjacentes= fatores_correcao_lat[fatores_correcao_lat$Latitude %in% latitudes_adjacentes, i+1]
    #ordenar com base em latitudes_ajacentes
    Fc_adjacentes= Fc_adjacentes[order(fatores_correcao_lat$Latitude[fatores_correcao_lat$Latitude %in% latitudes_adjacentes])]

    #interpolação linear
    Fc[i]= approx(x = latitudes_adjacentes, y = Fc_adjacentes, xout = latitude)$y
}

I= sum((temp/5)^1.514)
alpha= 6.75*10^-7*I^3 - 7.71*10^-5*I^2 + 0.0179*I + 0.492

EVT= 16.2*Fc*(10*temp/I)^alpha
EVT


##### MB25 #####


temp= c(23.9, 23.5, 22.4, 20.8, 17, 16, 16.9, 18.5, 20.3, 20.8, 23.1, 23)

fatores_correcao_lat= data.frame(
    Latitude = c(5, 0, -5, -10, -15, -20, -25, -30),
    jan = c(1.00, 1.02, 1.04, 1.08, 1.12, 1.14, 1.17, 1.20),
    fev = c(0.93, 0.94, 0.95, 0.97, 0.98, 1.10, 1.01, 1.03),
    mar = c(1.03, 1.04, 1.04, 1.05, 1.05, 1.05, 1.05, 1.06),
    abr = c(1.02, 1.01, 1.00, 0.99, 0.98, 0.97, 0.96, 0.95),
    mai = c(1.06, 1.01, 1.02, 1.01, 0.98, 0.96, 0.94, 0.92),
    jun = c(1.03, 1.01, 0.99, 0.96, 0.94, 0.91, 0.88, 0.85),
    jul = c(1.06, 1.04, 1.02, 1.00, 0.97, 0.95, 0.93, 0.90),
    ago = c(1.05, 1.04, 1.03, 1.01, 1.00, 0.99, 0.98, 0.96),
    set = c(1.01, 1.01, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00),
    out = c(1.03, 1.04, 1.05, 1.06, 1.07, 1.08, 1.10, 1.12),
    nov = c(0.99, 1.01, 1.03, 1.05, 1.07, 1.09, 1.11, 1.14),
    dez = c(1.02, 1.04, 1.06, 1.10, 1.12, 1.15, 1.18, 1.21))

latitude= -23.3

latitudes_adjacentes= fatores_correcao_lat$Latitude[order(abs(fatores_correcao_lat$Latitude - latitude))][1:2]

Fc= c()
for (i in 1:(ncol(fatores_correcao_lat)-1)) {
    #valor dos 2 Fcs adjacentes para o mês atual
    Fc_adjacentes= fatores_correcao_lat[fatores_correcao_lat$Latitude %in% latitudes_adjacentes, i+1]
    #ordenar com base em latitudes_ajacentes
    Fc_adjacentes= Fc_adjacentes[order(fatores_correcao_lat$Latitude[fatores_correcao_lat$Latitude %in% latitudes_adjacentes])]

    #interpolação linear
    Fc[i]= approx(x = latitudes_adjacentes, y = Fc_adjacentes, xout = latitude)$y
}

I= sum(0.09*temp^1.5)
alpha= 0.016*I+0.5

EVT= 16.2*Fc*(10*temp/I)^alpha
EVT


##### MB26 #####


#o abaco é usado para aplicar a equação do metodo de penman de forma mais simples e rápida
#foi utilizado o roteiro de cálculo da evapotranspiração pelo metodo penman-monteith-fao publicado pela embrapa para automatizar o calculo com o codigo e comparar os metodos 
dados= data.frame(Latitude= c(90, 80, 60, 40, 20, 0, -20, -40, -60, -80, -90),
    Jan= c(0, 0, 86, 358, 631, 844, 970, 998, 947, 804, 995),
    Fev= c(0, 3, 234, 538, 795, 963, 1020, 982, 881, 653, 956),
    Mar= c(55, 143, 424, 663, 821, 878, 832, 459, 201, 99, 656),
    Abr= c(518, 518, 687, 947, 914, 737, 737, 410, 50, 0, 92),
    Mai= c(903, 875, 866, 947, 912, 580, 608, 358, 50, 0, 0),
    Jun= c(1077, 1060, 983, 1001, 942, 480, 360, 77, 0, 0, 0),
    Jul= c(944, 930, 892, 941, 887, 792, 807, 77, 0, 0, 0),
    Ago= c(605, 600, 714, 843, 856, 820, 860, 453, 243, 30, 0),
    Set= c(136, 219, 494, 719, 740, 891, 860, 187, 447, 30, 0),
    Out= c(0, 17, 258, 528, 666, 866, 892, 187, 459, 0, 30),
    Nov= c(0, 0, 113, 397, 599, 873, 986, 413, 917, 0, 447),
    Dez= c(0, 0, 55, 318, 318, 829, 978, 1033, 1094, 0, 1110))

latitude= -20

T= 18
UR= 60
U2= 3
nD= 0.4
Ra= (dados[dados$Latitude==latitude,"Mar"]*4.184/100) #transformando em MJ/m²dia
G= 0 #devido ao baixo impacto, é possivel estimar o valor do fluxo de calor no solo como 0 segundo o manual da embrapa 
altitude= 0 #considerando que o reservatorio está no nivel do mar (afeta pouco o resultado)
alpha= 0.05 #albedo medio estimado para um reservatorio (albedo é a fração de radiação solar refletida por uma superficie)
sigma= 4.903*10^-9 #constante de stefan-boltzmann em MJ/m²dia

Patm= 101.3*((293-0.0065*altitude)/293)^5.26
gamma= 0.665*10^-3*Patm

es= 0.6108*exp(17.27*T/(T+237.3))
ea= es*UR/100
delta= 4098*(0.6108*exp(17.27*T/(T+237.3)))/(T+237.3)^2

Rs= (0.25+0.5*nD)*Ra
Rns= (1-alpha)*Rs
Rso= (0.75+2*10^-5*altitude)*Ra
Rnl= sigma*(T+273.16)^4*(0.34-0.14*ea^0.5)*(1.35*Rs/Rso-0.35)
Rn= Rns-Rnl

Et= (0.408*delta*(Rn-G)+gamma*900*U2*(es-ea)/(T+273))/(delta+gamma*(1+0.34*U2))
Et


##### MB27 #####


dados= data.frame(Latitude= c(90, 80, 60, 40, 20, 0, -20, -40, -60, -80, -90),
    Jan= c(0, 0, 86, 358, 631, 844, 970, 998, 947, 804, 995),
    Fev= c(0, 3, 234, 538, 795, 963, 1020, 982, 881, 653, 956),
    Mar= c(55, 143, 424, 663, 821, 878, 832, 459, 201, 99, 656),
    Abr= c(518, 518, 687, 947, 914, 737, 737, 410, 50, 0, 92),
    Mai= c(903, 875, 866, 947, 912, 580, 608, 358, 50, 0, 0),
    Jun= c(1077, 1060, 983, 1001, 942, 480, 360, 77, 0, 0, 0),
    Jul= c(944, 930, 892, 941, 887, 792, 807, 77, 0, 0, 0),
    Ago= c(605, 600, 714, 843, 856, 820, 860, 453, 243, 30, 0),
    Set= c(136, 219, 494, 719, 740, 891, 860, 187, 447, 30, 0),
    Out= c(0, 17, 258, 528, 666, 866, 892, 187, 459, 0, 30),
    Nov= c(0, 0, 113, 397, 599, 873, 986, 413, 917, 0, 447),
    Dez= c(0, 0, 55, 318, 318, 829, 978, 1033, 1094, 0, 1110))

latitude= -20

T= c(12,15,18,21,24)
UR= 60
U2= 3
nD= 0.4
Ra= (dados[dados$Latitude==latitude,"Mar"]*4.184/100)
G= 0
altitude= 0
alpha= 0.05
sigma= 4.903*10^-9

Patm= 101.3*((293-0.0065*altitude)/293)^5.26
gamma= 0.665*10^-3*Patm

Et= c()
for (i in 1:length(T)){
es= 0.6108*exp(17.27*T[i]/(T[i]+237.3))
ea= es*UR/100
delta= 4098*(0.6108*exp(17.27*T[i]/(T[i]+237.3)))/(T[i]+237.3)^2

Rs= (0.25+0.5*nD)*Ra
Rns= (1-alpha)*Rs
Rso= (0.75+2*10^-5*altitude)*Ra
Rnl= sigma*(T[i]+273.16)^4*(0.34-0.14*ea^0.5)*(1.35*Rs/Rso-0.35)
Rn= Rns-Rnl

Et[i]= (0.408*delta*(Rn-G)+gamma*900*U2*(es-ea)/(T[i]+273))/(delta+gamma*(1+0.34*U2))
}
Et

dados= data.frame(Latitude= c(90, 80, 60, 40, 20, 0, -20, -40, -60, -80, -90),
    Jan= c(0, 0, 86, 358, 631, 844, 970, 998, 947, 804, 995),
    Fev= c(0, 3, 234, 538, 795, 963, 1020, 982, 881, 653, 956),
    Mar= c(55, 143, 424, 663, 821, 878, 832, 459, 201, 99, 656),
    Abr= c(518, 518, 687, 947, 914, 737, 737, 410, 50, 0, 92),
    Mai= c(903, 875, 866, 947, 912, 580, 608, 358, 50, 0, 0),
    Jun= c(1077, 1060, 983, 1001, 942, 480, 360, 77, 0, 0, 0),
    Jul= c(944, 930, 892, 941, 887, 792, 807, 77, 0, 0, 0),
    Ago= c(605, 600, 714, 843, 856, 820, 860, 453, 243, 30, 0),
    Set= c(136, 219, 494, 719, 740, 891, 860, 187, 447, 30, 0),
    Out= c(0, 17, 258, 528, 666, 866, 892, 187, 459, 0, 30),
    Nov= c(0, 0, 113, 397, 599, 873, 986, 413, 917, 0, 447),
    Dez= c(0, 0, 55, 318, 318, 829, 978, 1033, 1094, 0, 1110))

latitude= -20

T= 18
UR= c(40, 50, 60, 70, 80)
U2= 3
nD= 0.4
Ra= (dados[dados$Latitude==latitude,"Mar"]*4.184/100)
G= 0
altitude= 0
alpha= 0.05
sigma= 4.903*10^-9

Patm= 101.3*((293-0.0065*altitude)/293)^5.26
gamma= 0.665*10^-3*Patm

es= 0.6108*exp(17.27*T/(T+237.3))

Et= c()
for (i in 1:length(UR)){
ea= es*UR[i]/100
delta= 4098*(0.6108*exp(17.27*T/(T+237.3)))/(T+237.3)^2

Rs= (0.25+0.5*nD)*Ra
Rns= (1-alpha)*Rs
Rso= (0.75+2*10^-5*altitude)*Ra
Rnl= sigma*(T+273.16)^4*(0.34-0.14*ea^0.5)*(1.35*Rs/Rso-0.35)
Rn= Rns-Rnl

Et[i]= (0.408*delta*(Rn-G)+gamma*900*U2*(es-ea)/(T+273))/(delta+gamma*(1+0.34*U2))
}
Et

dados= data.frame(Latitude= c(90, 80, 60, 40, 20, 0, -20, -40, -60, -80, -90),
    Jan= c(0, 0, 86, 358, 631, 844, 970, 998, 947, 804, 995),
    Fev= c(0, 3, 234, 538, 795, 963, 1020, 982, 881, 653, 956),
    Mar= c(55, 143, 424, 663, 821, 878, 832, 459, 201, 99, 656),
    Abr= c(518, 518, 687, 947, 914, 737, 737, 410, 50, 0, 92),
    Mai= c(903, 875, 866, 947, 912, 580, 608, 358, 50, 0, 0),
    Jun= c(1077, 1060, 983, 1001, 942, 480, 360, 77, 0, 0, 0),
    Jul= c(944, 930, 892, 941, 887, 792, 807, 77, 0, 0, 0),
    Ago= c(605, 600, 714, 843, 856, 820, 860, 453, 243, 30, 0),
    Set= c(136, 219, 494, 719, 740, 891, 860, 187, 447, 30, 0),
    Out= c(0, 17, 258, 528, 666, 866, 892, 187, 459, 0, 30),
    Nov= c(0, 0, 113, 397, 599, 873, 986, 413, 917, 0, 447),
    Dez= c(0, 0, 55, 318, 318, 829, 978, 1033, 1094, 0, 1110))

latitude= -20

T= 18
UR= 60
U2= 3
nD= c(0.2, 0.3, 0.4, 0.5, 0.6)
Ra= (dados[dados$Latitude==latitude,"Mar"]*4.184/100)
G= 0
altitude= 0
alpha= 0.05
sigma= 4.903*10^-9

Patm= 101.3*((293-0.0065*altitude)/293)^5.26
gamma= 0.665*10^-3*Patm

es= 0.6108*exp(17.27*T/(T+237.3))
ea= es*UR/100
delta= 4098*(0.6108*exp(17.27*T/(T+237.3)))/(T+237.3)^2


for (i in 1:length(nD)){
Rs= (0.25+0.5*nD[i])*Ra
Rns= (1-alpha)*Rs
Rso= (0.75+2*10^-5*altitude)*Ra
Rnl= sigma*(T+273.16)^4*(0.34-0.14*ea^0.5)*(1.35*Rs/Rso-0.35)
Rn= Rns-Rnl

Et[i]= (0.408*delta*(Rn-G)+gamma*900*U2*(es-ea)/(T+273))/(delta+gamma*(1+0.34*U2))
}
Et

dados= data.frame(Latitude= c(90, 80, 60, 40, 20, 0, -20, -40, -60, -80, -90),
    Jan= c(0, 0, 86, 358, 631, 844, 970, 998, 947, 804, 995),
    Fev= c(0, 3, 234, 538, 795, 963, 1020, 982, 881, 653, 956),
    Mar= c(55, 143, 424, 663, 821, 878, 832, 459, 201, 99, 656),
    Abr= c(518, 518, 687, 947, 914, 737, 737, 410, 50, 0, 92),
    Mai= c(903, 875, 866, 947, 912, 580, 608, 358, 50, 0, 0),
    Jun= c(1077, 1060, 983, 1001, 942, 480, 360, 77, 0, 0, 0),
    Jul= c(944, 930, 892, 941, 887, 792, 807, 77, 0, 0, 0),
    Ago= c(605, 600, 714, 843, 856, 820, 860, 453, 243, 30, 0),
    Set= c(136, 219, 494, 719, 740, 891, 860, 187, 447, 30, 0),
    Out= c(0, 17, 258, 528, 666, 866, 892, 187, 459, 0, 30),
    Nov= c(0, 0, 113, 397, 599, 873, 986, 413, 917, 0, 447),
    Dez= c(0, 0, 55, 318, 318, 829, 978, 1033, 1094, 0, 1110))

latitude= -20

T= 18
UR= 60
U2= c(1, 2, 3, 4, 5)
nD= 0.4
Ra= (dados[dados$Latitude==latitude,"Mar"]*4.184/100)
G= 0
altitude= 0
alpha= 0.05
sigma= 4.903*10^-9

Patm= 101.3*((293-0.0065*altitude)/293)^5.26
gamma= 0.665*10^-3*Patm

es= 0.6108*exp(17.27*T/(T+237.3))
ea= es*UR/100
delta= 4098*(0.6108*exp(17.27*T/(T+237.3)))/(T+237.3)^2

Rs= (0.25+0.5*nD)*Ra
Rns= (1-alpha)*Rs
Rso= (0.75+2*10^-5*altitude)*Ra
Rnl= sigma*(T+273.16)^4*(0.34-0.14*ea^0.5)*(1.35*Rs/Rso-0.35)
Rn= Rns-Rnl

Et= c()
for (i in 1:length(U2)){
Et[i]= (0.408*delta*(Rn-G)+gamma*900*U2[i]*(es-ea)/(T+273))/(delta+gamma*(1+0.34*U2[i]))
}
Et


##### MB28 #####


T= c(0, 2, 5, 10, 20, 30, 60, 90, 150)
Vtotal= c(0, 278, 658, 1173, 1924, 2500, 3345, 3875, 4595)

d= 35

deltat= c()
for (i in 1:length(T)-1){
    deltat[i]= T[i+1]-T[i]
}
deltav_deltat= c()
for (i in 1:length(Vtotal)-1){
    deltav_deltat[i]= (Vtotal[i+1]-Vtotal[i])/deltat[i]
}

f= deltav_deltat*10*60/(pi*d^2/4) #mm/h
f

T2= c()
for (i in 1:length(T)-1){
    T2[i]= T[i] + (T[i+1]-T[i])/2
}
T2

dados= data.frame("Tempo"=T2, "Infiltração"=f)

library(ggplot2)
ggplot(dados, aes(x = Tempo, y = Infiltração)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Taxa de infiltração ao longo do tempo", x = "Tempo (min)", y = "f (mm/h)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


##### MB29 #####


dados= data.frame("Dmargem"= c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22),
    "Profundidade"= c(0, 1, 4.3, 7.2, 8.5, 7.4, 5.6, 4.7, 3.5, 2.1, 1.4, 0),
    "V02"= c(0, 1.4, 1.9, 2.6, 2.9, 2.7, 2.5, 2.3, 2.1, 1.8, 1.5, 0),
    "V08"= c(0, 0.7, 1.2, 1.8, 2, 1.9, 1.7, 1.5, 1.3, 1.1, 1, 0))

Vmed= (dados$V02+dados$V08)/2

Vmed_elemento= c()
for (i in 1:length(Vmed)-1){
    Vmed_elemento[i]=(Vmed[i]+Vmed[i+1])/2
}
A= c()
distancia= c()
for (i in 1:nrow(dados)-1){
    distancia[i]= dados$Dmargem[i+1]-dados$Dmargem[i]
    A[i]=(dados$Profundidade[i]+dados$Profundidade[i+1])*distancia[i]/2
}
Q= sum(Vmed_elemento*A)
Q

library(ggplot2)
ggplot(dados, aes(x = Dmargem, y = Profundidade)) +
  geom_line(color = "blue", size = 0.7) +
  geom_point(color = "red", size = 1) +
  scale_y_reverse() +
  labs(title = "Seção transversal do rio", x = "Distância da margem (m)", y = "Profundidade (m)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1)


##### MB30 #####


#em uma função logaritmica, o valor do eixo y (nesse caso correspondente à velocidade (v)) cresce rapidamente com pequenas variações do eixo x (nesse caso correspondente à altura (y)) quando x se encontra próximo do 0 e passa a crescer cada vez menos conforme x se afasta de 0. dessa forma, faz sentido que o valor médio da velocidade seja obtido com uma altura mais proxima do fundo do que da superficie

#v(y)= a*log(y/yo), onde yo é um valor de y muito proximo de 0 (resultando em que a velocidade é igual a 0) e "a" é uma constante que pode ser calibrada
#integral de v(y) em y de yo até h: a*(h*log(h/yo)-h) - a*(yo*log(yo/yo)-yo) = a*(h*log(h/yo)-h) + a*yo = a*h*log(h/yo) - a*h + a*yo
#dividindo o resultado pela profundidade total: a*log(h/yo) - a + a*yo/h = a*(log(h/yo) - 1 + yo/h)
#substituindo o valor da velocidade média na equação da velocidade para encontrar o y: a*(log(h/yo) - 1 + yo/h) = a*log(y/yo) ---> log(y/yo) = log(h/yo)-1+yo/h ---> y/yo = (h/yo)*e^(-1+yo/h) ---> y= h*e^(-1+yo/h)
#assim, a posição da velocidade média é y= h*e^(-1+yo/h)
#como yo/h tende a 0:  y= h*e^-1 ---> y= 0.368*h   ou 63.2% da profundidade da superficie em direção ao fundo


##### MB31 #####


dados= data.frame("Dmargem"= c(0, 2, 4, 6, 8, 10, 12),
    "Profundidade"= c(0, 1, 3, 5.5, 4, 1.5, 0),
    "V02"= c(0, 1.4, 2, 3, 2.4, 1.5, 0),
    "V08"= c(0, 0.6, 1.2, 2, 1.6, 1.1, 0))

Vmed= (dados$V02+dados$V08)/2

Vmed_elemento= c()
for (i in 1:length(Vmed)-1){
    Vmed_elemento[i]=(Vmed[i]+Vmed[i+1])/2
}
Vmed_elemento
A= c()
distancia= c()
for (i in 1:nrow(dados)-1){
    distancia[i]= dados$Dmargem[i+1]-dados$Dmargem[i]
    A[i]=(dados$Profundidade[i]+dados$Profundidade[i+1])*distancia[i]/2
}
A
Q= sum(Vmed_elemento*A)
Q


##### MB32 #####


dados= data.frame("Dmargem"= c(2.5, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 47, 48.5),
    "Profundidade"= c(0, 0.9, 2.14, 2.36, 2.5, 2.54, 2.46, 2.46, 2.3, 2.2, 2.1, 2.1, 1.96, 1.94, 1.92, 1.6, 1.5, 0),
    "TOQ02"= c(0, 0, 3, 6, 12, 15, 16, 17, 16, 16, 16, 14, 12, 13, 11, 7, 3, 0),
    "TEMP02"= c(0, 46.2, 61.6, 45.4, 43.6, 42.6, 40.2, 42.6, 41.0, 41.0, 41.0, 41.0, 41.0, 41.0, 41.0, 41.0, 41.0, 42.0), 
    "TOQ08"= c(0, 0, 3, 7, 9, 12, 12, 11, 11, 9, 11, 10, 10, 11, 9, 6, 3, 0),
    "TEMP08"= c(0, 48.4, 52.2, 45.0, 43.6, 42.0, 40.1, 41.5, 40.1, 42.0, 41.0, 40.0, 41.0, 40.0, 41.0, 40.0, 41.0, 42.0))

ROT= 10

rotaçoes02= dados$TOQ02*ROT
rotaçoes08= dados$TOQ08*ROT

V02= c(0)
V08= c(0)
for (i in 2:nrow(dados)){
V02[i]= (rotaçoes02[i]/dados$TEMP02[i])*0.2473 + 0.0051
V08[i]= (rotaçoes08[i]/dados$TEMP08[i])*0.2473 + 0.0051
}

Vmed= (V02+V08)/2

Vmed_elemento= c()
for (i in 1:length(Vmed)-1){
    Vmed_elemento[i]=(Vmed[i]+Vmed[i+1])/2
}

distancia= c()
A= c()
for (i in 1:nrow(dados)-1){
    distancia[i]= dados$Dmargem[i+1]-dados$Dmargem[i]
    A[i]=(dados$Profundidade[i]+dados$Profundidade[i+1])*distancia[i]/2
}

Q= sum(Vmed_elemento*A)
Q

V=sum(Vmed_elemento*A)/sum(A)
V

library(ggplot2)
ggplot(dados, aes(x = Dmargem, y = Profundidade)) +
  geom_line(color = "blue", size = 0.7) +
  geom_point(color = "red", size = 1) +
  scale_y_reverse() +
  labs(title = "Seção Transversal do Rio", x = "Distância da margem (m)", y = "Profundidade (m)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 5)


##### MB33 #####


dados= data.frame(cota = c(0.420, 0.420, 0.420, 0.425, 2.130, 2.140, 1.710, 1.710, 1.090, 1.070, 
            0.455, 0.445, 0.445, 0.455, 0.475, 0.485, 0.475, 0.470, 0.570, 0.555,
            0.480, 0.560, 0.565, 0.675, 0.655, 0.640, 0.625, 0.700, 0.765, 0.765, 
            0.740, 0.770, 0.770, 0.770, 0.880, 0.885, 0.930, 0.880, 0.925, 0.880, 
            0.870, 0.930, 0.960, 0.970, 0.960, 0.960, 0.980, 0.970, 0.960, 0.960,
            1.000, 1.030, 1.050, 1.090, 1.100, 1.090, 1.090, 1.090, 1.050, 1.040,
            1.030, 1.000, 0.930, 1.140, 1.130, 1.120, 1.120, 1.110, 1.110, 1.090),
    vazao = c(59.038, 63.114, 61.299, 60.493, 479.713, 494.838, 375.403, 371.857, 226.558, 214.420, 
            63.297, 62.396, 60.540, 61.635, 66.434, 68.339, 66.585, 65.183, 89.329, 84.805, 
            69.073, 86.944, 88.581, 124.848, 117.134, 111.621, 105.579, 137.586, 167.460, 166.567, 
            154.101, 169.288, 168.390, 169.288, 222.242, 225.789, 247.074, 222.242, 243.527, 222.242, 
            217.235, 247.074, 262.199, 267.712, 262.199, 262.199, 272.326, 267.712, 262.199, 262.199,
            286.556, 302.578, 311.642, 329.204, 333.629, 329.204, 329.204, 329.204, 311.642, 306.011, 
            302.578, 286.556, 247.074, 363.503, 358.896, 354.295, 354.295, 349.704, 349.704, 329.204))

dados$cota= dados$cota*100 #cm

library(minpack.lm) #algoritmo de otimizaçao levenberg-marquardt
modelo = nlsLM(
  vazao ~ a * (cota - b)^c, data = dados,
  start = list(a = 1, b = 20, c = 1),
  lower = c(a = -Inf, b = -Inf, c = -Inf),
  upper = c(a = Inf, b = Inf, c = Inf),
  control = nls.lm.control(maxiter = 100))

dados_pred= predict(modelo)
RSS= sum((dados$vazao - dados_pred)^2)
TSS= sum((dados$vazao - mean(dados$vazao))^2)
R2= 1-RSS/TSS
R2

cota_pred= seq(coef(modelo)["b"], max(dados$cota)*1.2, length.out = 100)
vazao_pred= predict(modelo, data.frame(cota = cota_pred))
previsao= data.frame(cota = cota_pred, vazao = vazao_pred)

library(ggplot2)
ggplot() +
  geom_point(data= dados, aes(x = cota, y = vazao), color = "blue") +
  geom_line(data = previsao, aes(x = cota, y = vazao), color = "red") +
  labs(title = "Curva-Chave", x = "Cota (cm)", y = "Vazão (m³/s)") +
  annotate("text", x = max(dados$cota) * 0.75, y = max(dados$vazao) * 0.7, 
           label = sprintf("Q = %.2f*(h-%.2f)^%.2f\nR² = %.3f", coef(modelo)["a"], coef(modelo)["b"], coef(modelo)["c"], round(R2, 3)), 
           hjust = 0, size = 4, color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


##### MB34 #####


dados= data.frame(cota = c(0.420, 0.420, 0.420, 0.425, 2.130, 2.140, 1.710, 1.710, 1.090, 1.070, 
            0.455, 0.445, 0.445, 0.455, 0.475, 0.485, 0.475, 0.470, 0.570, 0.555,
            0.480, 0.560, 0.565, 0.675, 0.655, 0.640, 0.625, 0.700, 0.765, 0.765, 
            0.740, 0.770, 0.770, 0.770, 0.880, 0.885, 0.930, 0.880, 0.925, 0.880, 
            0.870, 0.930, 0.960, 0.970, 0.960, 0.960, 0.980, 0.970, 0.960, 0.960,
            1.000, 1.030, 1.050, 1.090, 1.100, 1.090, 1.090, 1.090, 1.050, 1.040,
            1.030, 1.000, 0.930, 1.140, 1.130, 1.120, 1.120, 1.110, 1.110, 1.090),
    vazao = c(59.038, 63.114, 61.299, 60.493, 479.713, 494.838, 375.403, 371.857, 226.558, 214.420, 
            63.297, 62.396, 60.540, 61.635, 66.434, 68.339, 66.585, 65.183, 89.329, 84.805, 
            69.073, 86.944, 88.581, 124.848, 117.134, 111.621, 105.579, 137.586, 167.460, 166.567, 
            154.101, 169.288, 168.390, 169.288, 222.242, 225.789, 247.074, 222.242, 243.527, 222.242, 
            217.235, 247.074, 262.199, 267.712, 262.199, 262.199, 272.326, 267.712, 262.199, 262.199,
            286.556, 302.578, 311.642, 329.204, 333.629, 329.204, 329.204, 329.204, 311.642, 306.011, 
            302.578, 286.556, 247.074, 363.503, 358.896, 354.295, 354.295, 349.704, 349.704, 329.204))

dados$cota= dados$cota*100

cotas= c(2.66, 2.52, 2.40, 2.25, 1.98, 1.90, 1.98, 2.07, 2.18, 2.14, 2.03, 1.91, 1.82, 1.80, 1.80, 1.82, 1.80, 1.83, 1.90, 1.96, 1.96, 1.98, 1.98, 1.93, 1.76, 1.62, 1.52, 1.51, 1.64, 1.82, 2.00,
2.12, 2.18, 2.15, 2.08, 1.98, 1.91, 1.86, 1.80, 1.82, 1.81, 1.79, 1.74, 1.68, 1.64, 1.58, 1.46, 1.40, 1.40, 1.44, 1.51, 1.66, 1.78, 1.86, 1.92, 1.97, 2.04, 2.08, 2.10,
2.14, 2.21, 2.36, 2.48, 2.56, 2.66, 2.73, 2.82, 2.96, 3.08, 3.14, 3.21, 3.26, 3.28, 3.26, 3.18, 3.07, 2.96, 2.88, 2.78, 2.69, 2.60, 2.44, 2.24, 2.04, 1.84, 1.71, 1.58, 1.48, 1.38, 1.3,
1.28, 1.28, 1.26, 1.19, 1.18, 1.22, 1.30, 1.39, 1.42, 1.40, 1.40, 1.38, 1.37, 1.34, 1.31, 1.30, 1.28, 1.33, 1.52, 1.73, 1.92, 2.06, 2.06, 2.08, 2.11, 2.16, 2.22, 2.22, 2.26, 2.24,
2.24, 2.28, 2.24, 2.16, 2.10, 2.04, 2.00, 2.04, 2.12, 2.23, 2.36, 2.52, 2.58, 2.62, 2.61, 2.58, 2.56, 2.52, 2.71, 3.17, 3.61, 4.00, 4.43, 4.81, 5.10, 5.44, 5.78, NA, NA, NA, NA,
6.12, 6.04, 5.99, 5.90, 5.76, 5.60, 5.44, 5.41, 5.08, NA, NA, 4.79, 4.63, 4.56, 4.51, 4.50, 4.42, 4.26, 4.20, 4.27, 4.40, 4.52, 4.43, 4.34, 4.29, 4.24, 4.19, 4.14, 4.12, 4.1,
4.17, 4.28, 4.33, 4.39, 4.45, 4.54, 4.75, 5.08, 5.55, 6.94, 7.92, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 7.88, 7.67, 7.38, 7.15, 6.95, 6.62,
6.40, 6.27, 6.20, 6.04, 5.75, 5.55, 5.35, 5.19, 5.12, 4.45, 4.80, 4.57, 4.38, 4.22, 3.92, 3.54, 3.34, 3.09, 2.90, 2.65, 2.30, 2.04, 1.85, 1.77, 1.73, 1.70, 1.68, 1.61, 1.54, 1.47, 1.43,
1.40, 1.38, 1.38, 1.38, 1.38, 1.40, 1.43, 1.51, 1.56, 1.54, 1.53, 1.55, 1.57, 1.54, 1.49, 1.87, 2.25, 2.60, 2.88, 2.98, 3.48, 3.58, 3.67, 3.73, 3.84, 4.03, 4.02, 4.53, 4.75, 4.95,
5.09, 5.19, 5.13, 5.05, 5.75, 5.55, 5.35, 5.05, 4.75, 3.59, 3.39, 3.15, 2.80, 2.55, 2.35, 2.15, 1.20, 1.68, 1.85, 1.93, 2.25, 2.36, 2.52, 2.62, 2.74, 2.74, 2.54, 2.28, 2.21, 2.18, 2.11,
2.08, 2.05, 1.00, 1.88, 1.88, 1.88, 1.85, 1.79, 1.65, 1.54, 1.47, 1.44, 1.50, 1.50, 1.50, 1.48, 1.44, 1.39, 1.39, 1.40, 1.47, 1.50, 1.50, 1.50, 1.48, 1.44, 1.37, 1.3, 1.22, 1.18,
1.17, 1.12, 1.08, 1.08, 1.08, 1.08, 1.07, 1.07, 1.06, 1.06, 1.05, 1.15, 1.28, 1.34, 1.48, 1.53, 1.56, 1.56, 1.67, 1.95, 2.06, 2.15, 2.30, 2.38, 2.40, 2.39, 2.32, 2.26, 2.18, 2.05, 1.89)

cotas=cotas*100

library(minpack.lm)
modelo = nlsLM(
  vazao ~ a * (cota - b)^c, data = dados,
  start = list(a = 1, b = 20, c = 1),
  lower = c(a = -Inf, b = -Inf, c = -Inf),
  upper = c(a = Inf, b = Inf, c = Inf),
  control = nls.lm.control(maxiter = 100))

vazao= predict(modelo, data.frame(cota = cotas))

data= seq.Date(from = as.Date("1983-01-01"), to = as.Date("1983-12-31"), by = "day")

dados= data.frame(vazao= vazao, data= data)

ggplot() +
  geom_line(data = dados, aes(x = data, y = vazao), color = "blue") +
  labs(title = "Fluviograma", x = "Data", y = "Vazão (m³/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


##### MB35 #####


dados= data.frame(cota = c(2.1, 2.5, 4.9, 7, 3.6, 2.9, 4.4, 6.4, 6, 4, 3.2, 5.6),
    vazao = c(340.95, 442.5, 1852.5, 4275, 900, 571.5, 1437.75, 3465, 2947.5, 1152, 710.25, 2520))

dados$cota= dados$cota*100 #cm

library(minpack.lm) #algoritmo de otimizaçao levenberg-marquardt
modelo = nlsLM(
  vazao ~ a * (cota - b)^c, data = dados,
  start = list(a = 1, b = 20, c = 1),
  lower = c(a = -Inf, b = -Inf, c = -Inf),
  upper = c(a = Inf, b = Inf, c = Inf),
  control = nls.lm.control(maxiter = 100))

dados_pred= predict(modelo)
RSS= sum((dados$vazao - dados_pred)^2)
TSS= sum((dados$vazao - mean(dados$vazao))^2)
R2= 1-RSS/TSS
R2

cota_pred= seq(coef(modelo)["b"], max(dados$cota)*1.2, length.out = 100)
vazao_pred= predict(modelo, data.frame(cota = cota_pred))
previsao= data.frame(cota = cota_pred, vazao = vazao_pred)

library(ggplot2)
ggplot() +
  geom_point(data= dados, aes(x = cota, y = vazao), color = "blue") +
  geom_line(data = previsao, aes(x = cota, y = vazao), color = "red") +
  labs(title = "Curva-Chave", x = "Cota (cm)", y = "Vazão (m³/s)") +
  annotate("text", x = max(dados$cota) * 0.1, y = max(dados$vazao) * 1, 
           label = sprintf("Q = %.2f*(h-%.2f)^%.2f\nR² = %.3f", coef(modelo)["a"], coef(modelo)["b"], coef(modelo)["c"], round(R2, 3)), 
           hjust = 0, size = 4, color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

dados2= data.frame(cota = c(2, 1.75, 1.5, 1.25, 1, 0.75, 0.5)*100)

dados2$vazao= predict(modelo, dados2)
dados2

dados3= data.frame(cota = c(0.5, 0.75, 1, 1.2, 1.4, 1.7, 2.6, 4.8, 5.9)*100)

dados3$vazao= predict(modelo, dados3)
dados3


##### MB36 #####


dias= c(12:30)
vazoes= c(278, 264, 251, 238, 226, 215, 5350, 8150, 6580, 
            1540, 505, 280, 219, 195, 179, 170, 161, 153, 146)

dados= data.frame(dias=dias, vazoes=vazoes)

library(ggplot2)
ggplot(dados, aes(x = dias, y = vazoes)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  labs(x = "Dias", y = "Vazão (m³/s)", title = "Hidrograma com escala logarítmica") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


vazao_div= vazoes[-1] / vazoes[-length(vazoes)]
indice_pA= which.max(vazao_div) #which retorna o indice

dia_pA= dias[indice_pA]


valores_antes_A= vazao_div[1:(indice_pA - 1)]
valor_ref= mean(valores_antes_A)

indice_segundo_ponto= which(abs(vazao_div[(indice_pA + 1):length(vazao_div)] - valor_ref) < 0.001)[1] #o menor valor para essa diferença pode ser alterado
dia_pB= dias[indice_segundo_ponto + indice_pA]


dia_pontos= c(dia_pA, dia_pB)

vazao_pontos= vazoes[dias %in% dia_pontos]


ajuste_eq_linear= lm(vazao_pontos ~ dia_pontos)

dias_separacao_esc= dia_pA:dia_pB
vazoes_esc_sub= predict(ajuste_eq_linear, newdata = data.frame(dia_pontos = dias_separacao_esc))
dados_esc_sub= data.frame(dias=dias_separacao_esc, vazoes=vazoes_esc_sub)

vazoes_separacao_esc= vazoes[dias %in% dias_separacao_esc]
vazoes_escoamento_superficial= vazoes_separacao_esc - vazoes_esc_sub
volume= sum(vazoes_escoamento_superficial*60*60*24)
volume

ggplot() +
  geom_point(data= dados, aes(x = dias, y = vazoes)) +
  geom_line(data= dados, aes(x = dias, y = vazoes)) +
  geom_line(data= dados_esc_sub, aes(x = dias, y = vazoes), color = "red", linetype = "dashed") +
  labs(x = "Dias", y = "Vazão (m³/s)", title = "Escoamento total e escoamento subterrâneo com o método da linha reta") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

dia_pM= dados[which.max(dados[,2]),1]

vazao_subtracao= vazoes[-1] - vazoes[-length(vazoes)]
dia_pI= dados[which.min(vazao_subtracao)+1,1]


dados1=data.frame(dia=c(dia_pA-1, dia_pA), vazoes=c(vazoes[dias %in% c(dia_pA-1, dia_pA)]))
dias1= dia_pA:dia_pM
k1= (dados1$vazoes[2] / dados1$vazoes[1])
vazoes1= c()
vazoes1[1]= dados1$vazoes[2]
for (i in 1:(length(dias1)-1)){
    vazoes1[i+1]= vazoes1[i]*k1
}

dados3=data.frame(dia=c(dia_pB, dia_pB+1), vazoes=c(vazoes[dias %in% c(dia_pB, dia_pB+1)]))
dias3= dia_pI:dia_pB
k3= (dados3$vazoes[2] / dados3$vazoes[1])
vazoes3= c()
vazoes3[length(dias3)]= dados3$vazoes[2]
for (i in (length(dias3)-1):1){
    vazoes3[i]= vazoes3[i+1]/k3
}

dados2=data.frame(dia=c(dia_pM, dia_pI), vazoes=c(vazoes1[length(vazoes1)], vazoes3[length(1)]))
eq_linear2= lm(vazoes ~ dia, dados2)
dias2= dia_pM:dia_pI
vazoes2= predict(eq_linear2, data.frame(dia = dias2))


dias_separacao_esc= dia_pA:dia_pB
vazoes_esc_sub= c(vazoes1, vazoes2[-1], vazoes3[-1])

vazoes_separacao_esc= vazoes[dias %in% dias_separacao_esc]
vazoes_escoamento_superficial= vazoes_separacao_esc - vazoes_esc_sub
volume= sum(vazoes_escoamento_superficial*60*60*24)
volume


dados_pred_1= data.frame(dia= dias1, vazoes=vazoes1)
dados_pred_2= data.frame(dia= dias2, vazoes=vazoes2)
dados_pred_3= data.frame(dia= dias3, vazoes=vazoes3)

ggplot() +
  geom_point(data= dados, aes(x = dias, y = vazoes)) +
  geom_line(data= dados, aes(x = dias, y = vazoes)) +
  geom_line(data= dados_pred_1, aes(x = dia, y = vazoes), color = "blue", linetype = "dashed") +
  geom_line(data= dados_pred_2, aes(x = dia, y = vazoes), color = "green", linetype = "dashed") +
  geom_line(data= dados_pred_3, aes(x = dia, y = vazoes), color = "red", linetype = "dashed") +
  labs(x = "Dias", y = "Vazão (m³/s)", title = "Escoamento total e escoamento subterrâneo com o método da depleção dupla") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


##### MB37 #####


dias= c(5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12, 12.5)
vazoes= c(5.6, 5, 4.5, 10, 39.5, 75, 82, 74, 62, 43, 20, 14.5, 8, 7.2, 6.5)

dados= data.frame(dias= dias, vazoes= vazoes)

vazao_div= vazoes[-1] / vazoes[-length(vazoes)]

aux=c()
for (i in 1:(length(vazao_div)-1)){
    if (abs(vazao_div[i+1]-vazao_div[i]) > 0.01){
        aux=c(aux,i)
    }
}
indice_pA= aux[1]+1

dia_pA= dias[indice_pA]

aux= c()
for (i in length(vazao_div):2){
    if (abs(vazao_div[i]-vazao_div[i-1]) > 0.01){
    aux= c(aux, i)
    }
}
indice_pB= aux[1]

dia_pB= dias[indice_pB]
dia_pB

k= (dados[indice_pA, 2] / dados[indice_pA-1, 2])
k

dados1= data.frame(dia=c(dia_pA, dia_pB), vazao=c(vazoes[dias %in% c(dia_pA, dia_pB)]))
eq_linear= lm(vazao ~ dia, dados1)

dias_pred= seq(dia_pA, dia_pB, by = 0.5)
vazoes_pred= predict(eq_linear, data.frame(dia= dias_pred))
dados_pred= data.frame(dias=dias_pred, vazoes=vazoes_pred)

library(ggplot2)
ggplot() +
  geom_point(data= dados, aes(x = dias, y = vazoes)) +
  geom_line(data= dados, aes(x = dias, y = vazoes)) +
  geom_line(data= dados_pred, aes(x = dias, y = vazoes), color = "red", linetype = "dashed") +
  labs(x = "Dias", y = "Vazão (m³/s)", title = "Escoamento total e escoamento subterrâneo com o método da linha reta") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

vazoes_esc_subterraneo_total= dados
vazoes_esc_subterraneo_total[vazoes_esc_subterraneo_total$dias %in% dados_pred$dias, "vazoes"]= dados_pred$vazoes[match(vazoes_esc_subterraneo_total$dias[vazoes_esc_subterraneo_total$dias %in% dados_pred$dias], dados_pred$dias)]
vazoes_esc_subterraneo_total

vazoes_escoamento_superficial= dados$vazoes - vazoes_esc_subterraneo_total$vazoes
volume= sum(vazoes_escoamento_superficial*12*60*60)
volume

A= 1000

h=volume/(A*10^3)
h

dados10mm= data.frame(dias= dias, vazoes = round(vazoes_escoamento_superficial*10/h, 2))
dados10mm

ggplot()+
    geom_point(data= dados10mm, aes(x= dias, y= vazoes)) +
    geom_line(data= dados10mm, aes(x= dias, y= vazoes)) +
    labs(x= "Dias", y= "Vazão (m³/s)", title="Hidrograma") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.45))


##### MB38 #####


tempo= c(0, 10, 20, 30, 40, 50, 60, 70, 80)
vazoes= c(20, 130, 102.5, 75, 47.5, 20, 16, 12.9, 10.2)

dados= data.frame(tempo= tempo, vazoes= vazoes)

library(ggplot2)
ggplot(dados, aes(x = tempo, y = vazoes)) +
  geom_line() +
  geom_point() +
  labs(x = "Tempo", y = "Vazão (m³/s)", title = "Hidrograma") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

vazao_div= vazoes[-1] / vazoes[-length(vazoes)]

indice_pA= 1

tempo_pA= tempo[indice_pA]

aux= c()
for (i in length(vazao_div):2){
    if (abs(vazao_div[i]-vazao_div[i-1]) > 0.05){
    aux= c(aux, i)
    }
}
indice_pB= aux[1]

tempo_pB= tempo[indice_pB]
tempo_pB

dados1= data.frame(tempo=c(tempo_pA, tempo_pB), vazao=c(vazoes[tempo %in% c(tempo_pA, tempo_pB)]))
eq_linear= lm(vazao ~ tempo, dados1)

tempo_pred= seq(tempo_pA, tempo_pB, by = 10)
vazoes_pred= predict(eq_linear, data.frame(tempo = tempo_pred))
dados_pred= data.frame(tempo=tempo_pred, vazoes=vazoes_pred)

vazoes_esc_subterraneo_total= dados
vazoes_esc_subterraneo_total[vazoes_esc_subterraneo_total$tempo %in% dados_pred$tempo, "vazoes"]= dados_pred$vazoes[match(vazoes_esc_subterraneo_total$tempo[vazoes_esc_subterraneo_total$tempo %in% dados_pred$tempo], dados_pred$tempo)]

vazoes_escoamento_superficial= dados$vazoes - vazoes_esc_subterraneo_total$vazoes
volume= sum(vazoes_escoamento_superficial*10*60*60)


h= 100*0.4

A= volume/(h*10^3)
A


##### MB39 #####


tempo= c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
vazoes= c(31.25, 25, 20, 130, 102.5, 75, 47.5, 20, 16, 12.8, 10.24)

dados= data.frame(tempo= tempo, vazoes= vazoes)

library(ggplot2)
ggplot(dados, aes(x = tempo, y = vazoes)) +
  geom_line() +
  geom_point() +
  labs(x = "Tempo", y = "Vazão (m³/s)", title = "Hidrograma") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

vazao_div= vazoes[-1] / vazoes[-length(vazoes)]

aux=c()
for (i in 1:(length(vazao_div)-1)){
    if (abs(vazao_div[i+1]-vazao_div[i]) > 0.01){
        aux=c(aux,i)
    }
}
indice_pA= aux[1]+1

tempo_pA= tempo[indice_pA]

aux= c()
for (i in length(vazao_div):2){
    if (abs(vazao_div[i]-vazao_div[i-1]) > 0.01){
    aux= c(aux, i)
    }
}
indice_pB= aux[1]

tempo_pB= tempo[indice_pB]

dados1= data.frame(tempo=c(tempo_pA, tempo_pB), vazao=c(vazoes[tempo %in% c(tempo_pA, tempo_pB)]))
eq_linear= lm(vazao ~ tempo, dados1)

tempo_pred= seq(tempo_pA, tempo_pB, by = 5)
vazoes_pred= predict(eq_linear, data.frame(tempo = tempo_pred))
dados_pred= data.frame(tempo=tempo_pred, vazoes=vazoes_pred)

vazoes_esc_subterraneo_total= dados
vazoes_esc_subterraneo_total[vazoes_esc_subterraneo_total$tempo %in% dados_pred$tempo, "vazoes"]= dados_pred$vazoes[match(vazoes_esc_subterraneo_total$tempo[vazoes_esc_subterraneo_total$tempo %in% dados_pred$tempo], dados_pred$tempo)]

vazoes_escoamento_superficial= dados$vazoes - vazoes_esc_subterraneo_total$vazoes
volume= sum(vazoes_escoamento_superficial*5*60*60)


h= 20*0.3

A= volume/(h*10^3)
A


