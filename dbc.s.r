##################################################################################
#Funcao para analisar DBC simples, comparar os trat por Tukey e ajustar regressao
##################################################################################
#O pacote agricolae deve estar instalado previamente;
#Caso o trat seja quantitativo, serao ajustados apenas modelos polinomiais, e apenas ate o 3o grau.
#trat: recebe um vetor com os tratamentos;
#bloco: recebe um vetor com os blocos;
#resp: recebe um vetor numerico com a variavel-resposta;
#quali: logico. Se TRUE, o trat é qualitativo e deve ser feito o teste de Tukey a 'sig'% de significância. Se FALSE, é quantitativo e uma regressao 
        #deve é feita.
##################################################################################

dbc.s<-function(trat, bloco, resp, quali=TRUE, mcomp='tukey', sig=0.05) {

source('tapply.stat.r')
source('reg.poly.r')

Trat<-factor(trat)
Bloco<-factor(bloco)
anava<-aov(resp~Trat+Bloco)
tab<-summary(anava)

colnames(tab[[1]])<-c('GL','SQ','QM','Fc','Pr>Fc')
tab[[1]]<-rbind(tab[[1]],c(apply(tab[[1]],2,sum)))
rownames(tab[[1]])<-c('Tratamento','Bloco','Resíduo','Total')
cv<-round(sqrt(tab[[1]][3,3])/mean(resp)*100, 2)
tab[[1]][4,3]=' '
cat('------------------------------------------------------------------------
Quadro da análise de variância\n------------------------------------------------------------------------\n')
print(tab[[1]])
cat('------------------------------------------------------------------------\nCV =',cv,'%\n')


#Teste de normalidade
pvalor.shapiro<-shapiro.test(anava$residuals)$p.value
cat('\n------------------------------------------------------------------------\nTeste de normalidade dos resíduos (Shapiro-Wilk)\n')
cat('p-valor: ',pvalor.shapiro, '\n')
if(pvalor.shapiro<0.05){cat('ATENÇÃO: a 5% de significância, os resíduos não podem ser considerados normais!
------------------------------------------------------------------------\n')}
else{cat('De acordo com o teste de Shapiro-Wilk a 5% de significância, os resíduos podem ser considerados normais.
------------------------------------------------------------------------\n')}

if(quali==TRUE) {
  
  if(mcomp=='tukey'){
source('order.group.r')
source('lastC.r')
source('HSD.test1.r')
    HSD.test1(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sig)
                    }
  if(mcomp=='duncan'){
source('HSD.DUNCAN.test.r')
     HSD.DUNCAN.test(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sig)            
                    }                   
  if(mcomp=='lsd'){
source('HSD.LSD.test.r')
     HSD.LSD.test(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sig)
                    }
  if(mcomp=='lsdb'){
source('HSD.LSDB.test.r')
     HSD.LSDB.test(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sig)
                    }
  if(mcomp=='sk'){
source('HSD.SCOTTKNOTT.test.r')
     HSD.SCOTTKNOTT.test(anova=anava,which="Trat",conf.level=1-sig)
                    }
  if(mcomp=='snk'){
source('HSD.SNK.test.r')
     HSD.SNK.test(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sig)
                    }
                }                   
else{   
    reg.poly(resp, trat, tab[[1]][3,1], tab[[1]][3,2], tab[[1]][1,1], tab[[1]][1,2])
}
}
