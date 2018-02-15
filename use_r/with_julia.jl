### ============== ### ============== ### ============== ###
##              Ejemplos de RCall para Use R              ##
##              Ollin Demian Langle Chimal                ##
##                    02 / 15 / 2017                      ##
### ============== ### ============== ### ============== ###

### ============== ### ============== ### ============== ###
##                                                        ##
##           Interactuando por primera vez                ##
##                                                        ##
### ============== ### ============== ### ============== ###


using OhMyREPL, RCall, DataFrames, RDatasets

X = randn(40,40)
@rput X

@time R"X %*% X";

Y = randn(4000,4000)
@rput Y

@time R"Y %*% Y";

function matmul(a,b)
    a*b
end

@time matmul(X,X);
@time multi=matmul(Y,Y);

@code_llvm matmul(X,X);

multi
@rput multi

### ============== ### ============== ### ============== ###
##                                                        ##
##       Usando ggplot2 para graficar objetos de R        ##
##                                                        ##
### ============== ### ============== ### ============== ###

mtcars = dataset("datasets","mtcars")
$
library(ggplot2)
ggplot($mtcars, aes(x=WT,y=MPG))+geom_point()


### ============== ### ============== ### ============== ###
##                                                        ##
##                  Ejemplo de la vida real               ##
##                                                        ##
### ============== ### ============== ### ============== ###


reval("""
library(RNeo4j)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
graph = startGraph('http://localhost:7474/db/data/', username='neo4j', password='')
""")

reval("""
query = "
MATCH (s:Senator)-[:VOTE{voted:'AUSENTE'}]->(e:edictum)<-[]-(d:day)-[:ATTENDED{attendancy:'ASISTENCIA'}]-(s:Senator)
RETURN s.senator as Senator, s.party as Party, count(*) as times ORDER BY times DESC
UNION
MATCH (s:Senator)
RETURN s.senator as Senator, s.party as Party, 0 as times
"
comision_comp <- cypher(graph,query)
comision <- ddply(comision_comp,c("Senator","Party"),numcolwise(sum))
""")

@rget comision

$
library(ggplot2)

ggplot(comision, aes(x=Party, y=times, fill=Party))+
  geom_boxplot()+
  scale_fill_manual(name = "Party",
                    values = c("PRI"="#fbb4ae",
                               "PAN"="#b3cde3",
                               "PRD"="#ffffcc",
                               "PVEM"="#ccebc5",
                               "PT"="#fed9a6",
                               "Independiente"="#decbe4"))+
  ggtitle("Vote Absence When Attended by Party")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(name="Vote absences when attended")

comision_julia=comision

### ============== ### ============== ### ============== ###
##                                                        ##
##                 Interpolación de objetos               ##
##                                                        ##
### ============== ### ============== ### ============== ###

ggplot($comision_julia, aes(x=Party, y=times, fill=Party))+
  geom_boxplot()+
  scale_fill_manual(name = "Party",
                    values = c("PRI"="#fbb4ae",
                               "PAN"="#b3cde3",
                               "PRD"="#ffffcc",
                               "PVEM"="#ccebc5",
                               "PT"="#fed9a6",
                               "Independiente"="#decbe4"))+
  ggtitle("Vote Absence When Attended by Party")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(name="Vote absences when attended")

### ============== ### ============== ### ============== ###
##                                                        ##
##              Definir funciones de R en Julia           ##
##                                                        ##
### ============== ### ============== ### ============== ###

R_kmeans = R"kmeans"
submulti = multi[:,[1,2,3]]
R_kmeans(submulti,2)

### ============== ### ============== ### ============== ###
##                                                        ##
##                Evaluación de expresiones               ##
##                                                        ##
### ============== ### ============== ### ============== ###

rcall(:sum, Float64[1.0, 4.0, 6.0])
