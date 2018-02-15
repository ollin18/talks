library(RNeo4j)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

graph = startGraph('http://localhost:7474/db/data/', username='neo4j', password='')



##########################################################################
##########################################################################
##########################################################################
##########################################################################
##############                                              ##############
##############                   Ausencias                  ##############
##############                                              ##############
##########################################################################
##########################################################################
##########################################################################
##########################################################################

query = "
MATCH (s:Senator)-[a:ATTENDED{attendancy:'AUSENTE'}]->(d:day)
RETURN s.senator as Senator, s.party as Party, count(d) as Absences ORDER BY Absences DESC
UNION
MATCH (s:Senator)
RETURN s.senator as Senator, s.party as Party, 0 as Absences
"

ausencias_comp <- cypher(graph,query)

ausencias <- ddply(ausencias_comp,c("Senator","Party"),numcolwise(sum))

abs_plot <- ggplot(ausencias, aes(x=Party, y=Absences, fill=Party))+
  geom_boxplot()+
  scale_fill_manual(name = "Party",
                     values = c("PRI"="#fbb4ae",
                                "PAN"="#b3cde3",
                                "PRD"="#ffffcc",
                                "PVEM"="#ccebc5",
                                "PT"="#fed9a6",
                                "Independiente"="#decbe4"))+
  ggtitle("Absence by Party")+
  theme(plot.title = element_text(hjust = 0.5))

abs_plot

##########################################################################
##########################################################################
##########################################################################
##########################################################################
##############                                              ##############
##############                   Comisión                   ##############
##############                                              ##############
##########################################################################
##########################################################################
##########################################################################
##########################################################################

query = "
MATCH (s:Senator)-[:ATTENDED{attendancy:'COMISIÓN OFICIAL'}]->(d:day)
RETURN s.senator as Senator, s.party as Party, count(d) as times ORDER BY times DESC
UNION
MATCH (s:Senator)-[:ATTENDED{attendancy:'AUSENTE'}]->(d:day)
RETURN s.senator as Senator, s.party as Party, count(d) as times ORDER BY times DESC
UNION
MATCH (s:Senator)
RETURN s.senator as Senator, s.party as Party, 0 as times
"

comision_comp <- cypher(graph,query)

comision <- ddply(comision_comp,c("Senator","Party"),numcolwise(sum))

View(comision)

com_plot <- ggplot(comision, aes(x=Party, y=times, fill=Party))+
  geom_boxplot()+
  scale_fill_manual(name = "Party",
                    values = c("PRI"="#fbb4ae",
                               "PAN"="#b3cde3",
                               "PRD"="#ffffcc",
                               "PVEM"="#ccebc5",
                               "PT"="#fed9a6",
                               "Independiente"="#decbe4"))+
  ggtitle("Official Comissions by Party")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(name="Absence by official comission")

com_plot


##########################################################################
##########################################################################
##########################################################################
##########################################################################
##############                                              ##############
##############                Justificadas                  ##############
##############                                              ##############
##########################################################################
##########################################################################
##########################################################################
##########################################################################

query = "
MATCH (s:Senator)-[:ATTENDED{attendancy:'JUSTIFICADA'}]->(d:day)
RETURN s.senator as Senator, s.party as Party, count(*) as times ORDER BY times DESC
UNION
MATCH (s:Senator)
RETURN s.senator as Senator, s.party as Party, 0 as times
"

comision_comp <- cypher(graph,query)

comision <- ddply(comision_comp,c("Senator","Party"),numcolwise(sum))

View(comision)

com_plot <- ggplot(comision, aes(x=Party, y=times, fill=Party))+
  geom_boxplot()+
  scale_fill_manual(name = "Party",
                    values = c("PRI"="#fbb4ae",
                               "PAN"="#b3cde3",
                               "PRD"="#ffffcc",
                               "PVEM"="#ccebc5",
                               "PT"="#fed9a6",
                               "Independiente"="#decbe4"))+
  ggtitle("Justified Absences by Party")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(name="Justified absences")

com_plot


##########################################################################
##########################################################################
##########################################################################
##########################################################################
##############                                              ##############
##############                Total Absences                ##############
##############                                              ##############
##########################################################################
##########################################################################
##########################################################################
##########################################################################

query = "
MATCH (s:Senator)-[:ATTENDED]->(d:day)
WHERE NOT (s:Senator)-[:ATTENDED{attendancy:'ASISTENCIA'}]->(d:day)
RETURN s.senator as Senator, s.party as Party, count(*) as times ORDER BY times DESC
UNION
MATCH (s:Senator)
RETURN s.senator as Senator, s.party as Party, 0 as times
"

comision_comp <- cypher(graph,query)

comision <- ddply(comision_comp,c("Senator","Party"),numcolwise(sum))

View(comision)

com_plot <- ggplot(comision, aes(x=Party, y=times, fill=Party))+
  geom_boxplot()+
  scale_fill_manual(name = "Party",
                    values = c("PRI"="#fbb4ae",
                               "PAN"="#b3cde3",
                               "PRD"="#ffffcc",
                               "PVEM"="#ccebc5",
                               "PT"="#fed9a6",
                               "Independiente"="#decbe4"))+
  ggtitle("Total Absences by Party")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(name="Total absences")

com_plot


##########################################################################
##########################################################################
##########################################################################
##########################################################################
##############                                              ##############
##############                Voting Absences               ##############
##############                                              ##############
##########################################################################
##########################################################################
##########################################################################
##########################################################################

query = "
MATCH (s:Senator)-[:VOTE{voted:'AUSENTE'}]->(e:edictum)
RETURN s.senator as Senator, s.party as Party,count(*) as times ORDER BY times DESC
UNION
MATCH (s:Senator)
RETURN s.senator as Senator, s.party as Party, 0 as times
"

comision_comp <- cypher(graph,query)

comision <- ddply(comision_comp,c("Senator","Party"),numcolwise(sum))

View(comision)

com_plot <- ggplot(comision, aes(x=Party, y=times, fill=Party))+
  geom_boxplot()+
  scale_fill_manual(name = "Party",
                    values = c("PRI"="#fbb4ae",
                               "PAN"="#b3cde3",
                               "PRD"="#ffffcc",
                               "PVEM"="#ccebc5",
                               "PT"="#fed9a6",
                               "Independiente"="#decbe4"))+
  ggtitle("Vote Absence by Party")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(name="Vote absences")

com_plot


##########################################################################
##########################################################################
##########################################################################
##########################################################################
##############                                              ##############
##############        Voting Absences Attending             ##############
##############                                              ##############
##########################################################################
##########################################################################
##########################################################################
##########################################################################

query = "
MATCH (s:Senator)-[:VOTE{voted:'AUSENTE'}]->(e:edictum)<-[]-(d:day)-[:ATTENDED{attendancy:'ASISTENCIA'}]-(s:Senator)
RETURN s.senator as Senator, s.party as Party, count(*) as times ORDER BY times DESC
UNION
MATCH (s:Senator)
RETURN s.senator as Senator, s.party as Party, 0 as times
"

comision_comp <- cypher(graph,query)

comision <- ddply(comision_comp,c("Senator","Party"),numcolwise(sum))

View(comision)

com_plot <- ggplot(comision, aes(x=Party, y=times, fill=Party))+
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

com_plot
