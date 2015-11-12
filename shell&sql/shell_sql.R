#i)
setwd("E:\\STA141/ASSIGNMENT/6/hw/airpline12_13")
dir = list.files()
pattern = c('SFO','SMF','OAK','LAX','JFK')


#caculate the number of th five airports
stat =
  
function(file, pattern){
  
  line = read.csv(file)
  aa = sapply(1:5, function(i) sum(grepl(pattern[i] , line[,15])))
  aa
}

system.time(sapply(1:12, function(i) stat(dir[i],pattern)))

tt = sapply(1:12, function(i) stat(dir[i],pattern))
result = sapply(1:5, function(i) sum(tt[i,]))
names(result) = c('SFO','SMF','OAK','LAX','JFK')
sort(result , decreasing = TRUE)

#ii)


subset = 
  #
  #file:the name of the file
  #get the lines which involve any of these five airports
  #
function(file){
  cmd = paste('E:/STA141/bin/grep  -e SFO -e SMF -e OAK -e LAX -e JFK',file ,'>> pairs.txt')
  shell(cmd)
}


sapply(1:12, function(i) subset(dir[i])) #get pairs.txt

shell("E:/STA141/bin/cut -d , -f 15,25 pairs.txt >> pairs1.txt")
pair = read.table('pairs1.txt',sep = ",")

num_out = apply(sapply(1:5,function(i) grepl(pattern[i], pair[[1]])), 2, sum)
num_in = apply(sapply(1:5,function(i) grepl(pattern[i], pair[[2]])), 2, sum)

num_in_out = num_out+num_in
names(num_in_out) = c('SFO','SMF','OAK','LAX','JFK')
sort(num_in_out, decreasing = TRUE)




#2. 
setwd("E:\\STA141/ASSIGNMENT/6/hwsql")

library(RSQLite)

db = dbConnect(SQLite(), dbname = 'lahman2013.sqlite')  

table_name = dbListTables(db)  #get the names of all tables

dbListFields(db,table_name[3])

# 1What years does the data cover? are there data for each of these years?

year.range =
  
function(pattern, name){
  if(sum(grepl(pattern,dbListFields(db,name))) == 1){
    cmd = paste('SELECT MAX(yearID), MIN(yearID), MAx(yearID)-MIN(yearID)+1 AS yearinterval
                ,COUNT(DISTINCT (yearID)) AS numberyear FROM ',name)
    dbGetQuery(db, cmd)
  }
}



pattern = 'yearID'
year = sapply(table_name, function(i) year.range(pattern, i))


#2. How many (unique) people are included in the database? How many are players, managers, etc?
people = dbGetQuery(db , 'SELECT DISTINCT playerID FROM Master')$playerID
managers = dbGetQuery(db, 'SELECT DISTINCT PlayerID FROM Managers')$playerID


length(people)
length(managers)


#3. What team won the World Series in 2000?
winner = dbGetQuery(db, 'SELECT name FROM Teams WHERE  yearID = 2000 AND WSWin = "Y" ')


#4. What team lost the World Series each year?

lose =dbGetQuery(db, 'SELECT name,yearID, WSWin, LgWIN FROM Teams WHERE WSWin = "N" AND LgWIN = "Y"')  #GROUP BY teamID



#5. Do you see a relationship between the number of games won in a season and winning the World
#Series?
winner.win = dbGetQuery(db,'SELECT yearID , W, G FROM Teams WHERE WSWin == "Y"')
winner.win.ratio =round( winner.win$W / winner.win$G,3)

loser.win = dbGetQuery(db,'SELECT yearID , W, G FROM Teams WHERE WSWin == "N"')
loser.win.ratio = round(loser.win$W / loser.win$G,3)

plot(density(winner.win.ratio),main = c("relationship"),col = 'red',xlim = c(0.1,0.9),xlab = 'ratio')
lines(density(loser.win.ratio),col = 'blue')
legend("topleft",legend = c("winer.ratio","loser.ratio"),col = c("red","blue"),lty = 1,cex=0.5)

#6. In 2003, what were the three highest salaries? (We refer here to unique salaries, i.e., more than
#one player might be paid one of these salaries.)

salary_1 = dbGetQuery(db, 'SELECT DISTINCT salary FROM Salaries ORDER BY salary')
-sort(-salary_1$salary)[1:3]


#7. For 1999, compute the total payroll of each of the different teams. Next compute the team
#payrolls for all years in the database for which we have salary information. Display these in a plot.

payroll = dbGetQuery(db, 'SELECT teamID, SUM(salary) AS SUM FROM Salaries WHERE yearID = 1990
                     GROUP BY teamID')


payroll.allyear = dbGetQuery(db, 'SELECT teamID,yearID, SUM(salary) AS payroll 
                             FROM Salaries GROUP BY teamID,yearID')
interaction.plot(payroll.allyear$yearID,payroll.allyear$teamID,
                 payroll.allyear$payroll,col = c(1:length(payroll.allyear$teamID)),trace.label = "nameID",
                 main = 'team payrolls for all years', xlab = 'year' , ylab = 'payrolls')


#8.Study the change in salary over time. Have salaries kept up with inflation, 
#fallen behind, or grown faster?


salary.years = dbGetQuery(db, 'SELECT yearID,AVG(salary) AS salary FROM Salaries GROUP BY yearID')

setwd('E://STA141/ASSIGNMENT/6/hwsql')
inf = read.table('inflation.txt', head = TRUE)

salary.change = salary.years$salary /inf$Annual
#PLOT
par(mfrow = c(1,3))
plot(salary.years$salary , main = 'original salary' , type = 'b',xaxt = 'n',
     pch =19 , xlab = 'year',ylab = 'salary')
axis(1,at=1:length(salary.years$yearID),labels=salary.years$yearID)
plot(inf$Annual,main = 'CPI' , type = 'b',xaxt = 'n',pch =19 , xlab = 'year',ylab = 'cpi')
axis(1,at=1:length(salary.years$yearID),labels=salary.years$yearID)
plot(salary.change,main = 'change of salary' , type = 'b',xaxt = 'n',pch =19 , xlab = 'year',ylab = 'salary')
axis(1,at=1:length(salary.years$yearID),labels=salary.years$yearID)

#9. Compare payrolls for the teams that are in the same leagues, and then in the same divisions.
#Are there any interesting characteristics? Have certain teams always had top payrolls over the
#years? Is there a connection between payroll and performance?
salary.info = dbGetQuery(db,'SELECT s.yearID, s.teamID, s.lgID, SUM(s.salary),
           t.DivID
           FROM Salaries AS s,
           Teams AS t
           WHERE s.teamID = t.teamID AND s.lgID =t.lgID AND s.yearID = t.yearID AND s.lgID = t.lgID
                         GROUP BY s.teamID, s.yearID;')


leagues = split(salary.info , salary.info$divID)
divisions = split(salary.info , salary.info$lgID)

a = rainbow(20)
#E
e.team.name = names(table(leagues$E[,2]))
plot(leagues$E[which(leagues$E[,2]==e.team.name[1]),1],
     leagues$E[which(leagues$E[,2]==e.team.name[1]),4],type = "l",ylim = c(1*10^6,2.5*10^8),xlim = c(1983,2013),col=a[1],
     main = 'payrolls for team in E leagues', xlab = 'year' , ylab = 'salary')
sapply(2:length(e.team.name), function(i) lines(leagues$E[which(leagues$E[,2]==e.team.name[i]),1],
                                              leagues$E[which(leagues$E[,2]==e.team.name[i]),4],type = "l",col=a[i]))
legend("topleft",legend = e.team.name,col = a[1:length(e.team.name)],lty = 1,cex=0.3)

c.team.name = names(table(leagues$C[,2]))
plot(leagues$C[which(leagues$C[,2]==c.team.name[1]),1],
     leagues$C[which(leagues$C[,2]==c.team.name[1]),4],type = "l",ylim = c(1*10^6,1.7*10^8),xlim = c(1994,2013),col=a[1],
     main = 'payrolls for team in C leagues', xlab = 'year' , ylab = 'salary')
sapply(2:length(c.team.name), function(i) lines(leagues$C[which(leagues$C[,2]==c.team.name[i]),1],
                                                leagues$C[which(leagues$C[,2]==c.team.name[i]),4],type = "l",col=a[i]))
legend("topleft",legend = c.team.name,col = a[1:length(c.team.name)],lty = 1,cex=0.3)

w.team.name = names(table(leagues$E[,2]))
plot(leagues$W[which(leagues$W[,2]==e.team.name[1]),1],
     leagues$W[which(leagues$W[,2]==e.team.name[1]),4],type = "l",ylim = c(1*10^6,2.5*10^8),xlim = c(1983,2013),col=a[1],
     main = 'payrolls for team in W leagues', xlab = 'year' , ylab = 'salary')
sapply(2:length(e.team.name), function(i) lines(leagues$W[which(leagues$E[,2]==w.team.name[i]),1],
                                                leagues$W[which(leagues$E[,2]==w.team.name[i]),4],type = "l",col=a[i]))
legend("topleft",legend = e.team.name,col = a[1:length(e.team.name)],lty = 1,cex=0.3)

interaction.plot(leagues$E[,1],leagues$E[,2],leagues$E[,4],type = "l",
                 col = c(1:length(table(leagues$E[,1]))),trace.label = "nameID",
                 main = 'payrolls for team in E leagues', xlab = 'year' , ylab = 'salary')
interaction.plot(leagues$C[,1],leagues$C[,2],leagues$C[,4],type = "l",
                 col = c(1:length(table(leagues$E[,1]))),trace.label = "nameID",
                 main = 'payrolls for team in F leagues', xlab = 'year' , ylab = 'salary')
interaction.plot(leagues$W[,1],leagues$W[,2],leagues$W[,4],type = "l",
                 col = c(1:length(table(leagues$E[,1]))),trace.label = "nameID",
                 main = 'payrolls for team in W leagues', xlab = 'year' , ylab = 'salary')

interaction.plot(divisions$AL[,1],divisions$AL[,2],divisions$AL[,4],type = "l",
                 col = c(1:length(table(divisions$AL[,1]))),trace.label = "nameID",
                 main = 'payrolls for team in AL division', xlab = 'year' , ylab = 'salary')
interaction.plot(divisions$NL[,1],divisions$NL[,2],divisions$NL[,4],type = "l",
                 col = c(1:length(table(divisions$NL[,1]))), trace.label = "nameID",
                 main = 'payrolls for team in NL division', xlab = 'year' , ylab = 'salary')

team.info = dbGetQuery(db,'SELECT s.yearID,s.teamID,SUM(s.salary) AS salary,
                         t.teamID,t.yearID, t.G,t.W,t.DivWin,t.WCWin,t.LgWin,t.WsWin 
                         FROM Salaries AS s,
                         Teams AS t
                         ON s.teamID = t.teamID
                          GROUP BY s.yearID, s.teamID;')


tt = split(team.info, team.info$yearID)
sapply(1:29, function(i) any(tt[[i]][order(-tt[[i]][,3]),][,8:11][1:3,]== 'Y'))

plot(team.info$salary,team.info$W/team.info$G,pch=19,cex=.5,
     main = 'relationship between wining rate and salary' , xlab = 'salary',
     ylab = 'win rate')
cor(team.info$salary,team.info$W/team.info$G)

#10. Has the distribution of home runs for players increased over the years?
#When answering the questions, try to summarize the results in convenient and informative form
#(e.g. tables and/or plots) that illustrate the key features.
aa = dbGetQuery(db,'SELECT * FROM Teams')

teams.HR =dbGetQuery(db,'SELECT teamID , yearID, SUM(HR) AS HR FROM Teams GROUP BY yearID, teamID')

teams.HR.year = split(teams.HR, teams.HR$teamID)

interaction.plot(teams.HR[,2],teams.HR[,1],teams.HR[,3],col=c(1:length(table(teams.HR[,1]))),
                 main = 'HR change', xlab = 'year', ylab = 'number of HR',trace.label = 'Teams HR')



hr.info = dbGetQuery(db,'SELECT playerID, yearID, HR FROM Batting order BY playerID;')
bat = split(batting.hr.info, batting.hr.info$playerID)
year.mean = sapply(1:length(bat), function(i) median(bat[[i]][[2]])) 
bat.hr = sapply(1:length(bat), function(i) sum(bat[[i]][[3]], na.rm =TRUE))

plot(year.mean, bat.hr, main = 'distribution of HR',xlab = 'year', ylab = 'hr')


