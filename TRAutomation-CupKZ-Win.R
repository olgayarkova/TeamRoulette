## Set up R
setwd("~/R/TeamRoulette")

## MODE FUNCTION
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## DISTANCE FUNCTION
dist <- function(pool){
  dm <- data.frame(matrix(NA, nrow(pool), nrow(pool)))
  colnames(dm) <- pool$TeamID
  rownames(dm) <- pool$TeamID
  
  p <- nrow(pool)
  if (p>1){
    for (i in 1:(p-1)){
      for (j in (i+1):p){
        dm[i, j] <- abs(pool$Cases.Contests[i] - pool$Cases.Contests[j]) +
          abs(pool$Cases.Bestresult[i] - pool$Cases.Bestresult[j]) +
          abs(pool$Experience.Total[i] - pool$Experience.Total[j])
        
        if(pool$Team.Full[i] + pool$Team.Full[j] > 4) {dm[i, j] <- NA}
      }
    }
  }
  dm
}

## GLUE FUNCTION
glue <- function(pool, dm){
  m <- min(dm, na.rm = TRUE)
  indi <- which(dm == m, arr.ind = TRUE)[1, 1]
  indj <- which(dm == m, arr.ind = TRUE)[1, 2]
  
  pool$Members[indi] <- paste(pool$Members[indi], pool$Members[indj], sep = ', ')
  pool$Team.Full[indi] <- pool$Team.Full[indi] + pool$Team.Full[indj]
  pool$Team.Full[indj] <- 0
  pool$Cases.Contests[indj] <- NA
  pool$Cases.Bestresult[indj] <- NA
  
  pool
}


## Import data
data <- read.csv('data.csv', encoding = 'UTF-8', stringsAsFactors = FALSE)
data.vars <- data.frame(oldvars = colnames(data), stringsAsFactors = FALSE)
data.vars$newvars <-data.vars$oldvars

## Correct variables names
source('CommonFields.R')
vars <- CommonFields(data.vars)
colnames(data) <- vars$newvars

## Convert dataset into long form
data.member1 <- data[, c(1:61)]
data.member2 <- data[, c(1:29, 62:93)]
data.member3 <- data[, c(1:29, 94:125)]
data.member4 <- data[, c(1:29, 126:157)]

membervars <- data.frame(oldvars = colnames(data.member1), stringsAsFactors = FALSE)
membervars <- CommonFields(membervars)
colnames(data.member1) <- membervars$newvars
colnames(data.member2) <- membervars$newvars
colnames(data.member3) <- membervars$newvars
colnames(data.member4) <- membervars$newvars

data.long <- rbind(data.member1, data.member2, data.member3, data.member4)
data.tidy <- data.long[data.long$Surname != '', ]
data.ordered <- data.tidy[order(data.tidy$Response.ID),]
write.csv(data.ordered, 'ordered.csv')

## Identifing teams
data.ordered$TeamID <- data.ordered$Response.ID
data.ordered$MemberID <- as.integer(rownames(data.ordered))

key <- cbind(data.ordered$Email1, data.ordered$Email2, data.ordered$Email3, data.ordered$Email4)

for (i in 1:nrow(key)){
  key[i,] <- key[i,][order(key[i, ])]
  data.ordered$TeamKey[i] <- paste(key[i,], collapse='-')
}

teamkeys <- unique(data.ordered$TeamKey)

for (i in 1:length(teamkeys)){
  key <- teamkeys[i]
  data.ordered$TeamID[data.ordered$TeamKey == teamkeys[i]] <- i
}

teamlist <- data.frame(table(data.ordered$TeamID))
colnames(teamlist) <- c('ID', 'Members.Registered')
teamlist$ID <- as.integer(teamlist$ID)


for (i in teamlist$ID){
  data.ordered$Members.Registered[data.ordered$TeamID == i] <- teamlist$Members.Registered[i]
}

## Identifing Team Roulette needs
data.ordered$TR.Needed <- FALSE
data.ordered$TR.Needed <- data.ordered$Team.Full != 'все 4 участника'

data.tr <- data.ordered[data.ordered$TR.Needed, ]
data.ft <- data.ordered[data.ordered$TR.Needed == FALSE, ]

## Getting work database ready
wdb <- data.frame(TeamID = data.tr$TeamID, stringsAsFactors = FALSE)
wdb$Team.Section <- factor(data.tr$Team.Section,
                              levels = c('Русская', 'Английская'))
wdb$Team.Full <- factor(data.tr$Team.Full, levels = c('я один', '2 участника', '3 участника'))
wdb$MemberID <- data.tr$MemberID
wdb$Homecity <- as.character(data.tr$Homecity)
wdb$Institution <- as.character(data.tr$Uni)
wdb$Institution[wdb$Institution == 'ВУЗа нет в списке'] <- as.character(data.tr$Uni.Custom[data.tr$Uni == 'ВУЗа нет в списке'])
# wdb$Institution[wdb$Institution == ''] <- as.character(data.tr$School[data.tr$Uni == ''])
wdb$Cases.Contests <- factor(data.tr$Cases.Contests,
                             levels = c('', 'Не участвовал', '1 раз', '2-3 раза', '4-5 раз', 'Больше 5 раз'))
wdb$Cases.Bestresult <- factor(data.tr$Cases.Bestresult,
                               levels = c('', 'Участник', 'High Quality 25%', 'High Quality 15%', 'Полуфиналист',
                                          'Финалист', 'Призер (2-3 место)', 'Победитель (1 место)'))
wdb$Experience.Total <- factor(data.tr$Experience.Total, levels = c('', 'нет', 'меньше полугода', 'меньше года',
                                                                    'от года до двух', 'два-три года',
                                                                    'три-пять лет', 'пять лет и больше'))

## TeamRoulette
teams <- unique(data.frame(cbind(TeamID = wdb$TeamID,
                                 Team.Full = wdb$Team.Full,
                                 Team.Section = wdb$Team.Section)))
for (i in teams$TeamID){
  teams$Homecity[teams$TeamID == i] <- getmode(wdb$Homecity[wdb$TeamID == i])
  teams$Institution[teams$TeamID == i] <- getmode(wdb$Institution[wdb$TeamID == i])
  teams$Cases.Contests[teams$TeamID == i] <- mean(as.integer(wdb$Cases.Contests[wdb$TeamID == i]))
  teams$Cases.Bestresult[teams$TeamID == i] <- mean(as.integer(wdb$Cases.Bestresult[wdb$TeamID == i]))
  teams$Experience.Total[teams$TeamID == i] <- mean(as.integer(wdb$Experience.Total[wdb$TeamID == i]))
  
  members <- wdb$MemberID[data.tr$TeamID == i]
  teams$Members[teams$TeamID == i] <- paste(members, collapse = ', ')
}

teams <- teams[order(teams$Team.Full, decreasing = TRUE),]

sections <- unique(teams$Team.Section)
cities <- unique(teams$Homecity)
institutions <- unique(teams$Institution)

## Optimal distribution: Educational institution and Homesity match
for (s in sections){
  for (c in 1:length(cities)){
    for (i in 1:length(institutions)){
      pool <- subset(teams, (Team.Section == s)&
                       (Homecity == cities[c])&
                       (Institution == institutions[i]))
      if (nrow(pool) > 1){
        n <- nrow(pool)*2
        for (j in 1:n){
          dm <- dist(pool)
          if (sum(!is.na(dm)) != 0){
            pool <- glue(pool, dm)
          } else {
              j <- n*2
          }
        }

        for (m in pool$TeamID){
          teams$MembersNew[teams$TeamID == m] <- pool$Members[pool$TeamID == m]
          teams$Team.Full[teams$TeamID == m] <- pool$Team.Full[pool$TeamID == m]
          teams$Cases.Contests[teams$TeamID == m] <- pool$Cases.Contests[pool$TeamID == m]
          teams$Cases.Bestresult[teams$TeamID == m] <- pool$Cases.Bestresult[pool$TeamID == m]
          teams$MembersNew[teams$TeamID == m] <- pool$Members[pool$TeamID == m]
        }
      }
    }
  }
}

teams <- teams[teams$Team.Full > 0,]
teams$MembersNew[is.na(teams$MembersNew)] <- teams$Members[is.na(teams$MembersNew)]
teams$MembersOriginal <- teams$Members
teams$Members <- teams$MembersNew

## Less quality: Homecity match
for (s in sections){
  for (c in 1:length(cities)){
    
    pool <- subset(teams, (Team.Section == s)&
                 (Homecity == cities[c]))
    if (nrow(pool) > 1){
      n <- nrow(pool)*2
      for (j in 1:n){
        dm <- dist(pool)
        if (sum(!is.na(dm)) != 0){
          pool <- glue(pool, dm)
          } else {
            j <- n*2
          }
        }
      for (m in pool$TeamID){
        teams$Team.Full[teams$TeamID == m] <- pool$Team.Full[pool$TeamID == m]
        teams$Cases.Contests[teams$TeamID == m] <- pool$Cases.Contests[pool$TeamID == m]
        teams$Cases.Bestresult[teams$TeamID == m] <- pool$Cases.Bestresult[pool$TeamID == m]
        teams$MembersNew[teams$TeamID == m] <- pool$Members[pool$TeamID == m]
      }
    }
  }
}

teams <- teams[teams$Team.Full > 0,]
teams$MembersNew[is.na(teams$MembersNew)] <- teams$Members[is.na(teams$MembersNew)]
teams$Members <- teams$MembersNew

## Rewriting members' teams
memberslist <- strsplit(teams$Members, split =", ")
newID <- data.frame(TeamID = rep(teams$TeamID, sapply(memberslist, length)),
                    Team.Full = rep(teams$Team.Full, sapply(memberslist, length)),
                    MemberID = as.integer(unlist(memberslist)))
for (i in newID$MemberID){
  data.tr$TeamID.New[data.tr$MemberID == i] <- newID$TeamID[newID$MemberID == i]
}

data.ft$TeamID.New <- data.ft$TeamID
data.ready <- rbind(data.ft, data.tr)
write.csv(data.ready, 'rouletted.csv')

