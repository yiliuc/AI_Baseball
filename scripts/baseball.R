## -----------------------------------------------------------------------------------------------
# Load the packages
library(tidyverse)
library(ggplot2)
library(here)
library(car)
library(plyr)
library(markovchain)
library(diagram)

# Update the header of the data (Optional if we already save the new data)
# source("scripts/update_data.R")

## -----------------------------------------------------------------------------------------------
# Load the data
batter <- read.csv(here("inputs/data/Batter_Data/2023_Guerrerojr.csv"))
pitcher <- read.csv(here("inputs/data/Pitcher_Data/2023_Gausman.csv"))
## Load the updated data file from update_data.R
data2023 <- read.csv(here("outputs/all2023_updated.csv"))

## -----------------------------------------------------------------------------------------------
# Create the unique identification
data2023$HALF.INNING <- with(data2023,
          paste(GAME_ID, INN_CT, BAT_HOME_ID))

# Compute the number of scores in each game
data2023$RUNS.SCORED <- with(data2023, (BAT_DEST_ID > 3) +
  (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))

# Define the new state which combines the runners at each base and number of outs
get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)
}

## -----------------------------------------------------------------------------------------------
# STATE refers to the beginning of each play
RUNNER1 <- ifelse(as.character(data2023[,"BASE1_RUN_ID"])=="", 0, 1)
RUNNER2 <- ifelse(as.character(data2023[,"BASE2_RUN_ID"])=="", 0, 1)
RUNNER3 <- ifelse(as.character(data2023[,"BASE3_RUN_ID"])=="", 0, 1)
data2023$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3,
                           data2023$OUTS_CT)

# NEW.STATE refers to the conclusion of each play.
NRUNNER1 <- with(data2023, as.numeric(RUN1_DEST_ID==1 |
                          BAT_DEST_ID==1))
NRUNNER2 <- with(data2023, as.numeric(RUN1_DEST_ID==2 |
                 RUN2_DEST_ID==2 | BAT_DEST_ID==2))
NRUNNER3 <- with(data2023, as.numeric(RUN1_DEST_ID==3 |
                 RUN2_DEST_ID==3 | RUN3_DEST_ID==3 | BAT_DEST_ID==3))
NOUTS <- with(data2023, OUTS_CT + EVENT_OUTS_CT)
data2023$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)

## -----------------------------------------------------------------------------------------------
# Filter the data which had scored
data2023 <- subset(data2023, (STATE != NEW.STATE) | (RUNS.SCORED > 0))

data.outs <- ddply(data2023, .(HALF.INNING), summarize,
                  Outs.Inning=sum(EVENT_OUTS_CT))
data2023 <- merge(data2023, data.outs)
data2023C <- subset(data2023, Outs.Inning == 3)
# We only consider the game where has a batting event
data2023C <- subset(data2023, BAT_EVENT_FL == TRUE)

data2023C$NEW.STATE <- recode(data2023C$NEW.STATE,
              "c('000 3', '100 3', '010 3', '001 3',
                 '110 3', '101 3', '011 3', '111 3')='3'")


## -----------------------------------------------------------------------------------------------
# Calculate the transition matrix
T.matrix <- with(data2023C, table(STATE, NEW.STATE))
P.matrix <- prop.table(T.matrix, 1)
# The transition matrix should be 24*25. Here we have 25*25 as we manually create a new row
P.matrix <- rbind(P.matrix, c(rep(0, 24), 1))

## -----------------------------------------------------------------------------------------------
# Visualize the transition matrix
mc <- new("markovchain", transitionMatrix = P.matrix)
plot(mc) 
# I am trying to adjust the format of the graph, like positions of the 
# nodes and size of arrows etc.

## -----------------------------------------------------------------------------------------------
# Simulations
count.runners.outs <- function(s){
  sum(as.numeric(strsplit(s,"")[[1]]), na.rm=TRUE)
  }

runners.outs <- sapply(dimnames(T.matrix)[[1]], count.runners.outs)[-25]
R <- outer(runners.outs + 1, runners.outs, FUN="-")
dimnames(R)[[1]] <- dimnames(T.matrix)[[1]][-25]
dimnames(R)[[2]] <- dimnames(T.matrix)[[1]][-25]
R <- cbind(R, rep(0, 24))

simulate.half.inning <- function(P, R, start=1){
   s <- start; path <- NULL; runs <- 0
   while(s < 25){
     s.new <- sample(1:25, 1, prob=P[s, ])
     path <- c(path, s.new)
     runs <- runs + R[s, s.new]
     s <- s.new
}
runs }

RUNS <- replicate(10000, simulate.half.inning(T.matrix, R))