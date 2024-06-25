#####################################
#         Baseball Analytics        #
#         Retrosheet Parser         #
#         Morris Greenberg          #
#                                   #
#####################################


#Modified version of https://gist.github.com/bayesball/8892981
#Dependencies: Chadwick cwevent command line function
#              (http://chadwick.sourceforge.net/doc/index.html)


download.retrosheet <- function(season, download.folder){
  # get zip file from retrosheet website
  if(!dir.exists(download.folder)){
    dir.create(download.folder)
  }
  if(!dir.exists(paste(download.folder, "zipped", sep="/"))){
    dir.create(paste(download.folder, "zipped", sep="/"))
  }
  
  download.file(
    url=paste("http://www.retrosheet.org/events/", season, "eve.zip", sep="")
    , destfile=paste(download.folder, "/zipped/", season, "eve.zip", sep="")
  )
}

unzip.retrosheet <- function(season, download.folder){
  #unzip retrosheet files
  if(!dir.exists(paste(download.folder, "unzipped", sep="/"))){
    dir.create(paste(download.folder, "unzipped", sep="/"))
  }
  unzip(paste(download.folder, "/zipped/", season, "eve.zip", sep=""), 
        exdir=paste(download.folder, "/unzipped", sep=""))
}

create.csv.file=function(year, download.folder){
  # http://chadwick.sourceforge.net/doc/cwevent.html#cwtools-cwevent
  # shell("cwevent -y 2000 2000TOR.EVA > 2000TOR.bev")
  wd = getwd()
  setwd(paste(download.folder, "/unzipped", sep=""))
  if (.Platform$OS.type == "unix"){
    system(paste(paste("cwevent -y", year, "-f 0-96"), 
                 paste(year,"*.EV*",sep=""),
                 paste("> all", year, ".csv", sep="")))} else {
                   shell(paste(paste("cwevent -y", year, "-f 0-96"), 
                               paste(year,"*.EV*",sep=""),
                               paste("> all", year, ".csv", sep="")))              
                 }
  setwd(wd)
}

create.csv.roster = function(year, download.folder){
  # creates a csv file of the rosters
  filenames <- list.files(path = paste(download.folder, "/unzipped/", sep=""))
  filenames.roster = 
    subset(filenames, substr(filenames, 4, 11)==paste(year,".ROS",sep=""))
  read.csv2 = function(file)
    read.csv(paste(download.folder, "/unzipped/", file, sep=""),header=FALSE)
  R = do.call("rbind", lapply(filenames.roster, read.csv2))
  names(R)[1:6] = c("Player.ID", "Last.Name", "First.Name", 
                    "Bats", "Pitches", "Team")
  wd = getwd()
  setwd(paste(download.folder, "/unzipped", sep=""))
  write.csv(R, file=paste("roster", year, ".csv", sep=""))
  setwd(wd)
}

cleanup = function(download.folder){
  # removes retrosheet files not needed
  wd = getwd()
  setwd(paste(download.folder, "/unzipped", sep=""))
  if (.Platform$OS.type == "unix"){
    system("rm *.EVN")
    system("rm *.EVA")
    system("rm *.ROS")
    system("rm TEAM*")} else {
      shell("del *.EVN")
      shell("del *.EVA")
      shell("del *.ROS")
      shell("del TEAM*")
    }       
  setwd(wd)
  setwd(paste(download.folder, "/unzipped", sep=""))
  if (.Platform$OS.type == "unix"){
    system("rm *.zip")} else {
      shell("del *.zip")
    }
  setwd(wd)
}

parse.retrosheet2.pbp = function(season, download.folder){
  download.retrosheet(season, download.folder)
  unzip.retrosheet(season, download.folder)
  create.csv.file(season, download.folder)
  create.csv.roster(season, download.folder)
  cleanup(download.folder)
}

parse.retrosheet2.pbp(2023, "C:/Users/Morris/My Drive/_Desktop/Baseball R Blog/_Retrosheet_Events2")