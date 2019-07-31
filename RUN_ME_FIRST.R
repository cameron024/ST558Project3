# Read in the nfl data
nfl <- read.csv("/Users/Cameron_E/ShinyApps/Project 3/pbp-2018.csv") 
nfl <- nfl %>% select(OffenseTeam, DefenseTeam, Down, ToGo, Yards, Formation, 
                        PlayType, PassType, IsIncomplete, RushDirection, IsFumble)
