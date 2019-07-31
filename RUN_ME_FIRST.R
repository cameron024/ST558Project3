# Download the pbp-2018.csv dataset

# Read in the nfl data
nfl <- read.csv("path/to/your/directory/containing/pbp-2018.csv") 
nfl <- nfl %>% select(OffenseTeam, DefenseTeam, Down, ToGo, Yards, Formation, 
                        PlayType, PassType, IsIncomplete, RushDirection, IsFumble)
