# Vahn Kessler
# Data Visualization Final
# 
# In February of this year, my roommate insisted that I download the chess.com app
# and play him in chess. And ever since, I've been hooked. My problem is that I'm
# terrible at chess. And it's largely because I've refused to learn any chess
# theory or strategies. Why? Because I'm stubborn. I'm no prodigy—not even close—
# but I figured if eight-year-olds can make it to world chess championships, my
# college-student brain and crippling chess.com app addiction should be just
# enough to improve somewhat over a few months.
# 
# I was wrong. It is months later and I am still unbelievably bad at chess.
# 
# However, my ego-fueled and naïve approach to playing chess has one benefit: I'm
# the perfect control group! Who else would play chess for hours a week and months
# on end without attempting to learn anything? I am as far removed as one can be
# from the best chess players in the world while still having a comprehensive
# understanding of the game.
# 
# In this project, I attempt to analyze my games through several different
# factors such as move count, board position, and elo. These factors will be
# explained in greater depth throughout the visualizations. I also analyze
# every game played in three different time classes: bullet, blitz, and rapid.
# A time class indicates the amount of time allotted to each player in a game.
# The time for each respective player counts down when it is their turn, and
# pauses when it is their opponent's turn. Bullet games allot 1-2 minutes for
# each player, blitz games allot 3-5 minutes for each player, and rapid games
# allot 5-15 minutes for each player. This results in 4 minute, 10 minute, and
# 30 minute games. The time class has a significant impact on one's gameplay.
# Less time on the clock means less time to think of potential moves and how
# the opponent will respond, and more opportunities for mistakes and irregular
# outcomes. Because move accuracy can be so different in the different time
# classes, it is both necessary and entertaining to compare games based on
# their time class. It is worth noting that the majority of my games played
# on chess.com are blitz games, so the data of my blitz games is the least
# susceptible to outliers.
# 
# A crucial part of my analysis is how my games compares to games played by
# Hikaru Nakamura, an American chess grandmaster and world champion in several
# tournaments. One of the most striking aspects of his gameplay is how accurate
# he is. In chess, accuracy refers to how closely a player compares to a chess
# engine, a machine-learning program that determines the 'best' move options.
# Hikaru averages 80% to 90% accuracy in his games, with a significant portion
# in the 95%-99% range. This is staggering. It means that he is choosing almost
# exclusively perfect moves in every game. Hikaru's gameplay is as close to a
# computer as possible. Thus, by comparing my games with Hikaru's, I am comparing
# two extremes. How does untrained, naïve gameplay compare to some of the most
# statistically accurate gameplay in the world? Just how bad am I? In these past
# three months, have I improved at all? We'll see what my analysis yields.

require(chessR)
require(ggplot2)
require(dplyr)
require(reshape2)
require(plotly)
require(maps)
require(lubridate)
require(chess)
require(rchess)
require(shiny)
require(chess.com)
require(bigchess)
require(tidyr)

# Some datasets.

chessV <- get_raw_chessdotcom("imvahn")
chessH <- get_raw_chessdotcom(usernames = "Hikaru", year_month = c(202302:202304))
mapdata <- map_data("world")
ex1 <- Chess$new("rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3")
ex2 <- Chess$new("k2r2r1/Q1p3bp/2N2R2/1p1p2pP/1P1P4/P1PBP3/6b1/RN2K3 b Q - 0 26")
ex3 <- Chess$new("3k4/3Q4/2K5/8/8/8/8/8 b - - 14 74")
ex4 <- Chess$new("3k4/8/2K1Q3/8/8/8/8/8 b - - 14 74")

# These first visualizations focus on the number of moves in games played by
# myself and Hikaru. There is a lot of information that can be revealed by the
# number of moves in a game. Every chess game has three stages: an opening, a
# midgame, and an endgame. The opening can last up to ~20 moves, but as few as
# 10. The midgame doesn't really have a set move amount because the endgame begins
# once most pieces have been removed from the board, which can happen after
# ~30 moves, ~60 moves, or even after ~120 moves. Keeping this in mind, the number
# of moves in a game tells a story of how the game plays out. A game with a short
# number of moves means that it ended quickly and decisively; this could be from a
# gap appearing in a player's defenses that was taken advantage of. Run the below
# line:

plot(ex1)

# With white's defense opened up, black's queen could puncture white's defenses
# and win the game.
# 
# The following example is one of a game that ended in 26 moves, soon into
# the midgame. By the midgame, the board could be in one of nearly infinite
# positions, so games that end in the midgame are often the result of a carefully
# prepared attack. Run the below line:

plot(ex2)

# This involved some careful preparation. White pushed their knight up to c6.
# This allowed white's queen to move to a7, checkmating the king. White's knight
# protected the queen, preventing black from stopping the checkmate.
# 
# The following example is one of a game that ended after 74 moves. By this time,
# nearly every piece had been removed from the board. Run the below line:

plot(ex3)

# The number of moves in a game can indicate substantially different outcomes.
# When accounting for time classes, especially games that only give each player
# 1-2 total minutes, many games can end simply because a player runs out of time,
# not because they got checkmated.
# 
# The first visualization looks at the distribution of moves in games that I won
# and in games that Hikaru won. The distribution accounts for the color of
# Hikaru's and my pieces. Because white goes first, the player who plays white
# always has a slight advantage. This becomes evident in the visualization.
# 
# Some data manipulation. Run the following lines:

chessV$nMoves<-return_num_moves(moves_string=chessV$Moves)
chessV%>%
  filter(Event=="Live Chess")->chessV
chessV<-chessV[!is.na(chessV$ECO),]
chessV$Winner<-NA
chessV$Winner[chessV$White == "imvahn" & chessV$Result == "1-0"]<-"White"
chessV$Winner[chessV$Black == "imvahn" & chessV$Result == "0-1"]<-"Black"

chessH$nMoves<-return_num_moves(moves_string=chessH$Moves)
chessH%>%
  filter(Event=="Live Chess")->chessH
chessH<-chessH[!is.na(chessH$ECO),]
chessH$Winner<-NA
chessH$Winner[chessH$White == "Hikaru" & chessH$Result == "1-0"]<-"White"
chessH$Winner[chessH$Black == "Hikaru" & chessH$Result == "0-1"]<-"Black"

# Now it's time for the shiny app. Run the following lines:

ui <- fluidPage(
  titlePanel("Density of the Number of Moves Per Game in Won Games"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId="gamemode",
        label="Choose a Time Class",
        choices=c("bullet","blitz","rapid"),
        selected="blitz")),
    mainPanel(
      plotOutput("Vahn"),
      plotOutput("Hikaru"))))

server<-function(input,output){
  output$Vahn<-renderPlot({
    ggplot(chessV%>%
             filter(TimeClass==input$gamemode)%>%
             filter(Winner=="White" | Winner=="Black"))+
      geom_density(aes(x=nMoves,fill=Winner),color="black",alpha=.5)+
      labs(title="Vahn",
           x="Number of Moves",
           y="Density",
           fill="Piece Color",
           color="Piece Color")+
      scale_fill_manual(values=c("black","white"))+
      xlim(c(0,150))})
  output$Hikaru<-renderPlot({
    ggplot(chessH%>%
             filter(TimeClass==input$gamemode)%>%
             filter(Winner=="White" | Winner=="Black"))+
      geom_density(aes(x=nMoves,fill=Winner),color="black",alpha=.5)+
      labs(title="Hikaru",
           x="Number of Moves",
           y="Density",
           fill="Piece Color",
           color="Piece Color")+
      scale_fill_manual(values=c("black","white"))+
      xlim(c(0,150))})}

shinyApp(ui = ui, server = server)

# Across all the time classes, I won more games in fewer moves as white than as
# black. This makes sense because of white's first-move advantage. As a chess
# game continues, the advantage from starting as white becomes less and less
# impactful. This statement is supported through looking at my bullet games.
# Because games are so quick, white's advantage is maximized. It makes sense
# that the majority of my bullet games as white are won in a very small number
# of moves. Interestingly, my bullet move distribution is tri-modal for both
# black and white. The high density of wins at ~50 and ~70 moves in bullet
# tells its own story: by the time the game has reached that many moves, players
# have only a few seconds left on their clock. This means that wins are determined
# by who can stall longer, forcing their opponent out of time. In bullet games,
# white's wins are determined by how effectively they can attack, whereas black's
# wins are determined by how effectively they defend.
# 
# Hikaru's data is less irregular than mine. The distributions of the number of
# moves in Hikaru's games are pretty consistent between colors. In general, his
# games are won regardless of starting color. This makes sense, because his
# gameplay is essentially perfect. When you're making the most accurate moves,
# starting advantages don't have much of an impact. That's part of the beauty of
# chess: handicaps imposed by the mechanics of the game can be overcome by pure
# skill. This also explains why the distributions of the number of moves are
# more similar in longer time classes. With more time to think, one has more time
# to come up with the best move. And, as Hikaru demonstrates, starting advantages
# can be overcome with accurate gameplay.
#
# Why are my distributions so much less consistent and so much funkier-looking
# than Hikaru's? Well, put simply, I'm much much much much much much worse.
# My games are determined more by luck than skill. It makes sense that my
# distributions are irregular, because my gameplay is irregular!
# 
# The next visualization looks at game outcomes (win, loss, or draw) to find out
# how the number of moves in a game changes depending on how the game ends.
# 
# Some more data manipulation. Run the following lines:

chessV$Win<-"Loss"
chessV$Win[chessV$Winner == "White" | chessV$Winner == "Black"]<-"Win"
chessV$Win[chessV$Result == "1/2-1/2"]<-"Draw"

chessH$Win<-"Loss"
chessH$Win[chessH$Winner == "White" | chessH$Winner == "Black"]<-"Win"
chessH$Win[chessH$Result == "1/2-1/2"]<-"Draw"

# Now it's time for the shiny app. Run the following lines:

ui <- fluidPage(
  titlePanel("Density of the Number of Moves Per Game Based on Game Outcome"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId="gamemode",
        label="Choose a Time Class",
        choices=c("bullet","blitz","rapid"),
        selected="blitz")),
    mainPanel(
      plotOutput("Vahn2"),
      plotOutput("Hikaru2"))))

server<-function(input,output){
  output$Vahn2<-renderPlot({
    ggplot(chessV%>%
             filter(TimeClass==input$gamemode))+
      geom_density(aes(x=nMoves, fill=Win),color="black",alpha=.3)+
      labs(title="Vahn",
           x="Number of Moves",
           y="Density",
           fill="Game Outcome")+
      scale_fill_manual(values=c("red","green","blue"))+
      xlim(c(0,150))})
  output$Hikaru2<-renderPlot({
    ggplot(chessH%>%
             filter(TimeClass==input$gamemode))+
      geom_density(aes(x=nMoves, fill=Win),color="black",alpha=.3)+
      labs(title="Hikaru",
           x="Number of Moves",
           y="Density",
           fill="Game Outcome")+
      scale_fill_manual(values=c("red","green","blue"))+
      xlim(c(0,150))})}

shinyApp(ui=ui, server=server)

# These distributions are very interesting. First, it's worth noting that games
# ending in a draw have a much lower density than those ending in a win or a loss.
# This makes sense, because most games are won or lost, not drawn.
# 
# The distributions of the number of moves in my blitz games point out a flaw in my
# capabilities as a player. Most of my drawn games happened at ~75 moves, while
# my won and lost games happened at ~20. This indicates that I struggle heavily
# in endgame positions. In the endgame, when there are fewer pieces on the board,
# it is much easier to draw a game. There are multiple reasons why this is the
# case:
# 1. The fifty move rule. This rule states that "a player can claim a draw if no
# capture has been made and no pawn has been moved in the last fifty moves."
# During the endgame, it is entirely possible that there are no pawns left on the
# board, eliminating the second clause of the rule. It is also easier to dodge
# capture, because there is more space on the board to move. Without any pieces in
# the way, players can run across the board, dodging attacks. It is harder to get
# pinned or trapped in one spot and captured. Because I am not good in endgame
# positions, I often struggle to effectively capture pieces before fifty moves
# have passed.
# 2. Stalemate. A stalemate occurs when a player cannot move any more pieces.
# Run the below line for an example:

plot(ex4)

# In this example, black cannot move anywhere. Stalemate.
# 3. Time. In bullet and blitz games, time is a crucial factor. By the time
# players have reached the endgame, there are seconds left on the clock. This
# leaves more opportunities for mistakes. When I am low on time, my thoughts
# change from "how do I win" to "how do I not run out of time first." This
# gives my opponent an opportunity to stall with the fifty move rule. It
# also means that I'm putting less thought into my moves. Less thought means
# more mistakes, such as an accidental stalemate or a failure to capture a piece
# in fifty moves.
# 
# Hikaru's draws, by comparison, happen at around the same number of moves as his
# wins and losses. This is because he is much better than me. Chess grandmasters
# operate on a different plane of existence: they have played so many games and
# have had so much practice that they can predict tens of moves in advance. Thus,
# time is not nearly as much of an issue for Hikaru as it is for me. However,
# Hikaru's blitz distribution is slightly bimodal, with the second hump at ~125
# moves. Chess.com matches players by their skill level. In Hikaru's case, he
# is playing individuals who are experts at stalling and dodging his attacks.
# Thus, he can fall victim to the fifty move rule or to a stalemate, or, in rare
# cases, by running out of time. These occurences are rare, though, which is why
# the second hump is barely visible. Hikaru's bullet distribution is significantly
# bimodal. The first hump lines up with his wins and losses, but the second one
# occurs at ~75 moves. Because bullet games are half the length as blitz games,
# time is a much more consequential factor—there are more opportunities to mess up.
# 
# The distribution of the number of moves in my won bullet games is trimodal,
# which, although weird, makes sense given the context of the first visualization.
# The distribution of the number of moves in my rapid games is ridiculous. This
# is because I have only played 32 rapid games, with 3 draws. Thus, the 3 draws
# are very evident.
#
# The next visualization maps the amount of times pieces were moved to certain
# positions on the board. It's essentially a 'heat map' of where pieces are placed
# most often. This required a gigantic amount of data manipulation, to the point
# where it literally breaks the shiny app. This means that you have a little bit
# of work to do. (I've also included a screenshot of every plot in the pdf
# submitted with this file so you don't actually have to do any work. But if
# you wanted to, you could.)
#
# If you want to compare BULLET games where Hikaru and I play as WHITE, run the
# following lines:

chessV%>%
  filter(TimeClass=="bullet")%>%
  filter(White=="imvahn")%>%
  select(Moves)->countplot1
chessH%>%
  filter(TimeClass=="bullet")%>%
  filter(White=="Hikaru")%>%
  select(Moves)->countplot2

# If you want to compare BULLET games where Hikaru and I play as BLACK, run the
# following lines:

chessV%>%
  filter(TimeClass=="bullet")%>%
  filter(Black=="imvahn")%>%
  select(Moves)->countplot1
chessH%>%
  filter(TimeClass=="bullet")%>%
  filter(Black=="Hikaru")%>%
  select(Moves)->countplot2

# If you want to compare BLITZ games where Hikaru and I play as WHITE, run the
# following lines:

chessV%>%
  filter(TimeClass=="blitz")%>%
  filter(White=="imvahn")%>%
  select(Moves)->countplot1
chessH%>%
  filter(TimeClass=="blitz")%>%
  filter(White=="Hikaru")%>%
  select(Moves)->countplot2

# If you want to compare BLITZ games where Hikaru and I play as BLACK, run the
# following lines:

chessV%>%
  filter(TimeClass=="blitz")%>%
  filter(Black=="imvahn")%>%
  select(Moves)->countplot1
chessH%>%
  filter(TimeClass=="blitz")%>%
  filter(Black=="Hikaru")%>%
  select(Moves)->countplot2

# If you want to compare RAPID games where Hikaru and I play as WHITE, run the
# following lines:

chessV%>%
  filter(TimeClass=="rapid")%>%
  filter(White=="imvahn")%>%
  select(Moves)->countplot1
chessH%>%
  filter(TimeClass=="rapid")%>%
  filter(White=="Hikaru")%>%
  select(Moves)->countplot2

# If you want to compare RAPID games where Hikaru and I play as BLACK, run the
# following lines:

chessV%>%
  filter(TimeClass=="rapid")%>%
  filter(Black=="imvahn")%>%
  select(Moves)->countplot1
chessH%>%
  filter(TimeClass=="rapid")%>%
  filter(Black=="Hikaru")%>%
  select(Moves)->countplot2

# Some data manipulation. This may take a while. YOU MUST RUN THE FOLLOWING
# LINES AFTER EVERY TIME YOU RUN ANY OF THE ABOVE LINES:

counts<-data.frame(letter=c(rep("a",8),rep("b",8),rep("c",8),rep("d",8),rep("e",8),rep("f",8),rep("g",8),rep("h",8)),
                   number=c(rep(c(1,2,3,4,5,6,7,8),8)),
                   count=rep(0,64))

for (j in 1:nrow(countplot1)){
  Moves_updated<-strsplit(countplot1$Moves, " ")[[j]]
  Moves_updated<-Moves_updated[grepl("^[A-Za-z][0-9a-zA-Z]+$",Moves_updated)]
  Moves_updated<-ifelse(nchar(Moves_updated)==3,substr(Moves_updated,2,3),Moves_updated)
  Moves_updated<-ifelse(nchar(Moves_updated)==4,substr(Moves_updated,3,4),Moves_updated)
  Moves_updated<-ifelse(nchar(Moves_updated)==5,substr(Moves_updated,4,5),Moves_updated)
  df<-data.frame(Moves_updated)
  for(i in 1:nrow(df)){
    df$letter[i]<-strsplit(df$Moves_updated,"")[[i]][1]
    df$number[i]<-strsplit(df$Moves_updated,"")[[i]][2]}
  df%>%
    mutate(number=as.numeric(number))%>%
    group_by(letter,number)%>%
    count(letter)->df
  plot<-left_join(counts,df,by=c("letter","number"))
  plot$n[is.na(plot$n)]<-0
  for(i in 1:nrow(plot)){
    counts$count[i]<-sum(counts$count[i],plot$n[i])}}

counts2<-data.frame(letter=c(rep("a",8),rep("b",8),rep("c",8),rep("d",8),rep("e",8),rep("f",8),rep("g",8),rep("h",8)),
                    number=c(rep(c(1,2,3,4,5,6,7,8),8)),
                    count=rep(0,64))

for (j in 1:nrow(countplot2)){
  Moves_updated2<-strsplit(countplot2$Moves, " ")[[j]]
  Moves_updated2<-Moves_updated2[grepl("^[A-Za-z][0-9a-zA-Z]+$",Moves_updated2)]
  Moves_updated2<-ifelse(nchar(Moves_updated2)==3,substr(Moves_updated2,2,3),Moves_updated2)
  Moves_updated2<-ifelse(nchar(Moves_updated2)==4,substr(Moves_updated2,3,4),Moves_updated2)
  Moves_updated2<-ifelse(nchar(Moves_updated2)==5,substr(Moves_updated2,4,5),Moves_updated2)
  df2<-data.frame(Moves_updated2)
  for(i in 1:nrow(df2)){
    df2$letter[i]<-strsplit(df2$Moves_updated2,"")[[i]][1]
    df2$number[i]<-strsplit(df2$Moves_updated2,"")[[i]][2]}
  df2%>%
    mutate(number=as.numeric(number))%>%
    group_by(letter,number)%>%
    count(letter)->df2
  plot2<-left_join(counts2,df2,by=c("letter","number"))
  plot2$n[is.na(plot2$n)]<-0
  for(i in 1:nrow(plot2)){
    counts2$count[i]<-sum(counts2$count[i],plot2$n[i])}}

# Now it's time for the shiny app. Run the following lines:

ui <- fluidPage(
  titlePanel("Chess Piece Position Heatmap"),
  mainPanel(
    plotOutput("Vahn4"),
    plotOutput("Hikaru4")))

server<-function(input,output){
  output$Vahn4<-renderPlot({
    ggplot(counts)+
      geom_tile(aes(x=letter,y=number,fill=count))+
      labs(title="Vahn",
           fill="Count")+
      scale_fill_distiller(palette="Greens",direction=1)+
      theme(axis.title.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.background=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())+
      scale_y_continuous(labels = as.character(counts$number), breaks = counts$number)})
  output$Hikaru4<-renderPlot({
    ggplot(counts2)+
      geom_tile(aes(x=letter,y=number,fill=count))+
      labs(title="Hikaru",
           fill="Count")+
      scale_fill_distiller(palette="Greens",direction=1)+
      theme(axis.title.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.background=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())+
      scale_y_continuous(labels = as.character(counts2$number), breaks = counts2$number)})}

shinyApp(ui=ui,server=server)

# The highest density of pieces in both mine and Hikaru's games happens at the
# center of the board. This makes sense, because most chess games start by
# fighting for control over the center of the board. By "control," I mean that
# whoever is in control over the center has their pieces either on the center
# squares or attacking the center squares, preventing their opponent from
# approaching. Control of the center allows for more mobility for your pieces
# as well as easy access to all parts of the board. Other squares of importance
# are c3, f3, c6, and f6. In the beginning of a game, knights often move to those
# squares because they're the only place a knight can move without any other
# pieces having to be moved first.
# 
# In general, these heatmaps demonstrate Hikaru's mastery of board positioning.
# In the blitz games, for example, the majority concentration of my pieces is on
# the center squares and the other four squares mentioned previously. Hikaru's
# blitz heatmap, on the other hand, displays a significant concentration of
# moves in the center 6x6 grid of the board, a much larger radius. The likely
# reason why Hikaru's heatmap displays such greater mobility is because he is
# familiar with a multitude of opening possibilities. When I play chess, I
# move my knights to the c3 and f3 (or c6 and f6, depending on the color) squares,
# push my center pawns forward, and proceed from there. I almost exclusively do
# this every game because I am most comfortable playing this way. My inexperience
# in chess means I don't know how to react to attacks if my knights and pawns
# aren't in a position I'm familiar with, so I put my knights and pawns in the
# same place every game. Hikaru, on the other hand, is comfortable playing from
# many different opening positions. He may begin his games by building up from
# the left side, or the right side, or by doing something else, rather than just
# going for control of the center of the board.
#
# My rapid games are interesting because they don't appear to follow any
# particular pattern. My rapid games as black showcase a diagonal line in the
# middle of the board, but my rapid games as white don't indicate anything like
# that. This is because I am more confident in rapid games. Because the timer is
# longer, I have more time to think about every move. I'm comfortable moving
# pieces other than my knights and center pawns because I feel like I have enough
# time to process my opponent's response and calculate the best moves. In bullet
# and blitz games, where every second counts, I'm more liable to overlooking a
# move or making a mistake when I face something I'm not used to, because I have
# less time to think about proper responses. Hikaru's rapid games also follow
# less of a pattern than his blitz or bullet games.
#
# The next visualization maps Elo in a time series from February to the end of
# April. Elo is a rating system used to calculate the relative skill levels of
# players in a zero-sum game; that is to say, a game where one side losing means
# the other side wins. Every time class has its own Elo, which means that
# players can be rated differently for different modes. After playing hundreds of
# games over several months, have I improved at all? Am I better in certain time
# classes? How does Hikaru compare?
# 
# Some data manipulation. Run the following lines:

chessV$UserELO<-NA
chessV$UserELO<-ifelse(chessV$White=="imvahn",chessV$WhiteElo,chessV$UserELO)
chessV$UserELO<-ifelse(chessV$Black=="imvahn",chessV$BlackElo,chessV$UserELO)
chessV$Date<-as.Date(chessV$Date)
chessV$UserELO<-as.numeric(chessV$UserELO)

chessH$UserELO<-NA
chessH$UserELO<-ifelse(chessH$White=="Hikaru",chessH$WhiteElo,chessH$UserELO)
chessH$UserELO<-ifelse(chessH$Black=="Hikaru",chessH$BlackElo,chessH$UserELO)
chessH$Date<-as.Date(chessH$Date)
chessH$UserELO<-as.numeric(chessH$UserELO)

# Now it's time for the shiny app. Run the following lines:

ui <- fluidPage(
  titlePanel("User Elo Over Time by Time Class"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId="gamemode",
        label="Choose a Time Class",
        choices=c("bullet","blitz","rapid"),
        selected="blitz",
        multiple=TRUE)),
    mainPanel(
      plotOutput("Vahn3"),
      plotOutput("Hikaru3"))))

server<-function(input,output){
  output$Vahn3<-renderPlot({
    ggplot(chessV%>%
             filter(Date>="2023-02-11")%>%
             filter(TimeClass==input$gamemode))+
      geom_line(aes(x=Date,y=UserELO,color=TimeClass))+
      ylim(c(0,3500))+
      labs(y="User Elo",
           title="Vahn",
           color="Time Class")+
      scale_color_brewer(palette="Accent")})
  output$Hikaru3<-renderPlot({
    ggplot(chessH%>%
             filter(TimeClass==input$gamemode))+
      geom_line(aes(x=Date,y=UserELO,color=TimeClass))+
      ylim(c(0,3500))+
      labs(y="User Elo",
           title="Hikaru",
           color="Time Class")+
      scale_color_brewer(palette="Accent")})}

shinyApp(ui=ui,server=server)

#Put simply, no. I have not improved. In fact, I think I've gotten worse. My
# blitz and bullet ratings are at around 300 Elo (and decreasing), which puts me in
# the 15th percentile of players. That means that I am worse than 85% of players
# on the chess.com website. My rapid rating is a little higher, at 600 Elo, placing
# me at around the 45th percentile. For a player like me who does not know any
# chess theory, every game requires me to analyze the board at every position. I
# don't have an encyclopedia of openings memorized, nor do I know any strategies.
# My skill in chess is completely dependent on the amount of time I have to think
# of a move. Hikaru's rating has remained pretty consistent over the months, at
# around 3300 Elo in bullet and blitz and 2800 Elo in rapid. While Hikaru hasn't
# "improved" in the sense that his Elo hasn't increased, he has remained #2 in
# the world in blitz and bullet and #6 in the world in rapid for several months,
# an impossibly impressive feat.
#
# When I made a chess.com account, I foolishly selected "intermediate" for my
# skill level. That started me off with 1200 Elo in every time class which, as is
# evident, decreased substantially and rapidly. In conclusion, I need to study.
#
# Chess is one of the oldest and most played games in the world, and has been
# played on every continent on the planet. Unlike sports, which can be
# inaccessible to many depending on the location (ice hockey is not very popular
# in Africa), the cost of gear, and how many friends are available, chess can be
# played by anyone anywhere. Thus, it has had large international impact, with
# chess masters originating from countries around the globe. Both Hikaru and I
# are Americans, so I wanted to see how great players are represented
# internationally.
#
# Run the following lines:

ui <- fluidPage(
  titlePanel("Chloropleth of Top 50 Players by Time Class"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId="gamemode",
        label="Choose a Time Class",
        choices=c("live_bullet","live_blitz","live_rapid"),
        selected="live_blitz")),
    mainPanel(
      plotOutput("Map"))))

server<-function(input,output){
  output$Map<-renderPlot({
    daily_leaders <- chessdotcom_leaderboard(game_type = input$gamemode)
    daily_leaders%>%
      mutate(region=gsub(pattern="https://api.chess.com/pub/country/","",country))->daily_leaders
    daily_leaders%>%
      mutate(region = gsub(pattern = "US","USA",region))->daily_leaders
    daily_leaders%>%
      mutate(region = gsub(pattern="XE","UK",region),
             region = gsub(pattern="WS","Samoa",region),
             region = gsub(pattern="NO","Norway",region),
             region = gsub(pattern="CL","Chile",region),
             region = gsub(pattern="RU","Russia",region),
             region = gsub(pattern="FR","France",region),
             region = gsub(pattern="UA","Ukraine",region),
             region = gsub(pattern="NL","Netherlands",region),
             region = gsub(pattern="AU","Australia",region),
             region = gsub(pattern="AZ","Azerbaijan",region),
             region = gsub(pattern="BY","Belarus",region),
             region = gsub(pattern="CN","China",region),
             region = gsub(pattern="DE","Denmark",region),
             region = gsub(pattern="GR","Germany",region),
             region = gsub(pattern="IN","India",region),
             region = gsub(pattern="PE","Peru",region),
             region = gsub(pattern="PL","Poland",region),
             region = gsub(pattern="RO","Romania",region),
             region = gsub(pattern="VN","Vietnam",region),
             region = gsub(pattern="RS","Serbia",region),
             region = gsub(pattern="CZ","Czech Republic",region),
             region = gsub(pattern="AE","United Arab Emirates",region),
             region = gsub(pattern="CA","Canada",region),
             region = gsub(pattern="IM","Isle of Man",region),
             region = gsub(pattern="UZ","Uzbekistan",region),
             region = gsub(pattern="GE","Georgia",region),
             region = gsub(pattern="ES","Spain",region),
             region = gsub(pattern="GI","Gibraltar",region),
             region = gsub(pattern="UY","Uruguay",region),
             region = gsub(pattern="IR","Iran",region),
             region = gsub(pattern="BR","Brazil",region),
             region = gsub(pattern="AR","Argentina",region),
             region = gsub(pattern="ZW","Zimbabwe",region),
             region = gsub(pattern="TR","Turkey",region),
             region = gsub(pattern="CO","Colombia",region),
             region = gsub(pattern="HR","Croatia",region),
             region = gsub(pattern="KZ","Kazakhstan",region),
             region = gsub(pattern="AM","Armenia",region),
             region = gsub(pattern="PG","Papua New Guinea",region),
             region = gsub(pattern="EG","Egypt",region))->daily_leaders
    daily_leaders%>%
      select(username,name,region)->daily_map
    daily_map%>%
      count(region)->daily_map
    ggplot()+
      geom_map(data=mapdata, aes(map_id=region),
               map=mapdata, fill="gray", color="black")+
      geom_map(data=daily_map, aes(map_id=region, fill=n), map= mapdata)+
      expand_limits(x=mapdata$long, y=mapdata$lat)+
      labs(x="",
           y="",
           title="",
           fill="Number of Players")+
      scale_fill_distiller(palette="GnBu",direction=1)})}

shinyApp(ui=ui, server=server)

# While the United States definitely hosts a majority of top players, masters
# can be seen across the map. Interestingly, a much greater majority of top bullet
# players live in the United States than in any other country. Fewer than five
# players per country from countries other than the United States are in the top
# 50 bullet players. In blitz and rapid time classes, the amount of top players in
# Russia, China, and India jumps up to ~7 players per country. There are also
# considerably more top players from South America who play bullet than there are
# in any other time class.
# 
# Many questions arose from my findings. I would like to know the locations of
# everyone in the world that I have played against. I would also like to know how
# that compares to the locations of players that Hikaru plays against. I wonder if
# it is different. As I play more games and actually attempt to study chess,
# I wonder how my visualizations will change. I wonder if I'll actually get better!