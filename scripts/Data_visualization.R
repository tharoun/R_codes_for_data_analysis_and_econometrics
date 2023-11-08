##########################################################################
#                        R code sample for data visualization            #                     
#                        by: Harouna TRAORE                              #
##########################################################################

### loading packages
source(here::here("scripts","Packages.R"))

########## Visualisation using ggplot ######

#### Bar plots : data etalons AFCON
data_etalons <- read_excel(here::here("raw_data", "data_etalons.xlsx"))

graph_data<-data_etalons %>% group_by(Adversaire,Resultat) %>%summarise(Nombre= n()) 

graph_data$Nombre<-as.integer(graph_data$Nombre)

# Use position = position_dodge() 
p <- ggplot(graph_data, aes(x = Adversaire, y =Nombre )) +
  geom_col(aes(color = Resultat, fill = Resultat), position = position_dodge(0.8), width = 0.7) +
  scale_color_manual(values = c("#FC4E07","#E7B800","#00AFBB"))+
  scale_fill_manual(values = c("#FC4E07","#E7B800","#00AFBB"))
p

p + geom_text(
  aes(label = Nombre, group = Resultat), 
  position = position_dodge(0.8),
  vjust = -0.3, size = 3.5
)

result_graph<-p+labs(title = "Résultat des matchs étalons vs futurs adversaires à la CAN",
                     x = " ADVERSAIRE", y = "NOMBRE",caption = "Source: Matchendirect.fr et travaux de l'auteur (T6H7)")+scale_y_continuous(limits = c(0,max(graph_data$Nombre)+1))+ geom_text(
                       aes(label = Nombre, group = Resultat), 
                       position = position_dodge(0.8),
                       vjust = -0.3, size = 7
                     )+
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", size = 15,color = "darkgreen",hjust = 0.5),
    legend.background = element_rect(
      fill = "white", 
      linewidth = 4, 
      colour = "white"
    ),
    legend.justification = c(0.5,0.85),
    legend.position = c(0.5,0.85),
    legend.text=element_text(size=9,face="bold"),
    legend.direction="horizontal",
    axis.ticks = element_line(colour = "grey70", linewidth = 9),
    panel.grid.major = element_line(colour = "grey70", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
    plot.caption = element_text(hjust=0,face = "bold")
  )

result_graph

#### Make a ggplot graph dynamic using gganimate

# Make 2 basic states and concatenate them:
a <- data.frame(group=c("A","B","C"), values=c(3,2,4), frame=rep('a',3))
b <- data.frame(group=c("A","B","C"), values=c(5,3,7), frame=rep('b',3))
data <- rbind(a,b)  

# Basic barplot:
ggplot(a, aes(x=group, y=values, fill=group)) + 
  geom_bar(stat='identity')

# Make a ggplot, but add frame=year: one image per year
ggplot(data, aes(x=group, y=values, fill=group)) + 
  geom_bar(stat='identity') +
  theme_bw() +
  # gganimate specific bits:
  transition_states(
    frame,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('sine-in-out')

# Save at gif:
anim_save(here::here("Outputs","result_graph_animate.gif"))

#### Saving graphs
ggsave(here::here("Outputs","result_graph.png"), width = 20, height = 20, units = "cm")

#### Boxplot with points 

### Sample data set
# Sample dataset
set.seed(3)
df <- data.frame(x = rexp(100),
                 group = sample(paste("Group", 1:3),
                                size = 100,
                                replace = TRUE))

# Vertical box plot by group
boxplot(x ~ group, data = df, col = "white")

# Points
stripchart(x ~ group,
           data = df,
           method = "jitter",
           pch = 19,
           col = 2:4,
           vertical = TRUE,
           add = TRUE)

###### lineplot  
# Data
df <- economics[economics$date > as.Date("2000-01-01"), ]

# Shade from 2000 to 2004 and from 2010 to 2015
shade <- data.frame(x1 = c(as.Date("2000-01-01"), as.Date("2013-01-01")),
                    x2 = c(as.Date("2003-01-01"), as.Date("2015-01-01")),
                    min = c(-Inf, -Inf), max = c(Inf, Inf))

ggplot() +
  geom_line(data = df, aes(x = date, y = unemploy),linewidth = 1)+
  geom_vline(xintercept = as.Date("2007-09-15"),
             linetype = 2, color = 2, linewidth = 1) +
  geom_rect(data = shade, aes(xmin = x1, xmax = x2, ymin = min, ymax = max),
            fill = c("green", "red"), alpha = 0.2)



#### Radar plot 
# Create data: note in High school for several students
set.seed(99)
data <- as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data) <- paste("mister" , letters[1:3] , sep="-")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(20,5) , rep(0,5) , data)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

# plot with default options:
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)
# Add a legend
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
