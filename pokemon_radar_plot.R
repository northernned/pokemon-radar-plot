# import libraries
library(ggplot2)
library(palettetown)
library(png)

# create the data with the stats for chosen pokemon

Articuno <- c(HP = 90, Attack = 85, 
              Defense = 100, Sp.Atk = 95, 
              Sp.Def = 125, Speed = 85)
Moltres <- c(HP = 90, Attack = 100, 
             Defense = 90, Sp.Atk = 125, 
             Sp.Def = 85, Speed = 90)
Zapdos <- c(HP = 90, Attack = 90, 
            Defense = 85, Sp.Atk = 125, 
            Sp.Def = 90, Speed = 100)

# Reason for hard coding Moltres' colour is because he shares a similar primary colour as Zapdos
Moltres_red <- "#E80000"
Zapdos_yellow <- pokepal('Zapdos', 1)
Articuno_blue <- pokepal('Articuno', 1)

data <- rbind(Articuno, Moltres, Zapdos)


Attributes <- colnames(data)
AttNo <- length(Attributes)


data <- cbind(data, data[,1])

# Create a function to draw the circles for the radar plot
draw_circle <- function(center = c(0,0),
                        diameter = 1,
                        npoints = 100){
  radius = diameter / 2
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + radius * cos(tt)
  yy <- center[2] + radius * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# draw the circles
circle1 <- draw_circle(diameter = 250)
circle2 <- draw_circle(diameter = 200)
circle3 <- draw_circle(diameter = 150)
circle4 <- draw_circle(diameter = 100)

angle_split <- (2 * pi) / (AttNo)
angle_split_seq <- seq(0, (2 * pi), angle_split)

# Create empty dataframes to collect the results
line_data <- data.frame(x = numeric, 
                        y = numeric, 
                        stringsAsFactors = F)
title_position <- data.frame(title = character,
                             x = numeric,
                             y = numeric,
                             stringsAsFactors = F)

# create plot background
for (i in 1:ncol(data)) {
  angle_multiplier <- if(i < ncol(data)){i}else{1}
  radians_for_segment <- angle_split_seq[i]
  
  x <- 150 * cos(radians_for_segment)
  y <- 150 * sin(radians_for_segment)
  temp <- data.frame(x = x, y = y, stringsAsFactors = FALSE)
  line_data <- rbind(temp, line_data)
  
  x <- 150 * cos(radians_for_segment)
  y <- 150 * sin(radians_for_segment)
  title <- colnames(data)[i]
  temp <- data.frame(title = title, 
                     x = x, 
                     y = y, 
                     stringsAsFactors = FALSE)
  title_position <- rbind(temp, 
                          title_position)}

# create the value labels data
values <- c(25, 50, 75, 100, 125)
radian_for_values <- angle_split / 2
x <- values * cos(radian_for_values)
y <- values * sin(radian_for_values)
values_position <- data.frame(values = values, x = x, y = y, stringsAsFactors = FALSE)

# add the origin values
line_data$x2 <- 0
line_data$y2 <- 0

# empty dataframe to catch result
polydata <- data.frame(pokemon = character,
                       value = numeric, 
                       radians = numeric, 
                       x = numeric, 
                       y = numeric, 
                       stringsAsFactors = FALSE)

# create polygon data
for (i in 1:ncol(data)) {
  for (p in 1:nrow(data)) {
    poke2calc <- data[p,]
    angle_multiplier <- if(i < ncol(data)){i}else{1}
    radians_for_segment <- angle_split_seq[i]
    x <- poke2calc[i] * cos(radians_for_segment)
    y <- poke2calc[i] * sin(radians_for_segment)
    pokemon<- rownames(data)[p]
    temp <- data.frame(pokemon = pokemon,
                       value = poke2calc[i],
                       radians = radians_for_segment, 
                       x = x, 
                       y = y, 
                       stringsAsFactors = FALSE)
    polydata <- rbind(temp, polydata)
  }
}

# Split the data for the pokemon
pokemon_database <- unique(polydata$pokemon)
pokemon1 <- polydata[which(polydata$pokemon == pokemon_database[1]),]
pokemon2 <- polydata[which(polydata$pokemon == pokemon_database[2]),]
pokemon3 <- polydata[which(polydata$pokemon == pokemon_database[3]),]

# Create the title string for each pokemon
pokemon1_title <- gsub('([[:upper:]])', 
                       '\\1',
                       pokemon_database[1])
pokemon2_title <- gsub('([[:upper:]])', 
                       '\\1',
                       pokemon_database[2])
pokemon3_title <- gsub('([[:upper:]])', 
                       '\\1',
                       pokemon_database[3])
# Start the png to create a hi-res image of the plot
png(file="examples\\original_legendary_birds.png",
    width = 800,
    height = 900,
    bg = "transparent")
# Create the radar background
ggplot() +
  xlim(c(-150, 150)) +
  ylim(c(-200, 200)) +
  # Add the Circles
  geom_polygon(data = circle1,
               aes(x = x, y = y),
               fill = "#5A5F72",
               colour = "#5A5F72") +
  geom_polygon(data = circle2,
               aes(x = x, y = y),
               fill = "#969696",
               colour = "#969696") +
  geom_polygon(data = circle3,
               aes(x = x, y = y),
               fill = "#5A5F72",
               colour = "#5A5F72") +
  geom_polygon(data = circle4,
               aes(x = x, y = y),
               fill = "#969696",
               colour = "#969696") +
  # Void the theme and and a background fill
  theme_void() +
  theme(plot.background = element_rect(
    fill = "#1F2833")) +
  # Add the segment lines and attribute/values titles
  geom_segment(data = line_data,
               aes(x = line_data$x, 
                   y = line_data$y,
                   xend = line_data$x2,
                   yend = line_data$y2),
               colour = "#FFFFFF",
               linetype = "dashed") +
  annotate("text",
           x = title_position$x,
           y = title_position$y,
           label = title_position$title,
           size = 4,
           colour = "#FFFFFF") +
  annotate("text",
           x = values_position$x,
           y = values_position$y,
           label = values_position$values,
           size = 3.5,
           colour = "#FFFFFF") +
  # Add Chart Title
  annotate("text",
           x = 150,
           y = 190,
           label = "Pokemon Radar Plot",
           size = 7,
           colour = "#969696",
           family = "Helvetica",
           fontface = "bold",
           hjust = 1) +
  annotate("text",
           x = 150,
           y = 170,
           label = "PokemonDB",
           size = 6,
           colour = "#5A5F72",
           family = "Helvetica",
           fontface = "bold",
           hjust = 1) +
  # Add pokemon 1 data
  geom_polygon(data = pokemon1,
               aes(x = x, y = y),
               fill = Zapdos_yellow,
               colour = Zapdos_yellow,
               alpha = 0.3) +
  annotate("text",
           x = -150,
           y = 190,
           label = pokemon1_title,
           size = 10,
           colour = Zapdos_yellow,
           family = "Helvetica",
           fontface = "bold",
           hjust = 0) +
  annotate("text", 
           x = -140, 
           y = 180, 
           label = "vs", 
           size = 5, 
           colour = "#969696", 
           family = "Helvetica", 
           hjust = 0) +
  # Add pokemon 2 data
  geom_polygon(data = pokemon2,
               aes(x = x, y = y),
               fill = Moltres_red,
               colour = Moltres_red,
               alpha = 0.3) +
  annotate("text",
           x = -150,
           y = 170,
           label = pokemon2_title,
           size = 10,
           colour = Moltres_red,
           family = "Helvetica",
           fontface = "bold",
           hjust = 0) +
  annotate("text", 
           x = -140, 
           y = 160, 
           label = "vs", 
           size = 5, 
           colour = "#969696", 
           family = "Helvetica", 
           hjust = 0) +
  # Add pokemon 2 data
  geom_polygon(data = pokemon3,
               aes(x = x, y = y),
               fill = Articuno_blue,
               colour = Articuno_blue,
               alpha = 0.3) +
  annotate("text",
           x = -150,
           y = 150,
           label = pokemon3_title,
           size = 10,
           colour = Articuno_blue,
           family = "Helvetica",
           fontface = "bold",
           hjust = 0)
dev.off()