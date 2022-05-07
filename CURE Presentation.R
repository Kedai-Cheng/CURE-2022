library(plotly)
###########################################
########## Central Limit Theorem ##########
###########################################
###################################
###### Binomial Distribution ######
###################################
set.seed(2022)
n <- 1000
sample.mean <- 5
sample.sd <- 2
random.norm <- rnorm(n=n , mean=sample.mean , sd=sample.sd)

### Histogram of a Normal Distribution ###
fit.norm.dist <- density(random.norm)

plot_ly() %>% 
  add_trace(x = random.norm, type = "histogram", 
            marker = list(color = "#003DA5",
                          line = list(color = "white",
                                      width = 0.01)),
            name = "Normal Random Sample" , showlegend = FALSE) %>%
  add_trace(x = fit.norm.dist$x, 
            y = fit.norm.dist$y, type = "scatter", mode = "lines", 
            line = list(width = 8 , dash="dash" , color = "#003DA5"),
            fill = NULL, yaxis = "y2", name = "Density" , showlegend = FALSE) %>% 
  layout(title = list(text = "<b> Distribution of Random Normal Data </b>",
                      x = 0.5,
                      y = 0.97,
                      font = list(size=36)),
         xaxis = list(title = "Random Values",
                      tickfont = list(size = 36),
                      titlefont = list(size = 36),
                      zeroline = FALSE),
         yaxis = list(title = "Counts",
                      tickfont = list(size = 36),
                      titlefont = list(size = 36),
                      zeroline = FALSE),
         bargap = 0.05,
         yaxis2 = list(overlaying = "y", side = "right",
                       title = "Density",
                       position=0.9,
                       tickfont = list(size = 36),
                       titlefont = list(size = 36))
  )

### Central Limit Theorem ###
B <- 500
mean.vec <- rep(NA,B)
set.seed(2022)
for (i in 1:B){
  boot.sample <- sample(random.norm , size=n , replace = TRUE)
  mean.vec[i] <- mean(boot.sample)
}

### Histogram of a Uniform Distribution ###
fit.boot.dist <- density(mean.vec)

plot_ly() %>% 
  add_trace(x = mean.vec, type = "histogram", 
            marker = list(color = "#003DA5",
                          line = list(color = "white",
                                      width = 0.01)),
            name = "Sample Means" , showlegend = FALSE) %>%
  add_trace(x = fit.boot.dist$x, 
            y = fit.boot.dist$y, type = "scatter", mode = "lines", 
            line = list(width = 8 , dash="dash" , color = "#003DA5"),
            fill = NULL, yaxis = "y2", name = "Density" , showlegend = FALSE) %>% 
  layout(title = list(text = "<b> Distribution of Sample Means </b>",
                      x = 0.5,
                      y = 0.97,
                      font = list(size=36)),
         xaxis = list(title = "Sample Means",
                      tickfont = list(size = 36),
                      titlefont = list(size = 36),
                      zeroline = FALSE),
         yaxis = list(title = "Counts",
                      tickfont = list(size = 36),
                      titlefont = list(size = 36),
                      zeroline = FALSE),
         bargap = 0.05,
         yaxis2 = list(overlaying = "y", side = "right",
                       title = "Density",
                       position=0.9,
                       tickfont = list(size = 36),
                       titlefont = list(size = 36))
  )

##################################
###### Uniform Distribution ######
##################################
set.seed(2022)
n <- 1000
min.uni <- 0
max.uni <- 20
random.uniform <- runif(n=n , min=min.uni , max=max.uni)

### Histogram of a Uniform Distribution ###
fit.uniform.dist <- density(random.uniform)

plot_ly() %>% 
  add_trace(x = random.uniform, type = "histogram", 
            marker = list(color = "#003DA5",
                          line = list(color = "white",
                                      width = 0.01)),
            name = "Uniform Random Sample" , showlegend = FALSE) %>%
  add_trace(x = fit.uniform.dist$x, 
            y = fit.uniform.dist$y, type = "scatter", mode = "lines", 
            line = list(width = 8 , dash="dash" , color = "#003DA5"),
            fill = NULL, yaxis = "y2", name = "Density" , showlegend = FALSE) %>% 
  layout(title = list(text = "<b> Distribution of Random Uniform Data </b>",
                      x = 0.5,
                      y = 0.97,
                      font = list(size=36)),
         xaxis = list(title = "Random Values",
                      tickfont = list(size = 36),
                      titlefont = list(size = 36),
                      zeroline = FALSE),
         yaxis = list(title = "Counts",
                      tickfont = list(size = 36),
                      titlefont = list(size = 36),
                      zeroline = FALSE),
         bargap = 0.05,
         yaxis2 = list(overlaying = "y", side = "right",
                       title = "Density",
                       position=0.9,
                       tickfont = list(size = 36),
                       titlefont = list(size = 36))
  )

### Central Limit Theorem ###
B <- 500
mean.vec <- rep(NA,B)
set.seed(2022)
for (i in 1:B){
  boot.sample <- sample(random.uniform , size=n , replace = TRUE)
  mean.vec[i] <- mean(boot.sample)
}

### Histogram of a Uniform Distribution ###
fit.boot.dist <- density(mean.vec)

plot_ly() %>% 
  add_trace(x = mean.vec, type = "histogram", 
            marker = list(color = "#003DA5",
                          line = list(color = "white",
                                      width = 0.01)),
            name = "Sample Means" , showlegend = FALSE) %>%
  add_trace(x = fit.boot.dist$x, 
            y = fit.boot.dist$y, type = "scatter", mode = "lines", 
            line = list(width = 8 , dash="dash" , color = "#003DA5"),
            fill = NULL, yaxis = "y2", name = "Density" , showlegend = FALSE) %>% 
  layout(title = list(text = "<b> Distribution of Sample Means </b>",
                      x = 0.5,
                      y = 0.97,
                      font = list(size=36)),
         xaxis = list(title = "Sample Means",
                      tickfont = list(size = 36),
                      titlefont = list(size = 36),
                      zeroline = FALSE),
         yaxis = list(title = "Counts",
                      tickfont = list(size = 36),
                      titlefont = list(size = 36),
                      zeroline = FALSE),
         bargap = 0.05,
         yaxis2 = list(overlaying = "y", side = "right",
                       title = "Density",
                       position=0.9,
                       tickfont = list(size = 36),
                       titlefont = list(size = 36))
  )

###################################
###### Binomial Distribution ######
###################################
set.seed(2022)
n <- 1000
sample.size <- 20
prob.success <- 0.1
random.binom <- rbinom(n=n , size=sample.size , prob=prob.success)

### Histogram of a Uniform Distribution ###
fit.binom.dist <- density(random.binom)

plot_ly() %>% 
  add_trace(x = random.binom, type = "histogram", 
            marker = list(color = "#003DA5",
                          line = list(color = "white",
                                      width = 0.01)),
            name = "Binomial Random Sample" , showlegend = FALSE) %>%
  add_trace(x = fit.binom.dist$x, 
            y = fit.binom.dist$y, type = "scatter", mode = "lines", 
            line = list(width = 8 , dash="dash" , color = "#003DA5"),
            fill = NULL, yaxis = "y2", name = "Density" , showlegend = FALSE) %>% 
  layout(title = list(text = "<b> Distribution of Random Binomial Data </b>",
                      x = 0.5,
                      y = 0.97,
                      font = list(size=36)),
         xaxis = list(title = "Random Values",
                      tickfont = list(size = 36),
                      titlefont = list(size = 36),
                      zeroline = FALSE),
         yaxis = list(title = "Counts",
                      tickfont = list(size = 36),
                      titlefont = list(size = 36),
                      zeroline = FALSE),
         bargap = 0.05,
         yaxis2 = list(overlaying = "y", side = "right",
                       title = "Density",
                       position=0.9,
                       tickfont = list(size = 36),
                       titlefont = list(size = 36))
  )

### Central Limit Theorem ###
B <- 500
mean.vec <- rep(NA,B)
set.seed(2022)
for (i in 1:B){
  boot.sample <- sample(random.binom , size=n , replace = TRUE)
  mean.vec[i] <- mean(boot.sample)
}

### Histogram of a Uniform Distribution ###
fit.boot.dist <- density(mean.vec)

plot_ly() %>% 
  add_trace(x = mean.vec, type = "histogram", 
            marker = list(color = "#003DA5",
                          line = list(color = "white",
                                      width = 0.01)),
            name = "Sample Means" , showlegend = FALSE) %>%
  add_trace(x = fit.boot.dist$x, 
            y = fit.boot.dist$y, type = "scatter", mode = "lines", 
            line = list(width = 8 , dash="dash" , color = "#003DA5"),
            fill = NULL, yaxis = "y2", name = "Density" , showlegend = FALSE) %>% 
  layout(title = list(text = "<b> Distribution of Sample Means </b>",
                      x = 0.5,
                      y = 0.97,
                      font = list(size=36)),
         xaxis = list(title = "Sample Means",
                      tickfont = list(size = 36),
                      titlefont = list(size = 36),
                      zeroline = FALSE),
         yaxis = list(title = "Counts",
                      tickfont = list(size = 36),
                      titlefont = list(size = 36),
                      zeroline = FALSE),
         bargap = 0.05,
         yaxis2 = list(overlaying = "y", side = "right",
                       title = "Density",
                       position=0.9,
                       tickfont = list(size = 36),
                       titlefont = list(size = 36))
  )

#########################################
########## Confidence Interval ##########
#########################################
set.seed(2022)
pop.mean <- 3
pop.sd <- 1
N <- 100 ## Sample Size
n.exp <- 100 ## Number of Experiments

x.mat <- matrix(NA,nrow=n.exp , ncol=N) # Each Row is a random sample.
x.mean <- sd.x <- rep(NA,n.exp)

for (i in 1:n.exp){
  x.mat[i,] <- rnorm(n=N , mean = pop.mean , sd= pop.sd)
  x.mean[i] <- mean(x.mat[i,])
  sd.x[i] <- sd(x.mat[i,])
}

### Two-Sided Confidence Interval ###
conf.level <- 0.95
MOE <- lower.bound <- upper.bound <- rep(NA,n.exp)

for (k in 1:n.exp){
  MOE[k] <- qnorm(1-(1-conf.level)/2 , mean = 0 , sd=1)*sd.x[k]/sqrt(N)
  lower.bound[k] <- x.mean[k]-MOE[k]
  upper.bound[k] <- x.mean[k]+MOE[k]
}

## Actual Coverage ###
actual.coverage <- rep(NA,n.exp)

for (j in 1:n.exp){
  if ((lower.bound[j] < pop.mean) && (upper.bound[j] > pop.mean)){
    actual.coverage[j] <- 1
  } else {actual.coverage[j] <- 0}
}

sum(actual.coverage)/n.exp

### Chart ###
vline <- function(x = 0, color = "#A6192E") {
  list(
    type = "line",
    x0 = x,
    x1 = x,
    yref = "paper",
    y0 = 0,
    y1 = 1,
    line = list(color = color , width = 2 , dash = "dash")
  )
}
plot <- plot_ly()%>%
  layout(
    title = list(text = paste(conf.level*100,"% Confidence Intervals"),
                 x = 0.55,
                 y = 0.975,
                 font = list(size=36)),
    xaxis = list(title = "Means",
                 tickfont = list(size = 36),
                 titlefont = list(size = 36),
                 zeroline= FALSE
    ),
    yaxis = list(title = "Experiment",
                 tickfont = list(size = 36),
                 titlefont = list(size = 36),
                 zeroline = FALSE),
    shapes = list(vline(pop.mean))
  )

for (k in 1:n.exp){
  if(lower.bound[k] > pop.mean | upper.bound[k] < pop.mean){
    line.color <- "#A6192E"
  } else {line.color <- "#0076A5"}
  plot <- plot %>%
    add_trace(x=c(lower.bound[k] , x.mean[k] , upper.bound[k]),
              y=k,
              type = "scatter" , mode = "lines+markers",
              marker = list(color = c("#003865","#A6192E","#003865") , 
                            size = 4) ,
              line = list(color = line.color , width = 2 , dash='solid'),
              name = paste('Bound',k) , showlegend = FALSE)
}
plot
