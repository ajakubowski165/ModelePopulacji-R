library(tidyverse)
library(deSolve)


logistic_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dN <- r * N * (1 - N/K)
    return(list(c(dN)))
  })
}

initial_state <- c(N = 1000)
parameters <- c(r = 0.2, K = 10000)
time <- seq(0, 100, by = 1)

output <- ode(y = initial_state, times = time, func = logistic_model, parms = parameters)

ggplot(data = as.data.frame(output), aes(x = time, y = N)) +
  geom_line(size = 2, color = "red") +
  theme(plot.background = element_rect(fill = "lightblue"),
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold")) +
        ggtitle("Model logistyczny Verhulsta") +
        xlab("Czas") +
        ylab("Liczba osobników")


