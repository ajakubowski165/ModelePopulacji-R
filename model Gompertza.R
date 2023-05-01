gompertz_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dN <- r * N * exp(-a * N)
    return(list(c(dN)))
  })
}


initial_state <- c(N = 50)
parameters <- c(r = 0.6, a = 0.05)
time <- seq(1, 50, by=1)


output <- ode(y = initial_state, times = time, func = gompertz_model, parms = parameters)


ggplot(data = as.data.frame(output), aes(x = time, y = N)) +
  geom_line(size = 2, color = "red") +
  theme(plot.background = element_rect(fill = "lightblue"),
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold")) +
        ggtitle("Model Gompertza") +
        xlab("Czas") +
        ylab("Liczba osobników")
