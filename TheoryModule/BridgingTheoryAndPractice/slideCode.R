## This script has code for visuals used on slides

## Plots illustrating prior distributions -------------

# uniform prior with infinite bounds (not tractable)
curve(dunif(x, min = -1, max = 1),from = -1, to = 1, 
      lwd=4,xlab="",ylab="",yaxt="n",xaxt="n",yaxt="n", col ="#C81F1F")
axis(1, at = c(-1,1), labels = c("-\u221E","\u221E" ),cex.axis = 4)
abline(v = -0, lty=2)

# uniform prior with real boundaries a and b
curve(dunif(x, min = -1, max = 1),from = -1.5, to = 1.5, 
      lwd=4,xlab="",ylab="",yaxt="n",xaxt="n",yaxt="n", col ="#C81F1F",ylim = c(0,1))
axis(1, at = c(-1,1), labels = c("a","b" ),cex.axis = 4)
abline(v = -0, lty=2)


# gaussian distribution centered at 0
curve(dnorm(x, mean = 0, sd = 5),from = -1, to = 1, 
      lwd=4,xlab="",ylab="",yaxt="n",bty="n",xaxt="n", col ="#C81F1F")
axis(side=1, labels = FALSE,lwd=2)
abline(v = 0, lty=2)

# gamma distribution for tau
curve(dgamma(x,shape = 0.2, scale = 0.1), from = 0, to = 5,
      lwd=4,xlab="",ylab="",yaxt="n",xaxt="n", col ="#C81F1F")
curve(dgamma(x,shape = 0.1, scale = 0.1), from = 0, to = 5,
      lwd=4, col ="#C81F1F", add= TRUE)
axis(side=1, labels = FALSE,lwd=2)
abline(v = 0, lty=2)

# gaussian distribution with shading
xcomb <- seq(-1, 1, by = 0.01)
dy <- dnorm(xcomb, mean = 0, sd= 0.2)
plot(xcomb, dy, type="l",xlab="",ylab="",yaxt="n",xaxt="n")
polygon(xcomb, dy,col="#FFC000",border = "#C81F1F",lwd=4)



library(viridis)
# x range
x <- seq(0, 10, length.out = 1000)

# Shape and rate values
a_vals <- c(0.1,0.5, 1, 2, 5)
b_vals <- c(0.1,0.5, 1, 2, 5)

cols_a <- viridis(length(a_vals))
cols_b <- viridis(length(b_vals))

# Set up 2x2 panels
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# ---------- Panel 1: Shape effect, rate = 1 ----------
b <- 1
plot(x, dgamma(x, shape = a_vals[1], rate = b),
     type = "l", lwd = 3, col = cols_a[1],
     ylim = c(0, 1),
     xlab = expression(tau),
     ylab = expression(p(tau)),
     main = "Shape effect (a), Rate (b) = 1")

for (i in 2:length(a_vals)) {
  lines(x, dgamma(x, shape = a_vals[i], rate = b),
        lwd = 3, col = cols_a[i])
}

legend("topright",
       legend = paste("a =", a_vals),
       col = cols_a, lwd = 3, cex = 0.8, bty="n")

# ---------- Panel 2: Rate effect, shape = 2 ----------
a <- 2
plot(x, dgamma(x, shape = a, rate = b_vals[1]),
     type = "l", lwd = 3, col = cols_b[1],
     ylim = c(0, 1),
     xlab = expression(tau),
     ylab = expression(p(tau)),
     main = "Rate effect (b), Shape (a) = 2")

for (i in 2:length(b_vals)) {
  lines(x, dgamma(x, shape = a, rate = b_vals[i]),
        lwd = 3, col = cols_b[i])
}

legend("topright",
       legend = paste("b =", b_vals),
       col = cols_b, lwd = 3, cex = 0.8, bty="n")

# ---------- Panel 3: Shape effect, rate = 0.5 ----------
b <- 0.5
plot(x, dgamma(x, shape = a_vals[1], rate = b),
     type = "l", lwd = 3, col = cols_a[1],
     ylim = c(0, 1),
     xlab = expression(tau),
     ylab = expression(p(tau)),
     main = "Shape effect (a), Rate (b) = 0.5")

for (i in 2:length(a_vals)) {
  lines(x, dgamma(x, shape = a_vals[i], rate = b),
        lwd = 3, col = cols_a[i])
}

legend("topright",
       legend = paste("a =", a_vals),
       col = cols_a, lwd = 3, cex = 0.8, bty="n")

# ---------- Panel 4: Rate effect, shape = 0.5 ----------
a <- 0.5
plot(x, dgamma(x, shape = a, rate = b_vals[1]),
     type = "l", lwd = 3, col = cols_b[1],
     ylim = c(0, 1),
     xlab = expression(tau),
     ylab = expression(p(tau)),
     main = "Rate effect (b), Shape (a) = 0.5")

for (i in 2:length(b_vals)) {
  lines(x, dgamma(x, shape = a, rate = b_vals[i]),
        lwd = 3, col = cols_b[i])
}


legend("topright",
       legend = paste("b =", b_vals),
       col = cols_b, lwd = 3, cex = 0.8, bty="n")

# Reset layout
par(mfrow = c(1, 1))

