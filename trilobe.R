# Script created by Sam Hince
# 02/22/2021

library(ggplot2)

r_roller <- 0.5 # in 
R_cam <- 6 # in
r_cam <- 2 # in

n_steps <- 100

n_ponits_bearing <- 30

####################################
piston_travel <- R_cam - r_cam
between_roller_centers <- R_cam + r_cam + (2 * r_roller)

# linear motion
linear_station <- rep(c(seq(0,n_steps),seq(n_steps,1)), 3)
linear_position <- (linear_station * (piston_travel / 100)) + r_cam

# rotary motion
theta <- seq(from = 0, to = (2*pi), length.out = length(linear_station))[-1]

###################################
df <- data.frame(r_center=numeric(), theta_center=numeric())
df <- rbind(df, data.frame(r_center = 0, theta_center = 0))
for(i in seq(1,length(linear_station))){
  r_center <- linear_position[i]
  theta_center <- theta[i]
  
  df <- rbind(df, data.frame(r_center, theta_center))
}

pl1 <- ggplot(df, aes(x = theta_center, y = r_center)) + geom_point(size = 1) + coord_polar(theta = "x")
print(pl1)

###################################
# add circles at each point:
df$x <- df$r_center * cos(df$theta_center)
df$y <- df$r_center * sin(df$theta_center)

pl2 <- ggplot(df, aes(x = x, y = y)) + geom_point(size = 1) + coord_fixed()
print(pl2)

dfdetailed <- data.frame(x=numeric(), y=numeric())

theta_about_point <- seq(from = 0, to = (2*pi), length.out = n_ponits_bearing+1)[-1]

for(j in 1:nrow(df)){
  x_at_r <- df$x[j] + (r_roller * sin(theta_about_point))
  y_at_r <- df$y[j] + (r_roller * cos(theta_about_point))
  
  dfdetailed <- rbind(dfdetailed, data.frame(x = x_at_r, y = y_at_r))
}

pl2 <- ggplot(dfdetailed, aes(x = x, y = y)) + geom_point(size = 0.1) + coord_fixed()
print(pl2)

