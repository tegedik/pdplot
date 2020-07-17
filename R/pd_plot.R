pd_plot <- function(x = seq(...), 
                      mean = NULL, 
                      sd = NULL, 
                      location = NULL, 
                      scale = NULL, 
                      size = NULL, 
                      prob = NULL, 
                      df=NULL, 
                      lambda=NULL,
                      rate=NULL, 
                      shape=NULL,
                      shape1=NULL,
                      shape2=NULL,
                      lines = FALSE,
                      type = c("normal", "logistic", "binomial", "chi-square", "poisson", "exponential", "cauchy", "beta", "gamma", "geometric")) {
dark_highlight <- c("#7C0000")
# Normal Distribution  
labels <- c()
  if (type == "normal") {
    if(lines == FALSE){
    plot(
      x = x, dnorm(x, mean = mean, sd = sd), 
      type = "l", lty = 1, lwd=2, col=dark_highlight,
      ylab = "Probability Density", xlab = "", main = "Normal Distribution"
    ) 
      a <- mean
      b <- sd
      labels <- bquote(mu~ .("=")~ .(a)~ .(",")~ sigma~ .("=")~ .(b))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=1, lwd=2, cex = 0.75)
      } else {
    lines(
        x = x, dnorm(x, mean = mean, sd = sd), 
        type = "l", lty = 2, lwd=1,
        ylab = "Probability Density", xlab = "", main = "Normal Distribution"
      )
      a <- mean
      b <- sd
      labels <- bquote(mu~ .("=")~ .(a)~ .(",")~ sigma~ .("=")~ .(b))
      legend("topright", inset=.02, title="Last Distribution",
                   legend= labels, lty=2, cex = 0.75)
    }

# Logistic
  } else if (type == "logistic") {
    if(lines == FALSE){
      plot(
        x = x, dlogis(x, location = location, scale = scale), 
        type = "l", lty = 1, lwd=2, col=dark_highlight,
        ylab = "Probability Density", xlab = "", main = "Logistic Distribution"
      ) 
      a <- location
      b <- scale
      labels <- bquote(mu~ .("=")~ .(a)~ .(",")~ s~ .("=")~ .(b))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=1, lwd=2, cex = 0.75)
    } else {
      lines(
        x = x, dlogis(x, location = location, scale = scale), 
        type = "l", lty = 2, lwd=1, 
        ylab = "Probability Density", xlab = "", main = "Logistic Distribution"
      )
      a <- location
      b <- scale
      labels <- bquote(mu~ .("=")~ .(a)~ .(",")~ s~ .("=")~ .(b))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=2, cex = 0.75)
    }

# Binomial
  } else if (type == "binomial") {
    if(lines == FALSE){
      plot(
        x = x, dbinom(x, size = size, prob = prob), 
        type = "b", lty = 1, lwd=2, col=dark_highlight,
        ylab = "Probability Mass", xlab = "", main = "Binomial Distribution"
      ) 
      a <- size
      b <- prob
      labels <- bquote(n~ .("=")~ .(a)~ .(",")~ p~ .("=")~ .(b))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=1, lwd=2, cex = 0.75)
    } else {
      lines(
        x = x, dbinom(x, size = size, prob = prob), 
        type = "b", lty = 2, lwd=1,
        ylab = "Probability Mass", xlab = "", main = "Binomial Distribution"
      )
      a <- size
      b <- prob
      labels <- bquote(n~ .("=")~ .(a)~ .(",")~ p~ .("=")~ .(b))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=2, cex = 0.75)
    }
    
# Chi-square
  } else if (type == "chi-square") {
    if(lines == FALSE){
      plot(
        x = x, dchisq(x, df=df, ncp=0), 
        type = "l", lty = 1, lwd=2, col=dark_highlight,
        ylab = "Probability Density", xlab = "", main = bquote(chi^2~ "Distribution")
      ) 
      a <- df
      #b <- prob
      labels <- bquote(chi^2~ .("=")~ .(a))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=1, lwd=2, cex = 0.75)
    } else {
      lines(
        x = x, dchisq(x, df=df, ncp=0), 
        type = "l", lty = 2, lwd=1,
        ylab = "Probability Density", xlab = "", main = bquote(chi^2~ "Distribution")
      )
      a <- df
     # b <- prob
      labels <- bquote(chi^2~ .("=")~ .(a))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=2, cex = 0.75)
    }
# poisson    
  } else if (type == "poisson") {
    if(lines == FALSE){
      plot(
        x = x, dpois(x, lambda=lambda), 
        type = "b", lty = 1, lwd=2, col=dark_highlight,
        ylab = "Probability Mass", xlab = "", main = "Poisson Distribution"
      ) 
      a <- lambda
      #b <- prob
      labels <- bquote(lambda~ .("=")~ .(a))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=1, lwd=2, cex = 0.75, col=dark_highlight)
    } else {
      lines(
        x = x, dpois(x, lambda=lambda), 
        type = "b", lty = 2, lwd=1,
        ylab = "Probability Mass", xlab = "", main = "Poisson Distribution"
      )
      a <- lambda
      # b <- prob
      labels <- bquote(lambda~ .("=")~ .(a))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=2, cex = 0.75)
    }
# exponential    
  } else if (type == "exponential") {
    if(lines == FALSE){
      plot(
        x = x, dexp(x, rate=rate), 
        type = "l", lty = 1, lwd=2, col=dark_highlight,
        ylab = "Probability Density", xlab = "", main = "Exponential Distribution"
      ) 
      a <- rate
      #b <- prob
      labels <- bquote(lambda~ .("=")~ .(a))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=1, lwd=2, cex = 0.75)
    } else {
      lines(
        x = x, dexp(x, rate=rate), 
        type = "l", lty = 2, lwd=1,
        ylab = "Probability Density", xlab = "", main = "Exponential Distribution"
      )
      a <- rate
      # b <- prob
      labels <- bquote(lambda~ .("=")~ .(a))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=2, cex = 0.75)
    }
# cauchy   
  } else if (type == "cauchy") {
    if(lines == FALSE){
      plot(
        x = x, dcauchy(x, location=location, scale=scale), 
        type = "l", lty = 1, lwd=2, col=dark_highlight,
        ylab = "Probability Density", xlab = "", main = "Cauchy Distribution"
      ) 
      a <- location
      b <- scale
      labels <- bquote(x~ .("=")~ .(a)~ .(",")~ gamma~ .("=")~ .(b))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=1, lwd=2, cex = 0.75)
    } else {
      lines(
        x = x, dcauchy(x, location=location, scale=scale), 
        type = "l", lty = 2, lwd=1,
        ylab = "Probability Density", xlab = "", main = "Cauchy Distribution"
      )
      a <- location
      b <- scale
      labels <- bquote(x~ .("=")~ .(a)~ .(",")~ gamma~ .("=")~ .(b))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=2, cex = 0.75)
    }
# beta    
  } else if (type == "beta") {
    if(lines == FALSE){
      plot(
        x = x, dbeta(x, shape1=shape1, shape2=shape2), 
        type = "l", lty = 1, lwd=2, col=dark_highlight,
        ylab = "Probability Density", xlab = "", main = "Beta Distribution"
      ) 
      a <- shape1
      b <- shape2
      labels <- bquote(alpha~ .("=")~ .(a)~ .(",")~ beta~ .("=")~ .(b))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=1, lwd=2, cex = 0.75)
    } else {
      lines(
        x = x, dbeta(x, shape1=shape1, shape2=shape2), 
        type = "l", lty = 2, lwd=1,
        ylab = "Probability Density", xlab = "", main = "Beta Distribution"
      )
      a <- shape1
      b <- shape2
      labels <- bquote(alpha~ .("=")~ .(a)~ .(",")~ beta~ .("=")~ .(b))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=2, cex = 0.75)
    }
# gamma    
  } else if (type == "gamma") {
    if(lines == FALSE){
      plot(
        x = x, dgamma(x, shape=shape, scale=scale), 
        type = "l", lty = 1, lwd=2, col=dark_highlight,
        ylab = "Probability Density", xlab = "", main = "Gamma Distribution"
      ) 
      a <- shape
      b <- scale
      labels <- bquote(k~ .("=")~ .(a)~ .(",")~ theta~ .("=")~ .(b))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=1, lwd=2, cex = 0.75)
    } else {
      lines(
        x = x, dgamma(x, shape=shape, scale=scale), 
        type = "l", lty = 2, lwd=1,
        ylab = "Probability Density", xlab = "", main = "Gamma Distribution"
      )
      a <- shape
      b <- scale
      labels <- bquote(k~ .("=")~ .(a)~ .(",")~ theta~ .("=")~ .(b))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=2, cex = 0.75)
    }
# geometric    
  } else if (type == "geometric") {
    if(lines == FALSE){
      plot(
        x = x, dgeom(x, prob=prob), 
        type = "b", lty = 1, lwd=2, col=dark_highlight,
        ylab = "Probability Mass", xlab = "", main = "Geometric Distribution"
      ) 
      a <- prob
      #b <- rate
      labels <- bquote(p~ .("=")~ .(a))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=1, lwd=2, cex = 0.75)
    } else {
      lines(
        x = x, dgeom(x, prob=prob), 
        type = "b", lty = 2, lwd=1,
        ylab = "Probability Mass", xlab = "", main = "Geometric Distribution"
      )
      a <- prob
     #b <- rate
      labels <- bquote(p~ .("=")~ .(a))
      legend("topright", inset=.02, title="Last Distribution",
             legend= labels, lty=2, cex = 0.75)
    }
    
  }

# end
}
