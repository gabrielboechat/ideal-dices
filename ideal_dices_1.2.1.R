set.seed(1)

# Creating the dice -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# How many times we will be throwing the dices? Give the desirable value for x

x = 200

vetor_lancamentos = rep(NA,x)
print(vetor_lancamentos)

# How many dices will be used? Give the desirable value for n

n = 2

# How many sides each dice will have? Give the desirable value for k

k = 6

# Sum outcome vector ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
modelo_lançamento_de_dados = function(n,k) {
  
  for (i in c(1:x)) {
    vetor_lancamentos[i] = sum(sample(1:k, n, replace = TRUE))
  }
  return(vetor_lancamentos)

}

vetor_grafico = modelo_lançamento_de_dados(n,k) # Vector with every sum of throwing n dices with k sides

# table(vetor_grafico) # How many each sum appears?

# Plot loop / Saving in a folder (ALWAYS CHECK THE PATHWAY OF THE IMAGES) -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

range = seq(n*1-0.5, n*k+0.5, 0.01)

dvalor = dnorm(range, mean = mean(vetor_grafico), sd = sd(vetor_grafico)) # Plotting normal distribution

mypath = file.path("C:","Users","gabri","Desktop","Animações","Plots","slot 1",paste("imagem_1.jpeg"))

jpeg(file = mypath)

hist(vetor_grafico[1], # Plotting the first histogram
     breaks = seq(n*1-0.5, n*k+0.5, 1),
     probability = TRUE,
     right = FALSE,
     xlim = c(n*1-0.5,n*k+1),
     ylim = c(0,1),
     xlab = "Possible sums",
     ylab = "Probability",
     main = paste("Probability of sums outcomes - Iteration nº 1"),
     col = "steelblue")

legend(x=n*k-2,y=0.9,c("Mean","Density","Normal"),cex=.8,col=c("red","blueviolet","black"),lty=c(2,1,1), lwd=c(1,2,2))

par(new = TRUE)

abline(v = mean(vetor_grafico[1]), lty = 2, col = "red")

mypath = file.path("C:","Users","gabri","Desktop","Animações","Plots","slot 1",paste("imagem_1.jpeg"))

jpeg(file = mypath)

for (h in c(2:x)) {

  mypath = file.path("C:","Users","gabri","Desktop","Animações","Plots","slot 1",paste("imagem_",h,".jpeg",sep = ""))
  
  jpeg(file = mypath)
  
vetor_grafico_reduzido = vetor_grafico[1:h]

hist(vetor_grafico_reduzido, # Plotting the first x's (after the first one) histograms
     breaks = seq(n*1-0.5, n*k+0.5, 1),
     probability = TRUE,
     right = FALSE,
     xlim = c(n*1-0.5,n*k+1),
     ylim = c(0,1),
     xlab = "Possible sums",
     ylab = "Probability",
     main = paste("Probability of sums outcomes - Iteration nº",h),
     col = "steelblue")

legend(x=n*k-2,y=0.9,c("Mean","Density","Normal"),cex=.8,col=c("red","blueviolet","black"),lty=c(2,1,1), lwd=c(1,2,2))


par(new = TRUE)

abline(v = mean(vetor_grafico_reduzido), lty = 2, col = "red")

par(new = TRUE)

if (h > 1) {
  plot(density(vetor_grafico_reduzido),
       xlim = c(n*1,n*k),
       lwd = 2,
       col="blueviolet",
       axes= FALSE, 
       xlab=NA,
       ylab=NA,
       main = NA)
  
  par(new = TRUE)
  
  dvalor = dnorm(range, mean = mean(vetor_grafico_reduzido), sd = sd(vetor_grafico_reduzido))
  
  plot(dvalor,
       xaxt = "n",
       yaxt = "n",
       type = "l",
       xlab = NA,
       ylab = NA,
       lwd = 2) 
  }

dev.off()
}