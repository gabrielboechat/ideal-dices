set.seed(3)

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

par(new = TRUE)

if (h > 1) {
  plot(density(vetor_grafico_reduzido),
       xlim = c(n*1,n*k),
       lwd = 2,
       col="blueviolet",
       axes= FALSE, 
       xlab=NA,
       ylab=NA,
       main = NA)}

dev.off()
}