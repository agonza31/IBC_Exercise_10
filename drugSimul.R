K <- 1000000 #carrying capacity
N0 <-1 #initial # of normal
M0 <-0 #initial # of mutant
rN <-0.1 #Normal growth rate
rM <-0.1 #Mutant growth rate
timesteps<-1000



#create vectors for N (normal cells) and M (mutant cells)
N<-numeric(length = timesteps)
N[1]<-N0

M<-numeric(length = timesteps)
M[1]<-M0


#simulate
for(t in 1:1000) {
  if(N[t] >= 100 && M[t] < 1) {
    M[t] <- 1 #The mutation in the Normal cells occurs when
              #the Normal cell population reaches 100. From
              #this point onwards, Mutant cells now exist
  }

  N[t+1] <- N[t] + rN*N[t]*(1-(N[t] + M[t])/K)
  M[t+1] <- M[t] + rM*M[t]*(1-(N[t] + M[t])/K)
  
  if(N[t+1] == N[t] && M[t+1] == M[t] && rN != -0.1) {
    N[t+1]<-N[t]-1
    rN <- -0.1  #Once normal cells reach carrying capacity,
                #treat them with drugs. This causes growth
                #rate to change to -0.1
    
    rM <- 0.05  #mutant cells also get treated, and
                #when they do, their growth rate is
                #slowed to half of the initial rate.
  }
}


#plot simulation
library(ggplot2)
simNorm<-data.frame(time=1:length(N),N=N)
simMut<-data.frame(time=1:length(M),M=M)

plotNM<-ggplot(data = simNorm, aes(x = time)) + geom_line(aes(y=N,color="normal")) + geom_line(aes(y = M,color="mutant"))

plotNM<-plotNM + labs(y="# Cells", x="Time (days)") + labs(color = "cell type")
plotNM