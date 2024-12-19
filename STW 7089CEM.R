#Install necessary libraries
install.packages("ggplot2")
install.packages("tidyr")
install.packages("plotly")
install.packages("corrplot")
install.packages("rsample")

#Load necessary libraries
library(ggplot2)
library(tidyr)
library(plotly)
library(corrplot)
library(rsample)

# Load the data
data <- read.csv("data.csv")
print(data)

#-----------------------Task 1: Preliminary data analysis--------------------------

#Time Series Plots of input and output signal

#Input Signals
input_signal_tsp <- ggplot(data,aes(x = time))+
  geom_line(aes(y=x1, color="x1"), size = 1)+
  geom_line(aes(y=x3, color="x3"), size = 1)+
  geom_line(aes(y=x4, color="x4"), size = 1)+
  geom_line(aes(y=x5, color="x5"), size = 1)+
  labs(title="Time Series Plot for Input Signals", x="Time(s)", y= "Signal Intensity")+
  scale_color_manual(values = c("x1"="blue", "x3"="red", "x4"="purple", "x5"="green"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Separate Plots for each signals
data_long <- data %>%
  pivot_longer(cols = c(x1, x3, x4, x5), names_to = "Signal", values_to = "Value")

s_input_signal_tsp <-ggplot(data_long, aes(x = time, y = Value, color = Signal))+
  geom_line(size = 1)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5)+
  labs(title = "Time Series Plot for Input Signals", x = "Time (s)", y = "Signal Intensity")+
  scale_color_manual(values = c("x1" = "blue", "x3" = "red", "x4" = "purple", "x5" = "green"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~ Signal,ncol = 1, scales = "free_y")


#Output Signal
output_signal_tsp <- ggplot(data,aes(x = time))+
  geom_line(aes(y=x2, color="x2"), size = 1)+
  labs(title="Time Series Plot for Output Signal", x="Time(s)", y= "Signal Intensity")+
  scale_color_manual(values = c("x2"="orange"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


#Display the time series plot
print(input_signal_tsp)
print(s_input_signal_tsp)
print(output_signal_tsp)

#Distribution for each signal (time-series) 

output_data_long <- data %>%
  pivot_longer(cols = c(x2), names_to = "Signal", values_to = "Value")

gg_x2 <- ggplot(output_data_long, aes(x=Value, fill = Signal, color = Signal))+
  geom_histogram(aes(y=after_stat(density)), bins = 10, alpha = 0.3, position = "identity")+
  stat_density(geom = "line", size = 1)+
  geom_rug()+
  labs(title="Distribution of x2 (Output)", x = "Signal Intensity", y = "Density")+
  scale_fill_manual(values = c("x2" = "orange"))+
  scale_color_manual(values = c("x2" = "orange"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(gg_x2) %>% layout(plot_bgcolor='#e5ecf6', xaxis=list(title="x2 Output Signal"), yaxis= list(title="Density"))


#Create a combined histogram and density plot for all input signals in a single plot
combined_hist_density_input_signals <- ggplot(data_long, aes(x = Value, fill = Signal, color = Signal))+
  geom_histogram(aes(y=after_stat(density)),bins = 30, alpha = 0.3, position = "identity")+
  stat_density(geom="line", size = 1, adjust = 1.5)+
  geom_rug() +
  labs(title = "Distribution for Input Signals", x = "Signal Intensity", y = "Frequency / Density")+
  scale_fill_manual(values = c("x1" = "blue", "x3" = "red", "x4" = "purple", "x5" = "green"))+
  scale_color_manual(values = c("x1" = "blue", "x3" = "red", "x4" = "purple", "x5" = "green"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Convert to interactive plot using ggplotly
ggplotly(combined_hist_density_input_signals) %>% layout(
  plot_bgcolor = '#e5ecf6',
  title = "Distribution of Input Signals",
  xaxis = list(title = "Signal Intensity"),
  yaxis = list(title = "Density")
)

#Create a combined histogram and density plot for all input signals in a separate plot
separate_hist_density_input_signals <- ggplot(data_long, aes(x = Value, fill = Signal, color = Signal))+
  geom_histogram(aes(y=after_stat(density)),bins = 30, alpha = 0.3, position = "identity")+
  stat_density(geom="line", size = 1, adjust = 1.5)+
  geom_rug() +
  labs(title = "Distribution for Input Signals", x = "Signal Intensity", y = "Frequency / Density")+
  scale_fill_manual(values = c("x1" = "blue", "x3" = "red", "x4" = "purple", "x5" = "green"))+
  scale_color_manual(values = c("x1" = "blue", "x3" = "red", "x4" = "purple", "x5" = "green"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~Signal, ncol = 2, scales = "free")

#Convert to interactive plot using ggplotly
ggplotly(separate_hist_density_input_signals) %>% layout(
  plot_bgcolor = '#e5ecf6',
  title = "Distribution of Input Signals",
  xaxis = list(title = "Signal Intensity"),
  yaxis = list(title = "Density")
)

print(combined_hist_density_input_signals)
print(separate_hist_density_input_signals)

#Calculate the correlation matrix for the signals (x1, x2, x3, x4, x5)
cor_matrix <- cor(data[, c("x1", "x2", "x3", "x4", "x5")])

#Visualize the correlation matrix using a heatmap
corrplot(cor_matrix, method = "color", type = "upper",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         addCoef.col = "black", number.cex = 0.7, tl.cex = 0.8,
         tl.col = "black",
         title = "Correlation Matrix for Signals",
         mar = c(0,0,1,0))

#Scatter plot for all signals
comined_scatter <- ggplot(data_long, aes(x = Value, y =x2 , color = Signal))+
  geom_point(alpha = 0.6) +
  labs(title = "Scatter Plot: Input Signals vs Output", x= "Input Signal Intensity", y = "Output")+
  scale_color_manual(values = c("x1" = "blue", "x3" = "red", "x4" = "purple", "x5" = "green"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5)

print(comined_scatter)

#Create a faceted scatter plot
scatter_facet <- ggplot(data_long, aes(x = Value, y = x2))+
  geom_point(aes(color = Signal), alpha = 0.7)+
  facet_wrap(~ Signal, scales = "free_x")+
  labs(title = "scatter Plots: Input Signals vs Output",
       x = "Input Signal Intensity",
       y = " Output Signal Intensity")+
  scale_color_manual(values = c("x1" = "blue", "x3" = "red", "x4" = "purple", "x5" = "green"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5)

print(scatter_facet)


#-----------------------------------Task 2: Regression--------------------------------

df<- data.frame(data)
print(df)

thetaHat <- function(model, y){
  return (solve(t(model) %*% model) %*% t(model) %*% y)
}

generateModel1 <- function(df){
  set.seed(100)
  ones = matrix(1, length(df$x1),1)
  theta_bias = runif(1, -1, 1) * ones
  return(cbind(df$x4, df$x3^2, theta_bias))
}

generateModel2 <- function(df){
  set.seed(100)
  ones = matrix(1, length(df$x1),1)
  theta_bias = runif(1, -1, 1) * ones
  return(cbind(df$x4, df$x3^2, df$x5, theta_bias))
}

generateModel3 <- function(df){
  set.seed(100)
  ones = matrix(1, length(df$x1),1)
  theta_bias = runif(1, -1, 1) * ones
  return(cbind(df$x3, df$x4, df$x5^3))
}

generateModel4 <- function(df){
  set.seed(100)
  ones = matrix(1, length(df$x1),1)
  theta_bias = runif(1, -1, 1) * ones
  return(cbind(df$x4, df$x3^2, df$x5^3, theta_bias))
}

generateModel5 <- function(df){
  set.seed(100)
  ones = matrix(1, length(df$x1),1)
  theta_bias = runif(1, -1, 1) * ones
  return(cbind(df$x4, df$x1^2, df$x3^2, theta_bias))
}

Model1 = generateModel1(df)
Model1_theta_hat = thetaHat(Model1, df$x2)
print("Model1 Theta hat")
print(Model1_theta_hat[,1])
print("Model 1 Y hat")
y_Hat_Model1 = Model1 %*% Model1_theta_hat
print(y_Hat_Model1[1:5,])

Model2 = generateModel2(df)
Model2_theta_hat = thetaHat(Model2, df$x2)
print("Model2 Theta hat")
print(Model2_theta_hat[,1])
print("Model2 Y hat")
y_Hat_Model2 = Model2 %*% Model2_theta_hat
print(y_Hat_Model2[1:5,])

Model3 = generateModel3(df)
Model3_theta_hat = thetaHat(Model3, df$x2)
print("Model3 Theta hat")
print(Model3_theta_hat[,1])
print("Model3 Y hat")
y_Hat_Model3 = Model3 %*% Model3_theta_hat
print(y_Hat_Model3[1:5,])

Model4 = generateModel4(df)
Model4_theta_hat = thetaHat(Model4, df$x2)
print("Model4 Theta hat")
print(Model4_theta_hat[,1])
print("Model4 Y hat")
y_Hat_Model4 = Model4 %*% Model4_theta_hat
print(y_Hat_Model4[1:5,])

Model5 = generateModel5(df)
Model5_theta_hat = thetaHat(Model5, df$x2)
print("Model5 Theta hat")
print(Model5_theta_hat[,1])
print("Model5 Y hat")
y_Hat_Model5 = Model5 %*% Model5_theta_hat
print(y_Hat_Model5[1:5,])

#----Task 2.2-------

calculateRSS <- function(y,y_hat_model){
  return(sum((y - y_hat_model)^2))
}

Rss_Model1 = calculateRSS(df$x2, y_Hat_Model1)
Rss_Model2 = calculateRSS(df$x2, y_Hat_Model2)
Rss_Model3 = calculateRSS(df$x2, y_Hat_Model3)
Rss_Model4 = calculateRSS(df$x2, y_Hat_Model4)
Rss_Model5 = calculateRSS(df$x2, y_Hat_Model5)
c(Rss_Model1, Rss_Model2, Rss_Model3,Rss_Model4, Rss_Model5)
list_Rss_Label = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")
list_Rss_Value = c(Rss_Model1, Rss_Model2, Rss_Model3,Rss_Model4, Rss_Model5)
list_RSS = c(list_Rss_Label, list_Rss_Value)
print(list_RSS)

#--------------------Task2.3-----------------------------------------

calculateVariance <- function(N, rss_model){
  return(rss_model/(N-1))
}

calculateLikelihood <- function(N, variance_model, rss_model){
  return (-(N/2)*(log(2*pi))-(N/2)*(log(variance_model))-(1/(2*variance_model))*rss_model)
}

N = length(df$x2)

Variance_Model1 = calculateVariance(N, Rss_Model1)
Variance_Model2 = calculateVariance(N, Rss_Model2)
Variance_Model3 = calculateVariance(N, Rss_Model3)
Variance_Model4 = calculateVariance(N, Rss_Model4)
Variance_Model5 = calculateVariance(N, Rss_Model5)

c(Variance_Model1, Variance_Model2, Variance_Model3, Variance_Model4, Variance_Model5)

Likelihood_1 = calculateLikelihood(N, Variance_Model1, Rss_Model1)
Likelihood_2 = calculateLikelihood(N, Variance_Model2, Rss_Model2)
Likelihood_3 = calculateLikelihood(N, Variance_Model3, Rss_Model3)
Likelihood_4 = calculateLikelihood(N, Variance_Model4, Rss_Model4)
Likelihood_5 = calculateLikelihood(N, Variance_Model5, Rss_Model5)

print(c(Likelihood_1, Likelihood_2, Likelihood_3, Likelihood_4, Likelihood_5))

#--------------------Task2.4-----------------------------------------

calculateAIC <- function(N, model_thetahat, likelihood_model){
  k_model = length(model_thetahat)
  return (2 * k_model - 2 * likelihood_model)
}

calculateBIC <- function(N, model_thetahat, likelihood_model){
  k_model = length(model_thetahat)
  return(k_model * log(N) - 2 * likelihood_model)
}

AIC_Model1 = calculateAIC(N, Model1_theta_hat, Likelihood_1)
BIC_Model1 = calculateBIC(N, Model1_theta_hat, Likelihood_1)
c(AIC_Model1, BIC_Model1)

AIC_Model2 = calculateAIC(N, Model2_theta_hat, Likelihood_2)
BIC_Model2 = calculateBIC(N, Model2_theta_hat, Likelihood_2)
c(AIC_Model2, BIC_Model2)

AIC_Model3 = calculateAIC(N, Model3_theta_hat, Likelihood_3)
BIC_Model3 = calculateBIC(N, Model3_theta_hat, Likelihood_3)
c(AIC_Model3, BIC_Model3)

AIC_Model4 = calculateAIC(N, Model4_theta_hat, Likelihood_4)
BIC_Model4 = calculateBIC(N, Model4_theta_hat, Likelihood_4)
c(AIC_Model4, BIC_Model4)

AIC_Model5 = calculateAIC(N, Model5_theta_hat, Likelihood_5)
BIC_Model5 = calculateBIC(N, Model5_theta_hat, Likelihood_5)
c(AIC_Model5, BIC_Model5)

list_models = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")
list_aic = c(AIC_Model1, AIC_Model2, AIC_Model3, AIC_Model4, AIC_Model5)
list_bic = c(BIC_Model1, BIC_Model2, BIC_Model3, BIC_Model4, BIC_Model5)
list_rss = c(Rss_Model1, Rss_Model2, Rss_Model3, Rss_Model4, Rss_Model5)
list_likelihood = c(Likelihood_1, Likelihood_2, Likelihood_3, Likelihood_4, Likelihood_5)
list_variance = c (Variance_Model1, Variance_Model2, Variance_Model3, Variance_Model4, Variance_Model5)

data.frame(Models = list_models,AIC = list_aic)
data.frame(Models = list_models,BIC = list_bic)

data.frame(Models = list_models, RSS = list_rss, AIC = list_aic, BIC = list_bic, Likelihood = list_likelihood)

data.frame(Models = list_models, Likelihood = list_likelihood, Variance = list_variance)



#-----------------------Task 2.5-----------------

calculateError <- function(y, y_hat){
  return(y-y_hat)
}

plotQQ <- function(model_error, title){
  error_fig = ggplot(data.frame(model_error), aes(sample = model_error))+
    geom_qq(color = "#195f90")+
    geom_qq_line(color = "red")
  return(ggplotly(error_fig) %>% layout(plot_bgcolor = '#e5ecf6', title= title, xaxis=list(title="Theoritical Quantities"), yaxis= list(title="Sample Quantities")))
}

Model1_Error = calculateError(df$x2, y_Hat_Model1)
plotQQ(Model1_Error, "QQ plot of model 1")

Model2_Error = calculateError(df$x2, y_Hat_Model2)
plotQQ(Model2_Error, "QQ plot of model 2")

Model3_Error = calculateError(df$x2, y_Hat_Model3)
plotQQ(Model3_Error, "QQ plot of model 3")

Model4_Error = calculateError(df$x2, y_Hat_Model4)
plotQQ(Model4_Error, "QQ plot of model 4")

Model5_Error = calculateError(df$x2, y_Hat_Model5)
plotQQ(Model5_Error, "QQ plot of Model 5")


#--------------------Task2.7-----------------

set.seed(100)
Split_Data = initial_split(df, prop = .8)
training_set = training(Split_Data)

testing_set = testing(Split_Data)


X_training_model = generateModel2(training_set)
X_testing_model = generateModel2(testing_set)
training_thetaHat = thetaHat(X_training_model, training_set$x2)

Y_testing_hat = X_testing_model %*% training_thetaHat
Y_training_hat = X_training_model %*% training_thetaHat

result = t.test(Y_training_hat, Y_testing_hat, mu = 500, alternative = "two.sided", conf.level = 0.95)
result

C_I1 = result$conf.int[1]
C_I2 = result$conf.int[2]
S_ERROR = result$stderr
S_ERROR

training_plot = ggplot(training_set, aes(x = x2))+
  stat_density(geom = "line", color = "#195f90")+
  geom_vline(xintercept = C_I1, linetype = "dashed", color = "red")+
  geom_vline(xintercept = C_I2, linetype = "dashed", color = "red")+
  geom_vline(xintercept = S_ERROR, linetype = "dashed", color = "black")
ggplotly(training_plot) %>% layout(plot_bgcolor = '#e5ecf6', title = "Distribution of training data", xaxis = list(title = "y"), yaxis = list(title = "Density"))

testing_plot = ggplot(testing_set, aes(x = x2))+
  stat_density(geom = "line", color = "#195f90")+
  geom_vline(xintercept = C_I1, linetype = "dashed", color = "red")+
  geom_vline(xintercept = C_I2, linetype = "dashed", color = "red")+
  geom_vline(xintercept = S_ERROR, linetype = "dashed", color = "black")
ggplotly(testing_plot) %>% layout(plot_bgcolor = '#e5ecf6', title = "Distribution of testing data", xaxis = list(title = "y"), yaxis = list(title = "Density"))


thetaHatTraining = thetaHat(X_training_model, training_set$x2)

dis_test = density(training_set$x2)
plot(dis_test, main = "Density plot of Y Signal")

z = 1.96
error=(as.matrix(testing_set$x2)- Y_testing_hat)

n_len = length(Y_testing_hat)
sd_error = sqrt(abs(sum(error^2)/n_len - 1))
sd_error

C_I_1 = z * sqrt(abs((error * (1 - error)) / n_len))
C_I_1

C_I_2 = z * sqrt(abs((error * (1 + error)) / n_len))
C_I_2


#---------------------------Task 3----------------------

Model2_theta_hat[4,]
Model2_theta_hat

arr_1 = 0
arr_2 = 0

f_value = 0
s_value = 0

thetabias <- Model2_theta_hat[4,]
thetaone <- Model2_theta_hat[1,]
thetatwo <- Model2_theta_hat[2,]
thetathree <- Model2_theta_hat[3,]
#thetanoise <- Model2_theta_hat[5,]

epsilon <- Rss_Model2 *2
num <- 100

counter <- 0
for (i in 1:num){
  p1 <- runif(1, -abs(thetabias), abs(thetabias))
  p2 <- runif(1, -abs(thetaone), abs(thetaone))
  abc_thetahat <- matrix(c(p1, p2, thetatwo, thetathree))
  abc_Y_Hat <- Model2 %*% abc_thetahat
  abc_RSS <- sum((df$x2 - abc_Y_Hat)^2)
  
  if (abc_RSS > epsilon){
    arr_1[i] <- p1
    arr_2[i] <- p2
    counter = counter+1
    f_value <- matrix(arr_1)
    s_value <- matrix(arr_2)
  }
}

abc_results = data.frame(f_value, s_value)  

abc_results

plot_ly(abc_results, x = ~f_value, y = ~s_value, type = "scatter", mode = "markers") %>% layout(plot_bgcolor = '#e5ecf6', title = "Joint and marginal Posterior Distribution", xaxis = list(title = "ABC thetabias"), yaxis = list(title = "ABC thetaone (X4)"))
