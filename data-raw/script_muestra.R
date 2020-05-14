### script muestra

data(iris)
Sys.sleep(20)
S <- mean(iris$Sepal.Length)
write.csv(S, "S.csv")
