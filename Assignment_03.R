library(ggplot2)

Name <- c("Jeb", "Donald", "Ted", "Marco", "Carly", "Hillary", "Bernie")
ABC_poll   <- c(  4,      62,      51,    21,      2,        14,       15)
CBS_poll   <- c( 12,      75,      43,    19,      1,        21,       19)

df_polls <- data.frame(Name, ABC_poll, CBS_poll)

str(df_polls)
head(df_polls)

# Compute Mean, Median, and Range

mean(df_polls$ABC_poll)
mean(df_polls$CBS_poll)
median(df_polls$ABC_poll)
median(df_polls$CBS_poll)
range(df_polls[, c("ABC_poll","CBS_poll")])

# Adds Column for difference between ABC and CBS
df_polls$Diff <- df_polls$CBS_poll - df_polls$ABC_pol

ggplot(df_polls) +
  geom_bar(stat = "identity", aes(Name, Diff))


ggplot(df_polls) +
  geom_bar(stat = "identity", aes(Name, ABC_poll))