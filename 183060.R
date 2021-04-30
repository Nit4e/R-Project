mat <- read.table("D:\\FINKI\\Na Anka za sa\\Vtor Semestar 2018 - 2019\\activities.csv", header = TRUE, sep = ",")
d <- c(mat$Calories..Raw.)
n <- length(d)
n
d <- sort(d)
int <- seq(100, 1300, by = 150)
k <- cut(d, int)
fi <- table(k)
fi
rel.fi <- fi / n
rel.fi
kum.fi <- cumsum(fi)
kum.fi
rel.kum.fi <- cumsum(fi) / n
rel.kum.fi
pfi <- rel.fi * 100
pkumfi <- rel.kum.fi * 100
d.table <- cbind(fi, rel.fi, kum.fi, pfi, pkumfi)
d.table

sr <- d
for(i in 1: length(int) - 1){
  sr <- c(sr, (int[i] + int[i+1]) / 2)
}

h <- hist(d, right = FALSE, breaks = int, col = "skyblue", main = "Calories burned", xlab = "Calories", ylab = "frequencies")


boi <- c("purple", "hotpink", "lightpink", "pink")
h <- hist(d, right = FALSE, breaks = int, col = boi, main = "Calories burned", xlab = "Calories", ylab = "frequencies")

p2 <- c(0, pfi)
plot(int, p2, axes = F, main = "Polygon of relative frequencies in %", xlab = "Intervals", ylab= "Relative frequencies in %")
axis(side = 1, at = int)
axis(side = 2)
lines(int, p2)

d1 <- c(mat$Temperature..Raw.)
d1 <- sort(d1)
d1
stem(d1, scale = 2)

d2 <- c(mat$Min..Elevation..Raw.)
d2 <- sort(d2)
plot(d2, d, main = "Calories burned vs. Min. Elevation", xlab = "Min. Elevation", ylab = "Calories burned")

modaf <- function(d){
  moda.d <- unique(d)
  moda.d[which.max(tabulate(match(d, moda.d)))]
}
moda <- modaf(d)
moda
medijana <- median(d)
medijana
prosek <- mean(d)
prosek

Q1 <- quantile(d, 0.25)
Q2 <- quantile(d, 0.50)
Q3 <- quantile(d, 0.75)
Q1
Q2
Q3
IQ <- Q3 - Q1
IQ
opseg <- max(d) - min(d)
opseg

disperzija <- sd(d)^2
disperzija
st.devijacija <- sd(d)
st.devijacija

korelacija <- cor.test(d, d2)
korelacija

z <- qnorm(0.975)
z
greska <- z * st.devijacija / sqrt(n)
greska
dolna_granica <- prosek - greska
gorna_granica <- prosek + greska
dolna_granica
gorna_granica

z1 <- qnorm(0.95)
z1
mi <- 500
e <- (prosek - mi) / (st.devijacija / sqrt(n))
e

l_regresija <- lm(d ~ d2)
summary(l_regresija)
plot(d2, d, main = "Regression analysis", xlab = "Min. Elevation Raw", ylab = "Calories Raw")
abline(l_regresija, col = 3, lwd = 3)

