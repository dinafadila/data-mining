ins <- read.csv("~/insurance.csv")

# str : buat tau tipe datanya
str(ins)

# summary : mean,median,kuartil,dll
summary(ins$age)

# simpangan baku
sd(ins$age)

# box plot, ylab : buat label
boxplot(ins$bmi, main="Boxplot of Insurance BMI",ylab="BMI") 

# histogram
hist(ins$charges, main="Histogram of Insurance Charges",xlab="Charges")

# scatterplot : buat liat pencilan
plot(x=ins$bmi, y=ins$charges, main="Scatterplot of BMIvs Charges", xlab="BMI", ylab="Charges")

# density : melihat persebaran, tersebar normal, kekiri / kekanan
plot(density(ins$bmi), main='Density Plot For InsuranceBMI')



#-------------- eksplorasi data multivariable ---------------------

# menggunakan covarian dan korelasi, buat dapet korelasi itung dulu covarian

# covarian 
cov(ins$age, ins$bmi)
cov(ins[, c("age", "bmi", "children", "charges")])

# korelasi [-1.1]
cor(ins$age, ins$bmi)
cor(ins[, c("age", "bmi", "children", "charges")])

# summary bmi terhadap sex(female,male)
aggregate(bmi ~ sex, summary, data = ins)\

# boxplot bmi terhadap age
boxplot(bmi ~ age, data = ins)

#-------------------- Scatter Plot ----------------------

# Scatter plot
with(ins, plot(age, bmi, col = sex, pch =as.numeric(sex)))

# Scatter plot with jitter, jitter buat nambah noise 
with(ins, plot(jitter(children), jitter(age)))

# Scatter plot with ggplot2
library(ggplot2)
qplot(age, charges, data = ins, facets = sex ~ .)

# Static 3D scatter plot
library(scatterplot3d)
with(ins, scatterplot3d(age, bmi, charges))

# Interactive 3D scatter 
plotlibrary(rgl)
with(ins, plot3d(age, bmi, charges))

# pairs : melihat keseluruhan scatter plot
pairs(ins[, c("age", "bmi", "children", "charges")])

# heat map
distMatrix <- as.matrix(dist(ins[, c("age", "bmi","children", "charges")]))
heatmap(distMatrix)

# level plot
library(lattice)
levelplot(children ~ bmi * age, ins, cuts = 5,col.regions = grey.colors(6)[6:1]) #6 baris 1 kolom, warnanya 6

# contour 
filled.contour(volcano, color = terrain.colors, asp = 1,plot.axes = contour(volcano, add = T))

# 3D Surface
persp(volcano, theta = 25, phi = 30, expand = 0.5, col ="pink")

#------------------------ Pararel Coordinate ---------------------

# Parallel coordinates : membaca data satu instance, misal 1 istance -> age rendah, bmi tinggi, child rendah,dll
# terus juga buat liat kalo umur tinggi rata2 bmi nya tinggi, dll
library(MASS)
parcoord(ins[, c("age", "bmi", "children", "charges")],col = ins$sex)

# Parallel coordinates with lattice, membagi berdasarkan female male
library(lattice)
parallelplot(~ins[, c("age", "bmi", "children","charges")] | sex, data = ins)

#------------------------ Simpan ke file --------------------------------

# buat ngatur 
# Save as a PNG file
png(“myPlot.png”)
plot(1:10, log(1:10))
graphics.off()

# Save as a JPEG file
jpeg(“myPlot.jpeg”)
plot(1:10, log(1:10))
graphics.off()

# Save as a PDF file
pdf(“myPlot.pdf”)
plot(1:10, log(1:10))
graphics.off()

# Save as a Postscript file
postscript(“myPlot.ps”)
plot(1:10, log(1:10))
graphics.off()




