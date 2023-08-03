#####################################################
#---------------------------------------------------#
#       Higher Spheres: Information Theory I:       #
#   Approximate Bayes Inference, Relative Entropy   #
#             and the KL-Divergence                 #
#           by Steffen Schwerdtfeger                #
#---------------------------------------------------#
#####################################################


###########################
# Variational Free Energy #
###########################

# EntropyY is greater than or equal to H(y)
EntropyY>=EntropyPOST

# EntropyY-EntropyPOST >= 0
EntropyY-EntropyPOST >= 0

# Generative Model
prior = c(.5,.5); likelihood = c(.8,.2)
joint = prior*likelihood

# Trueposterior (here calculated via Bayes; for
# simulations think of a supervised situation,
# so the true state is known):
modelevidence = sum(joint)
Truepost = joint/modelevidence

# Expected model evidence = Entropy
# In our case Entropy and surprisal are equivalent.
Entropy = -sum(modelevidence*log(modelevidence)+((1-modelevidence)*log(1-modelevidence)))
Surprisal = -log(modelevidence)
Temperature = 1

# Going through all the lines:
# H = H
Entropy==Entropy
-log(sum(joint))==Entropy 
# The below does not exactly work with vectors/matrices.
# For a single value result use: -log(.5*.8/.8)
-log(Truepost*modelevidence/Truepost)==Entropy
-log(sum(Truepost*(Truepost*modelevidence/Truepost)))==Entropy
# With expected value notation, i.e., average surprisal
-sum(Truepost*log(Truepost*modelevidence/Truepost))==Entropy

# E[E] - H = H (for the example from ATUT p. 4)
Energy = -sum(Truepost*log(Truepost/Truepost))
HelmholtzFE = Energy-(-Temperature*Entropy)

# F >= H
HelmholtzFE>=Entropy

# F = H (minimized FE)
HelmholtzFE==Entropy

# Minimizing Free Energy: 
# Example ATUT p.5
Qs1 = c(.5,.5)
Energy1 = -sum(Qs1*log(Truepost/Qs1))
VFE1 = Energy1-(-Temperature*Entropy)
Qs2 = c(.6,.4)
Energy2 = -sum(Qs2*log(Truepost/Qs2))
VFE2 = Energy2-(-Temperature*Entropy)
Qs3 = c(.7,.3)
Energy3 = -sum(Qs3*log(Truepost/Qs3))
VFE3 = Energy3-(-Temperature*Entropy)
Qs4 = c(.8,.2)
Energy4 = -sum(Qs4*log(Truepost/Qs4))
VFE4 = Energy4-(-Temperature*Entropy)


#################
# KL Divergence #
#################

# We will use a matrix to express pi:
pColor = matrix(c(.5, .5))

Hbefore =-sum(pColor*(log2(pColor)))


# EXAMPLE:
# Imagine having 5 green and 5 blue balls:
plot(x = 1,                 
     xlab = "X Label", 
     ylab = "Y Label",
     xlim = c(0, 3), 
     ylim = c(0, 3),
     main = "Blue and green balls",
     type = "n")

# Blue balls
points(1,2, col = "Blue")
points(0.4,2.8, col = "Blue")
points(1.2,2.2, col = "Blue")
points(1.7,0.7, col = "Blue")
points(0.9,0.6, col = "Blue")

# Green balls
points(1.6,1.6, col = "Green")
points(1.9,2.6, col = "Green")
points(2.3,0.6, col = "Green")
points(2.8,2.85, col = "Green")
points(2.4,1.6, col = "Green")

# Split at X = 1.5.
abline(v=1.5, col="black")

# Hleft is simple
Hleft = -sum(1*log2(1))

# Hleft probs as vector
probRight = c(1/6,5/6)
Hright = -sum(probRight*log2(probRight))

# Relative Enropy
Hsplit = .4*0 + .6*.65

# KL divergence / relative entropy:
Informationgain = Hbefore - Hsplit


#######################
# Jensen's Inequality #
#######################

# Define function x^2 = g(x) =
g <- function(x) (x^2)

# Plot of g(x) ranging from x=-1 to -7.
curve(g, -1, 6, ylab = "g(x)")
abline(v = 0)

# Points marking all possible payoffs
points(1,g(1)) # Payoff for x = 1
points(2,g(2)) #   ...  for x = 2
points(3,g(3)) #   ...
points(4,g(4)) # 
points(5,g(5)) # 
points(6,g(6)) #


# expected payoff from the game      ≥      payoff from expeted value of X
# expectation of a funcition                function of an expectation
#         E[g(x)]                    ≥        g[E(x)] => if g is convex

EX = sum(1/6*(1:6))
gEX = g(EX) 
Egx = sum(1/6*g(1:6))

# Jensen's inequality
# E[g(x)] ≥ g[E(x)]
Egx >= gEX

# gEX and Egx at x=EX
points(EX, gEX, col="blue") # x = EX, g(x) = gEX
points(EX,Egx, col="red")   # x = EX, g(x) = Egx

# We will now draw a linear function that goes through
# g(x) = Egx
# Slope b of f(x)=y=a+bx via two points:
P1 = matrix(c(0, 0))    # x =  0, g(x) =   0
P2 = matrix(c(EX,Egx))  # x = EX, g(x) = Egx
b = (P2[2]-P1[2])/(P2[1]-P1[1])

# We can now evaluate a by filling in a point
# say P2 in y = a + bx => a = -y + bx
a = -0+b*0

# f(x) = 4.33333*x
f = function(x) (b*x)
# add to plot
curve(f, -1, 6, ylab = "x", add=TRUE)

# In our case the value of x of the point where
# f(x) crosses g(x) is equal to the slope of 
# f(x), so we can evaluate the value of y 
# of our point via a shortcut. 
y = b*b

# Crossing points: 
# upper crossing point f(x) with g(x)
# where Egx=gEX!
segments(x0=0,y0=y, x1= b, y1=y, lty =3)
segments(x0=b,y0=0, x1= b, y1=y, lty =3)
text(x=5.3,y=18, label="E[g[4.33]]=g[E[4.33]]", srt = 3, col = "darkgreen")
text(x=-.6,y=y, label="g(x)=g(4.33)", srt = 3, col = "darkgreen")
text(x=b+.5,y=0.2, label="x=4.33", srt = 3, col = "darkgreen")

# Check P1:
EXP1 = sum(1/6*(0))
gEXP1 = f(EXP1) 
EgxP1 = sum(1/6*f(0))
# Is exactly equal?
EgxP1 == gEXP1

# Check P2:
EXP2 = sum(1/6*(b))
gEXP2 = f(EXP2) 
EgxP2 = sum(1/6*f(b))
# Is exactly equal?
EgxP2 == gEXP2

# Add rest of the points and some annotation:
# (EX|Egx)
segments(x0=0,y0=Egx, x1= EX, y1=Egx, lty =3)
segments(x0=EX,y0=0, x1= EX, y1=Egx, lty =3)
text(x=-.6,y=Egx, label="E[g[x]]", srt = 3, col = "red")
text(x=EX+.35,y=0.2, label="x=E[X]", srt = 3, col = "black")


# (EX|gEX)
segments(x0=0,y0=gEX, x1= EX, y1=gEX, lty =3)
segments(x0=EX,y0=0, x1= EX, y1=gEX, lty =3)
text(x=-.6,y=gEX, label="g[E[x]]", srt = 3, col = "blue")


