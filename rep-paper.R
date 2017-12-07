
# Replicating script ------------------------------------------------------
# Author: Alejandro Jiménez Rico <aljrico@gmail.com>

# Parameters to set
capK=0
alfa=0.0343
s=0.1544
capA=1
gamma=1-(alfa/(s*s*capA))
r=0
capT=30

x_0 = a*exp((alfa*pi_b - 0.5*s*s*pi_b*pi_b)*(capT - s) + s*pi_b*(w_t))
