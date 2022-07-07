from matplotlib import pyplot as plt
import numpy as np
from discreteTimeAnalysis import *
import math

A = DiscreteDistribution([4,8,20],[0.4,0.5,0.1])
B = DiscreteDistribution([4,5,6],[0.2,0.1,0.7])
C = B-A


Wn1 = DET(0) # empty system
Wn = DET(1)  # just for initialization

# power method
while Wn != Wn1: # comparison based on means of the distributions
  Wn = Wn1
  Wn1 = max( Wn+C ,0)
