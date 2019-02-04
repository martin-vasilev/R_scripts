# -*- coding: utf-8 -*-
"""
Created on Wed Oct 31 13:25:02 2018

@author: mvasilev
"""
# from: https://osdoc.cogsci.nl/3.1/visualangle/

def visAngle(h= 70, d= 80, r= 1920, size=12):
    
   from math import atan2, degrees
   # Calculate the number of degrees that correspond to a single pixel. This will
   # generally be a very small value, something like 0.03.
   deg_per_px = degrees(atan2(.5*h, d)) / (.5*r)
   print('%s degrees correspond to a single pixel' % deg_per_px)
   # Calculate the size of the stimulus in degrees
   size_in_deg = size * deg_per_px
   print('The size of the stimulus is %s pixels and %s visual degrees' \
        % (size, size_in_deg))
