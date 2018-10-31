# -*- coding: utf-8 -*-
"""
Created on Wed Oct 31 13:25:02 2018

@author: mvasilev
"""
# from: https://osdoc.cogsci.nl/3.1/visualangle/

def visAngle(height, width, distance, size):
    
    from math import atan2, degrees
    # height: Monitor height in cm
    # distance: Distance between monitor and participant in cm
    # width: Vertical resolution of the monitor
    # size: The stimulus size in pixels
    
    # Calculate the number of degrees that correspond to a single pixel. This will
    # generally be a very small value, something like 0.03.
    deg_per_px = degrees(atan2(.5*height, distance)) / (.5*width)

    # Calculate the size of the stimulus in degrees
    size_in_deg = size * deg_per_px
    
    return size_in_deg