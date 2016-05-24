# I will need to cluster the masts that belong to the same semantic
#   location in order to sensibly infer my location vectors
# In order to find a sensible clustering, I would like to evaluate the
#   dispersion of mast-locations if the given location is fixed.
# First measure:  Distribution of distance to mast for stop segments
# Second measure: Topological distance between the real cell of a stop and
#                 the connected mast
# Third measure:  Csaj-Approach: double the distance between the mast from the
#                 active voronoi cell and see whether the active mast is in
#                 that set.

