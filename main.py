#tetris implementation 
#7 diff pieces - each w 4 blocks moving down 

# L, R, U, D, Space key - all the way down to the bottom
#Collision Detection 
#Game ends when we collide 
#we keep moving by clearing rows 
#UI agnostic 


# Board - 2D Array - 0 empty and 1s if stuff is there - 10 X 20
# Shapes - 2D array as well  - list of coordinates 
# Soft update the board at each stage 
# If good then we can hard drop 


# Rotation:
# each of the 7 peieces have a point of rotation 
# - subtract a vector from that to get it to the origin - lets call it s
# - multiply each of those vectors that rep a shape by rotation matrix for 180 degrees 
# - add -s back to wtv u get after multiplying 


# I = [[(0, 1), (1, 1), (2, 1), (3, 1)]
#     - each shape becomes a bunch of coordiantes 