SJDM=function(roads,car,packages) {
  
  #store coordinates for the green packeges
  if (length(car$mem) == 0) {
    #xCoor = matrix(nrow = nrow(packages), ncol = 1)
    #yCoor = matrix(nrow = nrow(packages), ncol = 1)
    tmp = list()
    for (i in 1:nrow(packages)) {
      #xCoor[i] = packages[i,1]
      #yCoor[i] = packages[i,2]
      tmp = c(tmp, packages[i,1])
      tmp = c(tmp, packages[i,2])
    }
    #ourPackages = list(x=xCoor, y=yCoor)
    #print(tmp)
    #print("CONS")
    #print("x1:", tmp[1]$x, "y1:", tmp[1]$y)
    
    
    car$mem = tmp
  }
########################## First state
  #print(car$mem)
  px = unlist(car$mem[1])
  py = unlist(car$mem[2])
  print(paste("GoalNode"))
  print(paste("x1:", px, "y1: ", py))
  
  currentX = car$x
  currentY = car$y
  currentH = abs(currentX - px) + abs(currentY - py)
  currentG = 0
  currentF = currentG + currentH

  current = list(x = currentX, y = currentY, H = currentH, G = currentG, F = currentF, prevX = 0, prevY = 0, isCheck = 0)
  openlist = list()
  closedlist = list()
  
  print(paste("CurrentNode"))
  print(current)

  ################ get neighbours

  #find left
  if(current$x != 1){
    newX = current$x -1
    # call getNode
  }

  #find top
  if(current$y != nrow(roads$hroads)){
    newY = current$y + 1
    # call getNode
    topNodeCoor = list(x = current$x, y = newY)
    goalNode = list(x = px, y=py)
    topNode = getNewNode(topNodeCoor, current, roads, goalNode)
    print(paste("TopNode"))
    print(topNode)
  }

  #find right
  if(current$x != ncol(roads$vroads)){
    newX = current$x + 1
    # call getNode
  }

  #find bottom
  if(current$y != 1){
    newY = current$y -1
    # call getNode
  }

#  index = length(closedlist)
#  if(index == 0){
#    closedlist[index + 1] = current
#  } else {
#    bool = true
#    for (item in closedlist) {
#      if(item$x = current$x && item$y = current$y) {bool = false}
#    }
#    if (bool){closedlist[index + 1] = current}
#  }




  #print(packages)
  print(roads)
  #print(car)
  
  
  
  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
  }  
  car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  return (car)
}

getNewNode<-function(current, parent, roads,  goal) {
  currentX = current$x
  currentY = current$y
  difX = currentX - parent$x
  difY = currentY - parent$y
  currentH = abs(difX) + abs(difY)
  
  if(difX > 0)      {currentG = roads$vroads[parent$x, parent$y]} 
  else if(difY > 0) { currentG = roads$hroads[parent$x, parent$y]}
  else if(difY < 0) { currentG = roads$hroads[currentX, currentY]}
  else if(difX < 0) { currentG = roads$vroads[currentX, currentY]}
  else { currentG = 0}

  currentG = currentG + parent$G
  currentF = currentG + currentH

  current = list(x = currentX, y = currentY, H = currentH, G = currentG, F = currentF, prevX = parent$x, prevY = parent$y, isCheck = 0)
  return (current)
}


# if car -> load 0  basfall om bilens kordinater är målkordinater then wait
#   om grön -> hitta paket (A* alla gröna som inte har en 2a i sista kolumnen)
#     while gröna punker ej genomsökta (Utför A*) 
#   om röd -> titta igenom sista kolumnen efter vilken röd punkt vi ska till, sen A* it!!!!
#     Utför A*
#       Beräkna H värdet för den nuvarande punkten samt sen beräkna G och F tar sen nästa punkt
#       Hittar tillbaks genom prefChepestKordinaten till bilen och tar då och flyttar oss till
#       värdet som länkar till bilens kordinat.
#
#
# ejGenomgåda[int id]
# genomgåda[int id]
# [Punkt(
#       int id,
#       kordinat(int x,int y),
#       prevChepestKordinat(int x,int y),
#       int h, int g, int f)]
