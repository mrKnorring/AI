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
  openlist[[length(openlist)+1]] <- current

  #print(paste("CurrentNode"))
  #print(current)
  goalNode = list(x=px, y=py)
  
  ################ get neighbours
  closedlist = AStar(roads,openlist,closedlist,goalNode) 
  
  #Titta igenom closedList
  currentNode = closedlist[[length(closedlist)]]
  closedlist[[length(closedlist)]] = NULL
    while(length(closedlist) > 1){

  if(currentNode$prevX == closedlist[[length(closedlist)]]$x && currentNode$prevY == closedlist[[length(closedlist)]]$y){
      currentNode = closedlist[[length(closedlist)]]
  } 
    closedlist[[length(closedlist)]] = NULL
      }
  
  #print(openlist)
  #print(packages)
  #print(roads)
  #print(car)

  # Get witch direction we shall go
#   bestF = 10000
#   bestX = 0
#   bestY = 0
#   for (node in openlist) {
#     if(node$F < bestF){
#       bestF = node$F
#       bestX = node$x
#       bestY = node$y
#     }
#   }

#   difX1 = current$x - bestX
#   difY1 = current$y - bestY

  difX1 = current$x - currentNode$x
  difY1 = current$y - currentNode$y
  
  
  #print(paste("x: ", bestX, "y: ", bestY))

  nextMove = 0
  if(difY1 > 0)      {nextMove = 2}
  else if(difX1 > 0) {nextMove = 4}
  else if(difY1 < 0) {nextMove = 8}
  else if(difX1 < 0) {nextMove = 6}
  else              {nextMove = 5}
  
#  if (car$load>0) {
#    print(paste("Current load:",car$load))
#    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
#  }  
#  
#  car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  print(paste("Next move should be: ", nextMove))
  car$nextMove=readline("Enter next move.")
#  car$nextMove = nextMove
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  return (car)
}

getNewNode<-function(current, parent, roads,  goal) {
  currentX = current$x
  currentY = current$y
  difX = currentX - goal$x
  difY = currentY - goal$y
  currentH = abs(difX) + abs(difY)
  
  difX = currentX - parent$x
  difY = currentY - parent$y

  if(difX > 0)      {currentG = roads$hroads[parent$y, parent$x]} 
  else if(difY > 0) { currentG = roads$vroads[parent$y, parent$x]}
  else if(difY < 0) { currentG = roads$vroads[currentY, currentX]}
  else if(difX < 0) { currentG = roads$hroads[currentY, currentX]}
  else { currentG = 0}

  currentG = currentG + parent$G + 1
  currentF = currentG + currentH

  current = list(x = currentX, y = currentY, H = currentH, G = currentG, F = currentF, prevX = parent$x, prevY = parent$y, isCheck = 0)
  return (current)
}

openlistAdd <- function(openlist, node) {
  bool = 0
  for (elem in openlist) {
    if(elem$x == node$x && elem$y == node$y){
      if(node$F < elem$F){
        elem = node
      }
      bool = 1
      break
    }
  }
  if(bool == 0){
    openlist[[length(openlist)+1]] = node
  }
  return (openlist)
}




AStar=function(roads,openlist,closedlist,goalNode) {
  
  #g� igenom open list efter minsta F v�rdet.
  current = list()
  bestF = 10000
  for(node in openlist){
      if(node$F < bestF){
        bestF = node$F
      } 
  }
    for(i in 1:length(openlist)){
      node = openlist[[i]]
      #print(node)
      if(node$F == bestF){
        #print(bestF)
        #add to closedlist
        closedlist[[length(closedlist)+1]] = node
        #set the node with best F value to current 
        current = node
        #remove from openlist
        openlist[[i]] = NULL
        break
      } 
    }
  
  #�r detta m�l?
  #om ja, retunera closedlist, annars forts�tt.
  if(closedlist[[length(closedlist)]]$x == goalNode$x && closedlist[[length(closedlist)]]$y == goalNode$y){
    return(closedlist) 
  }
  
  #find left
  if(current$x != 1){
    newX = current$x -1
    # call getNode
    leftNodeCoor = list(x = newX, y = current$y)
    # goalNode = list(x = px, y=py)
    leftNode = getNewNode(leftNodeCoor, current, roads, goalNode)
    openlist = openlistAdd(openlist, leftNode)
    # print(paste("leftNode: ", leftNode$F))
  } 
  
  #find top
  if(current$y != nrow(roads$hroads)){
    newY = current$y + 1
    # call getNode
    topNodeCoor = list(x = current$x, y = newY)
    # goalNode = list(x = px, y=py)
    topNode = getNewNode(topNodeCoor, current, roads, goalNode)
    openlist = openlistAdd(openlist, topNode)
    # print(paste("topNode: ", topNode$F))
  } 
  
  #find right
  if(current$x != ncol(roads$vroads)){
    newX = current$x + 1
    # call getNode
    rightNodeCoor = list(x = newX, y = current$y)
    # goalNode = list(x = px, y=py)
    rightNode = getNewNode(rightNodeCoor, current, roads, goalNode)
    openlist = openlistAdd(openlist, rightNode)
    # print(paste("rightNode: ", rightNode$F))
  } 
  
  #find bottom
  if(current$y != 1){
    newY = current$y -1
    # call getNode
    bottomNodeCoor = list(x = current$x, y = newY)
    # goalNode = list(x = px, y=py)
    bottomNode = getNewNode(bottomNodeCoor, current, roads, goalNode)
    openlist = openlistAdd(openlist, bottomNode)
    # print(paste("bottomNode: ", bottomNode$F))
  }

  
  AStar(roads,openlist,closedlist,goalNode)
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
