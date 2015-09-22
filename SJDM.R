Benchmarking=function(temp, i){
  start=1
  runs=i-temp
  resultVector = matrix(nrow = runs, ncol = 4)
  result = 0
  result2 = 0
  avrageSJ=0
  avrageBasic=0
  avrageWins=0
  avragePr=0
  best=10000000
  worst=0
  while (temp < i){
    print(temp)
    set.seed(temp)
    result = runDeliveryMan(carReady = SJDM, dim = 10, turns = 2000, pause = 0.1, del = 5)
    resultVector[start, 1] = result
    avrageSJ=avrageSJ+result
    set.seed(temp)
    if (result<best){best=result}
    else if(result>worst){worst=result}
    result2 = runDeliveryMan(carReady = basicDM, dim = 10, turns = 2000, pause = 0.1, del = 5)
    resultVector[start, 2] = result2
    avrageBasic=avrageBasic+result2
    resultVector[start, 3] = result - result2
    avrageWins=avrageWins+(result - result2)
    if((result - result2)>0){
      resultVector[start, 4] = 1
      avragePr=avragePr+1
    } else{
      resultVector[start, 4] = 0
    }
    temp = temp + 1 
    start= start +1
  }
  
  print(resultVector)
  
  
  print("avrage turns for SJ")
  print(avrageSJ/runs)
  print("avrage turns for Basic")
  print(avrageBasic/runs)
  print("avrage skillnad i turer")
  print(avrageWins/runs)
  print("% förluster")
  print(avragePr/runs)
  
  print("Bäst antal rundor")
  print(best)
  print("Värsta antalet rundor")
  print(worst)
  
}

BenchmarkingFast=function(temp, i){
  start=1
  runs=i-temp
  resultVector = matrix(nrow = runs, ncol = 4)
  result = 0
  result2 = 0
  avrageSJ=0
  avrageBasic=0
  avrageWins=0
  avragePr=0
  dpPlot=F
  
  best=10000000
  worst=0
  
  while (temp < i){
    print(temp)
    set.seed(temp)
    result = runDeliveryMan(carReady = SJDM, dim = 10, turns = 2000, pause = 0, del = 5)
    resultVector[start, 1] = result
    avrageSJ=avrageSJ+result
    if (result<best){best=result}
    else if(result>worst){worst=result}
    set.seed(temp)
     result2 = runDeliveryMan(carReady = basicDM, dim = 10, turns = 2000, pause = 0, del = 5)
     resultVector[start, 2] = result2
     avrageBasic=avrageBasic+result2
    resultVector[start, 3] = result - result2
    avrageWins=avrageWins+(result - result2)
    if((result - result2)>0){
      resultVector[start, 4] = 1
      avragePr=avragePr+1
    } else{
      resultVector[start, 4] = 0
    }
    temp = temp + 1 
    start= start +1
  }
  
  print(resultVector)
  
  
  print("avrage turns for SJ")
  print(avrageSJ/runs)
  print("avrage turns for Basic")
  print(avrageBasic/runs)
  print("avrage skillnad i turer")
  print(avrageWins/runs)
  print("% förluster")
  print(avragePr/runs)
  
  print("Bäst antal rundor")
  print(best)
  print("Värsta antalet rundor")
  print(worst)
  
  }

SJDM=function(roads,car,packages) {

  #store coordinates for the green packeges
  tmp = list()
  tmpBestH = 1000
  if(car$load == 0){
    for (i in 1:nrow(packages)) {
      if(packages[i,5]==0){
        currentX = car$x
        currentY = car$y
        currentH = abs(currentX - packages[i,1]) + abs(currentY - packages[i,2])
       
        if(tmpBestH > currentH){
          tmp = i
          tmpBestH = currentH
        }
      }
    }
  } else {
    tmp = car$load
  }
  car$mem = tmp

  if(car$load==0){
    px = unlist(packages[car$mem,1])
    py = unlist(packages[car$mem,2])
  } else{
    px = unlist(packages[car$mem,3])
    py = unlist(packages[car$mem,4])
  }
  currentX = car$x
  currentY = car$y
  currentH = (abs(currentX - px) + abs(currentY - py))
  currentG = 0
  currentF = currentG + currentH

  current = list(x = currentX, y = currentY, H = currentH, G = currentG, F = currentF, prevX = 0, prevY = 0, isCheck = 0)
  openlist = list()
  closedlist = list()
  openlist[[length(openlist)+1]] <- current

# 
#   if(car$load > 0){
#     goalNode = list(x = packages[car$load, 3], y=packages[car$load, 4])
#   } else{
#     goalNode = list(x=px, y=py)
#   }
  goalNode = list(x=px, y=py)
#   
#   print(paste("GoalNode"))
#   print(paste("x1:", goalNode$x, "y1: ", goalNode$y))
#   
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
#print(paste("Next move should be: ", nextMove))
#car$nextMove=readline("Enter next move.")
  car$nextMove = nextMove
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  return (car)
}

getNewNode<-function(current, parent, roads,  goal) {
  currentX = current$x
  currentY = current$y
  difX = currentX - goal$x
  difY = currentY - goal$y
  currentH = (abs(difX) + abs(difY))
  
  difX = currentX - parent$x
  difY = currentY - parent$y

  if(difX > 0)      {currentG = roads$hroads[parent$y, parent$x]} 
  else if(difY > 0) { currentG = roads$vroads[parent$y, parent$x]}
  else if(difY < 0) { currentG = roads$vroads[currentY, currentX]}
  else if(difX < 0) { currentG = roads$hroads[currentY, currentX]}
  else { currentG = 0}

  currentG = currentG + parent$G + 2
  currentF = currentG + currentH

  current = list(x = currentX, y = currentY, H = currentH, G = currentG, F = currentF, prevX = parent$x, prevY = parent$y, isCheck = 0)
  return (current)
}

openlistAdd <- function(openlist, closedlist, node) {
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
  for (elem in closedlist) {
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

  #gå igenom open list efter minsta F värdet.
  current = list()
  bestF = 10000
  for(node in openlist){
    if(node$F < bestF){
      bestF = node$F
    } 
  }
  for(i in 1:length(openlist)){
    node = openlist[[i]]
      if(node$F == bestF){
        #add to closedlist
        closedlist[[length(closedlist)+1]] = node
        #set the node with best F value to current 
        current = node
        #remove from openlist
        openlist[[i]] = NULL
        break
      } 
    }

  #Är detta mål?
  #om ja, retunera closedlist, annars fortsätt.
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
    openlist = openlistAdd(openlist,closedlist, leftNode)
  } 
  
  #find top
  if(current$y != nrow(roads$hroads)){
    newY = current$y + 1
    # call getNode
    topNodeCoor = list(x = current$x, y = newY)
    # goalNode = list(x = px, y=py)
    topNode = getNewNode(topNodeCoor, current, roads, goalNode)
    openlist = openlistAdd(openlist,closedlist, topNode)
  } 
  
  #find right
  if(current$x != ncol(roads$vroads)){
    newX = current$x + 1
    # call getNode
    rightNodeCoor = list(x = newX, y = current$y)
    # goalNode = list(x = px, y=py)
    rightNode = getNewNode(rightNodeCoor, current, roads, goalNode)
    openlist = openlistAdd(openlist,closedlist, rightNode)
  } 
  
  #find bottom
  if(current$y != 1){
    newY = current$y -1
    # call getNode
    bottomNodeCoor = list(x = current$x, y = newY)
    # goalNode = list(x = px, y=py)
    bottomNode = getNewNode(bottomNodeCoor, current, roads, goalNode)
    openlist = openlistAdd(openlist,closedlist, bottomNode)
  }

  
  AStar(roads,openlist,closedlist,goalNode)
}
