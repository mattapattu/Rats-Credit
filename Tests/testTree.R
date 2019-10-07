library(data.tree)
library(networkD3)

DATA <- Node$new("Experiment in Marseille")
  region1 <- DATA$AddChild("SDM")
    rat1 <- region1$AddChild("rat_1")
    rat2 <- region1$AddChild("rat_2")
      session1 = rat2$AddChild("session_1", POS=2, EVENTS=list())
    
  region2 <- DATA$AddChild("SDL")
    rat3 <- region2$AddChild("rat_3")
    rat4 <- region2$AddChild("rat_4")
      session1 = rat3$AddChild("session_1", POS=3, EVENTS=list())

print(DATA)

setwd("~/ExpComputabrain/Data/")
pdf(paste(getwd(),"/SDM113/","tst",sep=""))
#Close to print pdf file
plot(c(1:20),c(1:20))
dev.off()

# SIMPLE TESTS

# Navigate thru the tree and find nodes
DATA$SDL$rat_3$session_1$POS
DATA$SDL$rat_3$session_1$name

str_detect(DATA$SDL$rat_3$name,"rat")

FindNode(DATA,"session_1")$POS
path=FindNode(DATA,"session_1")$pathString #[1]
nwPath=substr(path,str_locate(path,"/")[1],str_length(path)) 

DATA$fieldsAll

DATA$Get('name', filterFun = isLeaf)

print(DATA,"level")

DATA$Get('name', filterFun = function(x) x$level == 3)


#######################################
data(acme)
acme$Set(departmentId = 1:acme$totalCount, openingHours = NULL, traversal = "post-order")
acme$Set(head = c("Jack Brown", 
                  "Mona Moneyhead", 
                  "Dr. Frank N. Stein", 
                  "Eric Nerdahl"
),
filterFun = function(x) !x$isLeaf
)
print(acme, "departmentId", "head")

data(acme)

str(ToListSimple(acme))
str(ToListSimple(acme, keepOnly = "cost"))

str(ToListExplicit(acme))
str(ToListExplicit(acme, unname = TRUE))
str(ToListExplicit(acme, unname = TRUE, nameName = "id", childrenName = "descendants"))


useRtreeList <- ToListExplicit(DATA, unname = TRUE)
radialNetwork(List=useRtreeList)

str(ToListSimple(DATA))
str(ToListSimple(acme, keepOnly = "cost"))

str(ToListExplicit(acme))
str(ToListExplicit(acme, unname = TRUE))
str(ToListExplicit(acme, unname = TRUE, nameName = "id", childrenName = "descendants"))

Navigate(DATA,path="rat_3/session_1'")

DATA$Get('name', filterFun = isLeaf)


DATA$Get("rat 3")$`session 1`$positions$POS
DATA$Get("POS",  filterFun = isLeaf)
DATA$Climb(position = 1, name="positions")

session1$fieldsAll
