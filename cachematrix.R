#Peer graded assignment for David Ortega in the J.H  R programming course
#Week 3

#For this week we need a function which will calculate the inverse of a square
#matrix, which can be done for any square matrix A using solve(A).
#The focus is on lexical scoping and how parent environments can work on 
#some objects using the <<- operator.

#The function makeCacheMatrix() will return a list:
#calling the matrix and holding it's inverse.
#It will NOT calculate the inverse, because it may already contain it's inverse.

#For calculating and getting we will create another function called cacheSolve()
makeCacheMatrix <-function(A=matrix()){
        iA<-NULL #sets the inverse of a matrix to null when called for the first time
        set<-function(Af){
                A<<-Af # sets the A Matrix into a parent environment for it to
                #be manipulated with other functions
                iA<<-NULL #the same goes for the inverse when no matrix has
                #been set
        }
        get<-function()A #anonymous function which gets the matrix
        setiA<-function(solve) iA<<-solve #sets iA to be the inverse in a parent
        #environment
        getiA<-function()iA #anonymous function which gets the inverse
        list(set=set,get=get,setiA=setiA,getiA=getiA)
        #finally returns a named list to be called in the next function
}

#This function takes as an argument the list which was created and assigned
#to a variable using the makeCacheMatrix function
#This is why it can use the names given to the elements of the list
cacheSolve<-function(x,...){
        iA<-x$getiA()
        if(!is.null(iA)){
                message('getting cached data')
                return(iA)
                #if the value of iA has been set it is then retrived
        }
        Ati<-x$get() #gets matrix to inverse
        iA<-solve(Ati) #inverts matrix
        x$setiA(iA) #sets iA
        iA
        
}
#TEST
my_matrix<-makeCacheMatrix(matrix(c(1,4,-5,6,10,-1,-2,0,7),3,3))
my_matrix$get()
my_matrix$getiA()

cacheSolve(my_matrix)
my_matrix$getiA()

#value of the matrix can be reset with set
my_matrix$set(matrix(c(1,-4,5,6,-7,8,2,1,9),3,3))
my_matrix$get()
my_matrix$getiA()
cacheSolve(my_matrix)
my_matrix$getiA()
#Does not recalculate
cacheSolve(my_matrix)
