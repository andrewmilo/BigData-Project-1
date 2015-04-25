package HyperGeometricDistribution

import scala.math.BigDecimal

object HyperGeometricDistribution {
 
   def hyperGeoDistrib(popSize : Int, successStates: Int, draws:Int, sucesses:Int): Double = {
    (effBinomalCoeff(successStates, sucesses) * effBinomalCoeff(popSize - successStates, draws - sucesses))/effBinomalCoeff(popSize, draws)
  }
   
   def effBinomalCoeff(n: Int, k: Int): Double = {
    
    //Check and set newK depending on what is smaller (k or n-k) since n choose k == n choose (n-k)
    var newK = k
    if(k > n - k)
      newK = n - k
    
    //set the count i to 0
    var i = 0
    
    @annotation.tailrec 
    def ebc(n: Int, k : Int, acc: Double) : Double = {
      // If the count i is equal to the k value, return the accumulator 
      if (i == k) acc 
      //Otherwise increment i and call the function with n-1 (the next n), the same k, and the updated accumulator
      //Accumulator is updated by multiplying it with the current n and dividing by i.
      else {
        i += 1 
        ebc(n - 1, k, (acc * n) / i) 
      } 
    } 
    
    ebc(n, newK, 1.0)
    
    /* ITERATIVE VERSION
    var newK = k
    
    if(k > n - k)
      newK = n - k
    
    var acc = 1.0
    for(i <- 0 until newK){
      acc *= (n - i)
      acc /= (i + 1)
    }
    acc 
    */
  }
  //PREVIOUS VERSION OF binomial Coefficient using the factorial function below
  def binomialCoeff(n: Int, k: Int): BigDecimal = {   
     factorial(n) / (factorial(k) * factorial(n - k)) 
  }
 
  def factorial(n: Int): BigDecimal = {
    @annotation.tailrec
    def fact(n: BigDecimal, acc: BigDecimal): BigDecimal = {
      if (n == 0) acc
      else 
        fact(n - 1, n * acc)
    }
    fact(n, 1)
  }
}