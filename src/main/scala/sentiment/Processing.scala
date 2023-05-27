package sentiment

class Processing {
   
  /**********************************************************************************************
   *
   *                          Aufgabe 1
   *   
   *********************************************************************************************
  */
  def getWords(line:String):List[String]={
    /*
     * Extracts all words from a line
     * 
     * 1. Removes all characters which are not letters (A-Z or a-z)
     * 2. Shifts all words to lower case
     * 3. Extracts all words and put them into a list of strings
     */
    line.toLowerCase().replaceAll("[^A-Za-z]", " ").split(" ").filter(_.nonEmpty).toList
  }
  
  def getAllWords(l:List[(Int,String)]):List[String]={
    /*
     * Extracts all words from a List containing line number and line tuples
     * The words should be in the same order as they occur in the source document
     * 
     * Hint: Use the flatMap function
     */
    //val result = l.flatMap(x=>List(getWords(x._2)).filter(_.nonEmpty))
    getWords(l.flatMap(x=>List(x._2)).mkString(" "))

  }
  
  def countWords(l:List[String]):List[(String,Int)]={
    /*
     *  Gets a list of words and counts the occurrences of the individual words
     */

     l.map(x => (x, l.count(_ == x))).distinct

  }

  /**********************************************************************************************
   *
   *                          Aufgabe 2
   *
   *********************************************************************************************
  */

  def getAllWordsWithIndex(l: List[(Int, String)]): List[(Int, String)] = {
    l.flatMap(x=>getWords(x._2).flatMap(y=>List((x._1, y))))
  }

  def createInverseIndex(l: List[(Int, String)]): Map[String, List[Int]] ={


   getAllWordsWithIndex(l).flatMap(x=>Map(x._2 -> getAllWordsWithIndex(l).map(y => if (y._2 == x._2) y._1 else -1).filterNot(_ == -1))).toMap

  }

  def orConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = ???

  def andConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = ???
}


object Processing{
  
  def getData(filename:String):List[(Int,String)]={
    val url= getClass.getResource("/"+filename).getPath
    val src = scala.io.Source.fromFile(url.replaceAll("%20"," "))
    val iter = src.getLines()
    var c = -1
    val result= (for (row <- iter) yield {c=c+1;(c,row)}).toList
    src.close()
    result
  }
}