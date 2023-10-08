import scala.io.Source

object SimilitudEntreDocuments {

  def main(args: Array[String]): Unit = {
    // Your application code goes here
    //printWordOccurrence(freq(normalitza(readFileAsString("data/pg11.txt"))))

    val stringBook = normalitza(readFileAsString("data/pg11.txt"))
    val listStopWords = readStopWordsFromFile("data/english-stop.txt")

    displayNGrams(stringBook, 3)
  }

  /**
   * Displays the 10 most common n-grams
   * @param str string to search n-grams from
   * @param n number of words that make the n-grams
   */
  def displayNGrams(str: String, n: Int): Unit = {
    val words = str.split("\\s+").toList

    val listNGrams = words.sliding(n).toList
    val ngrams = listNGrams.map(ngram => ngram.mkString(" "))

    val listAppearances = ngrams.groupBy(identity).view.mapValues(_.size).toList.sortBy(_._2)(Ordering.Int.reverse)

    println("NGrams més freqüents:")
    for (ngram <- listAppearances.take(10)) {
      // Imprimim primer el 2: nombre de paraules que han aparegut n vegades, i després 1: nombre de vegades que han aparegut.
      println(f"${ngram._1}%-25s ${ngram._2}%3s")
    }
  }

  /**
   * Displays the appearances that have the most and the least number of words.
   * @param listCounts
   */
  def paraulaFreqFreq(listCounts: List[(String, Int)]): Unit = {
    /**
     * First we group by the second parameter all elements of the list, meaning we're going to get a Map of number of appearences and List of tuples with that number of appearances.
     * Then we apply mapValues to apply the function size to each one of the values (as it is a map those are the tuples of words appearing that many times).
     * This returns a map, that we transform to a list, containing a Number of appearances - number of words with that appearances.
     */
    val frequencyMap = listCounts.groupBy(_._2).view.mapValues(_.size).toList.sortBy(_._2)(Ordering.Int.reverse)

    val first10 = frequencyMap.take(10)
    val last5 = frequencyMap.takeRight(5)

    println("Les 10 frequencies mes frequents:")
    for (word <- first10){
      // Imprimim primer el 2: nombre de paraules que han aparegut n vegades, i després 1: nombre de vegades que han aparegut.
      println(word._2 + " paraules apareixen " + word._1 + " vegades")
    }

    println("%nLes 5 frequencies menys frequents:")
    for (word <- last5) {
      // Imprimim primer el 2: nombre de paraules que han aparegut n vegades, i després 1: nombre de vegades que han aparegut.
      println(word._2 + " paraules apareixen " + word._1 + " vegades")
    }
  }

  /**
   * Reads from file and returns a List containing all lines from file as strings.
   * @param path
   * @return
   */
  def readStopWordsFromFile(path: String): List[String] = {
    val source = Source.fromFile(path)
    try {
      source.getLines().toList
    } finally {
      source.close()
    }
  }

  /**
   * Prints information about a list containing words and their number of appearances in a text.
   * @param list
   */
  def printWordOccurrence(list: List[(String, Int)]): Unit = {

    // Calculate the total number of words and different Words
    val totalWords = list.map(_._2).sum
    val differentWords = list.length

    // Print the header
    println(s"Num de Paraules:    " + totalWords + "     Diferents:     " + differentWords)
    val header = f"Paraules       ocurrencies   frequencia"
    println(header)
    println("---------------------------------------")

    // Print the sorted word counts with frequency percentages
    for ((word, count) <- list) {
      val frequency = (count.toDouble / totalWords * 100).formatted("%.2f")
      // Negatiu alinea esquerra, positiu dreta
      println(f"${word}%-15s ${count}%5d ${frequency}%6s")
    }
  }

  /**
   * Returns a List<String, Int> that contains all words that appear in str and do not appear in stopWords ordered by number of appearances
   * @param str string containing text.
   * @param stopWords list of words to not include in the returned list.
   * @return
   */
  def nonStopFreq(str: String, stopWords: List[String]): List[(String, Int)] = {
    var words = str.split("\\s+").toList
    words = words.filter(x => !stopWords.contains(x))
    words.groupBy(identity).view.mapValues(_.size).toList.sortBy(_._2)(Ordering.Int.reverse)
  }

  /**
   * Returns a List<String, Int> that contains all words that appear in str ordered by number of appearances.
   * @param str
   * @return
   */
  def freq(str: String): List[(String, Int)] = {
    val words = str.split("\\s+").toList
    words.groupBy(identity).view.mapValues(_.size).toList.sortBy(_._2)(Ordering.Int.reverse)
  }

  /**
   * Reads file and returns a string containing it's contents
   * @param path String
   * @return
   */
  def readFileAsString(path: String): String = {
    val source = Source.fromFile(path)
    try {
      source.mkString
    } finally {
      source.close()
    }
  }

  /**
   * Returns the string str to lowercase and with any character other than a-z replaced by a space.
   * @param str
   * @return
   */
  def normalitza(str: String): String = {
    val newstr = str.toLowerCase().replaceAll("[^a-z]", " ")
    newstr
  }
}



