import scala.collection.SortedSet
import scala.io.Source

object SimilitudEntreDocuments {

  def main(args: Array[String]): Unit = {
    // Your application code goes here
    //printWordOccurrence(freq(normalitza(readFileAsString("data/pg11.txt"))))

    val stringBook1 = normalitza(readFileAsString("data/pg11.txt"))
    val stringBook2 = normalitza(readFileAsString("data/pg12.txt"))
    val listStopWords = readStopWordsFromFile("data/english-stop.txt")

    // FREQUENCIA DE PARAULES
    //printWordOccurrence(freq(stringBook1))
    // FREQUENCIA DE PARAULES SENSE STOP-WORDS
    //printWordOccurrence(nonStopFreq(stringBook1, listStopWords))
    // DISTRIBUCIÓ DE PARAULES
    //paraulaFreqFreq(stringBook1)
    // N-GRAMS
    //displayNGrams(stringBook1, 1)
    print("La similitud és de " + cosinesim(stringBook1, stringBook2, listStopWords))
  }

  def cosinesim(str1: String, str2: String, stopWords: List[String]): Double = {
    val vsm1 = getVectorSpaceModel(str1, 1, stopWords)
    val vsm2 = getVectorSpaceModel(str2, 1, stopWords)

    compareVectorSpaceModel(vsm1, vsm2)
  }

  //OLD METHOD (DELETE LATER)
  /*def compareVectorSpaceModel(vsm1: SortedSet[(String, Double)], vsm2: SortedSet[(String, Double)]): Double = {
    val ngramIn1 = vsm1.map { case (ngram, _) => ngram }
    val notIn1 = vsm2.filter(x => !(ngramIn1.contains(x._1))).map(_._1)

    val ngramIn2 = vsm2.map { case (ngram, _) => ngram }
    val notIn2 = vsm1.filter(x => !(ngramIn2.contains(x._1))).map(_._1)

    var mutable_vsm1: SortedSet[(String, Double)] = vsm1
    var mutable_vsm2: SortedSet[(String, Double)] = vsm2

    notIn1.foreach(ngram => {
      val tuple: (String, Double) = (ngram, 0)
      mutable_vsm1 += tuple
    })
    notIn2.foreach(ngram => {
      val tuple: (String, Double) = (ngram, 0)
      mutable_vsm2 += tuple
    })

    val component1 = Math.sqrt(mutable_vsm1.map(x => x._2*x._2).sum)
    val component2 = Math.sqrt(mutable_vsm2.map(x => x._2*x._2).sum)

    val sum = mutable_vsm1.zip(mutable_vsm2).map { case (ngram1, ngram2)  =>
      ngram1._2*ngram2._2
    }.sum

    println(sum)
    println(component1*component2)
    sum/(component1*component2)
  }*/

  def compareVectorSpaceModel(vsm1: Vector[(String, Double)], vsm2: Vector[(String, Double)]): Double = {
    val ngramIn1 = vsm1.map { case (ngram, _) => ngram }.toSet
    val notIn1 = vsm2.filter(x => !(ngramIn1.contains(x._1)))

    val ngramIn2 = vsm2.map { case (ngram, _) => ngram }.toSet
    val notIn2 = vsm1.filter(x => !(ngramIn2.contains(x._1)))

    var mutable_vsm1: Vector[(String, Double)] = vsm1
    var mutable_vsm2: Vector[(String, Double)] = vsm2

    notIn1.foreach(ngram => {
      val tuple: (String, Double) = (ngram._1, 0)
      mutable_vsm1 :+= tuple
    })
    notIn2.foreach(ngram => {
      val tuple: (String, Double) = (ngram._1, 0)
      mutable_vsm2 :+= tuple
    })

    // Sort both vectors by n-gram to ensure they are in the same order
    mutable_vsm1 = mutable_vsm1.sortBy(_._1)
    mutable_vsm2 = mutable_vsm2.sortBy(_._1)

    val component1 = Math.sqrt(mutable_vsm1.map(x => x._2 * x._2).sum)
    val component2 = Math.sqrt(mutable_vsm2.map(x => x._2 * x._2).sum)

    val sum = mutable_vsm1.zip(mutable_vsm2).map { case (ngram1, ngram2) =>
      ngram1._2 * ngram2._2
    }.sum

    sum / (component1 * component2)
  }


  /**
   * Returns the string passed with only one space separating each words and with all stopwords removed.
   * @param str
   * @param stopWords
   * @return
   */
  def getStringWithoutStopWords(str: String, stopWords: List[String]): String = {

    var words = str.split("\\s+").toList
    words = words.filter(x => !stopWords.contains(x))
    words.mkString(" ")
  }

  /**
   * Returns the vectorSpaceModel for the string str with n-word strings.
   * @param str
   * @param n
   * @return
   */
    //OLD METHOD (DELETE LATER)
    /*
  def getVectorSpaceModel(str: String, n: Int, stopWords: List[String]): SortedSet[(String, Double)] = {

    val nonStopString = getStringWithoutStopWords(str, stopWords)
    val wordsCounts = freqNGrams(nonStopString, n)
    val maxFreq = wordsCounts.map(_._2).max

    var setWeights: SortedSet[(String, Double)] = SortedSet()
    println("mf" + maxFreq)
    wordsCounts.foreach(x => {
      println(x._1 + " - " + calculaFrequenciaNormalitzada(x._2,maxFreq));
      val tuple: (String, Double) = (x._1 ,calculaFrequenciaNormalitzada(x._2,maxFreq))
      setWeights += tuple
    })

    setWeights
  }*/

    def getVectorSpaceModel(str: String, n: Int, stopWords: List[String]): Vector[(String, Double)] = {
      val nonStopString = getStringWithoutStopWords(str, stopWords)
      val wordsCounts = freqNGrams(nonStopString, n)
      val maxFreq = wordsCounts.map(_._2).max

      var vectorWeights: Vector[(String, Double)] = Vector()
      println("mf" + maxFreq)
      wordsCounts.foreach(x => {
        println(x._1 + " - " + calculaFrequenciaNormalitzada(x._2, maxFreq))
        val tuple: (String, Double) = (x._1, calculaFrequenciaNormalitzada(x._2, maxFreq))
        vectorWeights :+= tuple
      })

      vectorWeights
    }


  /**
   * Calcula la freqüència normalitzada d'una paraula en un text que conté una frequencia maxima freqMaxWord
   * @param freqWord
   * @param freqMaxWord
   * @return
   */
  def calculaFrequenciaNormalitzada(freqWord: Int, freqMaxWord: Int): Double = {
    freqWord.toDouble/freqMaxWord.toDouble
  }

  def freqNGrams(str: String, n: Int): List[(String, Int)] = {
    val words = str.split("\\s+").toList

    val listNGrams = words.sliding(n).toList
    val ngrams = listNGrams.map(ngram => ngram.mkString(" "))

    val listAppearances = ngrams.groupBy(identity).view.mapValues(_.size).toList.sortBy(_._2)(Ordering.Int.reverse)

    listAppearances
  }

  /**
   * Displays the 10 most common n-grams
   * @param str string to search n-grams from
   * @param n number of words that make the n-grams
   */
  def displayNGrams(str: String, n: Int): Unit = {

    val ngrams: List[(String, Int)] = freqNGrams(str, n)

    println("NGrams més freqüents:")
    for (ngram <- ngrams.take(10)) {
      // Imprimim primer el 2: nombre de paraules que han aparegut n vegades, i després 1: nombre de vegades que han aparegut.
      println(f"${ngram._1}%-25s ${ngram._2}%3s")
    }
  }

  /**
   * Displays the appearances that have the most and the least number of words.
   * @param str
   */
  def paraulaFreqFreq(str: String): Unit = {

    val listCounts: List[(String, Int)] = freq(str);

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

    println("\nLes 5 frequencies menys frequents:")
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
    for ((word, count) <- list.take(10)) {
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



