//returns HashMap with words separated by space in text as a key and number of repetitions of each as a value
 def wordCounter(text: String): scala.collection.mutable.Map[String, Int] = {
    val map = text.split(" ").map(_.toLowerCase).groupBy(identity).view.mapValues(_.size)
    val map2 = new mutable.HashMap[String, Int]().addAll(map)
    map2
  }