package odo.suffixarr

object Test{
  def main(args: Array[String]): Unit = {
    //    val string = "rikki-tikki-tikka"
        val string = "cabbage"
        implicit val char = BoundEnum.forElems(string)
        val issa = new SA_ISMaker(string)
        println(issa.showTypeMap)

        println(issa.start.guessLMSSort.induceSortL)
  }
}
