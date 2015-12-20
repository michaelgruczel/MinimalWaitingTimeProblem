object MinimumAverageWatingTimeCalculator {

  def calculate(numberOfCustomers:Int = 0, times:List[(Int, Int)] = List()) : Int = {

    checkConsistencyOfInputData(numberOfCustomers, times)

    ???
  }

  /*
   * number of customer between 1 and 100000
   * and for every cutomer a pair of ordering time and working time is given
   */
  def checkConsistencyOfInputData(numberOfCustomers: Int, times: List[(Int, Int)]): Unit = {
    if (numberOfCustomers == 0
      || times.length == 0
      || numberOfCustomers != times.length
      || numberOfCustomers > 100000
    ) throw new scala.IllegalArgumentException
  }

}
