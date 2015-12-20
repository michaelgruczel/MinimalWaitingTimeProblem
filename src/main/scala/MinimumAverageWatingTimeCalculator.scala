object MinimumAverageWatingTimeCalculator {

  /**
    * Main for starting from comman line
    * @param args number of customers and time e.g. sbt "run 3 0,3 1,9 2,5"
    */
  def main(args: Array[String]): Unit = {

    var numberOfCustomers = 0
    var times = List[(Int, Int)]()

    for (n <- args) {
      if(numberOfCustomers == 0) {
        numberOfCustomers = n.toInt
      } else {
        val split = n.split(",")
        val tuple = (split(0).toInt, split(1).toInt)
        times = times ::: List(tuple)
      }
    }
    println(calculate(numberOfCustomers, times))
  }

  /**
    * Calculation of minimal waiting time by execute first available job first
    *
    * - assumptions no parallel execution
    * - assumption best average time is important, not FIFO
    * - assumption not 2 jobs at the same ordering time with same processing time
    * - assumptions at a given time no knowledge about future job starts, means at
    * a given time only jobs scheduled for the time or earlier can be selected and it's
    *   not allowed to wait because of an incoming order in the future
    *
    * idea: shortest job first, but only select jobs for we do not have to wait, means already scheduled
    * - PRECONDITION: jobs ordered by starting time
    * - Step1: go to the time when first order comes
    * - Step2: select from all already scheduled jobs the fastest
    * - Step3: jump to the time when order is processed (starting time + execution)
    * - Step4: remove job and goto Step2 until all jobs are processed
    *
    *
    * @param numberOfCustomers number of customers/jobs
    * @param times ordering and execution times as list of tuples
    *              ((ordering job1,execution  time job1), (ordering job2,execution time job2), ...)
    * @return average waitng as int
    */
  def calculate(numberOfCustomers:Int = 0, times:List[(Int, Int)] = List()) : Int = {

    checkConsistencyOfInputData(numberOfCustomers, times)

    // Init with first oder
    var sortedJobs = times.sortWith(_._1 < _._1)
    var currentTime = BigInt(sortedJobs.head._1)
    var overallWaitingTime = BigInt(0)

    // loop until all jobs are executed
    while(sortedJobs.length > 0) {

      val fastestAlreadyOrderedJob = findFastestAllreadyOrderedJob(currentTime, sortedJobs)

      // recalculate times and jump to next appearing order
      val finishingTime = currentTime + BigInt(fastestAlreadyOrderedJob._2)
      val waitingTime = finishingTime - BigInt(fastestAlreadyOrderedJob._1)
      overallWaitingTime += waitingTime
      currentTime = finishingTime

      // remove already executed job
      sortedJobs = sortedJobs.filterNot(elm => elm == fastestAlreadyOrderedJob)
    }

    (overallWaitingTime / BigInt(numberOfCustomers)).intValue()
  }

  /*
   * number of customer between 1 and 100000
   * and for every cutomer a pair of ordering time and working time is given
   */
  private def checkConsistencyOfInputData(numberOfCustomers: Int, times: List[(Int, Int)]): Unit = {
    if (numberOfCustomers == 0
      || times.length == 0
      || numberOfCustomers != times.length
      || numberOfCustomers > 100000
    ) throw new scala.IllegalArgumentException
  }

  private def findFastestAllreadyOrderedJob(currentTime: BigInt, notExecutedJobs:List[(Int, Int)]) : (Int, Int) = {
    val allJobsStartingAtThatTime = notExecutedJobs.groupBy(_._1 <= currentTime).get(true)
    allJobsStartingAtThatTime.head.sortWith(_._2 < _._2).head
  }

}
