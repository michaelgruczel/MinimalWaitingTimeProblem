object MinimumAverageWatingTimeCalculator {

  /*
  * - assumptions no parallel execution
  * - assumption best average time is important, not FIFO
  * - assumption not 2 jobs at the same ordering time with same processing time
  * - assumptions at a given time no knowledge about future job starts, means at
  * - a given time only jobs scheduled for the time or earlier can be selected and it's
  *   not allowed to wait because of an incoming order in the future
  *
  * idea: shortest job first, but only select jobs for we do not have to wait, means already scheduled
  * - PRECONDITION: jobs ordered by starting time
  * - Step1: go to the time when first order comes
  * - Step2: select from all already scheduled jobs the fastest
  * - Step3: jump to the time when order is processed (starting time + execution)
  * - Step4: remove job and goto Step2 until all jobs are processed
  */
  def calculate(numberOfCustomers:Int = 0, times:List[(Int, Int)] = List()) : Int = {

    checkConsistencyOfInputData(numberOfCustomers, times)

    // Init with first oder
    var sortedJobs = times.sortWith(_._1 < _._1)
    var currentTime = sortedJobs.head._1
    var overallWaitingTime = 0

    // loop until all jobs are executed
    while(sortedJobs.length > 0) {

      val fastestAlreadyOrderedJob = findFastestAllreadyOrderedJob(currentTime, sortedJobs)

      // recalculate times and jump to next appearing order
      val finishingTime = currentTime + fastestAlreadyOrderedJob._2
      val waitingTime = finishingTime - fastestAlreadyOrderedJob._1
      overallWaitingTime += waitingTime
      currentTime = finishingTime

      // remove already executed job
      sortedJobs = sortedJobs.filterNot(elm => elm == fastestAlreadyOrderedJob)
    }

    (overallWaitingTime / numberOfCustomers)
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

  private def findFastestAllreadyOrderedJob(currentTime: Int, notExecutedJobs:List[(Int, Int)]) : (Int, Int) = {
    val allJobsStartingAtThatTime = notExecutedJobs.groupBy(_._1 <= currentTime).get(true)
    allJobsStartingAtThatTime.head.sortWith(_._2 < _._2).head
  }

}
