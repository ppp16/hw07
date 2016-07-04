import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.{ DisjointScheme, Importance, MetropolisHastings, ProposalScheme }
import com.cra.figaro.language.{ AtomicSelect, Element, Flip, Select }
import com.cra.figaro.library.collection.Container
import com.cra.figaro.library.compound.If
import com.cra.figaro.language._

import scala.collection.immutable.IndexedSeq

import com.quantifind.charts.Highcharts._

object Hw07 {

  /*
  For the following program:
  val x = Flip(0.8)
  val y = Flip(0.6)
  val z = If(x === y, Flip(0.9), Flip(0.1))
  z.observe(false)

  a)Run variable elimination to compute the exact posterior probability that y is
    true, given the observation about z .
  b) Run importance sampling using 1,000 samples, 2,000 samples, and so on up
    to 10,000 samples. For each number of samples, run the experiment 100
  times.
  Compute the root-mean-square error of importance sampling with the
  given number of samples, defined as follows:
  i)  For each experiment, measure the difference between the probability
    estimated by importance sampling and the exact probability computed by
    variable elimination. This is the error.
  ii) Compute the square of this error for each experiment.
  iii)Compute the mean of all these squared errors together.
  iv) Take the square root of the mean. This is the root mean squared error,
  which is a common way to measure the error of an inference algorithm.
  c) Plot the root-mean-square error on a chart. What kind of trend do you notice?
  2) Repeat exercise 1 for Metropolis-Hastings using the default proposal scheme.
  This time, use 10,000 samples, 20,000 samples, 30,000 samples, and so on up to
  100,000 samples.
  3) Now let’s change the program slightly by making the numerical parameters
    more extreme:
  val x = Flip(0.999)
  val y = Flip(0.99)
  val z = If(x === y, Flip(0.9999), Flip(0.0001))
  z.observe(false)
  a) Run variable elimination to get the exact posterior probability of y .
  b) Run importance sampling with 1,000,000 samples.
  c) In this program, the evidence is unlikely. But you should see that importance
    sampling is accurate on this program. Why do you think that is?
  4) Run Metropolis-Hastings on the same program using the default proposal
  scheme with 10,000,000 samples. You should see a poor result. (This may not
    happen every time, but over multiple runs the results will tend to be poor.) Why
  do you think this problem is hard for Metropolis-Hastings?
  5) Try writing a custom proposal scheme for Metropolis-Hastings:
  a) Because we’ll be proposing the Flip s inside the definition of z, we have to
  make them separate variables that we’ll refer to. Make the Flip(0.9999) a
    variable named z1 , and the Flip(0.0001) a variable named z2 , and use the
  new variables in the definition of z .
  b) Create a custom proposal scheme that behaves as follows:
  i)  With probability 0.1, propose z1 .
  ii) With probability 0.1, propose z2 .
  iii)With probability 0.8, propose both x and y .
    Run Metropolis-Hastings using this proposal scheme. This should produce
  better results. Why do you think this is the case?
  */

  /*
   *  Flag to set for plotting results
   *  Ploting creates a local server which
   *  will only terminate if u end the program manually 
   */
  val shouldPlot = true

  /**
   * Initializes variables of model A
   * used in Task 1 and 2
   * @return Array of variables and exact probability
   */
  def modelA(): (Array[Element[Boolean]]) = {
    val x = Flip(0.8)
    val y = Flip(0.6)
    val z = If(x === y, Flip(0.9), Flip(0.1))
    z.observe(false)
    val exact = VariableElimination.probability(y, true)
    return (Array(x, y, z))
  }
  /**
   * Initializes variables of model B
   * used in Task 3 and 4
   * @return Array of variables
   */
  def modelB(): (Array[Element[Boolean]]) = {
    val x = Flip(0.999)
    val y = Flip(0.99)
    val z = If(x === y, Flip(0.9999), Flip(0.0001))
    z.observe(false)
    return (Array(x, y, z))
  }

  /**
   * Initializes variables of model C
   * used in Task 5
   * @return Array of variables
   */
  def modelC(): (Array[Element[Boolean]]) = {
    val x = Flip(0.999)
    val y = Flip(0.99)
    val z1 = Flip(0.9999)
    val z2 = Flip(0.0001)
    val z = If(x === y, z1, z2)
    z.observe(false)
    return (Array(x, y, z, z1, z2))
  }

  /**
   *
   * @param rows of Minesweeper field
   * @param columns of Minesweeper field
   * @param difficulty in Interval [0,1]
   * @return multidimensional array of variables, which are 1 if mine is on corresponding field, 0 otherwise
   */
  def modelD(rows: Int, columns: Int, difficulty: Double): Array[Array[AtomicSelect[Int]]] = {
    val mine: Array[Array[AtomicSelect[Int]]] =
      Array.fill(rows, columns)(Select(difficulty -> 1, 1 - difficulty -> 0)) //initializes variables for mines
    val count: Array[Array[Element[Int]]] = Array.tabulate(rows, columns)((row, column) => { //initalizes variables for numbers of mines surround corresponding field
      val surrounds: IndexedSeq[AtomicSelect[Int]] = for { //contains all mines surrounding corresponding field of count variable
        i <- row - 1 to row + 1
        j <- column - 1 to column + 1
        if i >= 0 && i < rows && j >= 0 && j < columns // is in field?
        if i != row || j != column // is not count field?
      } yield mine(i)(j)
      Container(surrounds: _*).foldLeft(0)(_ + _) // saves sum to count
    })
    val observedField = Array(
      "?2M???????",
      "1?????????",
      "?1????????",
      "??????????",
      "??????????",
      "??????????",
      "??????????",
      "??????????",
      "??????????",
      "??????????")

    /*
     * observed field handling
     */
    for {
      i <- 0 until rows
      j <- 0 until columns
    } {
      observedField(i)(j) match {
        case 'M' => mine(i)(j).observe(1)
        case d if d.isDigit =>
          mine(i)(j).observe(0)
          count(i)(j).observe(d - '0')
        case '?' => ()
      }
    }
    return mine
  }

  /**
   * Calculates root-mean-square error between exact probability and Importance sampled probability
   *
   * @param x Variable x
   * @param y Variable y
   * @param z Variable z
   * @param exact probability
   * @param samplesize for importance sampling algorithms
   * @return root mean square error
   */
  def rMSEImp(x: Element[Boolean],
    y: Element[Boolean],
    z: Element[Boolean],
    exact: Double,
    samplesize: Int): Double = {
    val algs = for (i <- 0 to 99) yield Importance(samplesize.toInt, y)
    algs.foreach(_.start())
    val mean = algs
      .map(_.probability(y, true) - exact) // difference between probabilities
      .map(Math.pow(_, 2)) // square of difference
      .foldLeft(0.0)(_ + _) / 100 // mean of square
    return Math.sqrt(mean) // square root of mean
  }

  /**
   * Calculates root-mean-square error between exact probability and Metropolis Hastings sampled probability
   *
   * @param x Variable x
   * @param y Variable y
   * @param z Variable z
   * @param exact probability
   * @param samplesize for metropolis hastings sampling algorithms
   * @return root mean square error
   */
  def rMSEMet(x: Element[Boolean],
    y: Element[Boolean],
    z: Element[Boolean],
    exact: Double,
    samplesize: Int): Double = {

    var total = 0.0
    for (i <- 0 to 99) {
      Universe.createNew() // needed to reinitialize the algorithm
      val x = Flip(0.8)
      val y = Flip(0.6)
      val z = If(x === y, Flip(0.9), Flip(0.1))
      z.observe(false)
      var alg = MetropolisHastings(samplesize.toInt, ProposalScheme.default, y)
      alg.start()
      total += Math.pow(alg.probability(y, true) - exact, 2)
    }
    return Math.sqrt(total / 100)
  }

  /**
   * Generates a plot with the wisp library
   *
   * @param xValues A List of Integer values for the x-axis
   * @param yValues A List of Double values for the y-axis
   * @param xLabel The label for the x-axis
   * @param yLabel The label for the y-axis
   * @param Title The title for the chart
   */
  def generatePlot(xValues: List[Int], yValues: List[Double], xLabel: String, yLabel: String, Title: String) {
    line(xValues, yValues)
    title(Title)
    xAxis(xLabel)
    yAxis(yLabel)
  }

  def main(args: Array[String]) {
    /**
     *  Input Handling
     */

    args match {

      case Array("1a") =>
        println("")
        println("Running task 1 a ...")
        println("#######################################################")
        println("")
        val model = modelA()
        val x = model(0)
        val y = model(1)
        val z = model(2)
        val exact = VariableElimination.probability(y, true)
        println("Calculated exact probability for given model: " + exact)
        println("")

      case Array("1b", input) =>
        println("")
        println("Running task 1 b ...")
        println("Initializing Importance sampling algorithms with samplesize " + input)
        println("#######################################################")
        println("")
        val model = modelA()
        val x = model(0)
        val y = model(1)
        val z = model(2)
        val exact = VariableElimination.probability(y, true)
        println("For samplesize of " + input +
          " root-mean-square error is " + rMSEImp(x, y, z, exact, input.toInt))
        println("")

      case Array("1b") =>
        println("")
        println("Running task 1 b ...")
        println("Initializing Importance sampling algorithms")
        println("#######################################################")
        println("")
        val model = modelA()
        val x = model(0)
        val y = model(1)
        val z = model(2)
        val exact = VariableElimination.probability(y, true)
        var errors = List[Double]()
        var samples = List[Int]()
        for (i <- 1 to 10) {
          val result = rMSEImp(x, y, z, exact, i * 1000)
          if (shouldPlot) errors = errors ::: List(result)
          if (shouldPlot) samples = samples ::: List(i * 1000)
          println("Samplesize: " + i * 1000 + ", RMS Error: " + result)
        }
        if (shouldPlot) generatePlot(samples, errors, "Samples", "Error", "Root-mean-square error of importance sampling")
        println("")

      case Array("2", input) =>
        println("")
        println("Running task 2 ...")
        println("Initializing Metropolis-Hastings sampling algorithms with samplesize " + input)
        println("#######################################################")
        println("")
        val model = modelA()
        val x = model(0)
        val y = model(1)
        val z = model(2)
        val exact = VariableElimination.probability(y, true)
        println("For samplesize of " + input +
          " root-mean-square error is " + rMSEMet(x, y, z, exact, input.toInt))
        println("")

      case Array("2") =>
        println("")
        println("Running task 2 ...")
        println("Initializing Metropolis-Hastings sampling algorithms")
        println("#######################################################")
        println("")
        val model = modelA()
        val x = model(0)
        val y = model(1)
        val z = model(2)
        val exact = VariableElimination.probability(y, true)
        var errors = List[Double]()
        var samples = List[Int]()
        for (i <- 1 to 10) {
          val result = rMSEMet(x, y, z, exact, i * 10000)
          if (shouldPlot) errors = errors ::: List(result)
          if (shouldPlot) samples = samples ::: List(i * 10000)
          println("Samplesize: " + i * 10000 + ", RMS Error: " + result)
        }
        if (shouldPlot) generatePlot(samples, errors, "Samples", "Error", "Root-mean-square error of Metropolis-Hastings sampling")
        println("")

      case Array("3a") =>
        println("")
        println("Running task 3 a ...")
        println("Initializing Importance sampling algorithms")
        println("#######################################################")
        println("")
        val model = modelB()
        val x = model(0)
        val y = model(1)
        val z = model(2)
        val exact = VariableElimination.probability(y, true)
        println("Calculated exact probability for given model: " + exact)
        println("")

      case Array("3b", input) =>
        println("")
        println("Running task 3 b ...")
        println("Initializing Importance sampling algorithms")
        println("#######################################################")
        println("")
        val samplesize = input.toInt
        val model = modelB()
        val x = model(0)
        val y = model(1)
        val z = model(2)
        val alg = Importance(samplesize, x, y, z)
        alg.start()
        val exact = VariableElimination.probability(y, true)
        val result = alg.probability(y, true)
        println("Calculated exact probability for given model: " + exact)
        println("Sampling algorithm with Samplesize: " + samplesize + ", returns " + result)
        println("Root mean square error is " + Math.abs(exact - result))
        println("")

      case Array("3b") =>
        println("")
        println("Running task 3 b ...")
        println("Initializing Importance sampling algorithms")
        println("#######################################################")
        println("")
        val samplesize = 1000000
        val model = modelB()
        val x = model(0)
        val y = model(1)
        val z = model(2)
        val alg = Importance(samplesize, x, y, z)
        alg.start()
        val exact = VariableElimination.probability(y, true)
        val result = alg.probability(y, true)
        println("Calculated exact probability for given model: " + exact)
        println("Sampling algorithm with Samplesize: 1.000.000, returns " + result)
        println("Root mean square error is " + Math.abs(exact - result))
        println("")

      case Array("4", input) =>
        println("")
        println("Running task 4 ...")
        println("Initializing Metropolis-Hastings sampling algorithm")
        println("#######################################################")
        println("")
        val samplesize = input.toInt
        val model = modelB()
        val x = model(0)
        val y = model(1)
        val z = model(2)
        val exact = VariableElimination.probability(y, true)
        val alg = MetropolisHastings(samplesize, ProposalScheme.default, x, y, z)
        alg.start()
        val result = alg.probability(y, true)
        println("Calculated exact probability for given model: " + exact)
        println("Sampling algorithm with Samplesize: " + samplesize + ", returns " + result)
        println("Difference is " + Math.abs(exact - result))
        println("")

      case Array("4") =>
        println("")
        println("Running task 4 ...")
        println("Initializing Metropolis-Hastings sampling algorithm")
        println("#######################################################")
        println("")
        val samplesize = 10000000
        val model = modelB()
        val x = model(0)
        val y = model(1)
        val z = model(2)
        val exact = VariableElimination.probability(y, true)
        val alg = MetropolisHastings(samplesize, ProposalScheme.default, x, y, z)
        alg.start()
        val result = alg.probability(y, true)
        println("Calculated exact probability for given model: " + exact)
        println("Sampling algorithm with Samplesize: " + samplesize + ", returns " + result)
        println("Difference is " + Math.abs(exact - result))
        println("")

      case Array("5", input) =>
        println("")
        println("Running task 5 ...")
        println("Initializing Metropolis-Hastings sampling algorithm")
        println("#######################################################")
        println("")
        val samplesize = input.toInt
        val model = modelC()
        val x = model(0)
        val y = model(1)
        val z = model(2)
        val z1 = model(3)
        val z2 = model(4)
        val schema: ProposalScheme = {
          DisjointScheme(
            (0.1, () => ProposalScheme(z1)),
            (0.1, () => ProposalScheme(z2)),
            (0.9, () => ProposalScheme(x, y)))
        }
        val exact = VariableElimination.probability(y, true)
        val alg = MetropolisHastings(samplesize, schema, x, y, z)
        alg.start()
        val result = alg.probability(y, true)
        println("Calculated exact probability for given model: " + exact)
        println("Sampling algorithm with Samplesize: " + samplesize + ", returns " + result)
        println("Difference is " + Math.abs(exact - result))
        println("")

      case Array("5") =>
        println("")
        println("Running task 5 ...")
        println("Initializing Metropolis-Hastings sampling algorithm")
        println("#######################################################")
        println("")
        val samplesize = 10000000
        val model = modelC()
        val x = model(0)
        val y = model(1)
        val z = model(2)
        val z1 = model(3)
        val z2 = model(4)
        val schema: ProposalScheme = {
          DisjointScheme(
            (0.1, () => ProposalScheme(z1)),
            (0.1, () => ProposalScheme(z2)),
            (0.9, () => ProposalScheme(x, y)))
        }
        val exact = VariableElimination.probability(y, true)
        val alg = MetropolisHastings(samplesize, schema, x, y, z)
        alg.start()
        val result = alg.probability(y, true)
        println("Calculated exact probability for given model: " + exact)
        println("Sampling algorithm with Samplesize: " + samplesize + ", returns " + result)
        println("Difference is " + Math.abs(exact - result))
        println("")

      case Array("6") =>
        println("")
        println("Running task 6 ...")
        println("Initializing minesweeper variables and calculating exact probability")
        println("#######################################################")
        println("")
        val mine = modelD(10, 10, 0.4)
        val exact = VariableElimination.probability(mine(1)(1), 1)
        println("Exact probability is " + exact)
        val algImp = Importance(2000, mine.flatten: _*)
        algImp.start()
        val imp = algImp.probability(mine(1)(1), 1)
        println("Importance sampled probability is " + imp)

      case _ =>
        println("")
        println("Input not recognized.")
        println("Please input:")
        println("")
        println("run <Tasknum><Subtask> <samplesize>")
        println("subtask, samplesize are optional")
        println("")
    }
  }
}
