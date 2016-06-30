import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.Flip
import com.cra.figaro.library.compound.If
import com.cra.figaro.language.Element

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
  def rootMeanSquareError (x:Element[Boolean],
                           y:Element[Boolean],
                           z:Element[Boolean],
                           r:Double,
                           i:Int): Double = {

    val algs = for (i <- 0 to 99) yield Importance(i.toInt,x,y,z)
    algs.foreach(_.start())
    val mean = algs
        .map(_.probability(y,true)-r)
        .map(Math.pow(_,2))
        .foldLeft(0.0)((b,a) => b+a) / 100
    return Math.sqrt(mean)
  }

  def main(args: Array[String]) {
    val x = Flip(0.8)
    val y = Flip(0.6)
    val z = If(x === y, Flip(0.9), Flip(0.1))
    z.observe(false)
    val exact = VariableElimination.probability(y,true)

    /**
      *  Input Handling
      */

    args match {
      case Array("1a") =>
        println("")
        println("Running task a ...")
        println("Calculated exact probability for given model: " + exact )
      case Array("1b", input) =>
        println("")
        println("Running task b ...")
        println("Initializing sampling algorithms with samplesize " + input)
        println("#######################################################")
        println("")
        println("For samplesize of " + input + " root-mean-square error is " + rootMeanSquareError(x,y,z,exact,input.toInt))
        println("")
      case Array("1b") =>
        println("")
        println("Running task b ...")
        println("Initializing sampling algorithms")
        println("#######################################################")
        println("")
        for (i <- 1 to 10) {
          val result = rootMeanSquareError(x,y,z,exact,i*1000)
          println("Samplesize: " + i*1000 + ", RMS Error: " + result )
        }
        println("")

      case _ => println("Input not recognized.")
    }



  }
}
