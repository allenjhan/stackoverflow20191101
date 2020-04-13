object PermutationsTest {
  def main(args: Array[String]) = {
    printNIterations(50, tcopermutations(100).iterator)
  }

  def printNIterations(n: Int, it: Iterator[Seq[Int]]): Unit = {
    if (n<=0) ()
    else {
      if (it.hasNext) {
        println(it.next())
        printNIterations(n - 1, it)
      } else ()
    }

  }

  def naivepermutations(n: Int): Seq[Seq[Int]] = {
    def loop(acc: Seq[Int], remaining: Seq[Int]): Seq[Seq[Int]] = {
      remaining match {
        case s if s.size == 1 => {
          val current: Seq[Int] = acc
          Seq((current :+ s.head))
        }
        case _ => {
          for {
            x <- remaining
            comb <- loop(acc :+ x, remaining.filter(_ != x))
          } yield comb
        }
      }
    }

    loop(Seq(), (1 to n))
  }

  def tcopermutations(n: Int): LazyList[Seq[Int]] = {
    val start = (1 to n).map(Element(_, Left))

    def loop(v: Seq[Element]): LazyList[Seq[Element]] = {
      johnsonTrotter(v) match {
        case Some(s) => v #:: loop(s)
        case None => LazyList(v)
      }
    }
    loop(start).map(_.map(_.i))
  }

  def checkIfMobile(seq: Seq[Element], i: Int): Boolean = {
    val e = seq(i)

    def getAdjacent(s: Seq[Element], d: Direction, j: Int): Int = {
      val adjacentIndex = d match {
        case Left => j - 1
        case Right => j + 1
      }
      s(adjacentIndex).i
    }

    if (e.direction == Left && i == 0) false
    else if (e.direction == Right && i == seq.size - 1) false
    else if (getAdjacent(seq, e.direction, i) < e.i) true
    else false
  }

  def findLargestMobile(seq: Seq[Element]): Option[Int] = {
    val mobiles = (0 until seq.size).filter{j => checkIfMobile(seq, j)}
    if (mobiles.isEmpty) None
    else {
      val folded = mobiles.map(x=>(x,seq(x).i)).foldLeft(None: Option[(Int, Int)]){ case (acc, elem) =>
        acc match {
          case None => Some(elem)
          case Some((i, value)) => if (value > elem._2) Some((i, value)) else Some(elem)
        }
      }
      folded.map(_._1)
    }
  }

  def swapLargestMobile(seq: Seq[Element], index: Int): (Seq[Element], Int) = {
    val dir = seq(index).direction
    val value = seq(index).i
    dir match {
      case Right =>
        val folded = seq.foldLeft((None, Seq()): (Option[Element], Seq[Element])){(acc, elem) =>
          val matched = elem.i == value
          val newAccOpt = if (matched) Some(elem) else None
          val newAccSeq = acc._1 match {
            case Some(swapMe) => acc._2 :+ elem :+ swapMe
            case None => if (matched) acc._2 else acc._2 :+ elem
          }
          (newAccOpt, newAccSeq)
        }
        (folded._2, index + 1)
      case Left =>
        val folded = seq.foldRight((None, Seq()): (Option[Element], Seq[Element])){(elem, acc) =>
          val matched = elem.i == value
          val newAccOpt = if (matched) Some(elem) else None
          val newAccSeq = acc._1 match {
            case Some(swapMe) => swapMe +: elem +: acc._2
            case None => if (matched) acc._2 else elem +: acc._2
          }
          (newAccOpt, newAccSeq)
        }
        (folded._2, index - 1)
    }
  }

  def revDirLargerThanMobile(seq: Seq[Element], mobile: Int) = {
    def reverse(e: Element) = {
      e.direction match {
        case Left => Element(e.i, Right)
        case Right => Element(e.i, Left)
      }
    }
    seq.map{ elem =>
      if (elem.i > seq(mobile).i) reverse(elem)
      else elem
    }
  }

  def johnsonTrotter(curr: Seq[Element]): Option[Seq[Element]] = {
    findLargestMobile(curr).map { m =>
      val (swapped, newMobile) = swapLargestMobile(curr, m)
      revDirLargerThanMobile(swapped, newMobile)
    }
  }

  trait Direction
  case object Left extends Direction
  case object Right extends Direction

  case class Element(i: Int, direction: Direction)
}
