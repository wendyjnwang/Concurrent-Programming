import io.threadcso.{component, _}

object hammingNumbers {

  def merge (in1: ?[Int], in2: ?[Int], out: ![Int]) = proc{
    var val1 = in1?(); var val2 = in2?()
    repeat{
      if (val1 == val2){
        out!val1
        val1 = in1?()
        val2 = in2?()
      }
      if (val1 < val2 ) {
        out!val1
        val1 = in1?()
      }
      if (val1 > val2 ) {
        out!val2
        val2 = in2?()
      }
    }
    in1.closeIn();in2.closeIn();out.closeOut()
  }

  def teeCount[T](in: ?[T], out1: ![T], out2: ![T]) = proc {
    var count = 0
    repeat { val v = in?();
            (proc{out1!v} || proc{out2!v})()
            count += 1
            if (count == 1000){
              in.closeIn();out1.closeOut();out2.closeOut()
            }
    }
    in.closeIn();out1.closeOut();out2.closeOut()
  }

//  def mapClose[I,O](f: I => O)(in: ?[I], out: ![O]) = proc{
//      repeat { out!(f(in?))
//        if (in.canInput == false) {
//          out.closeOut(); in.closeIn()
//        }
//      }
//      out.closeOut(); in.closeIn()
//      }

  def network(): Unit = {
    val next, out, dist2, dist3, result2, result3, result5, merge23, merge235 = OneOne[Int]
    val mul2, mul3, mul5 = OneOneBuf[Int](500)
    (
      component.prefix(1)(merge235,next)
        || teeCount[Int](next,out,dist2)
        || component.tee(dist2,dist3,mul2)
        || component.tee(dist3,mul3,mul5)
        || component.map((x:Int) => x*2)(mul2,result2)
        || component.map((x:Int) => x*3)(mul3,result3)
        || component.map((x:Int) => x*5)(mul5,result5)
        || merge(result2,result3,merge23)
        || merge(merge23,result5,merge235)
        || component.console(out)
    )()
  }

  def main(args: Array[String]) = network
}
