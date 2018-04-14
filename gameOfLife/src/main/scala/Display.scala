import java.awt._
import io.threadcso._

class Display(N: Int, a:Array[Array[Boolean]]) extends Frame {
  // Define some constants
  private val blockSize = 6
  private val padding   = 1
  private val gridSize  = blockSize+2*padding
  
  // Set up the display
  private val pane = new ScrollPane()
  pane.setSize(N*gridSize, N*gridSize)
  pane.setSize(50*gridSize,50*gridSize)
  private val board = new Board()
  pane.add(board)
  this.add(pane, "Center")
  this.pack()
  this.setVisible(true)
  this.setTitle("Life")
  this.setSize(N*gridSize, N*gridSize)
  this.setSize(50*gridSize,50*gridSize)

  // Fill in all the squares
  def draw = {
    for (i <- 0 until N){
        for (j<-0 until N){
            if (a(i)(j)) board.drawAt(j,i) else board.blankAt(j,i)
        }
    }
  }

  override def paint(g: Graphics) = draw

  class Board extends Component{
    // Define colours
    val backgroundColor = Color.gray.brighter
    val blockColor      = Color.black

    // Paint the square at (x,y) in colour c
    private def paintAt(x: Int, y: Int, c: Color) = {    
      val g = getGraphics()
      g.setColor(c)
      g.fillRect(x*gridSize+padding, y*gridSize+padding, blockSize, blockSize)
    }

    // Draw a piece
    def drawAt(x: Int, y: Int) = paintAt(x, y, blockColor)

    // Draw a blank square
    def blankAt(x: Int, y: Int) = paintAt(x, y, backgroundColor)
  }

}

object GameOfLife {

  // seed for Blinker
//  val width = 5
//  val seed = Array.ofDim[Boolean](width,width)
//  seed(2)(1) = true
//  seed(2)(2) = true
//  seed(2)(3) = true

  //seed of Pulsar
  val width = 17
  val seed = Array.ofDim[Boolean](width,width)
  seed(2)(4)=true;seed(2)(5)=true;seed(2)(6) = true
  seed(2)(10)=true;seed(2)(11)=true;seed(2)(12) = true
  seed(4)(2)=true;seed(5)(2)=true;seed(6)(2) = true
  seed(4)(7)=true;seed(5)(7)=true;seed(6)(7)=true
  seed(4)(9)=true;seed(5)(9)=true;seed(6)(9)=true
  seed(4)(14)=true;seed(5)(14)=true;seed(6)(14)=true
  seed(7)(4)=true;seed(7)(5)=true;seed(7)(6)=true
  seed(7)(10)=true;seed(7)(11)=true;seed(7)(12)=true

  seed(9)(4)=true;seed(9)(5)=true;seed(9)(6) = true
  seed(9)(10)=true;seed(9)(11)=true;seed(9)(12) = true
  seed(10)(2)=true;seed(11)(2)=true;seed(12)(2) = true
  seed(10)(7)=true;seed(11)(7)=true;seed(12)(7)=true
  seed(10)(9)=true;seed(11)(9)=true;seed(12)(9)=true
  seed(10)(14)=true;seed(11)(14)=true;seed(12)(14)=true
  seed(14)(4)=true;seed(14)(5)=true;seed(14)(6)=true
  seed(14)(10)=true;seed(14)(11)=true;seed(14)(12)=true


  val WORKERS = 4
  // process Display shares the barrier with the workers
  val barrier = new Barrier(WORKERS+1)

  // Slowing the drawing-rate down
  def takeTime(t:Nanoseconds):Unit = {
      val deadline = nanoTime + t
      val ahead = deadline - nanoTime
      barrier.sync()
      barrier.sync()
      if (ahead >0 ) sleep(ahead)
  }

  def displayChart = proc{
      val display = new Display(width,seed)
      barrier.sync()
      while (true){
        display.draw
        takeTime(seconds(0.3))
      }
  }

  def worker (startrow:Int,ROWS:Int) = proc {
    val local = Array.ofDim[Boolean](ROWS,width)
    barrier.sync()

    while (true){
        for (row <- startrow until startrow + ROWS; col <-0 until width-1){
            val rowUp = (row-1+width)%width
            val colLeft = (col-1+width)%width
            val rowDown = (row+1)%width
            val colRight = (col+1)%width
//            println(f"$rowUp%s $colRight%s $rowDown%s $colRight%s")
            val neighbours = Array(seed(row)(colLeft), seed(row)(colRight),
              seed(rowUp)(col), seed(rowDown)(col),
              seed(rowUp)(colLeft), seed(rowDown)(colLeft),
              seed(rowUp)(colRight),seed(rowDown)(colRight))
            var count = 0
            for (bool <- neighbours){
              if (bool == true )
                count +=1
            }
            if (count < 2)
              local(row-startrow)(col) = false
            if (count > 3)
              local(row-startrow)(col) = false
            if (count == 3 )
              local(row-startrow)(col) = true
            if (count == 2 && seed(row)(col) == true)
              local(row-startrow)(col) = true
        }
      barrier.sync()
      for (row <- startrow until startrow + ROWS; col <-0 until width-1){
        seed(row)(col) = local(row-startrow)(col)
      }
      barrier.sync()
    }
  }

//    val System = worker(0,1) || worker(1,1) || worker(2,1)||worker(3,2)||displayChart
  val System = worker(0,4) || worker(4,4) || worker(8,4)||worker(12,5)||displayChart

  def main(args: Array[String]) = {
     System()
//    val display = new Display(width,seed)
//    display.draw
  }

}

