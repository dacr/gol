/*
 * Copyright 2016 David Crosson
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package gol

trait Cell {
  val alive:Boolean
}
case class LifeCell(age:Int=0) extends Cell {
  val alive=true
}
case class DeadCell(since:Int=0) extends Cell {
  val alive=false
}

// --------------------------------------------------------------------------------------

case class Board(cells:IndexedSeq[IndexedSeq[Cell]]) {  
  val rows = cells.size
  val cols = cells.head.size
  def neighbors(row:Int, col:Int) = {
    for {
      r <- row-1 to row+1
      if r>=0 && r<rows
      c <- col-1 to col+1
      if c>=0 && c<cols 
      if r!=row || c!=col
    } yield cells(r)(c)
  }
  def map(proc:(Int,Int,Cell)=>Cell):Board = {
    val newone = 
      for { row <- 0 until rows } yield
        for {col <- 0 until cols} yield proc(row,col,this(row,col))
    Board(newone)
  }
  def foreach(proc:(Int,Int,Cell)=>Unit) {
      for { row <- 0 until rows } 
        for {col <- 0 until cols}  proc(row,col,this(row,col))
  }
  def apply(row:Int,col:Int) = cells(row)(col)
  override def toString() = {
    val content = 
      for { row <- 0 until rows } yield
        for {col <- 0 until cols} yield cells(row)(col) match {
          case _:LifeCell => "x"
          case _:DeadCell => " "
        }
    def row2str(chs:IndexedSeq[String]) = chs.mkString("").replaceAll("\\s+$", "") 
    content.map{row2str}.mkString("\n")    
  }
}

object Board {
  def fill(rows:Int, cols:Int, filler:(Int,Int)=>Cell):Board = {
     val cells = 
       for { row <- 0 until rows } yield
         for {col <- 0 until cols} yield filler(row,col)
     Board(cells)
  }
  def empty(rows:Int, cols:Int):Board = fill(rows, cols, (_, _) => DeadCell())
}

// --------------------------------------------------------------------------------------

case class GameOfLife(board:Board, gen:Int=0) {
  def nextCell(row:Int, col:Int, cell:Cell):Cell = {
    val aliveCount=board.neighbors(row,col).filter(_.alive).size
    (aliveCount,cell) match {
      case (3,   c:DeadCell) => LifeCell()
      case (2|3, c:LifeCell) => LifeCell(c.age+1)
      case (2,   c:DeadCell) => c
      case (x,   c:LifeCell) if x<2 || x > 3 => DeadCell(gen)
      case (_,   c:DeadCell) => c
    }
  }
  def nextgen():GameOfLife =
    new GameOfLife(board.map(nextCell), gen+1)
  
  def add(row:Int, col:Int, patbasestr:String):GameOfLife = {
    val patbase = for {
      (line,row) <- patbasestr.split("\n").toList.zipWithIndex
      (ch, col) <- line.zipWithIndex
      if (ch!=' ')
    } yield (row,col)
    add(row,col,patbase)
  }
  def add(row:Int, col:Int, patbase:List[Tuple2[Int,Int]]):GameOfLife = {
    val pat = patbase.map{case(r,c) => (r+row, c+col)}    
    def frogFiller(r:Int, c:Int):Cell = if (pat.contains( (r,c) )) LifeCell() else DeadCell()
    val newboard = board.map{ (r,c,cell) =>
      if (pat.contains( (r,c) )) LifeCell() else cell
    }
    GameOfLife(newboard,gen)
  }
}

object GameOfLife {
  def apply(rows:Int, cols:Int):GameOfLife = new GameOfLife(Board.empty(rows,cols))  
}


// --------------------------------------------------------------------------------------

object GolPatterns {
  val blinker = 
    """xxx"""
  
  val toad    = 
    """ xxx
      |xxx
      |""".stripMargin
      
  val beacon  = 
    """xx
      |xx
      |  xx
      |  xx
      |""".stripMargin
      
  val glider =
    """ x
      |  x
      |xxx
      |""".stripMargin
      
  val pentadecathlon =
    """  xxx   xxx
      |
      |x    x x    x
      |x    x x    x
      |x    x x    x
      |  xxx   xxx
      |            
      |  xxx   xxx
      |x    x x    x
      |x    x x    x
      |x    x x    x
      |
      |  xxx   xxx
      |""".stripMargin

  val simple2complex = 
    """xxx
      |
      | x
      |
      |xxx
      |""".stripMargin
      
  val ship = 
    """x  x
      |    x
      |x   x
      | xxxx
      |""".stripMargin
  
  val snake =
    """x xx
      |xx x
      |""".stripMargin
    
}

// --------------------------------------------------------------------------------------

object Gol {
  
  def stdoutDisplay(gol:GameOfLife) = {
    import gol._
    for { row <- 0 until board.rows } {
      for { col <- 0 until board.cols } {
        if (board(row, col).alive) print('O') else print('_')
      }
      println()
    }
  }
  
  val consoleDisplay = {
    val display = new ConsoleDisplay
    (gol:GameOfLife) => {
      display.drawGol(gol)
      Thread.sleep(250L)
    }
  }
  
  @annotation.tailrec
  def loopOverGen(curgol:GameOfLife, remain:Int,process:(GameOfLife=>Unit)) {
    if (remain>0) {
      process(curgol)
      val nextgol = curgol.nextgen()
      loopOverGen(nextgol, remain-1,process)
    }
  }
  
  def main(args:Array[String]) {
    import GolPatterns._
    val gol =
      GameOfLife(40,120)
        .add(5,10,  blinker)
        .add(5,20,  toad)
        .add(5,30,  beacon)
        .add(5,40,  pentadecathlon)
        .add(10,70, simple2complex)
        .add(10,1,  glider)
        .add(15,10, ship)
        .add(20,6,  snake)
        .add(20,15, snake)

    //loopOverGen(gol, 5,stdoutDisplay)
    loopOverGen(gol, 200,consoleDisplay)
  }
}
