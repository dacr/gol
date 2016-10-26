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
  def map(proc:(Int,Int)=>Cell):Board = {
    val newone = 
      for { row <- 0 until rows } yield
        for {col <- 0 until cols} yield proc(row,col)
    Board(newone)
  }
  def foreach(proc:(Int,Int,Cell)=>Unit) {
      for { row <- 0 until rows } 
        for {col <- 0 until cols}  proc(row,col,this(row,col))
  }
  def apply(row:Int,col:Int) = cells(row)(col)
}

object Board {
  def filled(rows:Int, cols:Int, filler:(Int,Int)=>Cell):Board = {
     val cells = 
       for { row <- 0 until rows } yield
         for {col <- 0 until cols} yield filler(row,col)
     Board(cells)
  }
  def empty(rows:Int, cols:Int):Board = filled(rows, cols, (_, _) => DeadCell())
}

case class GameOfLife(board:Board, gen:Int) {
  def nextCell(row:Int, col:Int):Cell = {
    val cell = board(row,col)
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
}

object GameOfLife {
  def apply(rows:Int, cols:Int, filler:(Int,Int)=>Cell):GameOfLife = {
    val board = Board.filled(rows,cols,filler)
    new GameOfLife(board, 0)
  }
  
  def frog():GameOfLife = {
    val patbase = (1, 0)::(0, 3)::(1, 1)::(1, 2)::(0, 1)::(0, 2)::Nil
    val pat = patbase.map{case(r,c) => (r+5, c+5)}
    def frogFiller(r:Int, c:Int):Cell = if (pat.contains( (r,c) )) LifeCell() else DeadCell()
    GameOfLife(20,20,frogFiller)
  }
}


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
      Thread.sleep(1000L)
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
    val gol = GameOfLife.frog()
    //loopOverGen(gol, 5,stdoutDisplay)
    loopOverGen(gol, 60,consoleDisplay)
  }
}
