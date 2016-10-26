package gol

import com.googlecode.lanterna._
import com.googlecode.lanterna.terminal._
import com.googlecode.lanterna.screen._

class ConsoleDisplay {
  
		val terminal = new DefaultTerminalFactory().createTerminal()
		val screen = new TerminalScreen(terminal)
		val gr = screen.newTextGraphics()
		screen.startScreen()
		
		def drawGol(gol:GameOfLife) {
  		screen.clear()  		
  		gol.board.foreach{ case (row,col,cell) =>
  		  cell match {
  		    case c:LifeCell if c.age==1 => gr.setForegroundColor(TextColor.ANSI.GREEN)
  		    case c:LifeCell if c.age==2 => gr.setForegroundColor(TextColor.ANSI.MAGENTA)
  		    case _ => gr.setForegroundColor(TextColor.ANSI.BLACK)
  		  }
  		  if (cell.alive) gr.putString(col, row, "O")
  		}
  		screen.refresh()
		}

		def finished() {
		  screen.stopScreen()
		}
}