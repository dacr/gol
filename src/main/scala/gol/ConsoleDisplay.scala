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
  		  gr.setForegroundColor(TextColor.ANSI.RED)
  		  gr.putString(0, 0, "gen#"+gol.gen)
  		  val color = cell match {
  		    case c:LifeCell if c.age==0 => TextColor.ANSI.YELLOW
  		    case c:LifeCell if c.age==1 => TextColor.ANSI.GREEN
  		    case c:LifeCell if c.age==2 => TextColor.ANSI.MAGENTA
  		    case _ => TextColor.ANSI.BLACK
  		  }
  		  if (cell.alive) {
  		    gr.setForegroundColor(color)
  		    gr.putString(col, row, "O")
  		  }
  		}
  		screen.refresh()
		}

		def finished() {
		  screen.stopScreen()
		}
}