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

import org.scalatest._
import Matchers._

class GolTest extends FunSuite  {
  
  test("board basic test") {
    val board = Board.empty(11,10)
    board.rows should equal(11)
    board.cols should equal(10)
    board.neighbors(4, 4) should have size(8)
  }
  
  test("blinker test") {
    import GolPatterns._
    val gen0 = GameOfLife(3,3).add(1, 0, blinker)
    val gen1 = gen0.nextgen()
    val gen2 = gen1.nextgen()
    gen1.board.toString should equal(
      """ x
        | x
        | x""".stripMargin
    )
    gen2.board.toString should equal(gen0.board.toString)
  }
  
  test("toad test") {
    import GolPatterns._
    val gen0 = GameOfLife(4,4).add(1, 0, toad)
    val gen1 = gen0.nextgen()
    val gen2 = gen1.nextgen()
    gen1.board.toString should equal(
      """  x
        |x  x
        |x  x
        | x""".stripMargin
    )
    gen2.board.toString should equal(gen0.board.toString)
  }
  
}

