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
  
  test("board tests") {
    val board = Board.empty(11,10)
    board.rows should equal(11)
    board.cols should equal(10)
    board.neighbors(4, 4) should have size(8)
  }
  
}

