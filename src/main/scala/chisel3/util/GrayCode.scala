// SPDX-License-Identifier: Apache-2.0

package chisel3.util

import chisel3._
import chisel3.ltl.AssertProperty
import chisel3.test.UnitTest

object BinaryToGray {

  /** Turns a binary number into gray code. */
  def apply(in: UInt): UInt = in ^ (in >> 1)
}

object GrayToBinary {

  /** Inverts the [[BinaryToGray]] operation. */
  def apply(in: UInt, width: Int): UInt = apply(in(width - 1, 0))

  /** Inverts the [[BinaryToGray]] operation. */
  def apply(in: UInt): UInt = if (in.getWidth < 2) { in }
  else {
    val bits = in.getWidth - 2 to 0 by -1
    Cat(bits.scanLeft(in.head(1)) { case (prev, ii) => prev ^ in(ii) })
  }
}

package test {

  /** Exhaustively test the gray conversion on 16 bits. */
  object GrayCodeExhaustive extends RawModule with UnitTest with SimulationTestHarnessInterface with Public {
    // FIXME: `Public` should not be necessary since the module is explicitly
    // marked as a `SimulationTest`. A bug in firtool currently causes ports to
    // be deleted when the modules is not public. Remove `Public` when the bug
    // is fixed.
    val clock = IO(Input(Clock()))
    val init = IO(Input(Bool()))
    val done = IO(Output(Bool()))
    val success = IO(Output(Bool()))
    SimulationTest(this)

    withClockAndReset(clock, init) {
      val value = RegInit(0.U(16.W))
      val numMismatches = RegInit(0.U((value.getWidth + 1).W))
      val gray = BinaryToGray(value)
      val binary = GrayToBinary(gray)
      done := false.B
      when(value.andR || numMismatches >= 10.U) {
        done := true.B
      }.otherwise {
        when(value =/= binary) {
          printf(p"MISMATCH: $value -> $gray -> $binary\n")
          numMismatches := numMismatches + 1.U
        }
        value := value + 1.U
      }
      success := numMismatches === 0.U
    }
  }
}
