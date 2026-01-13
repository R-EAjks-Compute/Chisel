// SPDX-License-Identifier: Apache-2.0

package chisel3.testing

import firrtl.options.StageUtils.dramaticMessage
import java.io.{BufferedReader, ByteArrayOutputStream, File, IOException, InputStreamReader, PrintWriter}
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.Console.{withErr, withOut}
import scala.util.control.NoStackTrace

object FileCheck {

  object Exceptions {

    /** Indicates that `FileCheck` was not found. */
    class NotFound private[FileCheck] (message: String)
        extends RuntimeException(
          dramaticMessage(
            header = Some("FileCheck was not found! Did you forget to install it?"),
            body = message
          )
        )
        with NoStackTrace

    /** Indicates that `FileCheck` failed. */
    class NonZeroExitCode private[FileCheck] (binary: String, exitCode: Int, message: String)
        extends RuntimeException(
          dramaticMessage(
            header = Some(s"FileCheck returned a non-zero exit code."),
            body = s"""|Exit Code: $exitCode
                       |
                       |Command:
                       |  $binary
                       |
                       |Command output:
                       |---
                       |$message
                       |---""".stripMargin
          )
        )
        with NoStackTrace

  }

  /** Recursively delete a directory and all its contents. */
  private def deleteRecursively(file: File): Unit = {
    if (file.isDirectory) {
      file.listFiles().foreach(deleteRecursively)
    }
    file.delete()
  }

}

trait FileCheck {

  /** Helpers to run `FileCheck` on a string input. */
  implicit class StringHelpers(input: String) {

    /** Run `FileCheck` on a string with some options.
      *
      * {{{
      * import chisel3.testing.FileCheck
      * import chisel3.testing.scalatest.TestingDirectory
      * import org.scalatest.flatspec.AnyFlatSpec
      * import org.scalatest.matchers.should.Matchers
      *
      * class Foo extends AnyFlatSpec with Matchers with FileCheck with TestingDirectory {
      *
      *   behavior of ("Bar")
      *
      *   it should "work" in {
      *     "Hello world!".fileCheck()(
      *       """|CHECK:      Hello
      *          |CHECK-SAME: world
      *          |""".stripMargin
      *     )
      *   }
      *
      * }
      *
      * }}}
      *
      * @param fileCheckArgs arguments to pass directly to FileCheck
      * @param check a string of checks to pass to `FileCheck`
      * @param testingDirectory an implementation of [[HasTestingDirectory]]
      * that controls where intermediary files are written.
      *
      * @note See [FileCheck
      * Documentation](https://llvm.org/docs/CommandGuide/FileCheck.html) for
      * more information.
      */
    def fileCheck(fileCheckArgs: String*)(
      check: String
    )(implicit testingDirectory: HasTestingDirectory): Unit = {
      // Filecheck needs to have the thing to check in a file.
      //
      // TODO: This could be made ephemeral or use a named pipe?
      val baseDir = testingDirectory.getDirectory
      val dir = if (baseDir.isAbsolute) baseDir else Paths.get(System.getProperty("user.dir")).resolve(baseDir)
      Files.createDirectories(dir)
      val tempDir = Files.createTempDirectory(dir, "filecheck")
      val checkFile = tempDir.resolve("check")
      val inputFile = tempDir.resolve("input")
      Files.write(checkFile, check.getBytes)
      Files.write(inputFile, input.getBytes)

      val stdoutStream, stderrStream = new java.io.ByteArrayOutputStream
      val stdoutWriter = new PrintWriter(stdoutStream)
      val stderrWriter = new PrintWriter(stderrStream)

      val cmd = Seq("FileCheck", checkFile.toString) ++ fileCheckArgs
      val (exitCode, command) =
        try {
          val pb = new ProcessBuilder(cmd: _*)
          pb.redirectInput(inputFile.toFile)
          val process = pb.start()

          // Read stdout
          val stdoutReader = new BufferedReader(new InputStreamReader(process.getInputStream))
          var line: String = null
          while ({ line = stdoutReader.readLine(); line != null }) {
            stdoutWriter.println(line)
          }
          stdoutReader.close()

          // Read stderr
          val stderrReader = new BufferedReader(new InputStreamReader(process.getErrorStream))
          while ({ line = stderrReader.readLine(); line != null }) {
            stderrWriter.println(line)
          }
          stderrReader.close()

          val code = process.waitFor()
          (code, cmd)
        } catch {
          case a: IOException if a.getMessage.startsWith("Cannot run program") =>
            throw new FileCheck.Exceptions.NotFound(a.getMessage)
        }
      stdoutWriter.close()
      stderrWriter.close()

      if (exitCode == 0) {
        FileCheck.deleteRecursively(tempDir.toFile)
      } else {
        throw new FileCheck.Exceptions.NonZeroExitCode(
          s"cat $inputFile | ${command.mkString(" ")}",
          exitCode,
          stderrStream.toString
        )
      }
    }

  }

}
