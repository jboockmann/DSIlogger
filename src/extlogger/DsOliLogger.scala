
/**
 *
 * Copyright (C) 2017 University of Bamberg, Software Technologies Research Group
 * <https://www.uni-bamberg.de/>, <http://www.swt-bamberg.de/>
 *
 * This file is part of the Data Structure Investigator (DSI) project, which received financial support by the
 * German Research Foundation (DFG) under grant no. LU 1748/4-1, see
 * <http://www.swt-bamberg.de/dsi/>.
 *
 * DSI is licensed under the GNU GENERAL PUBLIC LICENSE (Version 3), see
 * the LICENSE file at the project's top-level directory for details or consult <http://www.gnu.org/licenses/>.
 *
 * DSI is free software: you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or any later version.
 *
 * DSI is a RESEARCH PROTOTYPE and distributed WITHOUT ANY
 * WARRANTY, without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * The following people contributed to the conception and realization of the present DSI distribution (in
 * alphabetic order by surname):
 *
 * - Jan H. Boockmann
 * - Gerald Lüttgen
 * - Thomas Rupprecht
 * - David H. White
 *
 */
 
 /**
 * DsOliLogger.scala created on Oct 21, 2014
 *
 * Description: This logging functionality conists of three main parts:
 * - Different hierarchy levels can be selected for debugging
 * - Debug functions can be disabled via macros to boost performance
 * - Pattern matching can be used for guiding the debugging output
 */
package extlogger

import scala.util.matching.Regex
import java.io.PrintWriter
import java.io.File
import java.text.SimpleDateFormat
import java.io.FileWriter
import scala.language.experimental.macros
import reflect.macros.Context
import extutil.DsOliPathUtils

object DsOliLogger {

  val on = false

  object LogLevel extends Enumeration {
    type LogLevel = Value
    val Debug, Verbose, Info, Warning, Error, Quiet = Value
  }

  object FilterType extends Enumeration {
    type FilterType = Value
    val DisplayAll, SearchForPattern, FilterPatternOut = Value
  }

  import LogLevel._
  import FilterType._

  // first invocation resets log
  var append = false
  var appendLblLog = false

  var curLevel = Error
  var fileLog = true
  var applyFiltersToFile = false

  var filterClass = ""
  var filterMethod = ""
  var filterType = DisplayAll
  var filterClassRegEx = filterClass.r
  var filterMethodRegEx = filterMethod.r

  var logPath = DsOliPathUtils.getLogFilePath

  val levelOrdering = Map(Debug -> 0, Verbose -> 1, Info -> 2, Warning -> 3, Error -> 4, Quiet -> 5)

  var stringBuffer: StringBuilder = new StringBuilder()
  var stringBufferCount = 0
  var flushBufferCount = 500000

  def printLoggerStatus(): Unit = {
    println("DsOliLogger:: status of on flag (false=no logging): " + on)
    println("DsOliLogger:: status of logPath: " + logPath)
  }

  def configLogger(debug: Boolean, fileLog: Boolean): Unit = {
    // Search for the pattern
    DsOliLogger.setSearchForPattern()

    // Set the debug level
    if (debug)
      DsOliLogger.setDebugLevel
    else
      DsOliLogger.setErrorLevel

    // Shall the file logging be disabled
    if (!fileLog) DsOliLogger.disableFileLog

  }

  def setLogLevel(level: LogLevel) = curLevel = level

  def stringContainsPattern(pattern: String, regEx: Regex, msg: String): Boolean = {
    val retVal = if (pattern == "") {
      true
    } else {
      if (regEx.findFirstIn(msg).isDefined) {
        true
      } else {
        false
      }
    }
    return retVal
  }

  def log(level: LogLevel, msg: String): Unit = {
    val displayMsg = msg

    if (curLevel != Quiet && (curLevel == Debug || levelOrdering(level) >= levelOrdering(curLevel))) {
      filterType match {
        case SearchForPattern =>
          if (stringContainsPattern(filterClass, filterClassRegEx, msg) && stringContainsPattern(filterMethod, filterMethodRegEx, msg)) {
            println(displayMsg)
          }
        case FilterPatternOut =>
          if (!msg.contains(filterClass) && !msg.contains(filterMethod)) {
            println(displayMsg)
          }
        case _ => println(displayMsg)
      }
    }

    if (fileLog) {
      // Filters can also be applied to the log file
      if (applyFiltersToFile) {
        filterType match {
          case SearchForPattern =>
            if (stringContainsPattern(filterClass, filterClassRegEx, msg) && stringContainsPattern(filterMethod, filterMethodRegEx, msg)) {
              stringBuffer.append(displayMsg + "\n")
              stringBufferCount += 1
            }
          case FilterPatternOut =>
            val classTest = if (filterClass == "") true else !stringContainsPattern(filterClass, filterClassRegEx, msg)
            val methodTest = if (filterMethod == "") true else !stringContainsPattern(filterMethod, filterMethodRegEx, msg)
            if (classTest && methodTest) {
              stringBuffer.append(displayMsg + "\n")
              stringBufferCount += 1
            }
          case _ => throw new Exception("Logging not supported: " + filterType)
        }
      } else {
        stringBuffer.append(displayMsg + "\n")
        stringBufferCount += 1
      }
      writeBuffer
    }
  }

  def writeBuffer(): Unit = {
    // Only write in bursts
    if (stringBufferCount > flushBufferCount) {
      val writer = new PrintWriter(new FileWriter(logPath, append))
      // for second invocation append to file
      append = true
      writer.write(stringBuffer.toString())
      stringBuffer = new StringBuilder("")
      stringBufferCount = 0
      writer.close()
    }
  }

  def flushBuffer(): Unit = {
    stringBufferCount = flushBufferCount + 1
    writeBuffer
  }

  def setLogPath(logFilePath: String): Unit = {
    this.logPath = logFilePath
  }

  def enableFiltersForFile(): Unit = {
    this.applyFiltersToFile = true
  }

  def disableFiltersForFile(): Unit = {
    this.applyFiltersToFile = false
  }

  def setFilterForClass(classFilter: String): Unit = {
    this.filterClass = classFilter
    this.filterClassRegEx = this.filterClass.r
  }

  def setFilterForMethod(methodFilter: String): Unit = {
    this.filterMethod = methodFilter
    this.filterMethodRegEx = this.filterMethod.r
  }

  def setSearchForPattern(): Unit = {
    this.filterType = SearchForPattern
  }

  def setFilterOutPattern(): Unit = {
    this.filterType = FilterPatternOut
  }

  def resetFilter(): Unit = {
    this.filterType = DisplayAll
  }

  def setDebugLevel(): Unit = {
    curLevel = Debug
  }

  def setVerboseLevel(): Unit = {
    curLevel = Verbose
  }

  def setInfoLevel(): Unit = {
    curLevel = Info
  }

  def setWarningLevel(): Unit = {
    curLevel = Warning
  }

  def setErrorLevel(): Unit = {
    curLevel = Error
  }

  def setQuietLevel(): Unit = {
    curLevel = Quiet
  }

  def verbose(msg: String): Unit = macro verbose_impl
  def verbose_impl(c: Context)(msg: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._
    if (on) {
      reify {
        DsOliLogger.log(Verbose, msg.splice)
      }
    } else {
      reify {
        // Nothing
      }
    }
  }

  def info(msg: String): Unit = macro info_impl
  def info_impl(c: Context)(msg: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._
    if (on) {
      reify {
        DsOliLogger.log(Info, msg.splice)
      }
    } else {
      reify {
        // Nothing
      }
    }
  }

  def debug(msg: String): Unit = macro debug_impl
  def debug_impl(c: Context)(msg: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._
    if (on) {
      reify {
        DsOliLogger.log(Debug, msg.splice)
      }
    } else {
      reify {
        // Nothing
      }
    }
  }

  def warning(msg: String): Unit = macro warning_impl
  def warning_impl(c: Context)(msg: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._
    if (on) {
      reify {
        DsOliLogger.log(Warning, msg.splice)
      }
    } else {
      reify {
        // Nothing
      }
    }
  }

  def error(msg: String): Unit = macro error_impl
  def error_impl(c: Context)(msg: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._
    if (on) {
      reify {
        DsOliLogger.log(Error, msg.splice)
      }
    } else {
      reify {
        // Nothing
      }
    }
  }

  def enableFileLog(): Unit = {
    fileLog = true
  }

  def disableFileLog(): Unit = {
    fileLog = false
  }

  def writeLabelLog(msg: String): Unit = {
    val writer = new PrintWriter(new FileWriter(DsOliPathUtils.getPath + "labellog", appendLblLog))
    // for second invocation append to file
    appendLblLog = true
    writer.write(msg + "\n")
    writer.close()
  }
}
