
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
 * DsOliPathUtils.scala created on Mar 24, 2015
 *
 * Description: Functions for calculating pathes to the log directory.
 */
package extutil

import java.io.File

object DsOliPathUtils {

  var project: String = ""
  var logDir = "log"
  var logFile = "log"

  def getXMLFile(): String = {
    project.reverse.replaceFirst("lmx.ecart/", "").replaceFirst("/.*", "").reverse
  }

  def getPath(): String = {
    val tmpFile = new File(project)
    val absolutePath = tmpFile.getAbsolutePath()
    val tmpPath = absolutePath.substring(0, absolutePath.lastIndexOf(File.separator)) + "/"
    val tmpDir = new File(tmpPath)
    if(!tmpDir.exists) tmpDir.mkdir()
    tmpPath
  }
  
  def getLogDirPath(): String = {
    val tmpPath = getPath() + logDir + "/"
    val tmpFile = new File(tmpPath)
    if(!tmpFile.exists) tmpFile.mkdir()
    tmpPath
  }
  
  def getLogFilePath() : String = {
    val tmpFilePath = getLogDirPath + logFile
    val tmpFile = new File(tmpFilePath)
    if(!tmpFile.exists) tmpFile.createNewFile()
    tmpFilePath
  }

}
