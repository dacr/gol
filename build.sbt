name := "gol"

version := "0.0.2-SNAPSHOT"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-unchecked", "-deprecation" , "-feature", "-language:implicitConversions")

mainClass in assembly := Some("gol.Gol")

jarName in assembly := "gol.jar"

libraryDependencies += "com.googlecode.lanterna" % "lanterna" % "3.0.0-beta3"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "junit" % "junit" % "4.12" % "test"

initialCommands in console := """
   |import gol._
   |""".stripMargin


sourceGenerators in Compile <+= 
 (sourceManaged in Compile, version, name, jarName in assembly) map {
  (dir, version, projectname, jarexe) =>
  val file = dir / "dummy" / "MetaInfo.scala"
  IO.write(file,
  """package gol 
    |object MetaInfo { 
    |  val version="%s"
    |  val project="%s"
    |  val jarbasename="%s"
    |}
    |""".stripMargin.format(version, projectname, jarexe.split("[.]").head) )
  Seq(file)
}
