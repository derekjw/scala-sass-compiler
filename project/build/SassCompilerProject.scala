import sbt._

class SassCompilerProject(info: ProjectInfo) extends DefaultProject(info)
{
  override def compileOptions = Optimize :: Unchecked :: super.compileOptions.toList
  
  val specs = "org.scala-tools.testing" %% "specs" % "1.6.5-SNAPSHOT" % "test->default"
  val scalacheck = "org.scala-tools.testing" %% "scalacheck" % "1.7" % "test->default"
  
  override def repositories = Set(ScalaToolsSnapshots)
}

