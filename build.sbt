import java.util.Optional

name := "triangulation"

version := "0.1"

scalaVersion := "2.12.7"
// графические библиотеки
libraryDependencies ++= Seq(
  "org.jogamp.gluegen" % "gluegen-rt-main" % "2.3.2",
  "org.jogamp.jogl" %"jogl-all-main" % "2.3.2",
  "junit" % "junit" % "4.12" % Test,
  "org.scalatest" %% "scalatest" % "3.1.0" % Test
)



// слишком декларативно, каждую строчку приходится гуглить
// перестарались и сделали хуже чем в gradle(((((
mainClass in (Compile, run) := Some("ru.udsu.algs.ui.DelaunayTriangulationExample")