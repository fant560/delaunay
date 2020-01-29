package ru.udsu.algs.ui

import java.awt.event.{MouseListener, WindowAdapter, WindowEvent}
import java.awt.{Color, Dimension, Frame}

import com.jogamp.opengl._
import com.jogamp.opengl.awt.GLCanvas
import com.jogamp.opengl.util.FPSAnimator
import ru.udsu.algs.DelaunayAlgorithm
import java.awt.event._

import com.jogamp.opengl.fixedfunc.{GLLightingFunc, GLMatrixFunc}
import ru.udsu.algs.utils.Triangle

import scala.collection.mutable.ListBuffer


object Constants {
  val DIMENSION = new Dimension(1200, 1024)
  val COLOR_TRIANGLE_FILL = new Color(151, 251, 152)
  val COLOR_TRIANGLE_EDGES = new Color(0, 0, 255)
  val COLOR_TRIANGLE_BORDER = new Color(255, 0, 0)
  val COLOR_BACKGROUND = new Color(105, 105, 105)
}

/**
 * Графическое представление алгоритма
 */
object DelaunayTriangulationExample {
  import Constants._

  def main(args: Array[String]): Unit = {
    val frame = new Frame("Пример триангуляции")
    frame.setResizable(false)
    val caps = new GLCapabilities(GLProfile.get("GL2"))
    caps.setSampleBuffers(true)
    caps.setNumSamples(8)


    val canvas = new GLCanvas(caps)

    val ex = new DelaunayTriangulationExample()
    val listener: MouseListener = ex
    canvas.addGLEventListener(ex)
    canvas.setPreferredSize(DIMENSION)
    canvas.addMouseListener(listener)

    frame.add(canvas)

    val animator = new FPSAnimator(canvas, 25)
    frame.addWindowListener(new WindowAdapter() {
      override def windowClosing(e: WindowEvent): Unit = {
        // runnable
        new Thread(() => {
          animator.stop
          System.exit(0)
        }).start()
      }
    })
    frame.setVisible(true)
    frame.pack()
    animator.start()
  }
}

class DelaunayTriangulationExample extends GLEventListener with MouseListener {

  import Constants._

  var algImplementation: DelaunayAlgorithm = _
  val pointSet = new ListBuffer[ru.udsu.algs.utils.Point]()

  override def init(drawable: GLAutoDrawable): Unit = {
    val gl = drawable.getGL.getGL2
    gl.glDisable(GL.GL_CULL_FACE)
    gl.glShadeModel(GLLightingFunc.GL_SMOOTH)
    gl.glClearColor(COLOR_BACKGROUND.getRed / 255.0f,
      COLOR_BACKGROUND.getGreen / 255.0f,
      COLOR_BACKGROUND.getBlue / 255.0f, 1)
    gl.glClearDepth(1.0f)
    gl.glEnable(GL.GL_DEPTH_TEST)
    gl.glDepthFunc(GL.GL_LEQUAL)
    gl.glHint(GL2ES1.GL_PERSPECTIVE_CORRECTION_HINT, GL.GL_NICEST)
    gl.setSwapInterval(1)
    gl.glDisable(GL.GL_CULL_FACE)
    algImplementation = new DelaunayAlgorithm(pointSet)
  }

  override def reshape(drawable: GLAutoDrawable,
                       x: Int,
                       y: Int,
                       width: Int,
                       height: Int): Unit = {
    val gl = drawable.getGL.getGL2
    gl.glMatrixMode(GLMatrixFunc.GL_PROJECTION)
    gl.glLoadIdentity()
    gl.glOrtho(0, DIMENSION.getWidth, DIMENSION.getHeight, 0, 1.0, -1.0)
    gl.glMatrixMode(GLMatrixFunc.GL_MODELVIEW)
    gl.glLoadIdentity()
  }

  override def display(drawable: GLAutoDrawable): Unit = {
    val gl = drawable.getGL.getGL2
    gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT)
    gl.glLoadIdentity()
    gl.glTranslatef(0.0f, 0.0f, 0.0f)
    gl.glLineWidth(1.0f)
    gl.glColor3ub(COLOR_TRIANGLE_FILL.getRed.asInstanceOf[Byte],
      COLOR_TRIANGLE_FILL.getGreen.asInstanceOf[Byte],
      COLOR_TRIANGLE_FILL.getBlue.asInstanceOf[Byte])
    gl.glBegin(GL.GL_TRIANGLES)

    def drawVertices(triangle: Triangle, isTwoDirection: Boolean): Unit = {
      val a = triangle.a
      val b = triangle.b
      val c = triangle.c
      if (isTwoDirection) {
        gl.glVertex2d(a.x, a.y)
        gl.glVertex2d(b.x, b.y)
        gl.glVertex2d(b.x, b.y)
        gl.glVertex2d(c.x, c.y)
        gl.glVertex2d(c.x, c.y)
        gl.glVertex2d(a.x, a.y)
      }else {
        gl.glVertex2d(a.x, a.y)
        gl.glVertex2d(b.x, b.y)
        gl.glVertex2d(c.x, c.y)
      }

    }

    algImplementation.triangulationContainer.triangles.foreach(drawVertices(_, isTwoDirection = false))

    gl.glEnd()
    gl.glColor3ub(COLOR_TRIANGLE_EDGES.getRed.asInstanceOf[Byte],
      COLOR_TRIANGLE_EDGES.getGreen.asInstanceOf[Byte],
      COLOR_TRIANGLE_EDGES.getBlue.asInstanceOf[Byte])
    gl.glBegin(GL.GL_LINES)
    algImplementation.triangulationContainer.triangles.foreach(drawVertices(_, isTwoDirection = true))
    gl.glEnd()
    // draw all points
    gl.glPointSize(5.5f)
    gl.glColor3f(0.2f, 1.2f, 0.25f)
    gl.glColor3ub(COLOR_TRIANGLE_BORDER.getRed.asInstanceOf[Byte],
      COLOR_TRIANGLE_BORDER.getGreen.asInstanceOf[Byte],
      COLOR_TRIANGLE_BORDER.getBlue.asInstanceOf[Byte])
    gl.glBegin(GL.GL_POINTS)

    pointSet.foreach(point => gl.glVertex2d(point.x, point.y))
    gl.glEnd()
  }

  def displayChanged(drawable: GLAutoDrawable, modeChanged: Boolean, deviceChanged: Boolean): Unit = {
  }

  @Override override def dispose(drawable: GLAutoDrawable): Unit = {
  }

  @Override def mouseClicked(e: MouseEvent): Unit = {
  }

  @Override def mousePressed(e: MouseEvent): Unit = {
    val p = e.getPoint
    pointSet += ru.udsu.algs.utils.Point(p.x, p.y)
    try {
      algImplementation.triangulation()
    } catch {
      case _: IllegalStateException =>
    }
  }

  @Override def mouseReleased(e: MouseEvent): Unit = {
  }

  @Override def mouseEntered(e: MouseEvent): Unit = {
  }

  @Override def mouseExited(e: MouseEvent): Unit = {
  }
}
