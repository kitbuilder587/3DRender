package render

import java.awt.Graphics2D
import java.awt.image.BufferedImage
import javax.swing.{JFrame, WindowConstants}

object Main {
  private val w = 1366
  private val h = 768

  def draw(g: Graphics2D): Unit = { //Создаем буффер в который рисуем кадр.
    val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    //Рисуем кадр.
    Render.render(img)
    g.drawImage(img, 0, 0, null)
  }

  def main(args: Array[String]): Unit = {
    val jf = new JFrame()
    jf.setSize(w, h) //размер экрана

    jf.setUndecorated(false) //показать заголовок окна

    jf.setTitle("Моя супер программа")
    jf.setVisible(true)
    jf.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    jf.createBufferStrategy(2)
    //в бесконечном цикле рисуем новый кадр
    while (true) {
      val frameLength = 1000 / 60 //пытаемся работать из рассчета  60 кадров в секунду
      val start = System.currentTimeMillis
      val bs = jf.getBufferStrategy
      val g = bs.getDrawGraphics.asInstanceOf[Graphics2D]
      g.clearRect(0, 0, jf.getWidth, jf.getHeight)
      draw(g)
      bs.show()
      g.dispose()
      val `end` = System.currentTimeMillis
      val len = `end` - start
      if (len < frameLength) Thread.sleep(frameLength - len)
    }
  }
}
