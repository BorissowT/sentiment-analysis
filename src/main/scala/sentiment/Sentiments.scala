package sentiment

import java.awt.{Color, GridLayout}
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYDotRenderer
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.ui.ApplicationFrame
import org.jfree.util.ShapeUtilities

/**
  * @author hendrik
  * modified by akarakochev
  */
class Sentiments(sentiFile: String) {

  val sentiments: Map[String, Int] = getSentiments(sentiFile)

  val proc = new Processing()

  /** ********************************************************************************************
    *
    * Aufgabe 5
    *
    * ********************************************************************************************
    */

  def getDocumentGroupedByCounts(filename: String, wordCount: Int): List[(Int, List[String])] = {
    val url = getClass.getResource("/" + filename).getPath
    val src = scala.io.Source.fromFile(url.replaceAll("%20", " "))
    val iter = src.getLines()
    val lines: List[String] = (for (row <- iter) yield {
      row
    }).toList
    src.close()
    lines.flatMap(x => proc.getWords(x)).grouped(wordCount).toList.zipWithIndex.map(x=>(x._2+1,x._1))

  }

  def getDocumentSplitByPredicate(filename: String, predicate:String=>Boolean): List[(Int, List[String])] = {
    val url = getClass.getResource("/" + filename).getPath
    val src = scala.io.Source.fromFile(url.replaceAll("%20", " "))
    val iter = src.getLines()
    val lines: List[String] = (for (row <- iter) yield {
      row
    }).toList
    src.close
    val lines_1st_line_skipped = lines.tail
    val result_2 = lines_1st_line_skipped.flatMap(x =>
      if(predicate(x))
        x
      else proc.getWords(x)).map(x=>x.toString).foldLeft(List(List.empty[String])){
        (word_tail, word)=>{
          if(predicate(word))
            {List.empty[String] :: word_tail}
          else
            (word :: word_tail.head) :: word_tail.tail
        }
    }.reverse.map(x=>x.reverse).filter(_.nonEmpty).zipWithIndex.map(x=>(x._2+1,x._1))

    result_2
  }

  def analyseSentiments(l: List[(Int, List[String])]): List[(Int, Double, Double)] = {
    val updatedList = l.flatMap(element =>List((element._1-1,
      element._2.map(word=>sentiments.getOrElse(word, 0)).filterNot(_ == 0).sum.toDouble / element._2.size.toDouble,
      element._2.map(word=>sentiments.getOrElse(word, 0)).filterNot(_ == 0).size.toDouble/element._2.size.toDouble
    )))

    updatedList
  }

  /** ********************************************************************************************
    *
    * Helper Functions
    *
    * ********************************************************************************************
    */

  def getSentiments(filename: String): Map[String, Int] = {
    val url = getClass.getResource("/" + filename).getPath
    val src = scala.io.Source.fromFile(url.replaceAll("%20"," "))
    val iter = src.getLines()
    val result: Map[String, Int] = (for (row <- iter) yield {
      val seg = row.split("\t"); (seg(0) -> seg(1).toInt)
    }).toMap
    src.close()
    result
  }

  def createGraph(data: List[(Int, Double, Double)], xlabel:String="Abschnitt", title:String="Sentiment-Analyse"): Unit = {

    //create xy series
    val sentimentsSeries: XYSeries = new XYSeries("Sentiment-Werte")
    data.foreach { case (i, sentimentValue, _) => sentimentsSeries.add(i, sentimentValue) }
    val relWordsSeries: XYSeries = new XYSeries("Relative Haeufigkeit der erkannten Worte")
    data.foreach { case (i, _, relWordsValue) => relWordsSeries.add(i, relWordsValue) }

    //create xy collections
    val sentimentsDataset: XYSeriesCollection = new XYSeriesCollection()
    sentimentsDataset.addSeries(sentimentsSeries)
    val relWordsDataset: XYSeriesCollection = new XYSeriesCollection()
    relWordsDataset.addSeries(relWordsSeries)

    //create renderers
    val relWordsDot: XYDotRenderer = new XYDotRenderer()
    relWordsDot.setDotHeight(5)
    relWordsDot.setDotWidth(5)
    relWordsDot.setSeriesShape(0, ShapeUtilities.createDiagonalCross(3, 1))
    relWordsDot.setSeriesPaint(0, Color.BLUE)

    val sentimentsDot: XYDotRenderer = new XYDotRenderer()
    sentimentsDot.setDotHeight(5)
    sentimentsDot.setDotWidth(5)

    //create xy axis
    val xax: NumberAxis = new NumberAxis(xlabel)
    val y1ax: NumberAxis = new NumberAxis("Sentiment Werte")
    val y2ax: NumberAxis = new NumberAxis("Relative Haeufigfkeit")

    //create plots
    val plot1: XYPlot = new XYPlot(sentimentsDataset, xax, y1ax, sentimentsDot)
    val plot2: XYPlot = new XYPlot(relWordsDataset, xax, y2ax, relWordsDot)

    val chart1: JFreeChart = new JFreeChart(plot1)
    val chart2: JFreeChart = new JFreeChart(plot2)
    val frame: ApplicationFrame = new ApplicationFrame(title)
    frame.setLayout(new GridLayout(2,1))

    val chartPanel1: ChartPanel = new ChartPanel(chart1)
    val chartPanel2: ChartPanel = new ChartPanel(chart2)

    frame.add(chartPanel1)
    frame.add(chartPanel2)
    frame.pack()
    frame.setVisible(true)
    
    println("Please press enter....")
    System.in.read()
    frame.setVisible(false)
    frame.dispose
  }
}
