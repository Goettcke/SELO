import java.awt.image.BufferedImage
import java.io.File
import java.net.{HttpURLConnection, URI, URL}
import java.nio.charset.StandardCharsets
import javax.imageio.ImageIO

import scala.io.Source

/**
  * Created by jogoe12 on 24/09/2017.
  */
object Core {
  val f = new File("C:\\Users\\jogoe12\\Documents\\GitHub\\SELO\\data\\somedata.txt")
  import java.lang.Math._
  def main(args: Array[String]): Unit = {
    val input: Seq[String] = Source.fromFile(f).getLines().toList
    val alphabet = input.mkString.distinct
    val n = input.size.toDouble
    val s = 20.toDouble
    /*small sample correction*/
    val ssCorrection = (1 / Math.log(2))*((s-1)/2*n.toDouble)

    def relativeFrequency(position : Int): Seq[(Double, Char)] = {
      val sequenceCol = input.map(x => x(position))
      alphabet.map(x => sequenceCol.count(y => y==x)/n.toDouble).zip(alphabet)

    }

    def relativeFrequencyForAA(position : Int, aa : Char ): Double = relativeFrequency(position).find(x => x._2 == aa).get._1

    // Shannon Entropy
    def entropy(position : Int) = {
      relativeFrequency(position).map{
        case (0D,_) => 0
        case x => x._1 * (Math.log(x._1)/Math.log(2))
      }.sum * (-1)
    }
    /*information content*/
    def infoCont(position : Int) = {
      (log(20) / log(2)) - (entropy(position)-ssCorrection)
    }

    def height(aa : Char, position : Int): Double = {
      relativeFrequencyForAA(position, aa) * infoCont(position)
    }


    val output = input.map(x => x.zipWithIndex.map(y => (y._1,height(y._1, y._2))))

    def prettyPrint = {
      val o = output.map{x =>
        x.map{y =>
          val num = f" ${y._1} ${y._2}%2.2f"
          num + Stream.continually(" ").take(8-num.length).mkString
        }.mkString(" | ")
      }

      o.zip(alphabet).map(x => x._2 + "  | " + x._1).mkString("\n" + Stream.continually("-").take(o.head.size).mkString + "-----\n")

    }

    val conn = new URL("http://weblogo.berkeley.edu/logo.cgi").openConnection().asInstanceOf[HttpURLConnection]

    val urlParameters = s"sequence=${input.mkString("%0D%0A")}" +
      s"&aligned_file=&format=PNG&logowidth=18&logoheight=5&logounits=cm&kind=AUTO&firstnum=1&logostart=&logoend=" +
      s"&smallsamplecorrection=on&symbolsperline=32&res=96&res_units=ppi&antialias=on&title=&barbits=&yaxis=on" +
      s"&yaxis_label=bits&xaxis=on&xaxis_label=&showends=on&shrink=0.5&fineprint=on&ticbits=1&colorscheme=DEFAULT" +
      s"&symbol1=KRH&color1=green&rgb1=&symbol2=DE&color2=blue&rgb2=&symbol3=AVLIPWFM&color3=red&rgb3=&symbol4=" +
      s"&color4=black&rgb4=&symbol5=&color5=purple&rgb5=&symbol6=&color6=orange&rgb6=&symbol7=&color7=black" +
      s"&rgb7=&color0=black&rgb0=&command=Create+Logo: undefined"
    val postData = urlParameters.getBytes(StandardCharsets.UTF_8)


/*
    url.setRequestProperty("sequence",input.mkString("%0D%0A"))
    url.setRequestProperty("aligned_file","")
    url.setRequestProperty("format","PNG")
    url.setRequestProperty("logowidth","18")
    url.setRequestProperty("logoheight","5")
    url.setRequestProperty("logounits","cm")
    url.setRequestProperty("kind","AUTO")
    url.setRequestProperty("firstnum","1")
    url.setRequestProperty("logostart","")
    url.setRequestProperty("logoend","")
    url.setRequestProperty("smallsamplecorrection","on")
    url.setRequestProperty("symbolsperline","32")
    url.setRequestProperty("res","96")
    url.setRequestProperty("res_units","ppi")
    url.setRequestProperty("antialias","on")
    url.setRequestProperty("title","")
    url.setRequestProperty("barbits","")
    url.setRequestProperty("yaxis","on")
    url.setRequestProperty("yaxis_label","bits")
    url.setRequestProperty("xaxis","on")
    url.setRequestProperty("xaxis_label","")
    url.setRequestProperty("showends","on")
    url.setRequestProperty("shrink","0.5")
    url.setRequestProperty("fineprint","on")
    url.setRequestProperty("ticbits","1")
    url.setRequestProperty("colorscheme","DEFAULT")

    url.setRequestProperty("symbol1","KRH")
    url.setRequestProperty("color1","green")
    url.setRequestProperty("rgb1","")

    url.setRequestProperty("symbol2","DE")
    url.setRequestProperty("color2","blue")
    url.setRequestProperty("rgb2","")

    url.setRequestProperty("symbol3","AVLIPWFM")
    url.setRequestProperty("color3","red")
    url.setRequestProperty("rgb3","")

    url.setRequestProperty("symbol4","")
    url.setRequestProperty("color4","black")
    url.setRequestProperty("rgb4","")

    url.setRequestProperty("symbol5","")
    url.setRequestProperty("color5","purple")
    url.setRequestProperty("rgb5","")

    url.setRequestProperty("symbol6","")
    url.setRequestProperty("color6","orange")
    url.setRequestProperty("rgb6","")

    url.setRequestProperty("symbol7","")
    url.setRequestProperty("color7","black")
    url.setRequestProperty("rgb7","")

    url.setRequestProperty("color0","black")

    url.setRequestProperty("rgb0","")

    url.setRequestProperty("command","Create+Logo: undefined")*/

    conn.setDoOutput( true );
    conn.setInstanceFollowRedirects( false );
    conn.setRequestMethod( "POST" );
    conn.setRequestProperty( "Content-Type", "application/x-www-form-urlencoded");
    conn.setRequestProperty( "charset", "utf-8");
    conn.setRequestProperty( "Content-Length", Integer.toString( postData.length ));
    conn.setUseCaches( false );

    println(prettyPrint)



  }


}
