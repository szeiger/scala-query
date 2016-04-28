package slick.util

import java.io.{OutputStreamWriter, StringWriter, PrintWriter}

/** Create a readable printout of a tree. */
case class TreePrinter(name: String = "", prefix: String = "", firstPrefix: String = null,
                       narrow: (Dumpable => Dumpable) = identity, mark: (Dumpable => Boolean) = (_ => false)) {
  import TreePrinter._

  def get(n: Dumpable) = {
    val buf = new StringWriter
    print(n, new PrintWriter(buf))
    buf.getBuffer.toString
  }

  def print(n: Dumpable, out: PrintWriter = new PrintWriter(new OutputStreamWriter(System.out))) {
    def dump(baseValue: Dumpable, prefix1: String, prefix2: String, name: String, level: Int) {
      val value = narrow(baseValue)
      val di =
        if(value eq null) DumpInfo("<error: narrowed to null>", "baseValue = "+baseValue)
        else value.getDumpInfo
      val multiLine = di.mainInfo contains '\n'
      val marked = mark(value)
      val markedDiName = if(marked) "< " + di.name + " >" else di.name
      out.print(
        prefix1 +
          cCyan + (if(name.nonEmpty) name + ": " else "") +
          (if(marked) cNormal+bYellow+cBlack else cYellow) + (if(multiLine) multi1 else "") + markedDiName +
          cNormal + (if(di.name.nonEmpty && di.mainInfo.nonEmpty) " " else "")
      )
      if(multiLine) {
        val lines = di.mainInfo.replace("\r", "").split('\n')
        out.println(if(di.attrInfo.isEmpty) "" else cBlue + di.attrInfo + cNormal)
        val p = prefix2 + Iterator.fill(name.length + (if(name.length == 0) 0 else 2))(' ').mkString + cYellow + multi2 + cNormal
        lines.foreach { l => out.println(p + l) }
      } else {
        out.println(di.mainInfo + (if(di.attrInfo.isEmpty) "" else " " + cBlue + di.attrInfo + cNormal))
      }
      val children = di.children.toIndexedSeq
      children.zipWithIndex.foreach { case ((name, value), idx) =>
        val (p1, p2) = if(idx == children.size-1) (lastChildPrefix1, lastChildPrefix2) else (childPrefix1, childPrefix2)
        val (cp1, cp2) = if(level % 2 == 0) (cBlue + p1, cBlue + p2) else (cGreen + p1, cGreen + p2)
        dump(value, prefix2 + cp1, prefix2 + cp2, name, level + 1)
      }
    }
    dump(n, if(firstPrefix ne null) firstPrefix else prefix, prefix, name, 0)
    out.flush()
  }

  def findMarkedTop(n: Dumpable): Dumpable = {
    def find(n: Dumpable): Option[Dumpable] = {
      val value = narrow(n)
      if(mark(value)) Some(n) else {
        val children = value.getDumpInfo.children.map(_._2).toVector
        val markedChildren = children.map(find).collect { case Some(d) => d }
        if(markedChildren.length > 1) Some(n)
        else if(markedChildren.length == 1) Some(markedChildren.head)
        else None
      }
    }
    find(n).getOrElse(n)
  }
}

object TreePrinter {
  def default = new TreePrinter

  private[TreePrinter] val (childPrefix1, childPrefix2, lastChildPrefix1, lastChildPrefix2, multi1, multi2) =
    ("\u2523 ", "\u2503 ", "\u2517 ", "  ", "\u250f ", "\u2507 ")

  val (cNormal, cBlack, cRed, cGreen, cYellow, cBlue, cMagenta, cCyan) =
    ("\u001B[0m", "\u001B[30m", "\u001B[31m", "\u001B[32m", "\u001B[33m", "\u001B[34m", "\u001B[35m", "\u001B[36m")
  val (bRed, bGreen, bYellow, bBlue, bMagenta, bCyan) =
    ("\u001B[41m", "\u001B[42m", "\u001B[43m", "\u001B[44m", "\u001B[45m", "\u001B[46m")

  private[this] val multi = "\u2507 "
  private[this] val multilineBorderPrefix = cYellow + multi + cNormal

  def multilineBorder(s: String): String =
    multilineBorderPrefix + s.replace("\n", "\n" + multilineBorderPrefix)
}

/** Interface for types that can be used in a tree dump */
trait Dumpable {
  /** Return the name, main info, attribute info and named children */
  def getDumpInfo: DumpInfo
}

/** The information required for dumping a single object */
case class DumpInfo(name: String, mainInfo: String = "", attrInfo: String = "", children: Iterable[(String, Dumpable)] = Vector.empty) {
  def getNamePlusMainInfo = if(name.nonEmpty && mainInfo.nonEmpty) name + " " + mainInfo else name + mainInfo
}

object DumpInfo {
  def simpleNameFor(cl: Class[_]): String = cl.getName.replaceFirst(".*\\.", "")

  def highlight(s: String) = TreePrinter.cGreen + s + TreePrinter.cNormal
}

/** Create a wrapper for a `Dumpable` to omit some nodes. */
object Ellipsis {
  def apply(n: Dumpable, poss: List[Int]*): Dumpable = new Dumpable {
    def getDumpInfo = {
      val parent = n.getDumpInfo
      if(poss.isEmpty) parent
      else if(poss contains Nil) DumpInfo("...")
      else parent.copy(children = parent.children.zipWithIndex.map { case ((name, ch), idx) =>
        val chposs = poss.filter(_.head == idx).map(_.tail)
        (name, apply(ch, chposs: _*))
      })
    }
  }
}
