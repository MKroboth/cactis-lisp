package eu.cactis.cactis_lisp

import com.googlecode.lanterna.screen.TerminalScreen
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import eu.cactis.cactis_lisp.ast._

object Repl extends App {

  def color_repr(around: String = io.AnsiColor.RESET)(atom: Atom): String = atom match {
    case x: Keyword => io.AnsiColor.GREEN ++ x.repr ++ around
    case x: StringAtom => io.AnsiColor.RED ++ x.repr ++ around
    case x: DecimalAtom => io.AnsiColor.BLUE ++ x.repr ++ around
    case x: NumberAtom => io.AnsiColor.BLUE ++ x.repr ++ around
    case x: ConsCell => x.repr(color_repr(around))
    case x => x.repr
  }

  def repl(terminal: String, interpreter: Interpreter, in: () => String, out: String => Unit): Unit = {
    val r = in()

    CactisLispCompiler(r) match {
      case Left(x) => println(x)
        repl(terminal, interpreter, in, out)

      case Right(x) =>

        val (result, newInterpreter) =
          x.foldLeft[(Atom, Interpreter)]((NilAtom, interpreter)){
            case ((_, interp), atom) =>
              interp.eval(atom)
          }

        out(if(terminal!="unknown") color_repr()(result).trim else result.repr)
        repl(terminal, newInterpreter, in, out)
    }
  }

      val term = if(args.contains("--")) Some("unknown") else sys.env.get("TERM")

     term match {
    case None =>
    /*  val terminalFactory = new DefaultTerminalFactory()
      val terminal = terminalFactory.createTerminal()
      val screen = new TerminalScreen(terminal)
      screen.clear()

//      terminal.
      repl("")*/
    case Some(terminal) => repl(terminal, Interpreter(), () => io.StdIn.readLine("(lisp) > "), println)
  }
}
