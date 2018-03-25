package eu.cactis.cactis_lisp

import java.util.UUID

import eu.cactis.cactis_lisp.ast._

import scala.annotation.tailrec
import scala.collection.{GenIterable, GenSeq, mutable}
case class SymbolDictEntry (calias: Option[Symbol] = None,
                            callP: Option[UUID] = None,
                            callPexpand: Boolean = true,
                            varP: Option[UUID] = None,
                            symbolType: Option[String] = None,
                            desc: Option[String] = None,
                            extra: Map[Symbol, Atom] = Map.empty) {
  def toMap: Map[Symbol, Any] = Map(
    'calias -> calias.getOrElse(()),
    'callP -> callP.map(_.toString).getOrElse(()),
    'varP -> varP.map(_.toString).getOrElse(()),
    'desc -> desc.getOrElse(()),
    'type -> symbolType.getOrElse(())
  ) ++ extra
}

object Interpreter {

  type Memory = Map[UUID, Atom]
  type SymbolDict = Map[Symbol, SymbolDictEntry]
  type NativeDict = Map[Symbol, ConsCell => Atom]
  type ContextArgs = Map[Symbol, Any]


  val defaultFunctions = Map[Symbol, Atom](
    'cadr -> CactisLispCompiler("(builtin-fn (x) (car (cdr x)))").right.get.head
  )

  val defaultSymbolDict = Map[Symbol, SymbolDictEntry](
    'QUOTE -> SymbolDictEntry(calias = Some(Symbol("builtin-quote"))),
    'QUASIQUOTE -> SymbolDictEntry(calias = Some(Symbol("builtin-quasiquote"))),
    'quasiquote -> SymbolDictEntry(calias = Some(Symbol("builtin-quasiquote"))),
    'quote -> SymbolDictEntry(calias = Some(Symbol("QUOTE"))),
    'car -> SymbolDictEntry(calias = Some(Symbol("builtin-car"))),
    'cdr -> SymbolDictEntry(calias = Some(Symbol("builtin-cdr"))),
    'lambda ->  SymbolDictEntry(calias = Some(Symbol("builtin-lambda"))),
    'cons -> SymbolDictEntry(calias = Some(Symbol("builtin-cons"))),
    'set -> SymbolDictEntry(calias = Some(Symbol("builtin-set"))),
    'cond -> SymbolDictEntry(calias = Some(Symbol("builtin-cond"))),
    'eq -> SymbolDictEntry(calias = Some(Symbol("builtin-eq"))),
    'id -> SymbolDictEntry(calias = Some(Symbol("builtin-id"))),
    'and -> SymbolDictEntry(calias = Some(Symbol("builtin-and"))),
    'defmacro -> SymbolDictEntry(calias = Some(Symbol("builtin-defmacro"))),

    'add -> SymbolDictEntry(calias = Some(Symbol("builtin-add"))),
    'subtract -> SymbolDictEntry(calias = Some(Symbol("builtin-subtract"))),
    'multiply -> SymbolDictEntry(calias = Some(Symbol("builtin-multiply"))),
    'divide -> SymbolDictEntry(calias = Some(Symbol("builtin-divide"))),

    '+ -> SymbolDictEntry(calias = Some(Symbol("add"))),
    '- -> SymbolDictEntry(calias = Some(Symbol("subtract"))),
    '* -> SymbolDictEntry(calias = Some(Symbol("multiply"))),
    '/ -> SymbolDictEntry(calias = Some(Symbol("divide"))),

    'or -> SymbolDictEntry(calias = Some(Symbol("builtin-or"))),
    'not -> SymbolDictEntry(calias = Some(Symbol("builtin-not"))),
    '= -> SymbolDictEntry(calias = Some(Symbol("eq"))),
    '=== -> SymbolDictEntry(calias = Some(Symbol("id"))),
    Symbol("repr-equals") -> SymbolDictEntry(calias = Some(Symbol("builtin-repr-equals"))),
    '~= -> SymbolDictEntry(calias = Some(Symbol("repr-equals"))),
    'lt -> SymbolDictEntry(calias = Some(Symbol("builtin-lt"))),
    '< -> SymbolDictEntry(calias = Some(Symbol("lt"))),
    'def -> SymbolDictEntry(calias = Some(Symbol("builtin-def"))),
  )

  private val uuids = defaultFunctions.keys.map(x => (x, UUID.randomUUID()))
  private val memmap = uuids.map{ case (key, uuid) => (uuid, defaultFunctions(key))}.toMap
  private val defaultFunctionSymbols = defaultFunctions.keys.map(x => (x, SymbolDictEntry(callP = uuids.toMap.get(x))))


  def apply(memory: Memory = memmap,
            symbolDict: SymbolDict = defaultSymbolDict ++ defaultFunctionSymbols,
            nativeDict: NativeDict = Map.empty[Symbol, Atom => Atom],
            contextArgs: ContextArgs = Map.empty[Symbol, Any] ++ Map('context -> 'main),
            trace: Boolean = false
           ): Interpreter ={

    new Interpreter(memory, symbolDict, nativeDict, contextArgs, trace)
  }

}

case class Interpreter(memory: Interpreter.Memory,
                       symbolDict: Interpreter.SymbolDict,
                       nativeDict: Interpreter.NativeDict, contextArgs: Interpreter.ContextArgs, TRACE: Boolean) {
  import LispUtils.fromNative

  def exception(message: String): (Atom, Interpreter) = (fromNative("EXCEPTION: " + message), this)

  val builtins: Map[Symbol, ConsCell => (Atom, Interpreter)] = Map(
    'car -> { cl =>
      val args = cl.toList.map{ x =>
        val (atom, contextInterpreter) = eval(x)
        atom
      }


      if(args.tail.isEmpty)
        args.head match {
          case cell: ConsCell => (cell.ar, this)
          case NilAtom => (NilAtom, this)
          case x => exception("Invalid arguments(" + x + ")")
        }
      else exception("Too much arguments for builtin car")
    },
    'cdr -> { cl =>
      val args = cl.toList.map{ x =>
        val (atom, contextInterpreter) = eval(x)
        atom
      }

      if(args.tail.isEmpty)
        args.head match {
          case cell: ConsCell => (cell.dr, this)
          case NilAtom => (NilAtom, this)
          case x => exception("Invalid arguments(" + x + ")")
        }
      else exception("Too much arguments for builtin cdr")
    },

    'quote -> { cell =>
      if(cell.dr == NilAtom)
        (cell.ar, this)
      else exception("Too much arguments for builtin quote")
      },

    'id -> { cl =>
      val args = cl.toList.map{ x =>
        val (atom, contextInterpreter) = eval(x)
        atom
      }

      if(args.size <= 1) (NilAtom, this) else {

        val equals = args.tail.foldLeft(true) { case (b, arg) => b && args.head.eq(arg) }

        (if (equals) SymbolAtom('t) else NilAtom, this)

      }    },

    Symbol("repr-equals") -> { cl =>
      val args = cl.toList.map{ x =>
        val (atom, contextInterpreter) = eval(x)
        atom
      }

      if(args.size <= 1) (NilAtom, this)
      else {
        val equals = args.tail.foldLeft(true) { case (b, arg) => b && args.head.repr == arg.repr }

        (if (equals) SymbolAtom('t) else NilAtom, this)
      }
    },

    'eq -> { cl =>
      val args = cl.toList.map{ x =>
        val (atom, contextInterpreter) = eval(x)
        atom
      }

      if(args.size <= 1) (NilAtom, this)
      else {
        val equals = args.tail.foldLeft(true) { case (b, arg) => b && args.head == arg }

        (if (equals) SymbolAtom('t) else NilAtom, this)
      }
    },

    'and -> { cl =>
      val args = cl.toList.map{ x =>
        val (atom, contextInterpreter) = eval(x)
        atom
      }

      if(args.size <= 1) (NilAtom, this) else {
        val equals = args.foldLeft(true) { case (b, arg) => b && arg != NilAtom }

        (if (equals) SymbolAtom('t) else NilAtom, this)
      }
    },

    'add -> { cl =>
      val args = cl.toList.map { x =>
        val (atom, contextInterpreter) = eval(x)
        atom
      }

      val value = args.tail.foldLeft(args.head) { case (b, arg) => b + arg }

      (value, this)
    },

    'subtract -> { cl =>
      val args = cl.toList.map { x =>
        val (atom, contextInterpreter) = eval(x)
        atom
      }

      val value = args.tail.foldLeft(args.head) { case (b, arg) => b - arg }

      (value, this)
    },

    'multiply -> { cl =>
      val args = cl.toList.map { x =>
        val (atom, contextInterpreter) = eval(x)
        atom
      }

      val value = args.tail.foldLeft(args.head) { case (b, arg) => b * arg }

      (value, this)
    },

    'divide -> { cl =>
      val args = cl.toList.map { x =>
        val (atom, contextInterpreter) = eval(x)
        atom
      }

      val value = args.tail.foldLeft(args.head) { case (b, arg) => b / arg }

      (value, this)
    },

    'or -> { cl =>
      val args = cl.toList.map{ x =>
        val (atom, contextInterpreter) = eval(x)
        atom
      }

      if(args.size <= 1) (NilAtom, this)
      else {
        val equals = args.foldLeft(false) { case (b, arg) => b || arg != NilAtom }

        (if (equals) SymbolAtom('t) else NilAtom, this)
      }
    },

    'not -> { cl =>
      val args = cl.toList.map{ x =>
        val (atom, contextInterpreter) = eval(x)
        atom
      }

      val equals = args.foldLeft(true){ case (b, arg) => b && arg == NilAtom}

      (if(equals) SymbolAtom('t) else NilAtom, this)
    },

    'lt -> { cl =>
      val args = cl.toList.map{ x =>
        val (atom, contextInterpreter) = eval(x)
        atom
      }

      val lt = args.tail.foldLeft(true){ case (b, arg) => b && args.head < arg }

      (if(lt) SymbolAtom('t) else NilAtom, this)
    },

    'cons -> { cl =>
      val args = cl.toList.map{ x =>
        val (atom, contextInterpreter) = eval(x)
        atom
      }
      if(args.length > 2) exception("Too much arguments for builtin cons")
      else if (args.length < 2) exception("Not enough arguments for builtin cons")
      else (ConsCell(args.head, args.tail.head), this)
    },

    'def -> { cl =>
      val args = cl.toList.map{ x =>
        val (atom, contextInterpreter) = eval(x)
        atom
      }
      if(args.length > 2) exception("Too much arguments for builtin set")
      else if (args.length < 2) exception("Not enough arguments for builtin set")
      else {
        val sym = args.head.asInstanceOf[SymbolAtom].value

        val uuid = UUID.randomUUID()
        (SymbolAtom(sym), newInterpreter(memory=memory.updated(uuid, args.tail.head), symbolDict.updated(sym, SymbolDictEntry(
          callP = Some(uuid)
        ))))
      }
    },

    'set -> { cl =>
      val args = cl.toList.map{ x =>
        val (atom, contextInterpreter) = eval(x)
        atom
      }
      if(args.length > 2) exception("Too much arguments for builtin set")
      else if (args.length < 2) exception("Not enough arguments for builtin set")
      else {
        val sym = args.head.asInstanceOf[SymbolAtom].value

        val uuid = UUID.randomUUID()

        (SymbolAtom(sym), newInterpreter(memory=memory.updated(uuid, args.tail.head), symbolDict.updated(sym, SymbolDictEntry(
          varP = Some(uuid)
        ))))
      }
    },

    'lambda -> { cl =>
      val args = cl.toList
      (fromNative(Symbol("builtin-fn") :: args), this)
    },

    'fn -> { cell =>
        val args = cell.toList.head.asInstanceOf[ConsCell].toList
        val body = cell.toList.tail.head.asInstanceOf[ConsCell]

        val zargs = args.zip(contextArgs('args).asInstanceOf[ConsCell].toList)

        val callUUID = UUID.randomUUID()

        val shit = mutable.MutableList.empty[UUID]

        val (mem, dict) = zargs.foldLeft[(Interpreter.Memory, Interpreter.SymbolDict)]((memory, symbolDict)) {
          case ((memory, dict), (SymbolAtom(x), value)) =>
          val (result, _) = eval(value)

          val uuid = UUID.randomUUID()

          assert(!memory.contains(uuid))

          shit += uuid

          (memory.updated(uuid, result), dict.updated(x, SymbolDictEntry(
            extra = Map(Symbol("callUUID:"+callUUID) -> SymbolAtom('t)),
            varP = Some(uuid)
          )))
        }

        val (rr, ii) = newInterpreter(symbolDict=dict, memory=mem, contextArgs=Map('context -> 'lambdacalled)).eval(body)
        (rr, ii.newInterpreter(memory=ii.memory -- shit.toList,
          symbolDict=ii.symbolDict -- ii.symbolDict.keySet.filter(sym =>
            ii.symbolDict(sym).extra.contains(Symbol("callUUID:"+callUUID)))))
    },
    'cond -> { cl =>
      val c = cl.toList
      if(!c.forall(x => x.isInstanceOf[ConsCell])) {
        exception("arguments must be given as pairs")
      } else {
        var result: Atom = null
        val elemx = c.map(elem => elem.asInstanceOf[ConsCell])
        for (pair <- elemx) {
          if(result == null) {
            val (condition, _) = eval(pair.ar)
            if(condition != NilAtom)
              result = pair.dr
          }
        }

        if(result == null)
          (NilAtom, this)
        else {
          val ret = result match {
            case cell: ConsCell if cell.dr == NilAtom =>
              eval(cell.ar)
            case _ => eval(result)
          }

          ret
        }
      }
    },
    'defmacro -> { cells =>
      exception("Not implemented!")
    },
    'quasiquote -> { cells =>
      exception("Not implemented")
    }
  )

  private def newInterpreter(memory: Map[UUID, Atom] = this.memory,
                             symbolDict: Map[Symbol, SymbolDictEntry] = this.symbolDict,
                             nativeDict: Map[Symbol, ConsCell => Atom] = this.nativeDict,
                             contextArgs: Map[Symbol, Any] = this.contextArgs) =
    Interpreter(memory=memory, symbolDict=symbolDict, nativeDict=nativeDict, contextArgs=contextArgs, trace=TRACE)

  private def symInfo(symbol: Symbol) =
    fromNative(List(symbol, Map('name -> symbol.name) ++ symbolDict.get(symbol).map(_.toMap).getOrElse(Map.empty)))


  @tailrec
  private def callSymbol(symbol: Symbol, cell: ConsCell): (Atom, Interpreter) =
    if(symbol.name.startsWith("builtin-")) {
      val newSym = Symbol(symbol.name.substring("builtin-".length))
      if(builtins.contains(newSym))
        builtins(newSym)(cell)
      else exception("Unknown builtin: " + newSym.name)
    } else if(symbolDict.contains(symbol)) {
      if(symbolDict(symbol).callP.nonEmpty) {
        val result = cell.toList

        val fun = memory(symbolDict(symbol).callP.get)

        eval(newInterpreter(contextArgs=Map('context -> 'sub, 'args -> ConsCell.mkCons(result))).eval(fun)._1)
      } else if(symbolDict(symbol).calias.isDefined) {
        callSymbol(symbolDict(symbol).calias.get, cell)
      } else exception("Symbol not callable " + symbol)
    } else exception("Symbol not callable " + symbol)

  def lookupSymbol(symbol: Symbol): (Atom) = if(symbol.name.startsWith("builtin-"))
    fromNative(List(symbol, Map('name -> symbol.name, 'type -> 'builtin)))
  else
    if(symbolDict.contains(symbol))
      if(symbolDict(symbol).varP.isDefined)
        if(memory.isDefinedAt(symbolDict(symbol).varP.get))
          memory(symbolDict(symbol).varP.get)
        else exception("Unknown memory location")._1
      else symInfo(symbol)
    else symInfo(symbol)

  def eval(atom: Atom): (Atom, Interpreter) = {
    atom match {
      case x @ ConsCell(SymbolAtom(m), rest: ConsCell) =>
        if(TRACE)
          println("CALL SYMBOL: " + x.repr)
        callSymbol(m, rest)
      case SymbolAtom(sym) =>
        (lookupSymbol(sym), this)
      case x: ConsCell =>
        exception("Unquoted cons cell: " + x.repr)
      case _ =>
        (atom, this)
    }
  }
}
