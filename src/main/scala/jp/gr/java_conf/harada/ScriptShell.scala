package jp.gr.java_conf.harada;
import scala.collection.JavaConverters._
import javax.script._
import java.io._
import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.interpreter.shell.Scripted
import scala.runtime._
import scala.concurrent._
import java.lang.reflect.{Method,Constructor,Modifier}

case class ScriptOK();
case class ScriptNone();
case class ScriptRet(retval:Any);
case class ScriptException(exception:Throwable);
case class ScriptExit(code:Int);

object ScriptShell {
  def main(args:Array[String]) {
    try {
      var scriptfile : File = null;
      var interactive = false;
      var inits : List[String] = Nil;
      var silent = false;
      var encoding : String = null;
      var n = args.size;
      var skip = 0;
      for (i<-0 until args.size if n == args.size) {
        if (skip > 0) skip -= 1 else if (args(i).charAt(0) != '-') n = i else args(i) match {
        case "-i" => interactive = true;
        case "--interactive" => interactive = true;
        case "-p" => skip = 1; inits :+= args(i+1);
        case "--previous" => skip = 1; inits :+= args(i+1);
        case "--silent" => silent = true;
        case "-f" => skip = 1; scriptfile = new File(args(i+1));
        case "--file" => skip = 1; scriptfile = new File(args(i+1));
        case "-e" => skip = 1; encoding = args(i+1);
        case "--encoding" => skip = 1; encoding = args(i+1);
        case _ => println("unknown option : " + args(i));
        }
      }
      if (n >= args.size) {
        println("script-shell (options) [scala/javascript] (name1=param1)..");
        println("[scala/javascript] specifies script language running.");
        println("(options)");
        println(" -f, --file [scriptfile] : run the content of [scriptfile].");
        println(" -i, --interactive : run interactively after the [scriptfile].");
        println(" --silent : not show running line.(= -init \":mode silent\"))");
        println(" -p, --previous [line] : run [line] before running [scriptfile].");
        println("       You can specify multiple options of this type.");
        println("   e.g. -p \":mode exception\" -p \"import java.io._\"");
        println(" -e, --encoding [enc] : read the [scriptfile] with the encoding.");
        System.exit(1);
      }
      val _this = new ScriptShell(args(n), encoding);
      val params = for (i<-(n+1) until args.size) yield {
        val k = args(i).indexOf('=');
        val key = args(i).substring(0, k);
        val value = args(i).substring(k+1);
if (!silent) println("[param]" + key + " = " + value);
        _this.put(key,value);
      }
      for (l<-inits) _this.runline(l);
      if (silent) _this.runMode = "silent";
      if (scriptfile != null) {
        _this.runFile(scriptfile) match {
        case ScriptOK => //
        case ScriptExit(code) => System.exit(code);
        case ScriptRet(ret) => // ignore
        }
      }
      if (interactive) _this.shell() match {
      case ScriptExit(code) => System.exit(code);
      }
      System.exit(0);
    } catch {
      case e: Exception => e.printStackTrace(System.out);
        System.exit(1);
    }
  }
  val stdreader = new BufferedReader(new InputStreamReader(System.in));
  def println(o:Any) {
    o match {
    case a : AnyRef => System.out.println(a);
    case x => System.out.println(x.toString);
    }
  }
  def print(o:Any) {
    o match {
    case a : AnyRef => System.out.print(a);
    case x => System.out.print(x.toString);
    }
  }
}

import ScriptShell._

class InputBuffer {
  var lines : List[String] = Nil;

  def isEmpty = {
    lines.find(_.size > 0) == None
  }
  def show() {
    for (line<-lines) println(line);
  }
  def clear() {
    lines = Nil;
  }
  def addLine(line:String) = {
    lines :+= line;
    true;
  }
}

class BracketInputBuffer extends InputBuffer {
  var bracket = 0;
  override def clear() {
    super.clear();
    bracket = 0;
  }
  override def addLine(line:String) = {
    if (!super.addLine(line)) false else {
      for (c<-line) c match {
      case '{' => bracket += 1;
      case '}' => bracket -= 1;
      case _ => // skip
      }
      (bracket == 0);
    }
  }
}
class PythonInputBuffer extends InputBuffer {
  override def addLine(line:String) = {
    if (!super.addLine(line)) false else {
      if (lines.size == 0) {
        if (line.endsWith(":")) false else true;
      } else if (line.startsWith(" ")) false else if (line.endsWith(":")) false else true;
    }
  }
}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.util._

/*
class LineReader(reader:BufferedReader) {
  val lines = new ConcurrentLinkedQueue[String]();
  def addLine(line:String) {
    lines.add(line);
  }
  def nextLine() = if (lines.isEmpty) None else Some(lines.poll);

  var end = false;
  while (!end) {
    Try {
      if (reader.ready) reader.readLine else throw new IllegalStateException("not ready");
    }
  }
}
*/

class ScriptShell(scriptname:String, encoding:String = null) {
  val (scriptEngine, buffer) = getEngine(scriptname);
  var runMode = "run";
  var lastVal : Any = null;
  var callStack : List[File] = Nil;
  def base : File = if (callStack == Nil) null else callStack.head;

  def getEngine(scriptname:String) = {
    val sem = new ScriptEngineManager();
    var engine = sem.getEngineByName(scriptname);
    val s = scriptname.toLowerCase;
    if (engine == null) {
      val engines = sem.getEngineFactories.asScala;
      engines.find(_.getEngineName.toLowerCase contains s) match {
      case Some(f) => engine = f.getScriptEngine;
      case None => throw new IllegalArgumentException("sciptnames : " + engines.map(_.getEngineName));
      }
    }

    val buffer = if (s.contains("javascript")) {
      engine.put("_this", this);
      engine.eval("function println(target) { print(target) }");
      new BracketInputBuffer
    } else if (s.contains("scala")) {
      val imain = engine match {
      case i : IMain =>
        i.settings.usejavacp.value_$eq(true);
        i;
      case x => null;
      }
      engine.eval("import scala.collection.JavaConverters._;");
      engine.eval("import scala.sys.process._;");
      engine.eval("import scala.language.postfixOps;");
      engine.eval("import jp.gr.java_conf.harada.ScriptShell._;");
      engine.put("_ScriptShell_a", this);
      engine.eval("implicit val _this : jp.gr.java_conf.harada.ScriptShell = _ScriptShell_a.asInstanceOf[jp.gr.java_conf.harada.ScriptShell];");
/*
      engine.eval("""implicit class ScriptShellCommand(private val sc:StringContext) extends AnyVal {
  def cmd(args:String*) : Any = _this.command(sc.standardInterpolator((s:String)=>s, args));
}""");
*/
      new BracketInputBuffer;
    } else if (s.contains("python") || s.contains("jython")) {
      new PythonInputBuffer;
    } else new InputBuffer;
    (engine, buffer);
  }

  def put(key:String, value:AnyRef) {
    scriptEngine.put(key, value);
  }
  def putWithType(key:String, value:AnyRef, typename:String) {
    scriptEngine.put("_ScriptShell_a", value);
    scriptEngine.eval(s"val ${key} = _ScriptShell_a.asInstanceOf[${typename}];");
 }
  def eval(key:String) = scriptEngine.eval(key);
  def get(key:String) = scriptEngine.eval(key);
  def getVariableNames() : Iterable[String] = scriptEngine match {
  case im : IMain =>
    for (term<-im.namedDefinedTerms; v=im.valueOfTerm(term) if v != None) yield term
  case sc: Scripted => 
    val im: IMain = sc.intp.asInstanceOf[IMain]
    for (term<-im.namedDefinedTerms; v=im.valueOfTerm(term) if v != None) yield term
  case x =>
//    scriptEngine.getContext().getBindings(ScriptContext.ENGINE_SCOPE).keySet.asScala;
    throw new UnsupportedOperationException("I can get variable names for only scala : " + x);
  }
  def bufferClear() {
    buffer.clear;
  }
  def runline(line:String) : Any = {
    try {
      if (line == "?") runCommand(List("help")) else 
      if (line.startsWith(":")) command(line.substring(1)) else
      if (!buffer.addLine(line)) ScriptNone else if (buffer.isEmpty) ScriptNone else {
        val ret = scriptEngine.eval(buffer.lines.mkString("\n"));
        buffer.clear;
        ScriptRet(ret);
      }
    } catch {
      case t:Throwable => ScriptException(t);
    }
  }

  private def arg2Class(params:List[String]) = try {
    Class.forName(params.head);
  } catch {
  case ce:ClassNotFoundException =>
    try {
      val obj = get(params.head).asInstanceOf[AnyRef];
      obj.getClass;
    } catch {
    case e : Exception => 
      val obj = eval(params.mkString(" "));
      obj.getClass;
    }
  }
  def parseParams(line:String) : List[String] = {
    def tos(a:Any) : String = a match {
    case null => "";
    case s : String => s;
    case x => x.toString;
    }
    var retlist : List[String] = Nil;
    var mode = ' ';
    var sb = new StringBuilder;
    for (c<-line) mode match {
    case ' ' => c match {
      case ' ' => if (sb.size > 0) {retlist :+= sb.toString; sb = new StringBuilder;}
      case '\"' => if (sb.size > 0) sb += c else mode = c;
      case '\'' => if (sb.size > 0) sb += c else mode = c;
      case '`' => if (sb.size > 0) sb += c else mode = c;
      case _ => sb += c;
      }
    case '\"' => c match {
      case '\"' => mode = ' '; retlist :+= sb.toString;
      case '\\' => mode = '\\';
      case _ => sb += c;
      }
    case '\'' => c match {
      case '\'' => mode = ' '; retlist :+= sb.toString;
      case _ => sb += c;
      }
    case '`' => c match {
      case '`' => 
        retlist :+= tos(command(sb.toString));
        mode = ' ';
      case _ => sb += c;
      }
    case '\\' => c match {
      case '\"' => mode = '\"'; sb += c;
      case '\\' => mode = '\"'; sb += c;
      case x => throw new IllegalArgumentException("unknow escape : \\"  + x);
      }
    }
    if (sb.size > 0) retlist :+= sb.toString;
    retlist;
  }
  def command(line:String) = runCommand(parseParams(line));
  def runCommand(command:List[String]) : Any = {
    command.head match {
    case "quit" => return if (command.size > 1) ScriptExit(command(1).toInt) else ScriptExit(0);
    case "exit" => return if (command.size > 1) ScriptExit(command(1).toInt) else ScriptExit(0);
    case "enginename" => println(scriptEngine.getFactory.getEngineName);
    case "buffer" => buffer.show();
    case "buffer" => buffer.show();
    case "clear" => buffer.clear;
    case "mode" => if (command.size <= 1) println("[mode]" + runMode) else command(1) match {
      case "silent" => runMode = command(1);
      case "run" => runMode = command(1);
      case "step" => runMode = command(1);
      case "exception" => runMode = command(1);
      }
    case "pre" => 
      if (buffer.lines.size > 0) buffer.lines = buffer.lines.take(buffer.lines.size - 1);
      buffer.show();
    case "help" => 
      val check : String = if (command.size == 1) null else command(1);
      for (l<-helpMethods if (check == null) || l._1.contains(check)) println(l._1 + " =>" + l._2);
      for (l<-helpCommands if (check == null) || l._1.contains(check)) println(":" + l._1 + " =>" + l._2);
    case "method" => 
      val cls = arg2Class(command.tail);
      val check : String = if (2 < command.size) command(2) else null;
      var i = 0;
      for (m<-cls.getMethods) {
        i += 1; 
        if ((check == null) || m.getName.contains(check)) println("[" + i + "]" + m);
      }
    case "constructor" => 
      val cls = arg2Class(command.tail);
      var i = 0;
      for (m<-cls.getConstructors) {
        i += 1;
        println("[" + i + "]" + m);
      }
    case "vars" => println(getVariableNames.mkString(", "));
    case "step" => runMode = "step";
    case "continue" => runMode = "exception";
    case "run" =>
      runFile(new File(command(1))) match {
      case ScriptOK => //
      case ScriptExit(code) => System.exit(code);
      case ScriptRet(ret) => return ScriptRet(ret);
      }
    case x => println("unknown command " + x);
    }
    ScriptOK;
  }
  val helpCommands = List(
        "enginename"->"show script engine name.",
        "mode (run/silent/step/exception)"->"set/show the mode running the script.",
        "buffer"->"show the buffer content with which it inputs the next line.",
        "clear"->"clear the buffer content",
        "pre"->"remove the last line of buffer content",
        "help (check)"->"show help for command whose help name (with params) contains (check).",
        "method [classname/variable name] (check)"->"show methods whose name contains (check)",
        "constructor [classname/variable name]"->"show constructors",
        "run [file]"->"run script [file]",
        "vars"->"show the variable names.(only scala)",
        "quit(exit)"->"exit script shell.");
  val helpMethods = List(
        "println(xx)"->"show xx.")
  def runFile(f:File) : Any = {
    callStack ::= f;
    val reader = if (encoding == null) new BufferedReader(new FileReader(f)) else new BufferedReader(new InputStreamReader(new FileInputStream(f), encoding));
    val retval = run(reader);
    callStack = callStack.tail;
    retval;
  }
  def run(reader:BufferedReader) : Any = {
    var predisp = ">";
    var end = false;
    while (!end) {
      if (runMode != "silent") print(predisp);
      reader.readLine match {
      case null => end = true;
      case line =>
        val  skip = if (line != "") false else if (buffer.isInstanceOf[PythonInputBuffer]) (buffer.lines.size == 0) else true;
        if (!skip) {
          var debug = (runMode == "step");
          if (runMode != "silent") println(line);
          while (debug) {
            print("step>");
            val l = stdreader.readLine;
            if (l == "") debug = false else runline(l) match {
              case ScriptExit(code) => return ScriptExit(code);
              case ScriptException(e) => e.printStackTrace(System.out);
              case _ => // ignore
            }
          }
          runline(line) match {
          case ScriptNone => // next
          case ScriptOK => // next
            lastVal = null;
          case ScriptExit(code) => 
            lastVal = ScriptExit(code);
            return lastVal;
          case ScriptRet(retval) => lastVal = retval;
          case ScriptException(e) =>
            lastVal = e;
            bufferClear;
            e.printStackTrace(System.out);
            if (runMode == "exception") runMode = "step";
          }
        }
      }
    }
    ScriptRet(lastVal);
  }
  def shell() : ScriptExit = {
    println("to show help, input ':help'.")
    val reader = stdreader;
    var predisp = ">";
    while (true) {
      print(predisp);
      val line = reader.readLine;
      runline(line) match {
      case ScriptNone => // next
      case ScriptOK => lastVal = null;
      case ScriptExit(code) => 
        lastVal = ScriptExit(code);
        return ScriptExit(code);
      case ScriptRet(retval) => lastVal = retval;
      case ScriptException(e) => 
        lastVal = e;
        bufferClear;
        e.printStackTrace(System.out);
      }
    }
    ScriptExit(0);
  }

  // ex. invokeStatic("java.lang.System", "getProperty", "os.name")
  def invokeStatic(clsname:String, methodname:String, params:AnyRef*) : AnyRef = {
    val cls = getClass().getClassLoader().loadClass(clsname);
    def checkParamTypes(paramtypes:Array[Class[_]], params:Seq[AnyRef]) : Boolean = {
      if (paramtypes.size != params.size) false else for (i<-0 until params.size) {
        if (!paramtypes(i).isInstance(params(i))) return false;
      }
      true;
    }
    for (m<-cls.getMethods if m.getName == methodname) {
      if (checkParamTypes(m.getParameterTypes, params)) {
        return m.invoke(null, params: _*);
      }
    }
    throw new IllegalArgumentException("no '" + methodname + "' method for " + cls  + " with " + params);
  }
}
