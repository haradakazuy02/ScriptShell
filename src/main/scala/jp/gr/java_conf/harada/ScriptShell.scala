package jp.gr.java_conf.harada;
import scala.collection.convert.WrapAsScala._
import javax.script._
import java.io._
import scala.tools.nsc.interpreter.IMain;

case class ScriptOK();
case class ScriptNone();
case class ScriptRet(retval:Any);
case class ScriptException(exception:Throwable);
case class ScriptExit(code:Int);

object ScriptShell {
  def main(args:Array[String]) {
    try {
      var scriptfile : File = null;
      var interpreter = false;
      var inits : List[String] = Nil;
      var n = args.size;
      var skip = 0;
      for (i<-0 until args.size if n == args.size) {
        if (skip > 0) skip -= 1 else if (args(i).charAt(0) != '-') n = i else args(i) match {
          case "-i" => interpreter = true;
          case "-init" => skip = 1; inits :+= args(i+1);
          case "-script" => skip = 1; scriptfile = new File(args(i+1));
          case _ => println("unknown option : " + args(i));
        }
      }
      if (n >= args.size) {
        println("ScriptShell (options) [scala/javascript] (name1=param1)..");
        println("[scala/javascript] specifies script language running.");
        println("(options)");
        println(" -script [scriptfile] : run the content of [scriptfile].");
        println(" -i : run on interpreter mode after running the [scriptfile].");
        println(" -init [line] : run the script [line] before running [scriptfile].");
        println("       You can specify multiple options of this type.");
        println("   e.g. -init \":mode exception\" -init \"import java.io._\"");
        System.exit(1);
      }
      val _this = new ScriptShell(args(n));

      val params = for (i<-(n+1) until args.size) yield {
        val k = args(i).indexOf('=');
        val key = args(i).substring(0, k);
        val value = args(i).substring(k+1);
println("[param]" + key + " = " + value);
        _this.put(key,value);
      }
      for (l<-inits) _this.runline(l);

      if (scriptfile != null) _this.run(new BufferedReader(new FileReader(scriptfile))) match {
        case ScriptOK => //
        case ScriptExit(code) => System.exit(code);
        case ScriptRet(ret) => // ignore
      }
      if (interpreter) _this.shell() match {
        case ScriptExit(code) => System.exit(code);
      }
      System.exit(0);
    } catch {
      case e: Exception => e.printStackTrace(System.out);
        System.exit(1);
    }
  }
  val stdreader = new BufferedReader(new InputStreamReader(System.in));
}

import ScriptShell._

class InputBuffer {
  var lines : List[String] = Nil;

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
class ScriptShell(scriptname:String) {
  val (scriptEngine, buffer) = getEngine(scriptname);
  var runMode = "run";

  def getEngine(scriptname:String) = {
    val sem = new ScriptEngineManager();
    val engine = sem.getEngineByName(scriptname);
    if (engine == null) throw new IllegalArgumentException("sciptnames : " + sem.getEngineFactories.map(_.getEngineName));

    val s = scriptname.toLowerCase;
    val buffer = if (s.contains("java")) {
      engine.put("_this", this);
      new BracketInputBuffer
    } else if (s.contains("scala")) {
      val imain = engine.asInstanceOf[IMain];
      imain.settings.usejavacp.value_$eq(true);
      engine.eval("import scala.collection.convert.WrapAsScala._");
      engine.eval("import scala.collection.convert.WrapAsJava._");
      engine.eval("import scala.sys.process._");
      engine.eval("import jp.gr.java_conf.harada.ScriptShell._");
      imain.bind("_this", "jp.gr.java_conf.harada.ScriptShell", this);
      engine.eval("""implicit class ScriptShellCommand(private val sc:StringContext) extends AnyVal {
  def cmd(args:String*) : Any = _this.command(sc.standardInterpolator((s:String)=>s, args));
}""");
      new BracketInputBuffer
    }  else new InputBuffer;
    (engine, buffer);
  }
  var iMain : IMain = if (scriptEngine.isInstanceOf[IMain]) scriptEngine.asInstanceOf[IMain] else null;

  def put(key:String, value:AnyRef) {
    scriptEngine.put(key, value);
  }
  def putWithType(key:String, value:AnyRef, typename:String) {
    if (iMain != null) iMain.bind(key, typename, value) else scriptEngine.put(key, value);
  }
  def eval(key:String) = scriptEngine.eval(key);
  def get(key:String) = scriptEngine.eval(key);
  def getVariableNames() : Iterable[String] = scriptEngine match {
    case im : IMain => im.namedDefinedTerms.map(_.toString);
    case _ => 
//      scriptEngine.getContext().getBindings(ScriptContext.ENGINE_SCOPE).keySet;
      throw new UnsupportedOperationException("I can't get variable names for Nashorn.");
  }
  def bufferClear() {
    buffer.clear;
  }
  def runline(line:String) : Any = {
    try {
      if (line == "?") runCommand(List("help")) else 
      if (line.startsWith(":")) command(line.substring(1)) else
      if (!buffer.addLine(line)) ScriptNone else {
        val ret = scriptEngine.eval(buffer.lines.mkString("\n"));
        buffer.clear;
        ScriptRet(ret);
      }
    } catch {
      case t:Throwable => ScriptException(t);
    }
  }

  private def arg2Class(name:String) = try {
    Class.forName(name);
  } catch {
    case ce:ClassNotFoundException =>
      val obj = get(name).asInstanceOf[AnyRef];
      obj.getClass;
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
      case "scriptname" => println(scriptEngine.getFactory.getEngineName);
      case "buffer" => buffer.show();
      case "buffer" => buffer.show();
      case "clear" => buffer.clear;
      case "mode" => if (command.size <= 1) println("[mode]" + runMode) else command(1) match {
        case "run" => runMode = command(1);
        case "step" => runMode = command(1);
        case "exception" => runMode = command(1);
        }
      case "pre" => 
        if (buffer.lines.size > 0) buffer.lines = buffer.lines.take(buffer.lines.size - 1);
        buffer.show();
      case "help" => 
        val check : String = if (command.size == 1) null else command(1);
        for (l<-helpLines if (check == null) || l._1.contains(check)) println(":" + l._1 + " =>" + l._2);
      case "method" => 
        val cls = arg2Class(command(1));
        val check : String = if (2 < command.size) command(2) else null;
        var i = 0;
        for (m<-cls.getMethods) {
          i += 1;
          if ((check == null) || m.getName.contains(check)) println("[" + i + "]" + m);
        }
      case "constructor" => 
        val cls = arg2Class(command(1));
        var i = 0;
        for (m<-cls.getConstructors) {
          i += 1;
          println("[" + i + "]" + m);
        }
      case "vars" => println(getVariableNames.mkString(", "));
      case "step" => runMode = "step";
      case "continue" => runMode = "exception";
      case x => println("unknown command " + x);
    }
    ScriptOK;
  }
  val helpLines = List(
        "scriptname"->"show script engine name.",
        "mode [run/step/exception]"->"set run mode in running script file.",
        "buffer"->"show the buffer content with which it inputs the next line.",
        "clear"->"clear the buffer content",
        "pre"->"remove the last line of buffer content",
        "help (check)"->"show help for command whose help name (with params) contains (check).",
        "method [classname/variable name] (check)"->"show methods whose name contains (check)",
        "constructor [classname/variable name]"->"show constructors",
        "vars"->"show the variable names.(only scala)",
        "quit"->"exit script shell.");


  def run(reader:BufferedReader) : Any = {
    var predisp = ">";
    var preval : Any = null;
    var end = false;
    while (!end) {
      print(predisp);
      reader.readLine match {
        case null => end = true;
        case "" => // skip
        case line =>
          var debug = (runMode == "step");
          println(line);
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
            case ScriptExit(code) => return ScriptExit(code)
            case ScriptRet(retval) => preval = retval;
            case ScriptException(e) =>
              bufferClear;
              e.printStackTrace(System.out);
              if (runMode == "exception") runMode = "step";
          }
      }
    }
    ScriptRet(preval);
  }
  def shell() : ScriptExit = {
    val reader = stdreader;
    var predisp = ">";
    var preval : Any = null;
    while (true) {
      print(predisp);
      val line = reader.readLine;
      runline(line) match {
        case ScriptNone => // next
        case ScriptOK => // next
        case ScriptExit(code) => return ScriptExit(code)
        case ScriptRet(retval) => preval = retval;
        case ScriptException(e) => 
          bufferClear;
          e.printStackTrace(System.out);
      }
    }
    ScriptExit(0);
  }
}