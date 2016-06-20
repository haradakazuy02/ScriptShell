package jp.gr.java_conf.harada;
import scala.collection.convert.WrapAsScala._
import javax.script._
import java.io._

case class ScriptOK();
case class ScriptNone();
case class ScriptRet(retval:Any);
case class ScriptException(exception:Throwable);
case class ScriptExit(code:Int);

object ScriptShell {
  var _this : ScriptShell = null;
  def main(args:Array[String]) {
    try {
      var scriptfile : File = null;
      var noinput = false;
      var step = false;
      var n = args.size;
      var skip = 0;
      for (i<-0 until args.size if n == args.size) {
        if (skip > 0) skip -= 1 else if (args(i).charAt(0) != '-') n = i else args(i) match {
          case "-noinput" => noinput = true;
          case "-step" => step = true;
          case "-script" =>
            skip = 1;
            scriptfile = new File(args(i+1));
          case _ => println("unknown option : " + args(i));
        }
      }
      if (n >= args.size) {
        println("ScriptShell (options) [scriptname] (name1=param1)..");
        println("[scriptname]でScriptEngineを取得しスクリプト実行する");
        println("javascript/scalaを指定");
        println("(options)");
        println(" -script [scriptfile] : 入力前に実行するスクリプトファイルを指定");
        println(" -noinput : 入力しないで終了.-scriptで実行してすぐ終了する場合に使用");
        println(" -step : stepモードで-scriptのファイルを実行する");
        System.exit(1);
      }
      _this = new ScriptShell(args(n));

      val params = for (i<-(n+1) until args.size) yield {
        val k = args(i).indexOf('=');
        val key = args(i).substring(0, k);
        val value = args(i).substring(k+1);
println("[param]" + key + " = " + value);
        _this.put(key,value);
      }
      if (step) _this.runMode = "step";

      if (scriptfile != null) _this.run(new BufferedReader(new FileReader(scriptfile))) match {
        case ScriptOK => //
        case ScriptExit(code) => System.exit(code);
        case ScriptRet(ret) => // ignore
      }
      if (!noinput) _this.shell() match {
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
import scala.tools.nsc.interpreter.IMain;

  def getEngine(scriptname:String) = {
    val sem = new ScriptEngineManager();
    val engine = sem.getEngineByName(scriptname);
    if (engine == null) throw new IllegalArgumentException("sciptnames : " + sem.getEngineFactories.map(_.getEngineName));

    val s = scriptname.toLowerCase;
    val buffer = if (s.contains("java")) {
      engine.put("_this", this);
      new BracketInputBuffer
    } else if (s.contains("scala")) {
      engine.asInstanceOf[IMain].settings.usejavacp.value_$eq(true);
      engine.eval("import scala.collection.convert.WrapAsScala._");
      engine.eval("import scala.collection.convert.WrapAsJava._");
      engine.eval("import scala.sys.process._");
      engine.eval("import jp.gr.java_conf.harada.ScriptShell._");
      new BracketInputBuffer
    }  else new InputBuffer;
    (engine, buffer);
  }
  def put(key:String, value:AnyRef) {
    scriptEngine.put(key, value);
  }
  def get(key:String) = {
    scriptEngine.eval(key);
  }
  def bufferClear() {
    buffer.clear;
  }
  def runline(line:String) : Any = {
    if (line == "?") runCommand(List(":help")) else 
    if (line.startsWith(":")) runCommand(line.split("\\s+").toList) else
    if (!buffer.addLine(line)) ScriptNone else {
      try {
        val ret = runscript(line);
        buffer.clear;
        ScriptRet(ret);
      } catch {
        case t:Throwable => ScriptException(t);
      }
    }
  }
  def runscript(line:String) = scriptEngine.eval(buffer.lines.mkString("\n"));

  private def arg2Class(name:String) = try {
    Class.forName(name);
  } catch {
    case ce:ClassNotFoundException =>
      val obj = scriptEngine.get(name).asInstanceOf[AnyRef];
      obj.getClass;
  }

  def runCommand(command:List[String]) : Any = {
    command.head match {
      case ":scriptname" => println(scriptEngine.getFactory.getEngineName);
      case ":buffer" => buffer.show();
      case ":clear" => buffer.clear;
      case ":pre" => 
        if (buffer.lines.size > 0) buffer.lines = buffer.lines.take(buffer.lines.size - 1);
        buffer.show();
      case ":quit" => 
        return if (command.size > 1) ScriptExit(command(1).toInt) else ScriptExit(0);
      case ":help" => 
        val check : String = if (command.size == 1) null else command(1);
        for (l<-helpLines if (check == null) || l._1.matches(check)) println(":" + l._1 + " =>" + l._2);
      case ":method" => 
        val cls = arg2Class(command(1));
        val check : String = if (2 < command.size) command(2) else null;
        var i = 0;
        for (m<-cls.getDeclaredMethods) {
          i += 1;
          if ((check == null) || m.getName.contains(check)) println("[" + i + "]" + m);
        }
      case ":constructor" => 
        val cls = arg2Class(command(1));
        var i = 0;
        for (m<-cls.getDeclaredConstructors) {
          i += 1;
          println("[" + i + "]" + m);
        }
      case ":step" => runMode = "step";
      case ":continue" => runMode = "run";
      case x => println("unknown command " + x);
    }
    ScriptOK;
  }
  val helpLines = List(
        "scriptname"->"show script engine name.",
        "buffer"->"show the buffer content with which it inputs with the next line.",
        "clear"->"clear the buffer content",
        "pre"->"remove the last line of buffer content",
        "help (reg)"->"show help for command whose name mathes (reg).",
        "method [classname/variable name] (check)"->"show methods whose name contains (check)",
        "constructor [classname/variable name]"->"show constructors",
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