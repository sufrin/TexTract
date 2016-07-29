/**
        $Id: textract.scala 75 2016-07-29 15:39:48Z sufrin $
*/
import scala.collection.mutable._
import java.io._
import java.util.regex._

case class TextException(s: String) extends Exception(s) {}

/** Workhorse for TexTract */
class Textractor
{ var encoding = "UTF-8"
  var compress = false
  var answers  = true
  var root     = new File("TEXTRACT")
  var force    = false
  var legacy   = false
  var number   = false
  var codePat  = "code"
  var ansPat   = "ans"
  
  def transDoc(aLine: String) =
  { var line = aLine
    line=line.replace("<",                      "&lt;")
    line=line.replace(">",                      "&gt;")
    line=line.replace("\\par",                  "<p>")
    line=line.replace("\\item",                 "<li>")
    line=line.replace("\\begin{itemize}",       "<ul>")
    line=line.replace("\\end{itemize}",         "</ul>")
    line=line.replace("\\begin{enumerate}",     "<ol>")
    line=line.replace("\\end{enumerate}",       "</ol>")
    line=line.replace("\\begin{verbatim}",      "<pre>")
    line=line.replace("\\end{verbatim}",        "</pre>")
    line=line.replaceAll("\\\\textit\\{(([^}]*|\\\\[{}]))*\\}", "<i>$1</i>")
    line=line.replaceAll("\\\\textbf\\{(([^}]*|\\\\[{}]))*\\}", "<b>$1</b>")
    line=line.replaceAll("\\\\texttt\\{(([^}]*|\\\\[{}]))*\\}", "<tt>$1</tt>")
    line=line.replaceAll("^[ \t]*$",            "<p>")
    line
  }
    
  object SinkWriter extends Writer
  { override def close() = {}
    override def flush() = {}
    override def write(cbuf: Array[Char], off: Int, len: Int) = {}
  }
  
  object PrintSink extends PrintWriter(SinkWriter) 
  
  class  ChunkWriter(s: StringWriter) extends PrintWriter (s, true)
  { 
    override def  toString = { s.flush; s.toString }
  }
    
  def chunkWriter = new ChunkWriter(new StringWriter)
  
  def isChunkName(name: String) = 
  { val n = name.trim
    (n startsWith "...") || (n matches "``([^']+)''")
  }
  
  def mkChunkName(name: String) = 
      name.trim.replaceAll("([ \\t]+|^\\.\\.\\.)", "").toLowerCase.replaceFirst("``([^']+)''", "$1")
  
  def commentChunkName(name: String) = 
      if (name.matches("[ \\t]*[.][.][.].*"))
         name.replaceFirst("([ \\t]*).*", "$1")+makeComment(name.replaceFirst("[.][.][.]", ""))
      else
      if (name.matches("[ \\t]*``([^']+)''"))
         name.replaceFirst("([ \\t]*).*", "$1")+name.replaceFirst("``([^']+)''", makeComment("$1"))
      else
         name
  
  var fileName = ""  // name of the current file bound to out
  
  var openComment, closeComment = ""
  val bad = ("", "")
  
  val comment  = new HashMap[String,(String,String)]
  comment .update(".tex",   ("% ", ""))
  comment .update(".scala", ("// ", ""))
  comment .update(".java",  ("// ", ""))
  comment .update(".c",     ("/* ", " */"))
  comment .update(".cpp",   ("/* ", " */"))
  comment .update(".hs",    ("-- ", ""))
  
  var linePragma = "" // current line pragma comment string
  
  val pragma  = new HashMap[String, String]
  pragma .update(".hs",    "{-# LINE %n \"%f\" #-}")
  pragma .update(".c",     "#line %n \"%f\"")
  pragma .update(".cpp",   "#line %n \"%f\"")
  
  def formatLinePragma(line: Int, file: String): String =
  { linePragma.replaceAllLiterally("%n", line.toString).replaceAllLiterally("%f", file)
  }
  
  def newComments(spec: String) : Unit =
  { def dotify(s: String) = if (s.startsWith(".")) s else "."+s
    try
    { val s = spec.substring(1).split(spec(0))
      var i = 0
      while (i+3<=s.length)
      { comment.update(dotify(s(i)), (s(i+1), s(i+2))) 
        i += 3
      }
    }
    catch
    { case _ : Throwable => System.err.println("Unsound comment specification: "+spec+" use /.ext/left/right")
    }
  }
  
  def newPragmas(spec: String) : Unit =
  { def dotify(s: String) = if (s.startsWith(".")) s else "."+s
    try
    { val s = spec.substring(1).split(spec(0))
      var i = 0
      while (i+2<=s.length)
      { pragma.update(dotify(s(i)), s(i+1))
        i += 2
      }
    }
    catch
    { case _ : Throwable  => System.err.println("Unsound pragma specification: "+spec+" use /.ext/format")
    }
  }
 
  def setCommentStrings: Unit =
  { val dot      = fileName.lastIndexOf('.')
    val (o, c)   = if (dot<0) bad else comment.getOrElse(fileName.substring(dot), bad)
    openComment  = o
    closeComment = c
    linePragma   = if (dot<0) "" else pragma.getOrElse(fileName.substring(dot), "")
  }
  
  def makeComment(line: String) : String = 
  { if (openComment=="") "" else openComment+line+closeComment
  }
  
  /** Source number format for named chunks */
  
  val sourceNumberFormat = "%%--%07d--%%"                   // a fixed-size format
  val sourceLineLength   = sourceNumberFormat.format(0).size
  
  def isSourceNumber(line: String) : Boolean = (line.size==sourceLineLength && line.startsWith("%--") && line.endsWith("--%"))
  
  def getSourceNumber(line: String) : Int = line.substring(3, 10).toInt  // PRE: isSourceNumber(line)

  /** First pass: digest the chunks, adding source line number markers to each for use at expansion time */
  def readChunks(file: File, source: Array[String]) =
  {
    val pat = 
        if (legacy)
           Pattern.compile("^\\s*\\\\(begin|end)\\s*\\{(class|obj|hideclass|hideobj)\\**\\}\\s*(\\[(.*)\\])?\\s*(\\{([^}]*)\\})?.*$")
        else
           Pattern.compile("^\\s*\\\\(begin|end)\\s*\\{[+-=|*]*("+codePat+")[+-=|*]*\\}\\s*(\\[(.*)\\])?\\s*(\\{([^}]*)\\})*.*$")
    
    val fileField = 
         if (legacy) 6 else 4
         
    var chunks = new HashMap[String, ChunkWriter]
    
    var send = false
    
    var out : ChunkWriter = null
    
    var lastFileName : String = ""
        
    for (i<-0 until source.length)
    { val m = pat.matcher(source(i))
      if (m.matches)
      { (m.group(1), m.group(2)) match
        { case ("end",   kind)  => { send = false; out = null }
          
          case ("begin", kind) =>
          { if (m.groupCount >= fileField) 
            {  var fileName = m group fileField
               if (fileName==null || fileName=="") fileName=lastFileName
               if (fileName!=null && fileName!="" && isChunkName(fileName))
               { send = true
                 lastFileName = fileName
                 fileName = mkChunkName(fileName)
                 chunks get fileName match 
                 { case Some(p) => out = p
                   case None =>
                   fileName match
                   {  case "" =>
                        out = chunkWriter
                        
                      case _  =>
                      { out  = chunkWriter
                        System.out.println("*   "+fileName)
                        chunks update(mkChunkName(fileName), out)
                      }
                   }
                 }
               }
            }
          } 
          
          case (_, _) => throw new Error("Catastrophe: pattern match implementation")         
        }
      }
      else
      if (send) 
      { out.println(sourceNumberFormat.format(i))
        out.println(source(i))
      }
    }
    chunks
  }
  
  /** Second pass: translate the file and expand the chunk names */
  def translate(file: File, source: Array[String])
  { val sourceModified = file.lastModified
  
    val chunks = readChunks(file, source)
    
    if (!chunks.isEmpty && !compress)
    { System.out.println("Warning: "+file+" defines snippets; output will be compressed and numbered")
      compress = true
      number = true
    }
    
    val pat = 
        if (legacy)
           Pattern.compile("^\\s*\\\\(begin|end)\\s*\\{(class|obj|hideclass|hideobj|doc|ans)\\**\\}\\s*(\\[(.*)\\])?\\s*(\\{([^}]*)\\})?.*$")
        else
           Pattern.compile("^\\s*\\\\(begin|end)\\s*\\{[+-=|*]*("+codePat+"|"+ansPat+")[+-=|*]*\\}\\s*(\\[(.*)\\])?\\s*(\\{([^}]*)\\})*.*$")
    
    val fileField = 
         if (legacy) 6 else 4
    
    var streams = new HashMap[String,PrintWriter]
    
    var out : PrintWriter = null    
    
    def outln(line: String)  = { out.println(line) }
    def output(line: String) = { out.print(line) }
    
    /*
       lastLineMarked is the source number of the last line whose location was marked in the output with
       a pragma for the target language. There is no point in marking successive lines in the output.
    */ 
    var lastLineMarked = -1
    
    def makeLineMark(i: Int): Unit =  
    {  if (i-lastLineMarked == 1) {}
       else 
          { outln(if (linePragma=="") makeComment((1+i).toString) else formatLinePragma(1+i, file.toString)) }
       lastLineMarked = i
    }
    
    def skipLN(out: PrintWriter, line: String)  : Unit   = 
    { if (!compress) out.println }
    
    def skipln(line: String) : Unit  = 
    { skipLN(out, line) }
    
    def skipAll(line: String) = 
    { for (s<-streams.values) if (s!=out) skipLN(s, line) }    
    
    def closeAll = 
    { for (s<-streams.values) s.close }
    
    def makeWriter(fileName: String) : PrintWriter = 
    { val f = new File(root, fileName)
      if (isChunkName(fileName))
         PrintSink
      else
      if (!force && f.canRead && f.lastModified >= sourceModified)
      {
        System.err.println("=   "+fileName)
        PrintSink
      }
      else
      { if (f.getParentFile!=null) f.getParentFile.mkdirs 
        System.err.println(">   "+fileName)
        new PrintWriter(f, encoding)
      }
    }
    
    def getChunk(chunkName: String)  =
    { 
      chunks.get(mkChunkName(chunkName)) 
    }
    
    def getWriter(atLine: Int, fileName: String) : PrintWriter =
    { streams.get(fileName) match
      { case Some(p) => p
        
        case None    => 
        { val p = makeWriter(fileName)
          streams update (fileName, p) 
          if (compress) 
          {  if (legacy) p.println("/* Extracted by TexTract from: "+file.getName +" */")
          }
          else
             for (i<-0 until atLine) skipLN(p, source(i))
          p 
        }
      }
    }
    
    val expanding = new Stack[String]
    
    def expandingChunks: String =
    { expanding reduceLeft ((l: String, r: String)=>(l+","+r))}
    
    def startExpanding(name: String) 
    { val theName = mkChunkName(name)
      if (expanding contains theName)
         throw new TextException("Cyclic chunk: "+expandingChunks)
      expanding.push(theName)
    }
    
    def stopExpanding(name: String)
    { expanding.pop }
    
    def expandlines(text: String) 
    { val lines = text.split("\n")
      for (i<-0 until lines.length)
      {   val line = lines(i)
          if (isSourceNumber(line))
             makeLineMark(getSourceNumber(line))
          else
          if (isChunkName(line)) 
          {  startExpanding(line)
             getChunk(line) match
             { case Some(text) => 
               { expandlines(text.toString)
               }
               case None       => 
               { System.out.println("?   "+line.trim)
                 outln(commentChunkName(line))
               }
             }
             stopExpanding(line)
          }
          else
          { outln(line)
          }
      }
    }

    
    var ans      = false        // Inside an ans environment
    var send     = false        // true when copying material to out
    var doc      = false        // true when transforming latex into http markup
    
    System.err.println(root)
    
    for (i<-0 until source.length)
    { val m = pat.matcher(source(i))
      if (m.matches)
      { var docFlipped = false
        (m.group(1), m.group(2)) match
        { case ("begin", "ans") => ans = true         
          case ("end",   "ans") => ans = false       
          case ("end",   "doc") => { doc  = false; docFlipped = true }
          case ("end",   kind)  => { send = false; out = PrintSink }
          
          case ("begin", kind) =>
          { val extension = kind match
            { case "class"     => ".java"
              case "hideclass" => ".java"
              case "obj"       => ".scala"
              case "hideobj"   => ".scala"
              case "doc"       => { doc = true; docFlipped = true; "" }
              case "code"      => { "" }
            }
            send = true
            if ((m.groupCount >= fileField) && (m.group(fileField) != null)) 
            {  val fileArg = m group fileField
               if (fileArg!="")
               {  fileName = fileArg
                  if (!isChunkName(fileName)) 
                  { fileName += extension 
                    if (!legacy) setCommentStrings 
                  }
               }
            }
            out = getWriter(i, fileName)
            if (!docFlipped) skipln(source(i))
            if (legacy && ans && !answers) outln("// An answer goes here ...")
          } 
          
          case (_, _) => throw new Error("Catastrophe: pattern match implementation")         
        
        }
        if (legacy && docFlipped) outln(if (doc) "/**" else "*/")
      }
      else if (send)
      { var line = source(i)
        
        if (ans && !answers) 
           { }
        else
        if (isChunkName(line)) 
        {  if (number) makeLineMark(i)
           startExpanding(line)
           getChunk(line) match
           { case Some(text) => 
             { if (number) makeLineMark(i)
               expandlines(text.toString)
             }
             case None       => 
             { System.out.println("?   "+line.trim)
               outln(commentChunkName(line))
             }
           }
           stopExpanding(line)
        }
        else
        {  if (doc) line = transDoc(line)
           if (number) makeLineMark(i)
           outln(line)
        }
      }
      skipAll(source(i))
    }
    closeAll
  }
    
  def process(file: File) =
  { val stream = new FileInputStream(file)
    val size   = stream.available
    val bytes  = new Array[Byte](size)
    stream.read(bytes, 0, size)
    val string = new String(bytes, encoding)
    stream.close
    translate(file, string split "\n")    
  }

  def main (args: Array[String])
  { for (arg<-args)
        if (arg startsWith "-enc=")  
        {
           encoding = arg.substring(5) 
        }
        else
        if (arg startsWith "-code=")  
        {
           codePat=(arg.substring(6)) 
        }
        else
        if (arg startsWith "-comments=")  
        {
           newComments(arg.substring(10)) 
        }
        else
        if (arg startsWith "-pragmas=")  
        {
           newPragmas(arg.substring(9)) 
        }
        else
        if (arg == "-f")             
        {
           force = true
        }
        else
        if (arg == "-l")             
        {
           force = true
        }
        else
        if (arg == "-c")             
        {
           legacy = true
        }
        else
        if (arg == "-n")             
        {
           number   = true
           compress = true
        }
        else
        if (arg startsWith "-c=")   
        { compress = true
          root=new File(arg substring 3) 
        } 
        else
        if (arg startsWith "-d=")   
        { 
          root=new File(arg substring 3) 
        } 
        else
        if (arg == "-a")     
        { 
           answers  = false
           compress = true 
        }
        else
        if (arg startsWith "-a=")     
        { 
           answers  = false
           compress = true 
           root=new File(arg substring 3) 
        }
        else
        if (arg startsWith "-")     
        { 
           System.err.print(
"""
Usage:                   textract [directive | path]*
Directives:
  -c                compress output (eliminate latex source lines)
  -a                suppress classes contained within {ans} environments (implies -c)
  -d=<root>         specify root directory for output  
  -c=<root>         implies -c -d=<root>  
  -a=<root>         implies -a -c=<root> 
  -f                force code generation, even if the generated files are up to date 
  -l                force legacy code block processing 
  -n                place line-number comments on output lines
  -enc=<enc>        Input and output file(s) are in the given encoding (default is UTF8)
  -comments=<specs> Specify output language comment conventions
  -pragmas=<specs>  Specify output language line number pragma conventions
  -code=<specs>     Specify the {code} pattern -- initially 'code'
  
  $Id: textract.scala 75 2016-07-29 15:39:48Z sufrin $
""")   
        }
        else
        { 
          process(new File(arg))
        }
  }   
}

/** Ant Task */
package ant
{ import org.apache.tools.ant.BuildException
  import org.apache.tools.ant.Task
  import org.apache.tools.ant.types._

  class  Textract extends Task
  { val ext = new Textractor
    
    private var file     : File         = null
    private var fileSets : Set[FileSet] = new HashSet[FileSet]
    private var errNone                 = true
    
    def setFile(aFile: File)                     = { file         = aFile }
    def setRoot(aFile: File)                     = { ext.root     = aFile }
    def setAnswers(on: Boolean)                  = { ext.answers  = on }
    def setCompress(on: Boolean)                 = { ext.compress = on }
    def setForce(on: Boolean)                    = { ext.force    = on }
    def setLegacy(on: Boolean)                   = { ext.legacy   = on }
    def setNumber(on: Boolean)                   = { ext.number   = on; ext.compress = on}
    def setErrnone(on: Boolean)                  = { errNone      = on }
    def setEnc(enc: String)                      = { ext.encoding = enc }
    def setCode(code: String)                    = { ext.codePat  = code }
    def setAns(code: String)                     = { ext.ansPat   = code }
    def setComments(spec: String)                = { ext.newComments(spec) }
    def setPragmas(spec: String)                 = { ext.newPragmas(spec) }
    def addConfiguredFileSet(fs: FileSet) : Unit = { fileSets+=fs }
    
    override def execute
    { if (file==null && fileSets.isEmpty)
         throw new BuildException("'file' attribute or  nested <fileset> element required")
      else
      {  val files = new HashSet[File]
         if (file != null) 
            files += file
         for (fileSet<-fileSets)
         { val ds  = fileSet.getDirectoryScanner(getProject)
           val dir = ds.getBasedir()
           for (file<-ds.getIncludedFiles)
               files += new File(dir, file)
         }
         if (files.isEmpty && errNone) 
            throw new BuildException("No source files to process: use 'errnone=off' if this is acceptable")
         for (file<-files) 
         try {
                ext.process(file)
             }
         catch
             { case TextException(s) => throw new BuildException(s)
             }
      }
    }
  }
}

/** Main program for use from command line */
object textract  
{ 
  def main (args: Array[String]) = new Textractor().main(args)
}
  

















