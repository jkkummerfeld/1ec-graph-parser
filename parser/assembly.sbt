import AssemblyKeys._

assemblySettings

mainClass in assembly := Some("edu.berkeley.nlp.graphparser.JKKMain")

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList("javax", "servlet", xs @ _*)         => MergeStrategy.first
    case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
    case "application.conf" => MergeStrategy.concat
    case "META-INF/MANIFEST.MF" => MergeStrategy.rename
    case x => old(x)
  }
}
