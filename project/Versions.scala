
import sbt.Process

object Versions {
  val scala = "2.11.6"
  
  val version_prefix = "0.13.0"
  val version_suffix = {
    val svnProc = Process(command = "svn", arguments = Seq("info"))
    val sedCommand = "s/^.*Revision:[[:space:]]\\{1,\\}\\([[:digit:]]\\{1,\\}\\).*$/\\1/p"
    val sedProc = Process(command = "sed", arguments = Seq("-n", sedCommand))
    val svnRevision = svnProc.#|(sedProc).!!.trim
    svnRevision
  }
  
  // OTI Trees version

  val version = version_prefix + "-" + version_suffix
  
  // OTI Core version
    
  val oti_core_prefix = "0.13.0"
  val oti_core_suffix = "754"
  val oti_core_version = oti_core_prefix+"-"+oti_core_suffix

}
