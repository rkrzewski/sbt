package sbt
package ivyint

import java.security.MessageDigest
import java.{ util => ju }
import ju.{ Arrays, Date }
// import org.apache.ivy.core.event._
import org.apache.ivy.core
import core.resolve._
import core.resolve.IvyNodeCallers.Caller
import core.module.id.{ ModuleRevisionId }
import core.module.descriptor.DefaultDependencyDescriptor
import core.report.{ ResolveReport, ConfigurationResolveReport }
import core.module.descriptor.{ DefaultDependencyDescriptor, DefaultModuleDescriptor, ModuleDescriptor }
import collection.mutable.Buffer

private[sbt] trait ConsolidatedResolveEngine extends ResolveEngine {
  /**
   * Resolve dependencies of a module described by a module descriptor.
   */
  override def resolve(md0: ModuleDescriptor, options0: ResolveOptions): ResolveReport = {
    val deps = md0.getDependencies.toVector
    val depsString = (deps map { dep =>
      dep.getDependencyRevisionId.toString + ";" +
        (dep.getModuleConfigurations map { x =>
          x + "->" + dep.getDependencyConfigurations(x).mkString(",")
        }).mkString(";")
    }).mkString("\n")
    val sha1 = Hash.toHex(Hash(depsString))
    println(sha1)

    val md1 = new DefaultModuleDescriptor(createID(sbtOrg, "temp-resolve-" + sha1, "1.0"), "release", null, false)
    md1.setLastModified(System.currentTimeMillis)
    for {
      x <- md0.getConfigurations
    } yield md1.addConfiguration(x)

    for {
      x <- md0.getDependencies
    } yield md1.addDependency(x)

    println(md1.toString)
    val options1 = new ResolveOptions(options0)
    options1.setOutputReport(false)
    val report0 = super.resolve(md1, options1)
    val rootNode = new IvyNode(new ResolveData(this, options0), md0)
    val report1 = generateFakeResolveReport(rootNode, md0, md1, report0, options0)
    val cacheManager = getSettings.getResolutionCacheManager
    if (options0.isOutputReport) this.outputReport(report1, cacheManager, options0)

    report1
  }

  private def generateFakeResolveReport(rootNode: IvyNode, md0: ModuleDescriptor, md1: ModuleDescriptor,
    report0: ResolveReport, options: ResolveOptions): ResolveReport = {
    import scala.collection.JavaConversions._
    val report = new ResolveReport(md0, options.getResolveId)
    val reportDate = new Date
    val confs = report0.getConfigurations
    for {
      conf <- confs
    } report.addReport(conf, new ConfigurationResolveReport(this, md0, conf, reportDate, options))

    val dependencies1: ju.List[IvyNode] =
      report0.getDependencies.asInstanceOf[ju.List[IvyNode]] map { node: IvyNode =>
        transformDependency(node, rootNode, md0, md1)
      }
    report.setDependencies(dependencies1, options.getArtifactFilter())
    report.setResolveTime(report0.getResolveTime)
    report
  }

  private def transformDependency(node0: IvyNode, rootNode: IvyNode, md0: ModuleDescriptor, md1: ModuleDescriptor): IvyNode = {
    val containsFakeCaller = (node0.getAllCallers exists { caller: Caller =>
      caller.getModuleRevisionId == md1.getModuleRevisionId
    })
    if (containsFakeCaller) {
      println("transformDependency: node0: " + node0.toString)
      val dd = Option(node0.getDependencyDescriptor(node0.getRoot)) getOrElse { new DefaultDependencyDescriptor(node0.getId, false) }
      val node1 = new IvyNode(node0.getData, rootNode, dd)
      for {
        rootModuleConf <- md0.getConfigurationsNames
        caller <- node0.getCallers(rootModuleConf)
        callerConf <- caller.getCallerConfigurations
      } {
        node1.
        node1.addCaller(rootModuleConf, rootNode, callerConf, callerConf,
          caller.getDependencyDescriptor.getDependencyConfigurations(callerConf), caller.getDependencyDescriptor)
      }
      node1
    } else node0
  }

  private def createID(organization: String, name: String, revision: String) =
    ModuleRevisionId.newInstance(organization, name, revision)
  private def sbtOrg = "org.scala-sbt"
}
