package advent2023.day25

import org.jgrapht.*
import org.jgrapht.graph.*
import org.jgrapht.graph.builder.*
import org.jgrapht.alg.StoerWagnerMinimumCut
import scala.jdk.CollectionConverters.*
import org.jgrapht.alg.connectivity.ConnectivityInspector

def buildGraph =
    val g =
        GraphTypeBuilder
            .undirected[String, DefaultEdge]()
            .allowingMultipleEdges(false)
            .allowingSelfLoops(false)
            .edgeClass(classOf[DefaultEdge])
            .weighted(false)
            .buildGraph()

    scala.io.Source
        .fromResource("day25.txt")
        .getLines()
        .toList
        .flatMap { case s"$e: $es" => es.split(" ").toList.map((e, _)) }
        .foldLeft(new GraphBuilder(g)) { case (a, (source, target)) =>
            a.addEdge(source, target)
        }
        .build()

def part1 =
    val g = buildGraph
    // all nodes
    val nodes = g.vertexSet().asScala

    // min cut - this returns the set of vertices on 1 end of the cut
    val m   = new StoerWagnerMinimumCut[String, DefaultEdge](g)
    val c1s = m.minCut().asScala

    // the second end must be the set difference with the total set of vertices
    val c2s = nodes.diff(m.minCut().asScala)

    // remove all the connecting edges between the 2 ends of the cut
    val edges = for (c1 <- c1s) yield for (c2 <- c2s) yield g.getEdge(c1, c2)
    g.removeAllEdges(edges.flatten.asJava)

    // find the connected components of the remaining graph and the product of the cardinalities
    val ci = new ConnectivityInspector[String, DefaultEdge](g)
    val c  = ci.connectedSets().asScala.map(_.asScala).toSet
    c.map(_.size).reduce(_ * _)

@main def day25 =
    println("\nDay 25\n------------")
    println(s"Part 1: $part1") // 543834
